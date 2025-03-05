use std::{collections::HashMap, sync::Arc};

use crate::ast as hast;
use crate::link;
use anyhow::bail;
use itertools::chain;
use itertools::Itertools;
use mem_opt::ast as opt;
use uuid::Uuid;

struct StackFrame {
    ident_name_to_info: HashMap<Arc<str>, Arc<StackVarInfo>>,
}

struct StackVarInfo {
    offset: usize,
    size: usize,
}

impl StackFrame {
    fn new(idents: &[Arc<hast::IdentDeclaration>]) -> Self {
        let mut ident_name_to_info = HashMap::new();
        let mut total_offset = 0;

        for ident in idents.iter().rev() {
            let size = match ident.as_ref() {
                hast::IdentDeclaration::Value { .. } => 1,
                hast::IdentDeclaration::Array { length, .. } => *length,
            };

            let info = StackVarInfo { size, offset: total_offset };

            total_offset += info.size;
            ident_name_to_info.insert(ident.name().clone(), Arc::new(info));
        }

        Self { ident_name_to_info }
    }

    fn get_info(&self, name: &str) -> anyhow::Result<&Arc<StackVarInfo>> {
        let Some(info) = self.ident_name_to_info.get(name) else {
            bail!("ident name {} is not in current stack frame", name);
        };

        Ok(info)
    }

    fn size(&self) -> usize {
        let return_addr_size = 1;
        let vars_size = self.ident_name_to_info.values().map(|i| i.size).sum::<usize>();
        return_addr_size + vars_size
    }
}

struct FuncManager {
    name_to_head_uuid: HashMap<Arc<str>, Uuid>,
}

impl FuncManager {
    fn try_new(procs: &[Arc<link::Proc>]) -> anyhow::Result<Self> {
        let name_to_head_uuid = procs
            .iter()
            .filter_map(|proc| match &proc.kind {
                link::ProcKind::Main => None,
                link::ProcKind::Func { name, .. } => Some((name, &proc.sub_procs)),
            })
            .map(|(name, sps)| {
                let Some(first_sp) = sps.first() else {
                    bail!("procedure `{}` has no sub-procedures", name);
                };

                Ok((name.clone(), first_sp.uuid))
            })
            .collect::<Result<HashMap<_, _>, _>>()?;

        Ok(Self { name_to_head_uuid })
    }

    fn get_head_uuid(&self, name: &str) -> anyhow::Result<Uuid> {
        let Some(uuid) = self.name_to_head_uuid.get(name) else {
            bail!("no head UUID registered for function `{}`", name)
        };

        Ok(*uuid)
    }
}

pub fn compile(linked: Vec<Arc<link::Proc>>) -> anyhow::Result<Vec<Arc<opt::Proc<opt::UMemLoc>>>> {
    let user_main_sp_uuid = find_user_main_sp_uuid(&linked)?;
    let func_manager = FuncManager::try_new(&linked)?;
    let opt_procs = linked
        .into_iter()
        .map(|p| compile_proc(&func_manager, &p))
        .collect::<Result<Vec<_>, _>>()?;

    let opt_procs = chain!([create_runner_proc(user_main_sp_uuid)], opt_procs).collect();
    Ok(opt_procs)
}

fn find_user_main_sp_uuid(procs: &[Arc<link::Proc>]) -> anyhow::Result<Uuid> {
    let Some(main_proc) = procs.iter().find(|proc| matches!(proc.kind, link::ProcKind::Main))
    else {
        bail!("could not find user main procedure");
    };

    let Some(first_sp) = main_proc.sub_procs.first() else {
        bail!("user main procedure has no sub-procedures");
    };

    Ok(first_sp.uuid)
}

fn create_runner_proc(user_main_sp_uuid: Uuid) -> Arc<opt::Proc<opt::UMemLoc>> {
    let jump_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));

    Arc::new(opt::Proc {
        kind: Arc::new(opt::ProcKind::Main),
        sub_procs: Arc::new(Vec::from([Arc::new(opt::SubProc {
            uuid: Uuid::new_v4(),
            commands: Arc::new(Vec::from([
                // initialize stack pointer to point to -1
                // (main func stack frame doesn't need return address)
                Arc::new(opt::Command::Set {
                    dest: Arc::new(opt::UMemLoc::StackPointer),
                    value: Arc::new(opt::Value::Literal((-1).to_string().into())),
                }),
                // jump to user main
                Arc::new(opt::Command::Set {
                    dest: jump_loc.clone(),
                    value: Arc::new(opt::Value::Label(user_main_sp_uuid)),
                }),
                Arc::new(opt::Command::Jump { src: jump_loc }),
            ])),
        })])),
    })
}

fn compile_proc(
    func_manager: &FuncManager,
    proc: &link::Proc,
) -> anyhow::Result<Arc<opt::Proc<opt::UMemLoc>>> {
    let (kind, stack_params) = match &proc.kind {
        link::ProcKind::Main => (opt::ProcKind::Func { name: "main".into() }, Vec::new()),
        link::ProcKind::Func { name, params } => (
            opt::ProcKind::Func { name: format!("func.{}", name).into() },
            params.iter().cloned().collect(),
        ),
    };

    let stack_vars = proc.idents.iter().cloned();
    let stack_idents = chain!(stack_params, stack_vars).collect_vec();

    let stack_frame = StackFrame::new(&stack_idents);
    let sub_procs = proc
        .sub_procs
        .iter()
        .enumerate()
        .map(|(i, sp)| compile_sub_proc(&stack_frame, func_manager, sp, i == 0))
        .collect::<Result<Vec<_>, _>>()?;

    let proc = opt::Proc { kind: Arc::new(kind), sub_procs: Arc::new(sub_procs) };

    Ok(Arc::new(proc))
}

fn compile_sub_proc(
    stack_frame: &StackFrame,
    func_manager: &FuncManager,
    sp: &link::SubProc,
    proc_head: bool,
) -> anyhow::Result<Arc<opt::SubProc<opt::UMemLoc>>> {
    // initialize stack vars
    let stack_frame_commands = if proc_head {
        let stack_frame_size_loc = Arc::new(opt::TempVar::new());

        Vec::from([
            Arc::new(opt::Command::Set {
                dest: Arc::new(opt::UMemLoc::Temp(stack_frame_size_loc.clone())),
                value: Arc::new(opt::Value::Literal(stack_frame.size().to_string().into())),
            }),
            Arc::new(opt::Command::Add {
                dest: Arc::new(opt::UMemLoc::StackPointer),
                left: Arc::new(opt::UMemLoc::StackPointer),
                right: Arc::new(opt::UMemLoc::Temp(stack_frame_size_loc)),
            }),
        ])
    } else {
        Vec::new()
    };

    let statement_commands: Vec<_> = sp
        .statements
        .iter()
        .map(|s| compile_statement(stack_frame, s))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .flatten()
        .collect();

    let call_commands = compile_call(stack_frame, func_manager, &sp.next_call)?;

    let commands = chain!(stack_frame_commands, statement_commands, call_commands).collect();

    let sp = opt::SubProc { uuid: sp.uuid, commands: Arc::new(commands) };

    Ok(Arc::new(sp))
}

fn compile_call(
    stack_frame: &StackFrame,
    func_manager: &FuncManager,
    call: &link::Call,
) -> anyhow::Result<Vec<Arc<opt::Command<opt::UMemLoc>>>> {
    let compiled = match call {
        link::Call::Func { name, param_pipelines, return_sub_proc } => {
            let mut param_assignments = Vec::new();
            let mut param_mem_locs = Vec::new();

            for pipeline in param_pipelines.as_ref() {
                let compiled_pipeline = compile_pipeline(stack_frame, pipeline)?;
                param_mem_locs.push(compiled_pipeline.mem_loc);
                param_assignments.extend(compiled_pipeline.commands);
            }

            let return_setup_commands = {
                let return_offset_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));
                let return_addr_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));
                let return_label_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));

                [
                    Arc::new(opt::Command::Set {
                        dest: return_offset_loc.clone(),
                        value: Arc::new(opt::Value::Literal(1.to_string().into())),
                    }),
                    Arc::new(opt::Command::Add {
                        dest: return_addr_loc.clone(),
                        left: Arc::new(opt::UMemLoc::StackPointer),
                        right: return_offset_loc.clone(),
                    }),
                    Arc::new(opt::Command::Set {
                        dest: return_label_loc.clone(),
                        value: Arc::new(opt::Value::Label(*return_sub_proc)),
                    }),
                    Arc::new(opt::Command::CopyDerefDest {
                        dest: return_addr_loc,
                        src: return_label_loc,
                    }),
                ]
            };

            let param_setup_commands =
                param_mem_locs.into_iter().enumerate().flat_map(|(i, param_value_loc)| {
                    let param_offset_loc =
                        Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));
                    let param_addr_loc =
                        Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));

                    Vec::from([
                        Arc::new(opt::Command::Set {
                            dest: param_offset_loc.clone(),
                            value: Arc::new(opt::Value::Literal((i + 2).to_string().into())),
                        }),
                        Arc::new(opt::Command::Add {
                            dest: param_addr_loc.clone(),
                            left: Arc::new(opt::UMemLoc::StackPointer),
                            right: param_offset_loc.clone(),
                        }),
                        Arc::new(opt::Command::CopyDerefDest {
                            dest: param_addr_loc,
                            src: param_value_loc,
                        }),
                    ])
                });

            let jump_commands = {
                let func_head_uuid = func_manager.get_head_uuid(name)?;
                let jump_label_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));

                [
                    Arc::new(opt::Command::Set {
                        dest: jump_label_loc.clone(),
                        value: Arc::new(opt::Value::Label(func_head_uuid)),
                    }),
                    Arc::new(opt::Command::Jump { src: jump_label_loc }),
                ]
            };

            chain!(param_assignments, return_setup_commands, param_setup_commands, jump_commands)
                .collect()
        },
        link::Call::SubProc(uuid) => {
            let return_label_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));

            Vec::from([
                Arc::new(opt::Command::Set {
                    dest: return_label_loc.clone(),
                    value: Arc::new(opt::Value::Label(*uuid)),
                }),
                Arc::new(opt::Command::Jump { src: return_label_loc }),
            ])
        },
        link::Call::IfElseBranch { cond_pipeline, then_sub_proc, else_sub_proc } => {
            let compiled_cond = compile_pipeline(stack_frame, cond_pipeline)?;

            let then_label_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));
            let else_label_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));

            let branch_commands = Vec::from([
                Arc::new(opt::Command::Set {
                    dest: then_label_loc.clone(),
                    value: Arc::new(opt::Value::Label(*then_sub_proc)),
                }),
                Arc::new(opt::Command::Branch {
                    cond: compiled_cond.mem_loc,
                    label: then_label_loc,
                }),
                Arc::new(opt::Command::Set {
                    dest: else_label_loc.clone(),
                    value: Arc::new(opt::Value::Label(*else_sub_proc)),
                }),
                Arc::new(opt::Command::Jump { src: else_label_loc }),
            ]);

            chain!(compiled_cond.commands, branch_commands).collect()
        },
        link::Call::Return => {
            // free stack vars
            let stack_frame_commands = {
                let stack_frame_size_loc = Arc::new(opt::TempVar::new());

                [
                    Arc::new(opt::Command::Set {
                        dest: Arc::new(opt::UMemLoc::Temp(stack_frame_size_loc.clone())),
                        value: Arc::new(opt::Value::Literal(stack_frame.size().to_string().into())),
                    }),
                    Arc::new(opt::Command::Sub {
                        dest: Arc::new(opt::UMemLoc::StackPointer),
                        left: Arc::new(opt::UMemLoc::StackPointer),
                        right: Arc::new(opt::UMemLoc::Temp(stack_frame_size_loc)),
                    }),
                ]
            };

            // get return label and jump there
            let jump_return_commands = {
                let label_offset_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));
                let label_addr_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));
                let label_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));

                [
                    Arc::new(opt::Command::Set {
                        dest: label_offset_loc.clone(),
                        value: Arc::new(opt::Value::Literal(1.to_string().into())),
                    }),
                    Arc::new(opt::Command::Add {
                        dest: label_addr_loc.clone(),
                        left: Arc::new(opt::UMemLoc::StackPointer),
                        right: label_offset_loc,
                    }),
                    Arc::new(opt::Command::Deref { dest: label_loc.clone(), src: label_addr_loc }),
                    Arc::new(opt::Command::Jump { src: label_loc }),
                ]
            };

            chain!(stack_frame_commands, jump_return_commands).collect()
        },
        link::Call::Terminate => Vec::from([Arc::new(opt::Command::Exit)]),
    };

    Ok(compiled)
}

fn compile_statement(
    stack_frame: &StackFrame,
    statement: &link::Statement,
) -> anyhow::Result<Vec<Arc<opt::Command<opt::UMemLoc>>>> {
    let assignments = match statement {
        link::Statement::Assign(assign) => {
            let compiled_ident_addr = compile_ident_to_addr(stack_frame, &assign.ident)?;

            let compiled_pipeline = compile_pipeline(stack_frame, &assign.pipeline)?;

            let mut assignments =
                chain!(compiled_pipeline.commands, compiled_ident_addr.commands,).collect_vec();

            if assign.deref_ident {
                // deref var
                assignments.push(Arc::new(opt::Command::Deref {
                    dest: compiled_ident_addr.mem_loc.clone(),
                    src: compiled_ident_addr.mem_loc.clone(),
                }));
            }

            // assign to var
            assignments.push(Arc::new(opt::Command::CopyDerefDest {
                dest: compiled_ident_addr.mem_loc,
                src: compiled_pipeline.mem_loc,
            }));

            assignments
        },
        link::Statement::Native(native) => match native.as_ref() {
            hast::NativeOperation::Out { ident } => {
                let compiled_ident_addr = compile_ident_to_addr(stack_frame, ident)?;
                let ident_value_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));

                chain!(
                    compiled_ident_addr.commands,
                    [
                        Arc::new(opt::Command::Deref {
                            dest: ident_value_loc.clone(),
                            src: compiled_ident_addr.mem_loc
                        }),
                        Arc::new(opt::Command::Out { src: ident_value_loc })
                    ]
                )
                .collect()
            },
            hast::NativeOperation::In { dest_ident } => {
                let compiled_ident_addr = compile_ident_to_addr(stack_frame, dest_ident)?;
                let answer_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));

                chain!(
                    compiled_ident_addr.commands,
                    [
                        Arc::new(opt::Command::In { dest: answer_loc.clone() }),
                        Arc::new(opt::Command::CopyDerefDest {
                            dest: compiled_ident_addr.mem_loc,
                            src: answer_loc
                        }),
                    ]
                )
                .collect()
            },
        },
    };

    Ok(assignments)
}

fn compile_pipeline(
    stack_frame: &StackFrame,
    pipeline: &hast::Pipeline,
) -> anyhow::Result<CompiledExpr> {
    let mut assignments = Vec::new();

    let initial_val = compile_value(stack_frame, &pipeline.initial_val)?;

    let mut val = initial_val.mem_loc;
    assignments.extend(initial_val.commands);

    for operation in pipeline.operations.as_ref() {
        match operation.as_ref() {
            hast::Operation::Deref => todo!(),
            hast::Operation::Add { operand } => {
                let compiled_value = compile_value(stack_frame, operand)?;
                let value_mem_loc = compiled_value.mem_loc;
                assignments.extend(compiled_value.commands);

                let output_mem_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));
                assignments.push(Arc::new(opt::Command::Add {
                    dest: output_mem_loc.clone(),
                    left: val.clone(),
                    right: value_mem_loc,
                }));

                val = output_mem_loc;
            },
            hast::Operation::Sub { operand } => todo!(),
            hast::Operation::Mul { operand } => todo!(),
            hast::Operation::Div { operand } => todo!(),
            hast::Operation::Mod { operand } => todo!(),
            hast::Operation::Eq { operand } => todo!(),
            hast::Operation::Neq { operand } => todo!(),
            hast::Operation::Gt { operand } => todo!(),
            hast::Operation::Lt { operand } => todo!(),
            hast::Operation::Gte { operand } => todo!(),
            hast::Operation::Lte { operand } => todo!(),
            hast::Operation::And { operand } => todo!(),
            hast::Operation::Or { operand } => todo!(),
            hast::Operation::Xor { operand } => todo!(),
            hast::Operation::Not => todo!(),
            hast::Operation::Join { operand } => todo!(),
        }
    }

    let compiled = CompiledExpr { mem_loc: val, commands: assignments };
    Ok(compiled)
}

fn compile_value(stack_frame: &StackFrame, value: &hast::Value) -> anyhow::Result<CompiledExpr> {
    let compiled = match value {
        hast::Value::Literal(literal) => {
            let temp = Arc::new(opt::TempVar::new());
            let assignments = Vec::from([Arc::new(opt::Command::Set {
                dest: Arc::new(opt::UMemLoc::Temp(temp.clone())),
                value: Arc::new(opt::Value::Literal(literal.clone())),
            })]);

            CompiledExpr { mem_loc: Arc::new(opt::UMemLoc::Temp(temp)), commands: assignments }
        },
        hast::Value::Ident(ident) => {
            let compiled_ident_addr = compile_ident_to_addr(stack_frame, ident)?;

            let ident_value_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));

            let assignments = chain!(
                compiled_ident_addr.commands,
                [Arc::new(opt::Command::Deref {
                    dest: ident_value_loc.clone(),
                    src: compiled_ident_addr.mem_loc,
                })]
            )
            .collect();

            CompiledExpr { mem_loc: ident_value_loc, commands: assignments }
        },
        hast::Value::Ref(ident) => compile_ident_to_addr(stack_frame, ident)?,
    };

    Ok(compiled)
}

fn compile_ident_to_addr(
    stack_frame: &StackFrame,
    ident: &hast::Ident,
) -> anyhow::Result<CompiledExpr> {
    let compiled = match ident {
        hast::Ident::Var { name } => {
            let var_info = stack_frame.get_info(name)?;
            let stack_offset_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new()))); // offset of var within the current stack frame
            let stack_addr_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));

            let assignments = Vec::from([
                Arc::new(opt::Command::Set {
                    dest: stack_offset_loc.clone(),
                    value: Arc::new(opt::Value::Literal(var_info.offset.to_string().into())),
                }),
                Arc::new(opt::Command::Sub {
                    dest: stack_addr_loc.clone(),
                    left: Arc::new(opt::UMemLoc::StackPointer),
                    right: stack_offset_loc,
                }),
            ]);

            CompiledExpr { mem_loc: stack_addr_loc, commands: assignments }
        },
        hast::Ident::Array { name, index } => {
            // let compiled_index = compile_pipeline(index);

            // CompiledExpr {
            //     mem_loc: Arc::new(opt::UMemLoc::Stack {
            //         var: todo!(),
            //         offset: Some(compiled_index.mem_loc),
            //     }),
            //     assignments: compiled_index.assignments,
            // }

            todo!()
        },
    };

    Ok(compiled)
}

#[derive(Debug)]
struct CompiledExpr {
    mem_loc: Arc<opt::UMemLoc>,
    commands: Vec<Arc<opt::Command<opt::UMemLoc>>>,
}
