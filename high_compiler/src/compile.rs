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
                hast::IdentDeclaration::Array { length, .. } => *length,
            };

            total_offset += size;
            let info = StackVarInfo { size, offset: total_offset - 1 };

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
    name_to_params: HashMap<Arc<str>, Arc<Vec<Arc<hast::IdentDeclaration>>>>,
}

impl FuncManager {
    fn try_new(procs: &[Arc<link::Proc>]) -> anyhow::Result<Self> {
        let mut name_to_head_uuid = HashMap::new();
        let mut name_to_params = HashMap::new();

        for proc in procs {
            let link::ProcKind::Func { name, params } = &proc.kind else { continue };

            let Some(first_sp) = proc.sub_procs.first() else {
                bail!("procedure `{}` has no sub-procedures", name);
            };

            name_to_head_uuid.insert(name.clone(), first_sp.uuid);
            name_to_params.insert(name.clone(), params.clone());
        }

        Ok(Self { name_to_head_uuid, name_to_params })
    }

    fn get_head_uuid(&self, name: &str) -> anyhow::Result<Uuid> {
        let Some(uuid) = self.name_to_head_uuid.get(name) else {
            bail!("no head UUID registered for function `{}`", name)
        };

        Ok(*uuid)
    }

    fn get_params(&self, name: &str) -> anyhow::Result<&Arc<Vec<Arc<hast::IdentDeclaration>>>> {
        let Some(params) = self.name_to_params.get(name) else {
            bail!("no params registered for function `{}`", name)
        };

        Ok(params)
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
    let jump_loc = temp();

    Arc::new(opt::Proc {
        kind: Arc::new(opt::ProcKind::Main),
        sub_procs: Arc::new(Vec::from([Arc::new(opt::SubProc {
            uuid: Uuid::new_v4(),
            commands: Arc::new(Vec::from([
                // initialize stack pointer to point to -1
                // (main func stack frame doesn't need return address)
                Arc::new(opt::Command::Set(Arc::new(opt::SetArgs {
                    dest: Arc::new(opt::UMemLoc::StackPointer),
                    value: Arc::new(opt::Value::Literal((-1).to_string().into())),
                }))),
                // jump to user main
                Arc::new(opt::Command::Set(Arc::new(opt::SetArgs {
                    dest: jump_loc.clone(),
                    value: Arc::new(opt::Value::Label(user_main_sp_uuid)),
                }))),
                Arc::new(opt::Command::Jump(Arc::new(opt::VoidArgs { src: jump_loc }))),
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
            Arc::new(opt::Command::Set(Arc::new(opt::SetArgs {
                dest: Arc::new(opt::UMemLoc::Temp(stack_frame_size_loc.clone())),
                value: Arc::new(opt::Value::Literal(stack_frame.size().to_string().into())),
            }))),
            Arc::new(opt::Command::Add(Arc::new(opt::BinaryArgs {
                dest: Arc::new(opt::UMemLoc::StackPointer),
                left: Arc::new(opt::UMemLoc::StackPointer),
                right: Arc::new(opt::UMemLoc::Temp(stack_frame_size_loc)),
            }))),
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
        link::Call::Func { name, param_exprs, return_sub_proc } => {
            let param_idents = func_manager.get_params(name)?;
            let mut param_stack_offset = 0;
            let mut param_setup_commands = Vec::new();
            for (ident, expr) in param_idents.iter().zip(param_exprs.as_ref()) {
                let elements = get_assign_expr_elements(expr);

                for (i, element) in elements.iter().enumerate() {
                    let compiled_pipeline = compile_pipeline(stack_frame, element)?;
                    param_setup_commands.extend(compiled_pipeline.commands);

                    let param_offset_loc = temp();
                    let param_addr_loc = temp();

                    let assign_commands = [
                        Arc::new(opt::Command::Set(Arc::new(opt::SetArgs {
                            dest: param_offset_loc.clone(),
                            value: Arc::new(opt::Value::Literal(
                                (param_stack_offset + i + 2).to_string().into(),
                            )),
                        }))),
                        Arc::new(opt::Command::Add(Arc::new(opt::BinaryArgs {
                            dest: param_addr_loc.clone(),
                            left: Arc::new(opt::UMemLoc::StackPointer),
                            right: param_offset_loc.clone(),
                        }))),
                        Arc::new(opt::Command::CopyDerefDest(Arc::new(opt::UnaryArgs {
                            dest: param_addr_loc,
                            src: compiled_pipeline.mem_loc,
                        }))),
                    ];

                    param_setup_commands.extend(assign_commands);
                }

                param_stack_offset += ident.size();
            }

            let return_setup_commands = {
                let return_offset_loc = temp();
                let return_addr_loc = temp();
                let return_label_loc = temp();

                [
                    Arc::new(opt::Command::Set(Arc::new(opt::SetArgs {
                        dest: return_offset_loc.clone(),
                        value: Arc::new(opt::Value::Literal(1.to_string().into())),
                    }))),
                    Arc::new(opt::Command::Add(Arc::new(opt::BinaryArgs {
                        dest: return_addr_loc.clone(),
                        left: Arc::new(opt::UMemLoc::StackPointer),
                        right: return_offset_loc.clone(),
                    }))),
                    Arc::new(opt::Command::Set(Arc::new(opt::SetArgs {
                        dest: return_label_loc.clone(),
                        value: Arc::new(opt::Value::Label(*return_sub_proc)),
                    }))),
                    Arc::new(opt::Command::CopyDerefDest(Arc::new(opt::UnaryArgs {
                        dest: return_addr_loc,
                        src: return_label_loc,
                    }))),
                ]
            };

            let jump_commands = {
                let func_head_uuid = func_manager.get_head_uuid(name)?;
                let jump_label_loc = temp();

                [
                    Arc::new(opt::Command::Set(Arc::new(opt::SetArgs {
                        dest: jump_label_loc.clone(),
                        value: Arc::new(opt::Value::Label(func_head_uuid)),
                    }))),
                    Arc::new(opt::Command::Jump(Arc::new(opt::VoidArgs { src: jump_label_loc }))),
                ]
            };

            chain!(param_setup_commands, return_setup_commands, jump_commands).collect()
        },
        link::Call::SubProc(uuid) => {
            let return_label_loc = temp();

            Vec::from([
                Arc::new(opt::Command::Set(Arc::new(opt::SetArgs {
                    dest: return_label_loc.clone(),
                    value: Arc::new(opt::Value::Label(*uuid)),
                }))),
                Arc::new(opt::Command::Jump(Arc::new(opt::VoidArgs { src: return_label_loc }))),
            ])
        },
        link::Call::IfElseBranch { cond_pipeline, then_sub_proc, else_sub_proc } => {
            let compiled_cond = compile_pipeline(stack_frame, cond_pipeline)?;

            let then_label_loc = temp();
            let else_label_loc = temp();

            let branch_commands = Vec::from([
                Arc::new(opt::Command::Set(Arc::new(opt::SetArgs {
                    dest: then_label_loc.clone(),
                    value: Arc::new(opt::Value::Label(*then_sub_proc)),
                }))),
                Arc::new(opt::Command::Branch(Arc::new(opt::CondArgs {
                    cond: compiled_cond.mem_loc,
                    src: then_label_loc,
                }))),
                Arc::new(opt::Command::Set(Arc::new(opt::SetArgs {
                    dest: else_label_loc.clone(),
                    value: Arc::new(opt::Value::Label(*else_sub_proc)),
                }))),
                Arc::new(opt::Command::Jump(Arc::new(opt::VoidArgs { src: else_label_loc }))),
            ]);

            chain!(compiled_cond.commands, branch_commands).collect()
        },
        link::Call::Return => {
            // free stack vars
            let stack_frame_commands = {
                let stack_frame_size_loc = Arc::new(opt::TempVar::new());

                [
                    Arc::new(opt::Command::Set(Arc::new(opt::SetArgs {
                        dest: Arc::new(opt::UMemLoc::Temp(stack_frame_size_loc.clone())),
                        value: Arc::new(opt::Value::Literal(stack_frame.size().to_string().into())),
                    }))),
                    Arc::new(opt::Command::Sub(Arc::new(opt::BinaryArgs {
                        dest: Arc::new(opt::UMemLoc::StackPointer),
                        left: Arc::new(opt::UMemLoc::StackPointer),
                        right: Arc::new(opt::UMemLoc::Temp(stack_frame_size_loc)),
                    }))),
                ]
            };

            // get return label and jump there
            let jump_return_commands = {
                let label_offset_loc = temp();
                let label_addr_loc = temp();
                let label_loc = temp();

                [
                    Arc::new(opt::Command::Set(Arc::new(opt::SetArgs {
                        dest: label_offset_loc.clone(),
                        value: Arc::new(opt::Value::Literal(1.to_string().into())),
                    }))),
                    Arc::new(opt::Command::Add(Arc::new(opt::BinaryArgs {
                        dest: label_addr_loc.clone(),
                        left: Arc::new(opt::UMemLoc::StackPointer),
                        right: label_offset_loc,
                    }))),
                    Arc::new(opt::Command::Deref(Arc::new(opt::UnaryArgs {
                        dest: label_loc.clone(),
                        src: label_addr_loc,
                    }))),
                    Arc::new(opt::Command::Jump(Arc::new(opt::VoidArgs { src: label_loc }))),
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
            let elements = get_assign_expr_elements(&assign.expr);
            let mut element_assignments = Vec::new();

            for (i, element) in elements.iter().enumerate() {
                let compiled_ident_addr = compile_ident_to_addr(stack_frame, &assign.ident)?;
                let compiled_offset = compile_pipeline(
                    stack_frame,
                    &hast::Pipeline {
                        initial_val: Arc::new(hast::Value::Literal(i.to_string().into())),
                        operations: Arc::new(Vec::new()),
                    },
                )?;

                let compile_dest_addr =
                    compile_addr_offset(compiled_ident_addr.mem_loc, compiled_offset.mem_loc)?;

                let compiled_pipeline = compile_pipeline(stack_frame, element)?;

                let mut assignments = chain!(
                    compiled_pipeline.commands,
                    compiled_ident_addr.commands,
                    compiled_offset.commands,
                    compile_dest_addr.commands
                )
                .collect_vec();

                if assign.deref_ident {
                    // deref var
                    assignments.push(Arc::new(opt::Command::Deref(Arc::new(opt::UnaryArgs {
                        dest: compile_dest_addr.mem_loc.clone(),
                        src: compile_dest_addr.mem_loc.clone(),
                    }))));
                }

                // assign to var
                assignments.push(Arc::new(opt::Command::CopyDerefDest(Arc::new(opt::UnaryArgs {
                    dest: compile_dest_addr.mem_loc,
                    src: compiled_pipeline.mem_loc,
                }))));

                element_assignments.extend(assignments);
            }

            element_assignments
        },
        link::Statement::Native(native) => match native.as_ref() {
            hast::NativeOperation::Out { ident } => {
                let compiled_ident_addr = compile_ident_to_addr(stack_frame, ident)?;
                let ident_value_loc = temp();

                chain!(
                    compiled_ident_addr.commands,
                    [
                        Arc::new(opt::Command::Deref(Arc::new(opt::UnaryArgs {
                            dest: ident_value_loc.clone(),
                            src: compiled_ident_addr.mem_loc
                        }))),
                        Arc::new(opt::Command::Out(Arc::new(opt::VoidArgs {
                            src: ident_value_loc
                        }))),
                    ]
                )
                .collect()
            },
            hast::NativeOperation::In { dest_ident } => {
                let compiled_ident_addr = compile_ident_to_addr(stack_frame, dest_ident)?;
                let answer_loc = temp();

                chain!(
                    compiled_ident_addr.commands,
                    [
                        Arc::new(opt::Command::In(Arc::new(opt::InputArgs {
                            dest: answer_loc.clone()
                        }))),
                        Arc::new(opt::Command::CopyDerefDest(Arc::new(opt::UnaryArgs {
                            dest: compiled_ident_addr.mem_loc,
                            src: answer_loc
                        }))),
                    ]
                )
                .collect()
            },
        },
    };

    Ok(assignments)
}

fn get_assign_expr_elements(expr: &hast::AssignExpr) -> Arc<Vec<Arc<hast::Pipeline>>> {
    match expr {
        hast::AssignExpr::Pipeline(pipeline) => Arc::new(Vec::from([Arc::clone(pipeline)])),
        hast::AssignExpr::Array(array) => array.elements.clone(),
    }
}

fn compile_pipeline(
    stack_frame: &StackFrame,
    pipeline: &hast::Pipeline,
) -> anyhow::Result<CompiledExpr> {
    let mut commands = Vec::new();

    let initial_val = compile_value(stack_frame, &pipeline.initial_val)?;

    let mut val = initial_val.mem_loc;
    commands.extend(initial_val.commands);

    for operation in pipeline.operations.as_ref() {
        let compiled_operation = match operation.as_ref() {
            hast::Operation::Deref => compile_unary_operation(|out| {
                Vec::from([Arc::new(opt::Command::Deref(Arc::new(opt::UnaryArgs {
                    dest: out,
                    src: val.clone(),
                })))])
            }),
            hast::Operation::Add { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::Add(Arc::new(opt::BinaryArgs {
                        dest: out,
                        left: val.clone(),
                        right: operand,
                    })))])
                })
            },
            hast::Operation::Sub { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::Sub(Arc::new(opt::BinaryArgs {
                        dest: out,
                        left: val.clone(),
                        right: operand,
                    })))])
                })
            },
            hast::Operation::Mul { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::Mul(Arc::new(opt::BinaryArgs {
                        dest: out,
                        left: val.clone(),
                        right: operand,
                    })))])
                })
            },
            hast::Operation::Div { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::Div(Arc::new(opt::BinaryArgs {
                        dest: out,
                        left: val.clone(),
                        right: operand,
                    })))])
                })
            },
            hast::Operation::Mod { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::Mod(Arc::new(opt::BinaryArgs {
                        dest: out,
                        left: val.clone(),
                        right: operand,
                    })))])
                })
            },
            hast::Operation::Eq { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::Eq(Arc::new(opt::BinaryArgs {
                        dest: out,
                        left: val.clone(),
                        right: operand,
                    })))])
                })
            },
            hast::Operation::Neq { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::Neq(Arc::new(opt::BinaryArgs {
                        dest: out,
                        left: val.clone(),
                        right: operand,
                    })))])
                })
            },
            hast::Operation::Gt { operand } => todo!(),
            hast::Operation::Lt { operand } => todo!(),
            hast::Operation::Gte { operand } => todo!(),
            hast::Operation::Lte { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::Lte(Arc::new(opt::BinaryArgs {
                        dest: out,
                        left: val.clone(),
                        right: operand,
                    })))])
                })
            },
            hast::Operation::And { operand } => todo!(),
            hast::Operation::Or { operand } => todo!(),
            hast::Operation::Xor { operand } => todo!(),
            hast::Operation::Not => compile_unary_operation(|out| {
                Vec::from([Arc::new(opt::Command::Not(Arc::new(opt::UnaryArgs {
                    dest: out,
                    src: val.clone(),
                })))])
            }),
            hast::Operation::Join { operand } => todo!(),
        }?;

        commands.extend(compiled_operation.commands);
        val = compiled_operation.output_loc;
    }

    let compiled = CompiledExpr { mem_loc: val, commands };
    Ok(compiled)
}

struct CompiledOperation {
    commands: Vec<Arc<opt::Command<opt::UMemLoc>>>,
    output_loc: Arc<opt::UMemLoc>,
}

fn compile_unary_operation(
    compiler: impl FnOnce(Arc<opt::UMemLoc>) -> Vec<Arc<opt::Command<opt::UMemLoc>>>,
) -> anyhow::Result<CompiledOperation> {
    let output_loc = temp();
    let commands = compiler(output_loc.clone());

    Ok(CompiledOperation { output_loc, commands })
}

fn compile_binary_operation(
    stack_frame: &StackFrame,
    operand: &hast::Value,
    compiler: impl FnOnce(Arc<opt::UMemLoc>, Arc<opt::UMemLoc>) -> Vec<Arc<opt::Command<opt::UMemLoc>>>,
) -> anyhow::Result<CompiledOperation> {
    let compiled_value = compile_value(stack_frame, operand)?;
    let value_loc = compiled_value.mem_loc;

    let output_loc = temp();

    let operation_commands = compiler(value_loc, output_loc.clone());

    let commands = chain!(compiled_value.commands, operation_commands).collect();

    Ok(CompiledOperation { commands, output_loc })
}

fn compile_value(stack_frame: &StackFrame, value: &hast::Value) -> anyhow::Result<CompiledExpr> {
    let compiled = match value {
        hast::Value::Literal(literal) => {
            let temp = Arc::new(opt::TempVar::new());
            let assignments = Vec::from([Arc::new(opt::Command::Set(Arc::new(opt::SetArgs {
                dest: Arc::new(opt::UMemLoc::Temp(temp.clone())),
                value: Arc::new(opt::Value::Literal(literal.clone())),
            })))]);

            CompiledExpr { mem_loc: Arc::new(opt::UMemLoc::Temp(temp)), commands: assignments }
        },
        hast::Value::Ident(ident) => {
            let compiled_ident_addr = compile_ident_to_addr(stack_frame, ident)?;

            let ident_value_loc = temp();

            let assignments = chain!(
                compiled_ident_addr.commands,
                [Arc::new(opt::Command::Deref(Arc::new(opt::UnaryArgs {
                    dest: ident_value_loc.clone(),
                    src: compiled_ident_addr.mem_loc,
                })))]
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
        hast::Ident::Var { name } => compile_ident_name_to_start_addr(stack_frame, name)?,
        hast::Ident::Array { name, index } => {
            let compiled_index = compile_pipeline(stack_frame, index)?;
            let compiled_ident_name = compile_ident_name_to_start_addr(stack_frame, name)?;
            let offset_addr =
                compile_addr_offset(compiled_ident_name.mem_loc, compiled_index.mem_loc)?;

            let commands =
                chain!(compiled_index.commands, compiled_ident_name.commands, offset_addr.commands)
                    .collect();

            CompiledExpr { mem_loc: offset_addr.mem_loc, commands }
        },
    };

    Ok(compiled)
}

fn compile_addr_offset(
    addr_loc: Arc<opt::UMemLoc>,
    offset_loc: Arc<opt::UMemLoc>,
) -> anyhow::Result<CompiledExpr> {
    let indexed_addr_loc = temp();

    let index_commands = [Arc::new(opt::Command::Add(Arc::new(opt::BinaryArgs {
        dest: indexed_addr_loc.clone(),
        left: addr_loc,
        right: offset_loc,
    })))];

    let commands = chain!(index_commands).collect();

    Ok(CompiledExpr { mem_loc: indexed_addr_loc, commands })
}

fn compile_ident_name_to_start_addr(
    stack_frame: &StackFrame,
    ident_name: &str,
) -> anyhow::Result<CompiledExpr> {
    let var_info = stack_frame.get_info(ident_name)?;
    let stack_offset_loc = temp(); // offset of var within the current stack frame
    let stack_addr_loc = temp();

    let commands = Vec::from([
        Arc::new(opt::Command::Set(Arc::new(opt::SetArgs {
            dest: stack_offset_loc.clone(),
            value: Arc::new(opt::Value::Literal(var_info.offset.to_string().into())),
        }))),
        Arc::new(opt::Command::Sub(Arc::new(opt::BinaryArgs {
            dest: stack_addr_loc.clone(),
            left: Arc::new(opt::UMemLoc::StackPointer),
            right: stack_offset_loc,
        }))),
    ]);

    Ok(CompiledExpr { mem_loc: stack_addr_loc, commands })
}

fn temp() -> Arc<opt::UMemLoc> {
    Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())))
}

#[derive(Debug)]
struct CompiledExpr {
    mem_loc: Arc<opt::UMemLoc>,
    commands: Vec<Arc<opt::Command<opt::UMemLoc>>>,
}
