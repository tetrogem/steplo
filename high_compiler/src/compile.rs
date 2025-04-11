use std::{collections::HashMap, sync::Arc};

use crate::ast as hast;
use crate::link;
use anyhow::bail;
use itertools::chain;
use itertools::Itertools;
use mem_opt::ast;
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

fn stack_pointer() -> Arc<opt::UMemLoc> {
    Arc::new(opt::UMemLoc::StackPointer)
}

fn literal(literal: impl ScratchLiteral) -> Arc<ast::Expr<opt::UMemLoc>> {
    literal.to_expr()
}

trait ScratchLiteral {
    fn to_expr(&self) -> Arc<ast::Expr<opt::UMemLoc>>;
}

impl ScratchLiteral for f64 {
    fn to_expr(&self) -> Arc<ast::Expr<ast::UMemLoc>> {
        Arc::new(ast::Expr::Value(Arc::new(ast::Value::Literal(self.to_string().into()))))
    }
}

impl ScratchLiteral for &str {
    fn to_expr(&self) -> Arc<ast::Expr<ast::UMemLoc>> {
        Arc::new(ast::Expr::Value(Arc::new(ast::Value::Literal((*self).into()))))
    }
}

fn label(uuid: Uuid) -> Arc<ast::Expr<opt::UMemLoc>> {
    Arc::new(ast::Expr::Value(Arc::new(ast::Value::Label(uuid))))
}

fn mem_loc_expr(mem_loc: Arc<opt::UMemLoc>) -> Arc<ast::Expr<opt::UMemLoc>> {
    Arc::new(ast::Expr::MemLoc(mem_loc))
}

fn temp_mem_loc() -> Arc<opt::UMemLoc> {
    Arc::new(opt::UMemLoc::Temp(Arc::new(ast::TempVar::new())))
}

fn binary_args(
    left: Arc<ast::Expr<opt::UMemLoc>>,
    right: Arc<ast::Expr<opt::UMemLoc>>,
) -> Arc<ast::BinaryArgs<opt::UMemLoc>> {
    Arc::new(ast::BinaryArgs { left, right })
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
                Arc::new(opt::Command::SetMemLoc { mem_loc: stack_pointer(), val: literal(-1.0) }),
                // jump to user main
                Arc::new(opt::Command::SetMemLoc {
                    mem_loc: jump_loc.clone(),
                    val: label(user_main_sp_uuid),
                }),
                Arc::new(opt::Command::Jump(mem_loc_expr(jump_loc))),
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
        let stack_frame_size_loc = temp_mem_loc();

        Vec::from([
            Arc::new(opt::Command::SetMemLoc {
                mem_loc: stack_frame_size_loc.clone(),
                val: literal(stack_frame.size() as f64),
            }),
            Arc::new(opt::Command::SetMemLoc {
                mem_loc: stack_pointer(),
                val: Arc::new(ast::Expr::Add(binary_args(
                    mem_loc_expr(stack_pointer()),
                    mem_loc_expr(stack_frame_size_loc),
                ))),
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
        link::Call::Func { name, param_exprs, return_sub_proc } => {
            let param_idents = func_manager.get_params(name)?;
            let mut param_stack_offset = 0;
            let mut param_setup_commands = Vec::new();
            for (ident, expr) in param_idents.iter().zip(param_exprs.as_ref()) {
                let elements = compile_assign_expr_elements(stack_frame, expr)?;

                for (i, element) in elements.into_iter().enumerate() {
                    param_setup_commands.extend(element.commands);

                    let param_offset_loc = temp();
                    let param_addr_loc = temp();

                    let assign_commands = [
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: param_offset_loc.clone(),
                            val: literal((param_stack_offset + i + 2) as f64),
                        }),
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: param_addr_loc.clone(),
                            val: Arc::new(ast::Expr::Add(binary_args(
                                mem_loc_expr(stack_pointer()),
                                mem_loc_expr(param_offset_loc),
                            ))),
                        }),
                        Arc::new(opt::Command::SetStack {
                            addr: mem_loc_expr(param_addr_loc),
                            val: mem_loc_expr(element.mem_loc),
                        }),
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
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: return_offset_loc.clone(),
                        val: literal(1.0),
                    }),
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: return_addr_loc.clone(),
                        val: Arc::new(ast::Expr::Add(binary_args(
                            mem_loc_expr(stack_pointer()),
                            mem_loc_expr(return_offset_loc.clone()),
                        ))),
                    }),
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: return_label_loc.clone(),
                        val: label(*return_sub_proc),
                    }),
                    Arc::new(opt::Command::SetStack {
                        addr: mem_loc_expr(return_addr_loc),
                        val: mem_loc_expr(return_label_loc),
                    }),
                ]
            };

            let jump_commands = {
                let func_head_uuid = func_manager.get_head_uuid(name)?;
                let jump_label_loc = temp();

                [
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: jump_label_loc.clone(),
                        val: label(func_head_uuid),
                    }),
                    Arc::new(opt::Command::Jump(mem_loc_expr(jump_label_loc))),
                ]
            };

            chain!(param_setup_commands, return_setup_commands, jump_commands).collect()
        },
        link::Call::SubProc(uuid) => {
            let return_label_loc = temp();

            Vec::from([
                Arc::new(opt::Command::SetMemLoc {
                    mem_loc: return_label_loc.clone(),
                    val: label(*uuid),
                }),
                Arc::new(opt::Command::Jump(mem_loc_expr(return_label_loc))),
            ])
        },
        link::Call::IfElseBranch { cond_pipeline, then_sub_proc, else_sub_proc } => {
            let compiled_cond = compile_pipeline(stack_frame, cond_pipeline)?;

            let then_label_loc = temp();
            let else_label_loc = temp();

            let branch_commands = Vec::from([
                Arc::new(opt::Command::SetMemLoc {
                    mem_loc: then_label_loc.clone(),
                    val: label(*then_sub_proc),
                }),
                Arc::new(opt::Command::Branch {
                    cond: mem_loc_expr(compiled_cond.mem_loc),
                    to: mem_loc_expr(then_label_loc),
                }),
                Arc::new(opt::Command::SetMemLoc {
                    mem_loc: else_label_loc.clone(),
                    val: label(*else_sub_proc),
                }),
                Arc::new(opt::Command::Jump(mem_loc_expr(else_label_loc))),
            ]);

            chain!(compiled_cond.commands, branch_commands).collect()
        },
        link::Call::Return => {
            // free stack vars
            let stack_frame_commands = {
                let stack_frame_size_loc = temp();

                [
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: stack_frame_size_loc.clone(),
                        val: literal(stack_frame.size() as f64),
                    }),
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: stack_pointer(),
                        val: Arc::new(ast::Expr::Sub(binary_args(
                            mem_loc_expr(stack_pointer()),
                            mem_loc_expr(stack_frame_size_loc),
                        ))),
                    }),
                ]
            };

            // get return label and jump there
            let jump_return_commands = {
                let label_offset_loc = temp();
                let label_addr_loc = temp();
                let label_loc = temp();

                [
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: label_offset_loc.clone(),
                        val: literal(1.0),
                    }),
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: label_addr_loc.clone(),
                        val: Arc::new(ast::Expr::Add(binary_args(
                            mem_loc_expr(stack_pointer()),
                            mem_loc_expr(label_offset_loc),
                        ))),
                    }),
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: label_loc.clone(),
                        val: Arc::new(ast::Expr::Deref(mem_loc_expr(label_addr_loc))),
                    }),
                    Arc::new(opt::Command::Jump(mem_loc_expr(label_loc))),
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
            let elements = compile_assign_expr_elements(stack_frame, &assign.expr)?;
            let mut element_assignments = Vec::new();

            for (i, element) in elements.into_iter().enumerate() {
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

                let mut assignments = chain!(
                    element.commands,
                    compiled_ident_addr.commands,
                    compiled_offset.commands,
                    compile_dest_addr.commands
                )
                .collect_vec();

                if assign.deref_ident {
                    // deref var
                    assignments.push(Arc::new(opt::Command::SetMemLoc {
                        mem_loc: compile_dest_addr.mem_loc.clone(),
                        val: mem_loc_expr(compile_dest_addr.mem_loc.clone()),
                    }));
                }

                // assign to var
                assignments.push(Arc::new(opt::Command::SetStack {
                    addr: mem_loc_expr(compile_dest_addr.mem_loc),
                    val: mem_loc_expr(element.mem_loc),
                }));

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
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: ident_value_loc.clone(),
                            val: Arc::new(ast::Expr::Deref(mem_loc_expr(
                                compiled_ident_addr.mem_loc
                            ))),
                        }),
                        Arc::new(opt::Command::Out(mem_loc_expr(ident_value_loc))),
                    ]
                )
                .collect()
            },
            hast::NativeOperation::In { dest_ident } => {
                let compiled_ident_addr = compile_ident_to_addr(stack_frame, dest_ident)?;

                chain!(
                    compiled_ident_addr.commands,
                    [
                        Arc::new(opt::Command::In),
                        Arc::new(opt::Command::SetStack {
                            addr: mem_loc_expr(compiled_ident_addr.mem_loc),
                            val: Arc::new(ast::Expr::InAnswer),
                        }),
                    ]
                )
                .collect()
            },
        },
    };

    Ok(assignments)
}

fn compile_assign_expr_elements(
    stack_frame: &StackFrame,
    expr: &hast::AssignExpr,
) -> anyhow::Result<Vec<CompiledExpr>> {
    let compileds = match expr {
        hast::AssignExpr::Pipeline(pipeline) => {
            Vec::from([compile_pipeline(stack_frame, pipeline)?])
        },
        hast::AssignExpr::Array(array) => array
            .elements
            .iter()
            .map(|pipeline| compile_pipeline(stack_frame, pipeline))
            .collect::<anyhow::Result<Vec<_>>>()?
            .into_iter()
            .collect(),
        hast::AssignExpr::Slice(slice) => {
            let elements = (slice.start_in..slice.end_ex).map(|i| {
                let ident_addr = compile_ident_to_addr(stack_frame, &slice.ident)?;
                let offset = compile_pipeline(
                    stack_frame,
                    &hast::Pipeline {
                        initial_val: Arc::new(hast::Value::Literal(i.to_string().into())),
                        operations: Arc::new(Vec::new()),
                    },
                )?;

                let element_addr = compile_addr_offset(ident_addr.mem_loc, offset.mem_loc)?;
                let element = compile_addr_deref(&element_addr.mem_loc);

                let commands = chain!(
                    ident_addr.commands,
                    offset.commands,
                    element_addr.commands,
                    element.commands
                )
                .collect();

                Ok(CompiledExpr { mem_loc: element.mem_loc, commands })
            });

            elements.collect::<anyhow::Result<Vec<_>>>()?
        },
    };

    Ok(compileds)
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
                Vec::from([Arc::new(opt::Command::SetMemLoc {
                    mem_loc: out,
                    val: Arc::new(ast::Expr::Deref(mem_loc_expr(val.clone()))),
                })])
            }),
            hast::Operation::Add { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::SetMemLoc {
                        mem_loc: out,
                        val: Arc::new(ast::Expr::Add(binary_args(
                            mem_loc_expr(val.clone()),
                            mem_loc_expr(operand),
                        ))),
                    })])
                })
            },
            hast::Operation::Sub { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::SetMemLoc {
                        mem_loc: out,
                        val: Arc::new(ast::Expr::Sub(binary_args(
                            mem_loc_expr(val.clone()),
                            mem_loc_expr(operand),
                        ))),
                    })])
                })
            },
            hast::Operation::Mul { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::SetMemLoc {
                        mem_loc: out,
                        val: Arc::new(ast::Expr::Mul(binary_args(
                            mem_loc_expr(val.clone()),
                            mem_loc_expr(operand),
                        ))),
                    })])
                })
            },
            hast::Operation::Div { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::SetMemLoc {
                        mem_loc: out,
                        val: Arc::new(ast::Expr::Div(binary_args(
                            mem_loc_expr(val.clone()),
                            mem_loc_expr(operand),
                        ))),
                    })])
                })
            },
            hast::Operation::Mod { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::SetMemLoc {
                        mem_loc: out,
                        val: Arc::new(ast::Expr::Mod(binary_args(
                            mem_loc_expr(val.clone()),
                            mem_loc_expr(operand),
                        ))),
                    })])
                })
            },
            hast::Operation::Eq { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::SetMemLoc {
                        mem_loc: out,
                        val: Arc::new(ast::Expr::Eq(binary_args(
                            mem_loc_expr(val.clone()),
                            mem_loc_expr(operand),
                        ))),
                    })])
                })
            },
            hast::Operation::Neq { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::SetMemLoc {
                        mem_loc: out,
                        val: Arc::new(ast::Expr::Neq(binary_args(
                            mem_loc_expr(val.clone()),
                            mem_loc_expr(operand),
                        ))),
                    })])
                })
            },
            hast::Operation::Gt { operand } => todo!(),
            hast::Operation::Lt { operand } => todo!(),
            hast::Operation::Gte { operand } => todo!(),
            hast::Operation::Lte { operand } => {
                compile_binary_operation(stack_frame, operand, |operand, out| {
                    Vec::from([Arc::new(opt::Command::SetMemLoc {
                        mem_loc: out,
                        val: Arc::new(ast::Expr::Lte(binary_args(
                            mem_loc_expr(val.clone()),
                            mem_loc_expr(operand),
                        ))),
                    })])
                })
            },
            hast::Operation::And { operand } => todo!(),
            hast::Operation::Or { operand } => todo!(),
            hast::Operation::Xor { operand } => todo!(),
            hast::Operation::Not => compile_unary_operation(|out| {
                Vec::from([Arc::new(opt::Command::SetMemLoc {
                    mem_loc: out,
                    val: Arc::new(ast::Expr::Not(mem_loc_expr(val.clone()))),
                })])
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
        hast::Value::Literal(lit) => {
            let value_temp = temp();
            let assignments = Vec::from([Arc::new(opt::Command::SetMemLoc {
                mem_loc: value_temp.clone(),
                val: literal(lit.as_ref()),
            })]);

            CompiledExpr { mem_loc: value_temp, commands: assignments }
        },
        hast::Value::Ident(ident) => {
            let ident_addr = compile_ident_to_addr(stack_frame, ident)?;
            let value = compile_addr_deref(&ident_addr.mem_loc);

            let commands = chain!(ident_addr.commands, value.commands).collect();

            CompiledExpr { mem_loc: value.mem_loc, commands }
        },
        hast::Value::Ref(ident) => compile_ident_to_addr(stack_frame, ident)?,
    };

    Ok(compiled)
}

fn compile_addr_deref(addr: &Arc<opt::UMemLoc>) -> CompiledExpr {
    let ident_value_loc = temp();
    let commands = Vec::from([Arc::new(opt::Command::SetMemLoc {
        mem_loc: ident_value_loc.clone(),
        val: Arc::new(ast::Expr::Deref(mem_loc_expr(addr.clone()))),
    })]);

    CompiledExpr { mem_loc: ident_value_loc, commands }
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

    let index_commands = [Arc::new(opt::Command::SetMemLoc {
        mem_loc: indexed_addr_loc.clone(),
        val: Arc::new(ast::Expr::Add(binary_args(
            mem_loc_expr(addr_loc),
            mem_loc_expr(offset_loc),
        ))),
    })];

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
        Arc::new(opt::Command::SetMemLoc {
            mem_loc: stack_offset_loc.clone(),
            val: literal(var_info.offset as f64),
        }),
        Arc::new(opt::Command::SetMemLoc {
            mem_loc: stack_addr_loc.clone(),
            val: Arc::new(ast::Expr::Sub(binary_args(
                mem_loc_expr(stack_pointer()),
                mem_loc_expr(stack_offset_loc),
            ))),
        }),
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
