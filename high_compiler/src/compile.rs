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
    ident_name_to_info: HashMap<Arc<hast::Name>, Arc<StackVarInfo>>,
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
            let size = ident.size();
            total_offset += size;
            let info = StackVarInfo { size, offset: total_offset - 1 };

            ident_name_to_info.insert(ident.name().clone(), Arc::new(info));
        }

        Self { ident_name_to_info }
    }

    fn get_info(&self, name: &hast::Name) -> anyhow::Result<&Arc<StackVarInfo>> {
        let Some(info) = self.ident_name_to_info.get(name) else {
            bail!("ident name {} is not in current stack frame", name.str);
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
    name_to_head_uuid: HashMap<Arc<hast::Name>, Uuid>,
    name_to_params: HashMap<Arc<hast::Name>, Arc<Vec<Arc<hast::IdentDeclaration>>>>,
}

impl FuncManager {
    fn try_new(procs: &[Arc<link::Proc>]) -> anyhow::Result<Self> {
        let mut name_to_head_uuid = HashMap::new();
        let mut name_to_params = HashMap::new();

        for proc in procs {
            let link::ProcKind::Func { name, params } = &proc.kind else { continue };

            let Some(first_sp) = proc.sub_procs.first() else {
                bail!("procedure `{}` has no sub-procedures", name.str);
            };

            name_to_head_uuid.insert(name.clone(), first_sp.uuid);
            name_to_params.insert(name.clone(), params.elements.clone());
        }

        Ok(Self { name_to_head_uuid, name_to_params })
    }

    fn get_head_uuid(&self, name: &hast::Name) -> anyhow::Result<Uuid> {
        let Some(uuid) = self.name_to_head_uuid.get(name) else {
            bail!("no head UUID registered for function `{}`", name.str)
        };

        Ok(*uuid)
    }

    fn get_params(
        &self,
        name: &hast::Name,
    ) -> anyhow::Result<&Arc<Vec<Arc<hast::IdentDeclaration>>>> {
        let Some(params) = self.name_to_params.get(name) else {
            bail!("no params registered for function `{}`", name.str)
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

impl ScratchLiteral for hast::Literal {
    fn to_expr(&self) -> Arc<ast::Expr<ast::UMemLoc>> {
        self.str.as_ref().to_expr()
    }
}

impl<T: ScratchLiteral> ScratchLiteral for &T {
    fn to_expr(&self) -> Arc<ast::Expr<ast::UMemLoc>> {
        T::to_expr(self)
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
            ])),
            call: Arc::new(opt::Call::Jump(mem_loc_expr(jump_loc))),
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
            opt::ProcKind::Func { name: format!("func.{}", name.str).into() },
            params.elements.iter().cloned().collect(),
        ),
    };

    let stack_vars = proc.idents.elements.iter().cloned();
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

    let compiled_call = compile_call(stack_frame, func_manager, &sp.next_call)?;

    let commands =
        chain!(stack_frame_commands, statement_commands, compiled_call.commands).collect();

    let sp = opt::SubProc { uuid: sp.uuid, commands: Arc::new(commands), call: compiled_call.call };

    Ok(Arc::new(sp))
}

struct CompiledCall {
    commands: Vec<Arc<opt::Command<opt::UMemLoc>>>,
    call: Arc<opt::Call<opt::UMemLoc>>,
}

fn compile_call(
    stack_frame: &StackFrame,
    func_manager: &FuncManager,
    call: &link::Call,
) -> anyhow::Result<CompiledCall> {
    let compiled = match call {
        link::Call::Func { name, param_exprs, return_sub_proc } => {
            let param_idents = func_manager.get_params(name)?;
            let mut param_stack_offset = 0;
            let mut param_setup_commands = Vec::new();
            for (ident, expr) in param_idents.iter().zip(param_exprs.elements.as_ref()) {
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

            let (jump_commands, jump_call) = {
                let func_head_uuid = func_manager.get_head_uuid(name)?;
                let jump_label_loc = temp();

                (
                    [Arc::new(opt::Command::SetMemLoc {
                        mem_loc: jump_label_loc.clone(),
                        val: label(func_head_uuid),
                    })],
                    Arc::new(opt::Call::Jump(mem_loc_expr(jump_label_loc))),
                )
            };

            CompiledCall {
                commands: chain!(param_setup_commands, return_setup_commands, jump_commands)
                    .collect(),
                call: jump_call,
            }
        },
        link::Call::SubProc(uuid) => {
            let return_label_loc = temp();

            CompiledCall {
                commands: Vec::from([Arc::new(opt::Command::SetMemLoc {
                    mem_loc: return_label_loc.clone(),
                    val: label(*uuid),
                })]),
                call: Arc::new(opt::Call::Jump(mem_loc_expr(return_label_loc))),
            }
        },
        link::Call::IfElseBranch { cond_expr, then_sub_proc, else_sub_proc } => {
            let compiled_cond = compile_expr(stack_frame, cond_expr)?;

            let then_label_loc = temp();
            let else_label_loc = temp();

            let (branch_commands, branch_call) = (
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: then_label_loc.clone(),
                        val: label(*then_sub_proc),
                    }),
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: else_label_loc.clone(),
                        val: label(*else_sub_proc),
                    }),
                ]),
                Arc::new(opt::Call::Branch {
                    cond: mem_loc_expr(compiled_cond.mem_loc),
                    then_to: mem_loc_expr(then_label_loc),
                    else_to: mem_loc_expr(else_label_loc),
                }),
            );

            CompiledCall {
                commands: chain!(compiled_cond.commands, branch_commands).collect(),
                call: branch_call,
            }
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
            let (jump_return_commands, jump_return_call) = {
                let label_offset_loc = temp();
                let label_addr_loc = temp();
                let label_loc = temp();

                (
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
                    ],
                    Arc::new(opt::Call::Jump(mem_loc_expr(label_loc))),
                )
            };

            CompiledCall {
                commands: chain!(stack_frame_commands, jump_return_commands).collect(),
                call: jump_return_call,
            }
        },
        link::Call::Terminate => {
            CompiledCall { commands: Vec::new(), call: Arc::new(opt::Call::Exit) }
        },
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
                let compiled_ident_addr = compile_place_to_addr(stack_frame, &assign.loc)?;
                let compiled_offset = compile_expr(
                    stack_frame,
                    &hast::Expr::Literal(Arc::new(hast::Literal { str: i.to_string().into() })),
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

                let dest_addr_loc = compile_dest_addr.mem_loc;

                // assign to var
                assignments.push(Arc::new(opt::Command::SetStack {
                    addr: mem_loc_expr(dest_addr_loc),
                    val: mem_loc_expr(element.mem_loc),
                }));

                element_assignments.extend(assignments);
            }

            element_assignments
        },
        link::Statement::Native(native) => match native.as_ref() {
            hast::NativeOperation::Out { ident } => {
                let compiled_ident_addr = compile_place_to_addr(stack_frame, ident)?;
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
                let compiled_ident_addr = compile_place_to_addr(stack_frame, dest_ident)?;

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
        hast::AssignExpr::Expr(expr) => Vec::from([compile_expr(stack_frame, expr)?]),
        hast::AssignExpr::Span(array) => array
            .elements
            .elements
            .iter()
            .map(|expr| compile_expr(stack_frame, expr))
            .collect::<anyhow::Result<Vec<_>>>()?
            .into_iter()
            .collect(),
        hast::AssignExpr::Slice(slice) => {
            let elements = (slice.start_in..slice.end_ex).map(|i| {
                let ident_addr = compile_place_to_addr(stack_frame, &slice.place)?;
                let offset = compile_expr(
                    stack_frame,
                    &hast::Expr::Literal(Arc::new(hast::Literal { str: i.to_string().into() })),
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

fn compile_expr(stack_frame: &StackFrame, expr: &hast::Expr) -> anyhow::Result<CompiledExpr> {
    let compiled = match expr {
        hast::Expr::Literal(lit) => {
            let value_temp = temp();
            let assignments = Vec::from([Arc::new(opt::Command::SetMemLoc {
                mem_loc: value_temp.clone(),
                val: literal(lit.as_ref()),
            })]);

            CompiledExpr { mem_loc: value_temp, commands: assignments }
        },
        hast::Expr::Place(ident) => {
            let ident_addr = compile_place_to_addr(stack_frame, ident)?;
            let value = compile_addr_deref(&ident_addr.mem_loc);

            let commands = chain!(ident_addr.commands, value.commands).collect();

            CompiledExpr { mem_loc: value.mem_loc, commands }
        },
        hast::Expr::Ref(expr) => compile_place_to_addr(stack_frame, &expr.place)?,
        hast::Expr::Paren(expr) => compile_paren_expr(stack_frame, expr)?,
    };

    Ok(compiled)
}

fn compile_paren_expr(
    stack_frame: &StackFrame,
    expr: &hast::ParenExpr,
) -> anyhow::Result<CompiledExpr> {
    match expr {
        hast::ParenExpr::Unary(unary) => compile_unary_paren_expr(stack_frame, unary),
        hast::ParenExpr::Binary(binary) => compile_binary_paren_expr(stack_frame, binary),
    }
}

fn compile_unary_paren_expr(
    stack_frame: &StackFrame,
    expr: &hast::UnaryParenExpr,
) -> anyhow::Result<CompiledExpr> {
    let res_loc = temp();
    let operand_ops = compile_expr(stack_frame, &expr.operand)?;

    let expr_commands = match expr.op {
        hast::UnaryParenExprOp::Not(_) => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Not(mem_loc_expr(operand_ops.mem_loc))),
        })]),
    };

    Ok(CompiledExpr {
        mem_loc: res_loc,
        commands: chain!(operand_ops.commands, expr_commands).collect(),
    })
}

fn compile_binary_paren_expr(
    stack_frame: &StackFrame,
    expr: &hast::BinaryParenExpr,
) -> anyhow::Result<CompiledExpr> {
    let res_loc = temp();
    let left_ops = compile_expr(stack_frame, &expr.left)?;
    let right_ops = compile_expr(stack_frame, &expr.right)?;

    let expr_commands = match expr.op {
        hast::BinaryParenExprOp::Add(_) => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Add(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Sub(_) => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Sub(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Mul(_) => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Mul(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Div(_) => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Div(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Mod(_) => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Mod(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Eq(_) => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Eq(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Neq(_) => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Not(Arc::new(opt::Expr::Eq(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))))),
        })]),
        hast::BinaryParenExprOp::Lt(_) => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Lt(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Gt(_) => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Gt(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Lte(_) => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Not(Arc::new(opt::Expr::Gt(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))))),
        })]),
        hast::BinaryParenExprOp::Gte(_) => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Not(Arc::new(opt::Expr::Lt(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))))),
        })]),
        hast::BinaryParenExprOp::Join(_) => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Join(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Or(_) => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Or(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        _ => todo!(),
    };

    Ok(CompiledExpr {
        mem_loc: res_loc,
        commands: chain!(left_ops.commands, right_ops.commands, expr_commands).collect(),
    })
}

fn compile_addr_deref(addr: &Arc<opt::UMemLoc>) -> CompiledExpr {
    let ident_value_loc = temp();
    let commands = Vec::from([Arc::new(opt::Command::SetMemLoc {
        mem_loc: ident_value_loc.clone(),
        val: Arc::new(ast::Expr::Deref(mem_loc_expr(addr.clone()))),
    })]);

    CompiledExpr { mem_loc: ident_value_loc, commands }
}

fn compile_place_to_addr(
    stack_frame: &StackFrame,
    place: &hast::Place,
) -> anyhow::Result<CompiledExpr> {
    let compiled_head = compile_place_head_to_add(stack_frame, &place.head)?;
    let compiled_place = match &place.offset {
        None => compiled_head,
        Some(offset) => {
            let compiled_index = compile_expr(stack_frame, &offset.expr)?;
            let offset_addr = compile_addr_offset(compiled_head.mem_loc, compiled_index.mem_loc)?;

            let commands =
                chain!(compiled_index.commands, compiled_head.commands, offset_addr.commands)
                    .collect();

            CompiledExpr { mem_loc: offset_addr.mem_loc, commands }
        },
    };

    Ok(compiled_place)
}

fn compile_place_head_to_add(
    stack_frame: &StackFrame,
    place_head: &hast::PlaceHead,
) -> anyhow::Result<CompiledExpr> {
    match place_head {
        hast::PlaceHead::Ident(ident) => compile_ident_name_to_start_addr(stack_frame, &ident.name),
        hast::PlaceHead::Deref(deref) => compile_expr(stack_frame, &deref.addr),
    }
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
    ident_name: &hast::Name,
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
