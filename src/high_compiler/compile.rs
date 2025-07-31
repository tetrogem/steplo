use std::{collections::HashMap, sync::Arc};

use super::type_resolved_ast as hast;
use crate::link;
use crate::mem_opt::ast as opt;
use crate::srced::Srced;
use anyhow::bail;
use itertools::Itertools;
use itertools::chain;
use uuid::Uuid;

struct StackFrame {
    ident_name_to_info: HashMap<Arc<str>, Arc<StackVarInfo>>,
}

struct StackVarInfo {
    offset: u32,
    size: u32,
}

impl StackFrame {
    fn new(idents: &[hast::Ref<hast::IdentDeclaration>]) -> Self {
        let mut ident_name_to_info = HashMap::new();
        let mut total_offset = 0;

        for ident in idents.iter().rev() {
            let size = ident.val.size;
            total_offset += size;
            let info = StackVarInfo { size, offset: total_offset - 1 };

            ident_name_to_info.insert(ident.val.name.val.str.clone(), Arc::new(info));
        }

        Self { ident_name_to_info }
    }

    fn get_info(&self, name: &hast::Name) -> anyhow::Result<&Arc<StackVarInfo>> {
        let Some(info) = self.ident_name_to_info.get(&name.str) else {
            bail!("ident name {} is not in current stack frame", name.str);
        };

        Ok(info)
    }

    fn size(&self) -> u32 {
        let return_addr_size = 1;
        let vars_size = self.ident_name_to_info.values().map(|i| i.size).sum::<u32>();
        return_addr_size + vars_size
    }
}

struct FuncManager {
    name_to_head_uuid: HashMap<Arc<str>, Uuid>,
    name_to_params: HashMap<Arc<str>, hast::Ref<Vec<hast::Ref<hast::IdentDeclaration>>>>,
}

impl FuncManager {
    fn try_new(procs: &[hast::Ref<link::Proc>]) -> anyhow::Result<Self> {
        let mut name_to_head_uuid = HashMap::new();
        let mut name_to_params = HashMap::new();

        for proc in procs {
            let link::ProcKind::Func { name, params } = &proc.val.kind else { continue };

            let Some(first_sp) = proc.val.sub_procs.val.first() else {
                bail!("procedure `{}` has no sub-procedures", name.val.str);
            };

            name_to_head_uuid.insert(name.val.str.clone(), first_sp.val.uuid);
            name_to_params.insert(name.val.str.clone(), params.clone());
        }

        Ok(Self { name_to_head_uuid, name_to_params })
    }

    fn get_head_uuid(&self, name: &hast::Name) -> anyhow::Result<Uuid> {
        let Some(uuid) = self.name_to_head_uuid.get(&name.str) else {
            bail!("no head UUID registered for function `{}`", name.str)
        };

        Ok(*uuid)
    }

    fn get_params(
        &self,
        name: &hast::Name,
    ) -> anyhow::Result<&hast::Ref<Vec<hast::Ref<hast::IdentDeclaration>>>> {
        let Some(params) = self.name_to_params.get(&name.str) else {
            bail!("no params registered for function `{}`", name.str)
        };

        Ok(params)
    }
}

pub fn compile(
    linked: Vec<hast::Ref<link::Proc>>,
) -> anyhow::Result<Vec<Arc<opt::Proc<opt::UMemLoc>>>> {
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

fn literal(literal: impl ScratchLiteral) -> Arc<opt::Expr<opt::UMemLoc>> {
    literal.to_expr()
}

trait ScratchLiteral {
    fn to_expr(&self) -> Arc<opt::Expr<opt::UMemLoc>>;
}

impl ScratchLiteral for bool {
    fn to_expr(&self) -> Arc<opt::Expr<opt::UMemLoc>> {
        Arc::new(opt::Expr::Value(Arc::new(opt::Value::Literal(
            match self {
                true => "true",
                false => "false",
            }
            .into(),
        ))))
    }
}

struct Int(f64);
struct Float(f64);

impl ScratchLiteral for Float {
    fn to_expr(&self) -> Arc<opt::Expr<opt::UMemLoc>> {
        // use debug format so that integer floats (e.g. 1, 0) output with decimals: (1.0, 0.0)
        Arc::new(opt::Expr::Value(Arc::new(opt::Value::Literal(format!("{:?}", self.0).into()))))
    }
}

impl ScratchLiteral for Int {
    fn to_expr(&self) -> Arc<opt::Expr<opt::UMemLoc>> {
        // use disp;ay format so that integer floats (e.g. 1, 0) output without decimals: (1, 0)
        Arc::new(opt::Expr::Value(Arc::new(opt::Value::Literal(format!("{}", self.0).into()))))
    }
}

impl ScratchLiteral for &str {
    fn to_expr(&self) -> Arc<opt::Expr<opt::UMemLoc>> {
        Arc::new(opt::Expr::Value(Arc::new(opt::Value::Literal((*self).into()))))
    }
}

impl ScratchLiteral for hast::Literal {
    fn to_expr(&self) -> Arc<opt::Expr<opt::UMemLoc>> {
        match self {
            Self::Val(str) => str.as_ref().to_expr(),
            Self::Int(num) | Self::Uint(num) => Int(*num).to_expr(),
            Self::Num(num) => Float(*num).to_expr(),
            Self::Bool(bool) => bool.to_expr(),
        }
    }
}

impl<T: ScratchLiteral> ScratchLiteral for &T {
    fn to_expr(&self) -> Arc<opt::Expr<opt::UMemLoc>> {
        T::to_expr(self)
    }
}

fn label(uuid: Uuid) -> Arc<opt::Expr<opt::UMemLoc>> {
    Arc::new(opt::Expr::Value(Arc::new(opt::Value::Label(uuid))))
}

fn mem_loc_expr(mem_loc: Arc<opt::UMemLoc>) -> Arc<opt::Expr<opt::UMemLoc>> {
    Arc::new(opt::Expr::MemLoc(mem_loc))
}

fn temp_mem_loc() -> Arc<opt::UMemLoc> {
    Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())))
}

fn binary_args(
    left: Arc<opt::Expr<opt::UMemLoc>>,
    right: Arc<opt::Expr<opt::UMemLoc>>,
) -> Arc<opt::BinaryArgs<opt::UMemLoc>> {
    Arc::new(opt::BinaryArgs { left, right })
}

fn find_user_main_sp_uuid(procs: &[hast::Ref<link::Proc>]) -> anyhow::Result<Uuid> {
    let Some(main_proc) = procs.iter().find(|proc| matches!(proc.val.kind, link::ProcKind::Main))
    else {
        bail!("could not find user main procedure");
    };

    let Some(first_sp) = main_proc.val.sub_procs.val.first() else {
        bail!("user main procedure has no sub-procedures");
    };

    Ok(first_sp.val.uuid)
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
                Arc::new(opt::Command::SetMemLoc {
                    mem_loc: stack_pointer(),
                    val: literal(Int(-1.)),
                }),
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
    proc: &hast::Ref<link::Proc>,
) -> anyhow::Result<Arc<opt::Proc<opt::UMemLoc>>> {
    let (kind, stack_params) = match &proc.val.kind {
        link::ProcKind::Main => (opt::ProcKind::Func { name: "main".into() }, Vec::new()),
        link::ProcKind::Func { name, params } => (
            opt::ProcKind::Func { name: format!("func.{}", name.val.str).into() },
            params.val.to_vec(),
        ),
    };

    let stack_vars = proc.val.idents.val.iter().cloned();
    let stack_idents = chain!(stack_params, stack_vars).collect_vec();

    let stack_frame = StackFrame::new(&stack_idents);
    let sub_procs = proc
        .val
        .sub_procs
        .val
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
    sp: &hast::Ref<link::SubProc>,
    proc_head: bool,
) -> anyhow::Result<Arc<opt::SubProc<opt::UMemLoc>>> {
    // initialize stack vars
    let stack_frame_commands = if proc_head {
        let stack_frame_size_loc = temp_mem_loc();

        Vec::from([
            Arc::new(opt::Command::SetMemLoc {
                mem_loc: stack_frame_size_loc.clone(),
                val: literal(Int(stack_frame.size() as f64)),
            }),
            Arc::new(opt::Command::SetMemLoc {
                mem_loc: stack_pointer(),
                val: Arc::new(opt::Expr::Add(binary_args(
                    mem_loc_expr(stack_pointer()),
                    mem_loc_expr(stack_frame_size_loc),
                ))),
            }),
        ])
    } else {
        Vec::new()
    };

    let statement_commands: Vec<_> = sp
        .val
        .statements
        .val
        .iter()
        .map(|s| compile_statement(stack_frame, s))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .flatten()
        .collect();

    let compiled_call = compile_call(stack_frame, func_manager, &sp.val.next_call.val)?;

    let commands =
        chain!(stack_frame_commands, statement_commands, compiled_call.commands).collect();

    let sp =
        opt::SubProc { uuid: sp.val.uuid, commands: Arc::new(commands), call: compiled_call.call };

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
            let param_idents = func_manager.get_params(&name.val)?;
            let mut param_stack_offset: u32 = 0;
            let mut param_setup_commands = Vec::new();
            for (ident, assign_exprs) in param_idents.val.iter().zip(&param_exprs.val) {
                let assign_exprs = compile_assign_exprs(stack_frame, assign_exprs)?;

                for assign_expr in assign_exprs.into_iter() {
                    param_setup_commands.extend(assign_expr.offset.commands);
                    param_setup_commands.extend(assign_expr.expr.commands);

                    let param_stack_offset_loc = temp();
                    let param_addr_loc = temp();
                    let offset_param_addr_loc = temp();

                    let assign_commands = [
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: param_stack_offset_loc.clone(),
                            val: literal(Int(f64::from(param_stack_offset + 2))),
                        }),
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: param_addr_loc.clone(),
                            val: Arc::new(opt::Expr::Add(binary_args(
                                mem_loc_expr(stack_pointer()),
                                mem_loc_expr(param_stack_offset_loc),
                            ))),
                        }),
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: offset_param_addr_loc.clone(),
                            val: Arc::new(opt::Expr::Add(binary_args(
                                mem_loc_expr(param_addr_loc.clone()),
                                mem_loc_expr(assign_expr.offset.mem_loc),
                            ))),
                        }),
                        Arc::new(opt::Command::SetStack {
                            addr: mem_loc_expr(offset_param_addr_loc),
                            val: mem_loc_expr(assign_expr.expr.mem_loc),
                        }),
                    ];

                    param_setup_commands.extend(assign_commands);
                }

                param_stack_offset += ident.val.size;
            }

            let return_setup_commands = {
                let return_offset_loc = temp();
                let return_addr_loc = temp();
                let return_label_loc = temp();

                [
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: return_offset_loc.clone(),
                        val: literal(Int(1.)),
                    }),
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: return_addr_loc.clone(),
                        val: Arc::new(opt::Expr::Add(binary_args(
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
                let func_head_uuid = func_manager.get_head_uuid(&name.val)?;
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
                        val: literal(Int(stack_frame.size() as f64)),
                    }),
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: stack_pointer(),
                        val: Arc::new(opt::Expr::Sub(binary_args(
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
                            val: literal(Int(1.)),
                        }),
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: label_addr_loc.clone(),
                            val: Arc::new(opt::Expr::Add(binary_args(
                                mem_loc_expr(stack_pointer()),
                                mem_loc_expr(label_offset_loc),
                            ))),
                        }),
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: label_loc.clone(),
                            val: Arc::new(opt::Expr::StackDeref(mem_loc_expr(label_addr_loc))),
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
    statement: &hast::Ref<link::Statement>,
) -> anyhow::Result<Vec<Arc<opt::Command<opt::UMemLoc>>>> {
    let commands = match &statement.val {
        link::Statement::Assign(assign) => {
            let compiled_place_addr = compile_place_to_addr(stack_frame, &assign.val.place.val)?;
            let compiled_assign = compile_addr_assignment(
                stack_frame,
                &compiled_place_addr.mem_loc,
                &assign.val.expr,
            )?;

            chain!(compiled_place_addr.commands, compiled_assign).collect()
        },
        link::Statement::Native(native) => match &native.val {
            hast::NativeOperation::Out { place } => {
                let compiled_place_addr = compile_place_to_addr(stack_frame, &place.val)?;
                let ident_value_loc = temp();

                chain!(
                    compiled_place_addr.commands,
                    [
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: ident_value_loc.clone(),
                            val: Arc::new(opt::Expr::StackDeref(mem_loc_expr(
                                compiled_place_addr.mem_loc
                            ))),
                        }),
                        Arc::new(opt::Command::Out(mem_loc_expr(ident_value_loc))),
                    ]
                )
                .collect()
            },
            hast::NativeOperation::In { dest_place } => {
                let compiled_place_addr = compile_place_to_addr(stack_frame, &dest_place.val)?;

                chain!(
                    compiled_place_addr.commands,
                    [
                        Arc::new(opt::Command::In),
                        Arc::new(opt::Command::SetStack {
                            addr: mem_loc_expr(compiled_place_addr.mem_loc),
                            val: Arc::new(opt::Expr::InAnswer),
                        }),
                    ]
                )
                .collect()
            },
            hast::NativeOperation::Random { dest_place, min, max } => {
                let compiled_place_addr = compile_place_to_addr(stack_frame, &dest_place.val)?;
                let compiled_min = compile_expr(stack_frame, min)?;
                let compiled_max = compile_expr(stack_frame, max)?;

                chain!(
                    compiled_place_addr.commands,
                    compiled_min.commands,
                    compiled_max.commands,
                    [Arc::new(opt::Command::SetStack {
                        addr: mem_loc_expr(compiled_place_addr.mem_loc),
                        val: Arc::new(opt::Expr::Random(binary_args(
                            mem_loc_expr(compiled_min.mem_loc),
                            mem_loc_expr(compiled_max.mem_loc)
                        ))),
                    })]
                )
                .collect()
            },
            hast::NativeOperation::StdoutClear => Vec::from([Arc::new(opt::Command::ClearStdout)]),
            hast::NativeOperation::StdoutRead { dest_place, index } => {
                let compiled_place_addr = compile_place_to_addr(stack_frame, &dest_place.val)?;
                let compiled_index = compile_expr(stack_frame, index)?;

                chain!(
                    compiled_place_addr.commands,
                    compiled_index.commands,
                    [Arc::new(opt::Command::SetStack {
                        addr: mem_loc_expr(compiled_place_addr.mem_loc),
                        val: Arc::new(opt::Expr::StdoutDeref(Arc::new(opt::Expr::MemLoc(
                            compiled_index.mem_loc
                        ))))
                    })]
                )
                .collect()
            },
            hast::NativeOperation::StdoutWrite { val, index } => {
                let compiled_val = compile_expr(stack_frame, val)?;
                let compiled_index = compile_expr(stack_frame, index)?;

                chain!(
                    compiled_val.commands,
                    compiled_index.commands,
                    [Arc::new(opt::Command::WriteStdout {
                        index: mem_loc_expr(compiled_index.mem_loc),
                        val: mem_loc_expr(compiled_val.mem_loc),
                    })]
                )
                .collect()
            },
            hast::NativeOperation::StdoutLen { dest_place } => {
                let compiled_place_addr = compile_place_to_addr(stack_frame, &dest_place.val)?;

                chain!(
                    compiled_place_addr.commands,
                    [Arc::new(opt::Command::SetStack {
                        addr: mem_loc_expr(compiled_place_addr.mem_loc),
                        val: Arc::new(opt::Expr::StdoutLen),
                    })]
                )
                .collect()
            },
        },
    };

    Ok(commands)
}

fn compile_addr_assignment(
    stack_frame: &StackFrame,
    compiled_head_loc: &Arc<opt::UMemLoc>,
    assign_exprs: &hast::Ref<Vec<hast::Ref<hast::AssignExpr>>>,
) -> anyhow::Result<Vec<Arc<opt::Command<opt::UMemLoc>>>> {
    let assign_exprs = compile_assign_exprs(stack_frame, assign_exprs)?;
    let mut assignment_commands = Vec::new();

    for assign_expr in assign_exprs.into_iter() {
        let compile_dest_addr =
            compile_addr_offset(compiled_head_loc.clone(), assign_expr.offset.mem_loc)?;

        let mut commands = chain!(
            assign_expr.expr.commands,
            assign_expr.offset.commands,
            compile_dest_addr.commands
        )
        .collect_vec();

        commands.push(Arc::new(opt::Command::SetStack {
            addr: mem_loc_expr(compile_dest_addr.mem_loc),
            val: mem_loc_expr(assign_expr.expr.mem_loc),
        }));

        assignment_commands.extend(commands);
    }

    Ok(assignment_commands)
}

struct CompiledAssignExpr {
    offset: CompiledExpr,
    expr: CompiledExpr,
}

fn compile_assign_exprs(
    stack_frame: &StackFrame,
    assign_exprs: &hast::Ref<Vec<hast::Ref<hast::AssignExpr>>>,
) -> anyhow::Result<Vec<CompiledAssignExpr>> {
    assign_exprs
        .val
        .iter()
        .map(|assign_expr| {
            let compiled_expr = compile_expr(stack_frame, &assign_expr.val.expr)?;
            let compiled_offset = compile_expr(
                stack_frame,
                &Arc::new(Srced {
                    range: assign_expr.range,
                    val: hast::Expr::Literal(Arc::new(Srced {
                        range: assign_expr.range,
                        val: hast::Literal::Uint(f64::from(assign_expr.val.offset)),
                    })),
                }),
            )?;

            Ok(CompiledAssignExpr { expr: compiled_expr, offset: compiled_offset })
        })
        .collect::<Result<Vec<_>, _>>()
}

fn compile_expr(
    stack_frame: &StackFrame,
    expr: &hast::Ref<hast::Expr>,
) -> anyhow::Result<CompiledExpr> {
    let compiled = match &expr.val {
        hast::Expr::Literal(lit) => {
            let value_temp = temp();
            let assignments = Vec::from([Arc::new(opt::Command::SetMemLoc {
                mem_loc: value_temp.clone(),
                val: literal(&lit.val),
            })]);

            CompiledExpr { mem_loc: value_temp, commands: assignments }
        },
        hast::Expr::Place(place) => {
            let place_addr = compile_place_to_addr(stack_frame, &place.val)?;
            let value = compile_addr_deref(&place_addr.mem_loc);

            let commands = chain!(place_addr.commands, value.commands).collect();

            CompiledExpr { mem_loc: value.mem_loc, commands }
        },
        hast::Expr::Ref(place) => compile_place_to_addr(stack_frame, &place.val)?,
        hast::Expr::Paren(expr) => compile_paren_expr(stack_frame, &expr.val)?,
    };

    Ok(compiled)
}

fn compile_paren_expr(
    stack_frame: &StackFrame,
    expr: &hast::ParenExpr,
) -> anyhow::Result<CompiledExpr> {
    match expr {
        hast::ParenExpr::Unary(unary) => compile_unary_paren_expr(stack_frame, &unary.val),
        hast::ParenExpr::Binary(binary) => compile_binary_paren_expr(stack_frame, &binary.val),
    }
}

fn compile_unary_paren_expr(
    stack_frame: &StackFrame,
    expr: &hast::UnaryParenExpr,
) -> anyhow::Result<CompiledExpr> {
    let res_loc = temp();
    let operand_ops = compile_expr(stack_frame, &expr.operand)?;

    let expr_commands = match expr.op.val {
        hast::UnaryParenExprOp::Not => Vec::from([Arc::new(opt::Command::SetMemLoc {
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

    let expr_commands = match expr.op.val {
        hast::BinaryParenExprOp::Add => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Add(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Sub => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Sub(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Mul => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Mul(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Div => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Div(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Mod => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Mod(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Eq => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Eq(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Neq => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Not(Arc::new(opt::Expr::Eq(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))))),
        })]),
        hast::BinaryParenExprOp::Lt => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Lt(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Gt => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Gt(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Lte => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Not(Arc::new(opt::Expr::Gt(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))))),
        })]),
        hast::BinaryParenExprOp::Gte => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Not(Arc::new(opt::Expr::Lt(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))))),
        })]),
        hast::BinaryParenExprOp::Join => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Join(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::Or => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::Or(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
        hast::BinaryParenExprOp::And => Vec::from([Arc::new(opt::Command::SetMemLoc {
            mem_loc: res_loc.clone(),
            val: Arc::new(opt::Expr::And(binary_args(
                mem_loc_expr(left_ops.mem_loc),
                mem_loc_expr(right_ops.mem_loc),
            ))),
        })]),
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
        val: Arc::new(opt::Expr::StackDeref(mem_loc_expr(addr.clone()))),
    })]);

    CompiledExpr { mem_loc: ident_value_loc, commands }
}

fn compile_place_to_addr(
    stack_frame: &StackFrame,
    place: &hast::Place,
) -> anyhow::Result<CompiledExpr> {
    let compiled_head = compile_place_head_to_add(stack_frame, &place.head.val)?;
    let compiled_place = match &place.offset {
        None => compiled_head,
        Some(offset) => {
            let compiled_index = compile_expr(stack_frame, offset)?;
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
        hast::PlaceHead::Ident(ident) => {
            compile_ident_name_to_start_addr(stack_frame, &ident.val.name.val)
        },
        hast::PlaceHead::Deref(deref) => compile_expr(stack_frame, &deref.val.addr),
    }
}

fn compile_addr_offset(
    addr_loc: Arc<opt::UMemLoc>,
    offset_loc: Arc<opt::UMemLoc>,
) -> anyhow::Result<CompiledExpr> {
    let indexed_addr_loc = temp();

    let index_commands = [Arc::new(opt::Command::SetMemLoc {
        mem_loc: indexed_addr_loc.clone(),
        val: Arc::new(opt::Expr::Add(binary_args(
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
            val: literal(Int(var_info.offset as f64)),
        }),
        Arc::new(opt::Command::SetMemLoc {
            mem_loc: stack_addr_loc.clone(),
            val: Arc::new(opt::Expr::Sub(binary_args(
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
