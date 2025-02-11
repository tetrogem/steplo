use std::{collections::HashMap, sync::Arc};

use anyhow::bail;
use inter::ez;
use itertools::Itertools;
use uuid::Uuid;

use crate::link;

pub fn compile(program: &link::Program) -> anyhow::Result<ez::Program> {
    let stack_list = Arc::new(ez::List { uuid: Uuid::new_v4(), name: "stack".into() });
    let stdout_list = Arc::new(ez::List { uuid: Uuid::new_v4(), name: "stdout".into() });

    let mut reg_name_to_var = HashMap::<Arc<str>, Arc<ez::Variable>>::new();

    for register_name in program.registers.iter() {
        reg_name_to_var.insert(
            Arc::clone(register_name),
            Arc::new(ez::Variable { uuid: Uuid::new_v4(), name: Arc::clone(register_name) }),
        );
    }

    let mut sub_proc_name_to_broadcast = HashMap::<Arc<str>, Arc<ez::Broadcast>>::new();

    for proc in program.procedures.as_ref() {
        if let link::ProcedureKind::Sub { name } = &proc.kind {
            sub_proc_name_to_broadcast.insert(
                Arc::clone(name),
                Arc::new(ez::Broadcast { name: Arc::clone(name), uuid: Uuid::new_v4() }),
            );
        }
    }

    let mut compiled_main_proc: Option<Vec<Arc<ez::Op>>> = None;
    let mut compiled_sub_procs = Vec::<CompiledSubProc>::new();

    struct CompiledSubProc {
        broadcast: Arc<ez::Broadcast>,
        compiled_ops: Vec<Arc<ez::Op>>,
    }

    let mut ast = program.procedures.iter().peekable();

    while let Some(proc) = ast.next() {
        let compiled_proc_ops = compile_proc(
            proc,
            ast.peek().map(|x| x.as_ref()),
            &stack_list,
            &stdout_list,
            &sub_proc_name_to_broadcast,
            &reg_name_to_var,
        )?;

        match &proc.kind {
            link::ProcedureKind::Main => {
                let prev_main_ops = compiled_main_proc.replace(compiled_proc_ops);
                if prev_main_ops.is_some() {
                    bail!("More than one main procedure found");
                }
            },
            link::ProcedureKind::Sub { name } => {
                let Some(broadcast) = sub_proc_name_to_broadcast.get(name) else {
                    bail!("could not find broadcast for sub procedure named '{}'", name)
                };

                compiled_sub_procs.push(CompiledSubProc {
                    broadcast: Arc::clone(broadcast),
                    compiled_ops: compiled_proc_ops,
                });
            },
        }
    }

    let Some(compiled_main_ops) = compiled_main_proc else { bail!("No main procedure found") };

    let stack_root = Arc::new(ez::Op::Event(ez::EventOp::WhenFlagClicked));
    let reset_ops = Vec::from([
        // delete all of stack
        Arc::new(ez::Op::Data(ez::DataOp::DeleteAllOfList { list: Arc::clone(&stack_list) })),
        // delete all of stdout
        Arc::new(ez::Op::Data(ez::DataOp::DeleteAllOfList { list: Arc::clone(&stdout_list) })),
        // init stack memory
        Arc::new(ez::Op::Control(ez::ControlOp::Repeat {
            times: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::Int(200_000)))),
            looped_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                root: Arc::new(ez::Op::Data(ez::DataOp::AddToList {
                    list: Arc::clone(&stack_list),
                    item: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String("".into())))),
                })),
                rest: Arc::new(Vec::new()),
            }))),
        })),
    ]);

    let mut stacks = Vec::new();

    let main_stack = Arc::new(ez::Stack {
        root: stack_root,
        rest: Arc::new(reset_ops.into_iter().chain(compiled_main_ops).collect()),
    });

    stacks.push(main_stack);

    let variables = reg_name_to_var.values().cloned().collect();
    let broadcasts = compiled_sub_procs.iter().map(|proc| Arc::clone(&proc.broadcast)).collect();

    for compiled_proc in compiled_sub_procs {
        let root = Arc::new(ez::Op::Event(ez::EventOp::WhenBroadcastReceived {
            broadcast: Arc::clone(&compiled_proc.broadcast),
        }));

        let stack = Arc::new(ez::Stack { root, rest: Arc::new(compiled_proc.compiled_ops) });
        stacks.push(stack);
    }

    let stage = Arc::new(ez::Stage {
        variables: Arc::new(variables),
        broadcasts: Arc::new(broadcasts),
        lists: Arc::new(Vec::from([stack_list])),
        stacks: Arc::new(stacks),
    });

    Ok(ez::Program { monitors: Arc::new(Vec::new()), stages: Arc::new(Vec::from([stage])) })
}

fn compile_proc(
    proc: &link::Procedure,
    next_proc: Option<&link::Procedure>,
    stack_list: &Arc<ez::List>,
    stdout_list: &Arc<ez::List>,
    sub_proc_name_to_broadcast: &HashMap<Arc<str>, Arc<ez::Broadcast>>,
    reg_name_to_var: &HashMap<Arc<str>, Arc<ez::Variable>>,
) -> anyhow::Result<Vec<Arc<ez::Op>>> {
    let mut ez_compiled_ops = Vec::<Arc<ez::Op>>::new();
    for command in &proc.body.commands {
        let ez_ops =
            compile_data_command(command.as_ref(), stack_list, stdout_list, reg_name_to_var)?;
        ez_compiled_ops.extend(ez_ops);
    }

    let next_call_ez_op = compile_call(
        &proc.body.next_call,
        next_proc,
        stack_list,
        sub_proc_name_to_broadcast,
        reg_name_to_var,
    )?;

    if let Some(ez_op) = next_call_ez_op {
        ez_compiled_ops.push(Arc::new(ez_op));
    }

    Ok(ez_compiled_ops)
}

fn lit(addr: &str) -> Arc<ez::Expr> {
    Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(addr.into()))))
}

fn list_get(list: &Arc<ez::List>, index: &Arc<ez::Expr>) -> Arc<ez::Expr> {
    data_expr(ez::DataOp::ItemOfList { list: Arc::clone(list), index: Arc::clone(index) })
}

fn mem_get(
    stack_list: &Arc<ez::List>,
    reg_name_to_var: &HashMap<Arc<str>, Arc<ez::Variable>>,
    addr: &link::Value,
) -> anyhow::Result<Arc<ez::Expr>> {
    let expr = match addr {
        link::Value::Literal { val } => {
            data_expr(ez::DataOp::ItemOfList { list: Arc::clone(stack_list), index: lit(val) })
        },
        link::Value::Register { name } => {
            let Some(var) = reg_name_to_var.get(name) else {
                bail!("Failed to find register with name `{}`", name)
            };

            Arc::new(ez::Expr::Variable(Arc::clone(var)))
        },
    };

    Ok(expr)
}

fn operator_expr(op: ez::OperatorOp) -> Arc<ez::Expr> {
    Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(op))))
}

fn data_expr(op: ez::DataOp) -> Arc<ez::Expr> {
    Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(op))))
}

fn mem_set(
    stack_list: &Arc<ez::List>,
    reg_name_to_var: &HashMap<Arc<str>, Arc<ez::Variable>>,
    addr: &link::Value,
    value: &Arc<ez::Expr>,
) -> anyhow::Result<Arc<ez::Op>> {
    let op = match addr {
        link::Value::Literal { val: literal } => list_set(stack_list, &lit(&literal), value),
        link::Value::Register { name } => {
            let Some(var) = reg_name_to_var.get(name) else {
                bail!("Failed to find register with name `{}`", name)
            };

            Arc::new(ez::Op::Data(ez::DataOp::SetVariableTo {
                variable: Arc::clone(var),
                value: Arc::clone(value),
            }))
        },
    };

    Ok(op)
}

fn list_set(list: &Arc<ez::List>, index: &Arc<ez::Expr>, value: &Arc<ez::Expr>) -> Arc<ez::Op> {
    Arc::new(ez::Op::Data(ez::DataOp::ReplaceItemOfList {
        list: Arc::clone(list),
        index: Arc::clone(index),
        item: Arc::clone(value),
    }))
}

macro_rules! stack {
    ($root:expr $(, $($rest:expr),* $(,)?)?) => {{
        let rest = vec![$($($rest),*)*];

        Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
            root: $root,
            rest: Arc::new(rest),
        })))
    }};
}

fn try_lit(value: &link::Value) -> anyhow::Result<Arc<ez::Expr>> {
    match value {
        link::Value::Literal { val } => Ok(lit(&val)),
        link::Value::Register { name } => bail!("Expected literal, found register"),
    }
}

fn compile_data_command(
    command: &link::DataCommand,
    stack_list: &Arc<ez::List>,
    stdout_list: &Arc<ez::List>,
    reg_name_to_var: &HashMap<Arc<str>, Arc<ez::Variable>>,
) -> anyhow::Result<Vec<Arc<ez::Op>>> {
    let mem_set = |addr, value| mem_set(stack_list, reg_name_to_var, addr, value);
    let mem_get = |addr| mem_get(stack_list, reg_name_to_var, addr);

    let ops = match command {
        link::DataCommand::Set(args) => Vec::from([mem_set(&args.dest, &try_lit(&args.val)?)?]),
        link::DataCommand::Move(args) => Vec::from([mem_set(&args.dest, &mem_get(&args.val)?)?]),
        link::DataCommand::MoveDerefDest(args) => {
            Vec::from([list_set(stack_list, &mem_get(&args.dest)?, &mem_get(&args.val)?)])
        },
        link::DataCommand::MoveDerefSrc(args) => {
            Vec::from([mem_set(&args.dest, &list_get(stack_list, &mem_get(&args.val)?))?])
        },
        link::DataCommand::Add(args) => Vec::from([mem_set(
            &args.dest,
            &operator_expr(ez::OperatorOp::Add {
                num_a: mem_get(&args.left)?,
                num_b: mem_get(&args.right)?,
            }),
        )?]),
        link::DataCommand::Sub(args) => Vec::from([mem_set(
            &args.dest,
            &operator_expr(ez::OperatorOp::Subtract {
                num_a: mem_get(&args.left)?,
                num_b: mem_get(&args.right)?,
            }),
        )?]),
        link::DataCommand::Out(args) => {
            Vec::from([Arc::new(ez::Op::Data(ez::DataOp::AddToList {
                list: Arc::clone(stdout_list),
                item: mem_get(&args.val)?,
            }))])
        },
        link::DataCommand::Eq(args) => {
            Vec::from([Arc::new(ez::Op::Control(ez::ControlOp::IfElse {
                condition: operator_expr(ez::OperatorOp::Equals {
                    operand_a: mem_get(&args.left)?,
                    operand_b: mem_get(&args.right)?,
                }),
                then_substack: stack!(mem_set(&args.dest, &lit("1"))?),
                else_substack: stack!(mem_set(&args.dest, &lit(""))?),
            }))])
        },
        link::DataCommand::Not(args) => {
            Vec::from([Arc::new(ez::Op::Control(ez::ControlOp::IfElse {
                condition: operator_expr(ez::OperatorOp::Equals {
                    operand_a: mem_get(&args.val)?,
                    operand_b: lit(""),
                }),
                then_substack: stack!(mem_set(&args.dest, &lit("1"))?),
                else_substack: stack!(mem_set(&args.dest, &lit(""))?),
            }))])
        },
        link::DataCommand::In(args) => {
            let ask_op = Arc::new(ez::Op::Sensing(ez::SensingOp::AskAndWait { question: lit("") }));

            let save_answer_op = mem_set(
                &args.val,
                &Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Sensing(ez::SensingOp::Answer)))),
            )?;

            Vec::from([ask_op, save_answer_op])
        },
        link::DataCommand::Mul(args) => Vec::from([mem_set(
            &args.dest,
            &operator_expr(ez::OperatorOp::Multiply {
                num_a: mem_get(&args.left)?,
                num_b: mem_get(&args.right)?,
            }),
        )?]),
        link::DataCommand::Div(args) => Vec::from([mem_set(
            &args.dest,
            &operator_expr(ez::OperatorOp::Divide {
                num_a: mem_get(&args.left)?,
                num_b: mem_get(&args.right)?,
            }),
        )?]),
        link::DataCommand::Mod(args) => Vec::from([mem_set(
            &args.dest,
            &operator_expr(ez::OperatorOp::Mod {
                num_a: mem_get(&args.left)?,
                num_b: mem_get(&args.right)?,
            }),
        )?]),
        link::DataCommand::Neq(args) => {
            Vec::from([Arc::new(ez::Op::Control(ez::ControlOp::IfElse {
                condition: operator_expr(ez::OperatorOp::Equals {
                    operand_a: mem_get(&args.left)?,
                    operand_b: mem_get(&args.right)?,
                }),
                then_substack: stack!(mem_set(&args.dest, &lit(""))?),
                else_substack: stack!(mem_set(&args.dest, &lit("1"))?),
            }))])
        },
        link::DataCommand::Gt(args) => {
            Vec::from([Arc::new(ez::Op::Control(ez::ControlOp::IfElse {
                condition: operator_expr(ez::OperatorOp::GreaterThan {
                    operand_a: mem_get(&args.left)?,
                    operand_b: mem_get(&args.right)?,
                }),
                then_substack: stack!(mem_set(&args.dest, &lit("1"))?),
                else_substack: stack!(mem_set(&args.dest, &lit(""))?),
            }))])
        },
        link::DataCommand::Lt(args) => {
            Vec::from([Arc::new(ez::Op::Control(ez::ControlOp::IfElse {
                condition: operator_expr(ez::OperatorOp::LessThan {
                    operand_a: mem_get(&args.left)?,
                    operand_b: mem_get(&args.right)?,
                }),
                then_substack: stack!(mem_set(&args.dest, &lit("1"))?),
                else_substack: stack!(mem_set(&args.dest, &lit(""))?),
            }))])
        },
        // TODO: optimize these into NOT (opposite inequality)
        link::DataCommand::Gte(args) => {
            let left_expr = mem_get(&args.left)?;
            let right_expr = mem_get(&args.right)?;

            Vec::from([Arc::new(ez::Op::Control(ez::ControlOp::IfElse {
                condition: operator_expr(ez::OperatorOp::Or {
                    operand_a: operator_expr(ez::OperatorOp::GreaterThan {
                        operand_a: Arc::clone(&left_expr),
                        operand_b: Arc::clone(&right_expr),
                    }),
                    operand_b: operator_expr(ez::OperatorOp::Equals {
                        operand_a: left_expr,
                        operand_b: right_expr,
                    }),
                }),
                then_substack: stack!(mem_set(&args.dest, &lit("1"))?),
                else_substack: stack!(mem_set(&args.dest, &lit(""))?),
            }))])
        },
        link::DataCommand::Lte(args) => {
            let left_expr = mem_get(&args.left)?;
            let right_expr = mem_get(&args.right)?;

            Vec::from([Arc::new(ez::Op::Control(ez::ControlOp::IfElse {
                condition: operator_expr(ez::OperatorOp::Or {
                    operand_a: operator_expr(ez::OperatorOp::LessThan {
                        operand_a: Arc::clone(&left_expr),
                        operand_b: Arc::clone(&right_expr),
                    }),
                    operand_b: operator_expr(ez::OperatorOp::Equals {
                        operand_a: left_expr,
                        operand_b: right_expr,
                    }),
                }),
                then_substack: stack!(mem_set(&args.dest, &lit("1"))?),
                else_substack: stack!(mem_set(&args.dest, &lit(""))?),
            }))])
        },
        link::DataCommand::And(args) => {
            Vec::from([Arc::new(ez::Op::Control(ez::ControlOp::IfElse {
                condition: operator_expr(ez::OperatorOp::Or {
                    operand_a: operator_expr(ez::OperatorOp::Equals {
                        operand_a: mem_get(&args.left)?,
                        operand_b: lit(""),
                    }),
                    operand_b: operator_expr(ez::OperatorOp::Equals {
                        operand_a: mem_get(&args.right)?,
                        operand_b: lit(""),
                    }),
                }),
                then_substack: stack!(mem_set(&args.dest, &lit(""))?),
                else_substack: stack!(mem_set(&args.dest, &lit("1"))?),
            }))])
        },
        link::DataCommand::Or(args) => {
            Vec::from([Arc::new(ez::Op::Control(ez::ControlOp::IfElse {
                condition: operator_expr(ez::OperatorOp::And {
                    operand_a: operator_expr(ez::OperatorOp::Equals {
                        operand_a: mem_get(&args.left)?,
                        operand_b: lit(""),
                    }),
                    operand_b: operator_expr(ez::OperatorOp::Equals {
                        operand_a: mem_get(&args.right)?,
                        operand_b: lit(""),
                    }),
                }),
                then_substack: stack!(mem_set(&args.dest, &lit(""))?),
                else_substack: stack!(mem_set(&args.dest, &lit("1"))?),
            }))])
        },
        link::DataCommand::Xor(args) => {
            let left_false = operator_expr(ez::OperatorOp::Equals {
                operand_a: mem_get(&args.left)?,
                operand_b: lit(""),
            });

            let right_false = operator_expr(ez::OperatorOp::Equals {
                operand_a: mem_get(&args.right)?,
                operand_b: lit(""),
            });

            let left_f_right_t = operator_expr(ez::OperatorOp::And {
                operand_a: Arc::clone(&left_false),
                operand_b: operator_expr(ez::OperatorOp::Not { operand: Arc::clone(&right_false) }),
            });

            let left_t_right_f = operator_expr(ez::OperatorOp::And {
                operand_a: operator_expr(ez::OperatorOp::Not { operand: left_false }),
                operand_b: right_false,
            });

            Vec::from([Arc::new(ez::Op::Control(ez::ControlOp::IfElse {
                condition: operator_expr(ez::OperatorOp::Or {
                    operand_a: left_f_right_t,
                    operand_b: left_t_right_f,
                }),
                then_substack: stack!(mem_set(&args.dest, &lit("1"))?),
                else_substack: stack!(mem_set(&args.dest, &lit(""))?),
            }))])
        },
        link::DataCommand::Join(args) => Vec::from([mem_set(
            &args.dest,
            &operator_expr(ez::OperatorOp::Join {
                string_a: mem_get(&args.left)?,
                string_b: mem_get(&args.right)?,
            }),
        )?]),
    };

    Ok(ops)
}

fn compile_call(
    call: &link::Call,
    next_proc: Option<&link::Procedure>,
    stack_list: &Arc<ez::List>,
    sub_proc_name_to_broadcast: &HashMap<Arc<str>, Arc<ez::Broadcast>>,
    reg_name_to_var: &HashMap<Arc<str>, Arc<ez::Variable>>,
) -> anyhow::Result<Option<ez::Op>> {
    let ez_op = match call {
        link::Call::Jump { proc_name_addr } => Some(ez::Op::Event(ez::EventOp::BroadcastAndWait {
            input: mem_get(stack_list, reg_name_to_var, &proc_name_addr)?,
        })),
        link::Call::Passthrough => match next_proc {
            None => None,
            Some(next_proc) => match &next_proc.kind {
                link::ProcedureKind::Main => {
                    bail!("Attempted to passthrough to main");
                },
                link::ProcedureKind::Sub { name } => {
                    let Some(next_proc_broadcast) = sub_proc_name_to_broadcast.get(name) else {
                        bail!("Could not find broadcast for passthrough proc");
                    };

                    Some(ez::Op::Event(ez::EventOp::BroadcastAndWait {
                        input: Arc::new(ez::Expr::Broadcast(Arc::clone(next_proc_broadcast))),
                    }))
                },
            },
        },
        link::Call::Exit => None,
        link::Call::Branch { proc_name_addr, cond_addr } => {
            let cond_op = mem_get(stack_list, reg_name_to_var, cond_addr)?;
            let null_op = lit("");

            let passthrough_op = match next_proc {
                None => None,
                Some(next_proc) => match &next_proc.kind {
                    link::ProcedureKind::Main => {
                        bail!("Attempted to passthrough to main");
                    },
                    link::ProcedureKind::Sub { name } => {
                        let Some(next_proc_broadcast) = sub_proc_name_to_broadcast.get(name) else {
                            bail!("Could not find broadcast for passthrough proc");
                        };

                        Some(ez::Op::Event(ez::EventOp::BroadcastAndWait {
                            input: Arc::new(ez::Expr::Broadcast(Arc::clone(next_proc_broadcast))),
                        }))
                    },
                },
            };

            let false_op = passthrough_op.map(|passthrough_op| {
                Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                    root: Arc::new(passthrough_op),
                    rest: Arc::new(Vec::new()),
                })))
            });

            let jump_op = Arc::new(ez::Op::Event(ez::EventOp::BroadcastAndWait {
                input: mem_get(stack_list, reg_name_to_var, proc_name_addr)?,
            }));

            let true_op = Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                root: jump_op,
                rest: Arc::new(Vec::new()),
            })));

            let cond_op =
                Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Equals {
                    operand_a: cond_op,
                    operand_b: null_op,
                }))));

            let branch_op = match false_op {
                Some(false_op) => {
                    ez::Op::Control(ez::ControlOp::IfElse {
                        condition: cond_op,
                        // if condition was false, passthrough
                        then_substack: false_op,
                        // if condition was true, jump
                        else_substack: true_op,
                    })
                },
                // if
                None => ez::Op::Control(ez::ControlOp::If {
                    condition: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(
                        ez::OperatorOp::Not { operand: cond_op },
                    )))),
                    then_substack: true_op,
                }),
            };

            Some(branch_op)
        },
    };

    Ok(ez_op)
}
