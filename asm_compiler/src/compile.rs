use std::{collections::HashMap, sync::Arc};

use anyhow::bail;
use inter::ez;
use uuid::Uuid;

use crate::{ast, link};

pub fn compile(ast: &[&link::Procedure]) -> anyhow::Result<ez::Program> {
    let stack_list = Arc::new(ez::List { uuid: Uuid::new_v4(), name: "stack".into() });
    let stdout_list = Arc::new(ez::List { uuid: Uuid::new_v4(), name: "stdout".into() });

    let mut sub_proc_name_to_broadcast = HashMap::<Arc<str>, Arc<ez::Broadcast>>::new();

    for proc in ast {
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

    let mut ast = ast.iter().peekable();

    while let Some(proc) = ast.next() {
        let compiled_proc_ops = compile_proc(
            proc,
            ast.peek().map(|x| **x),
            &stack_list,
            &stdout_list,
            &sub_proc_name_to_broadcast,
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

    let broadcasts =
        Arc::new(compiled_sub_procs.iter().map(|proc| Arc::clone(&proc.broadcast)).collect());

    for compiled_proc in compiled_sub_procs {
        let root = Arc::new(ez::Op::Event(ez::EventOp::WhenBroadcastReceived {
            broadcast: Arc::clone(&compiled_proc.broadcast),
        }));

        let stack = Arc::new(ez::Stack { root, rest: Arc::new(compiled_proc.compiled_ops) });
        stacks.push(stack);
    }

    let stage = Arc::new(ez::Stage {
        broadcasts,
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
) -> anyhow::Result<Vec<Arc<ez::Op>>> {
    let mut ez_compiled_ops = Vec::<Arc<ez::Op>>::new();
    for command in &proc.body.commands {
        let ez_ops = compile_data_command(command.as_ref(), stack_list, stdout_list);
        ez_compiled_ops.extend(ez_ops);
    }

    let next_call_ez_op =
        compile_call(&proc.body.next_call, next_proc, stack_list, sub_proc_name_to_broadcast)?;

    if let Some(ez_op) = next_call_ez_op {
        ez_compiled_ops.push(Arc::new(ez_op));
    }

    Ok(ez_compiled_ops)
}

fn lit(addr: &str) -> Arc<ez::Expr> {
    Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(addr.into()))))
}

fn index_expr(list: &Arc<ez::List>, index: &Arc<ez::Expr>) -> Arc<ez::Expr> {
    data_expr(ez::DataOp::ItemOfList { list: Arc::clone(list), index: Arc::clone(index) })
}

fn operator_expr(op: ez::OperatorOp) -> Arc<ez::Expr> {
    Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(op))))
}

fn data_expr(op: ez::DataOp) -> Arc<ez::Expr> {
    Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(op))))
}

fn set(list: &Arc<ez::List>, index: &Arc<ez::Expr>, value: &Arc<ez::Expr>) -> Arc<ez::Op> {
    Arc::new(ez::Op::Data(ez::DataOp::ReplaceItemOfList {
        list: Arc::clone(list),
        index: Arc::clone(index),
        item: Arc::clone(value),
    }))
}

fn compile_data_command(
    command: &link::DataCommand,
    stack_list: &Arc<ez::List>,
    stdout_list: &Arc<ez::List>,
) -> Vec<Arc<ez::Op>> {
    match command {
        link::DataCommand::Set(args) => {
            Vec::from([set(stack_list, &lit(&args.dest), &lit(&args.val))])
        },
        link::DataCommand::Move(args) => {
            Vec::from([set(stack_list, &lit(&args.dest), &index_expr(stack_list, &lit(&args.val)))])
        },
        link::DataCommand::MoveDerefDest(args) => Vec::from([set(
            stack_list,
            &index_expr(stack_list, &lit(&args.dest)),
            &index_expr(stack_list, &lit(&args.val)),
        )]),
        link::DataCommand::MoveDerefSrc(args) => Vec::from([set(
            stack_list,
            &lit(&args.dest),
            &index_expr(stack_list, &index_expr(stack_list, &lit(&args.val))),
        )]),
        link::DataCommand::Add(args) => Vec::from([set(
            stack_list,
            &lit(&args.dest),
            &operator_expr(ez::OperatorOp::Add {
                num_a: index_expr(stack_list, &lit(&args.left)),
                num_b: index_expr(stack_list, &lit(&args.right)),
            }),
        )]),
        link::DataCommand::Sub(args) => Vec::from([set(
            stack_list,
            &lit(&args.dest),
            &operator_expr(ez::OperatorOp::Subtract {
                num_a: index_expr(stack_list, &lit(&args.left)),
                num_b: index_expr(stack_list, &lit(&args.right)),
            }),
        )]),
        link::DataCommand::Out(args) => {
            Vec::from([Arc::new(ez::Op::Data(ez::DataOp::AddToList {
                list: Arc::clone(stdout_list),
                item: index_expr(stack_list, &lit(&args.val)),
            }))])
        },
        link::DataCommand::Eq(args) => {
            let dest_addr = lit(&args.dest);

            let true_op = Arc::new(ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(stack_list),
                index: Arc::clone(&dest_addr),
                item: lit("1"),
            }));

            let false_op = Arc::new(ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(stack_list),
                index: dest_addr,
                item: lit(""),
            }));

            let condition_expr = operator_expr(ez::OperatorOp::Equals {
                operand_a: index_expr(stack_list, &lit(&args.left)),
                operand_b: index_expr(stack_list, &lit(&args.right)),
            });

            Vec::from([Arc::new(ez::Op::Control(ez::ControlOp::IfElse {
                condition: condition_expr,
                then_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                    root: true_op,
                    rest: Default::default(),
                }))),
                else_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                    root: false_op,
                    rest: Default::default(),
                }))),
            }))])
        },
        link::DataCommand::Not(args) => {
            let dest_addr = lit(&args.dest);

            let true_op = Arc::new(ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(stack_list),
                index: Arc::clone(&dest_addr),
                item: lit("1"),
            }));

            let false_op = Arc::new(ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(stack_list),
                index: dest_addr,
                item: lit(""),
            }));

            let condition_expr = operator_expr(ez::OperatorOp::Equals {
                operand_a: index_expr(stack_list, &lit(&args.val)),
                operand_b: lit(""),
            });

            Vec::from([Arc::new(ez::Op::Control(ez::ControlOp::IfElse {
                condition: condition_expr,
                then_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                    root: true_op,
                    rest: Default::default(),
                }))),
                else_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                    root: false_op,
                    rest: Default::default(),
                }))),
            }))])
        },
        link::DataCommand::In(args) => {
            let ask_op = Arc::new(ez::Op::Sensing(ez::SensingOp::AskAndWait { question: lit("") }));

            let save_answer_op = set(
                stack_list,
                &lit(&args.val),
                &Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Sensing(ez::SensingOp::Answer)))),
            );

            Vec::from([ask_op, save_answer_op])
        },
    }
}

fn compile_call(
    call: &link::Call,
    next_proc: Option<&link::Procedure>,
    stack_list: &Arc<ez::List>,
    sub_proc_name_to_broadcast: &HashMap<Arc<str>, Arc<ez::Broadcast>>,
) -> anyhow::Result<Option<ez::Op>> {
    let ez_op = match call {
        link::Call::Jump { proc_name_addr } => Some(ez::Op::Event(ez::EventOp::BroadcastAndWait {
            input: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                list: Arc::clone(stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    proc_name_addr,
                ))))),
            })))),
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
            let cond_op =
                Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                    list: Arc::clone(stack_list),
                    index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                        cond_addr,
                    ))))),
                }))));

            let null_op = Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String("".into()))));

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
                input: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                    ez::DataOp::ItemOfList {
                        list: Arc::clone(stack_list),
                        index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            Arc::clone(proc_name_addr),
                        )))),
                    },
                )))),
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
