use std::{collections::HashMap, sync::Arc};

use anyhow::bail;
use inter::ez;
use uuid::Uuid;

use crate::ast;

pub fn compile(ast: &[&ast::Procedure]) -> anyhow::Result<ez::Program> {
    let stack_list = Arc::new(ez::List { uuid: Uuid::new_v4(), name: "stack".into() });
    let stdout_list = Arc::new(ez::List { uuid: Uuid::new_v4(), name: "stdout".into() });

    let mut sub_proc_name_to_broadcast = HashMap::<Arc<str>, Arc<ez::Broadcast>>::new();

    for proc in ast {
        if let ast::ProcedureKind::Sub { name } = &proc.kind {
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

    for proc in ast {
        let compiled_proc_ops = compile_proc(
            proc.commands.iter().map(AsRef::as_ref),
            &stack_list,
            &stdout_list,
            &sub_proc_name_to_broadcast,
        )?;

        match &proc.kind {
            ast::ProcedureKind::Main => {
                let prev_main_ops = compiled_main_proc.replace(compiled_proc_ops);
                if prev_main_ops.is_some() {
                    bail!("More than one main procedure found");
                }
            },
            ast::ProcedureKind::Sub { name } => {
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

fn compile_proc<'a>(
    commands: impl Iterator<Item = &'a ast::Command>,
    stack_list: &Arc<ez::List>,
    stdout_list: &Arc<ez::List>,
    sub_proc_name_to_broadcast: &HashMap<Arc<str>, Arc<ez::Broadcast>>,
) -> anyhow::Result<Vec<Arc<ez::Op>>> {
    let mut ez_compiled_ops = Vec::<Arc<ez::Op>>::new();
    for command in commands {
        let ez_op = match command {
            ast::Command::Set(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &args.dest.str,
                ))))),
                item: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &args.val.str,
                ))))),
            }),
            ast::Command::Move(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &args.dest.str,
                ))))),
                item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                    list: Arc::clone(stack_list),
                    index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                        &args.val.str,
                    ))))),
                })))),
            }),
            ast::Command::MoveDerefDest(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(stack_list),
                index: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                    ez::DataOp::ItemOfList {
                        list: Arc::clone(stack_list),
                        index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            Arc::clone(&args.dest.str),
                        )))),
                    },
                )))),
                item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                    list: Arc::clone(stack_list),
                    index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                        &args.val.str,
                    ))))),
                })))),
            }),
            ast::Command::MoveDerefSrc(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &args.dest.str,
                ))))),
                item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                    list: Arc::clone(stack_list),
                    index: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                        ez::DataOp::ItemOfList {
                            list: Arc::clone(stack_list),
                            index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                Arc::clone(&args.val.str),
                            )))),
                        },
                    )))),
                })))),
            }),
            ast::Command::Add(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &args.dest.str,
                ))))),
                item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(
                    ez::OperatorOp::Add {
                        num_a: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                            ez::DataOp::ItemOfList {
                                list: Arc::clone(stack_list),
                                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                    Arc::clone(&args.left.str),
                                )))),
                            },
                        )))),
                        num_b: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                            ez::DataOp::ItemOfList {
                                list: Arc::clone(stack_list),
                                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                    Arc::clone(&args.right.str),
                                )))),
                            },
                        )))),
                    },
                )))),
            }),
            ast::Command::Sub(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &args.dest.str,
                ))))),
                item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(
                    ez::OperatorOp::Subtract {
                        num_a: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                            ez::DataOp::ItemOfList {
                                list: Arc::clone(stack_list),
                                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                    Arc::clone(&args.left.str),
                                )))),
                            },
                        )))),
                        num_b: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                            ez::DataOp::ItemOfList {
                                list: Arc::clone(stack_list),
                                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                    Arc::clone(&args.right.str),
                                )))),
                            },
                        )))),
                    },
                )))),
            }),
            ast::Command::Jump(args) => ez::Op::Event(ez::EventOp::BroadcastAndWait {
                input: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                    ez::DataOp::ItemOfList {
                        list: Arc::clone(stack_list),
                        index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            Arc::clone(&args.val.str),
                        )))),
                    },
                )))),
            }),
            ast::Command::BranchEq(args) => {
                let broadcast_op = Arc::new(ez::Op::Event(ez::EventOp::BroadcastAndWait {
                    input: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                        ez::DataOp::ItemOfList {
                            list: Arc::clone(stack_list),
                            index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                Arc::clone(&args.dest.str),
                            )))),
                        },
                    )))),
                }));

                let left_op =
                    Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                        list: Arc::clone(stack_list),
                        index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            Arc::clone(&args.left.str),
                        )))),
                    }))));

                let right_op =
                    Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                        list: Arc::clone(stack_list),
                        index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            Arc::clone(&args.right.str),
                        )))),
                    }))));

                let condition_expr =
                    ez::Expr::derived(&Arc::new(ez::Op::Operator(ez::OperatorOp::Equals {
                        operand_a: left_op,
                        operand_b: right_op,
                    })));

                ez::Op::Control(ez::ControlOp::If {
                    condition: condition_expr,
                    then_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                        root: broadcast_op,
                        rest: Default::default(),
                    }))),
                })
            },
            ast::Command::Out(args) => {
                let addr = Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &args.val.str,
                )))));

                let value = Arc::new(ez::Expr::Derived(Arc::new(inter::ez::Op::Data(
                    ez::DataOp::ItemOfList { list: Arc::clone(stack_list), index: addr },
                ))));

                ez::Op::Data(ez::DataOp::AddToList { list: Arc::clone(stdout_list), item: value })
            },
            ast::Command::Eq(args) => {
                let dest_addr = Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                    Arc::clone(&args.dest.str),
                ))));

                let true_op = Arc::new(ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                    list: Arc::clone(stack_list),
                    index: Arc::clone(&dest_addr),
                    item: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String("1".into())))),
                }));

                let false_op = Arc::new(ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                    list: Arc::clone(stack_list),
                    index: dest_addr,
                    item: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String("".into())))),
                }));

                let left_op =
                    Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                        list: Arc::clone(stack_list),
                        index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            Arc::clone(&args.left.str),
                        )))),
                    }))));

                let right_op =
                    Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                        list: Arc::clone(stack_list),
                        index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            Arc::clone(&args.right.str),
                        )))),
                    }))));

                let condition_expr =
                    ez::Expr::derived(&Arc::new(ez::Op::Operator(ez::OperatorOp::Equals {
                        operand_a: left_op,
                        operand_b: right_op,
                    })));

                ez::Op::Control(ez::ControlOp::IfElse {
                    condition: condition_expr,
                    then_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                        root: true_op,
                        rest: Default::default(),
                    }))),
                    else_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                        root: false_op,
                        rest: Default::default(),
                    }))),
                })
            },
            ast::Command::Not(args) => {
                let dest_addr = Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                    Arc::clone(&args.dest.str),
                ))));

                let true_op = Arc::new(ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                    list: Arc::clone(stack_list),
                    index: Arc::clone(&dest_addr),
                    item: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String("1".into())))),
                }));

                let false_op = Arc::new(ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                    list: Arc::clone(stack_list),
                    index: dest_addr,
                    item: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String("".into())))),
                }));

                let val_op =
                    Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                        list: Arc::clone(stack_list),
                        index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            Arc::clone(&args.val.str),
                        )))),
                    }))));

                let condition_expr =
                    ez::Expr::derived(&Arc::new(ez::Op::Operator(ez::OperatorOp::Equals {
                        operand_a: val_op,
                        operand_b: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            "".into(),
                        )))),
                    })));

                ez::Op::Control(ez::ControlOp::IfElse {
                    condition: condition_expr,
                    then_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                        root: true_op,
                        rest: Default::default(),
                    }))),
                    else_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                        root: false_op,
                        rest: Default::default(),
                    }))),
                })
            },
        };

        ez_compiled_ops.push(Arc::new(ez_op));
    }

    Ok(ez_compiled_ops)
}
