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
        // let ez_op = match command {
        //     ast::Command::Data(command) => compile_data_command(command, stack_list, stdout_list),
        //     ast::Command::Branch(command) => compile_branch_command(command, stack_list),
        // };

        let ez_op = compile_data_command(command.as_ref(), stack_list, stdout_list);
        ez_compiled_ops.push(Arc::new(ez_op));
    }

    let next_call_ez_op =
        compile_call(&proc.body.next_call, next_proc, stack_list, sub_proc_name_to_broadcast)?;

    if let Some(ez_op) = next_call_ez_op {
        ez_compiled_ops.push(Arc::new(ez_op));
    }

    Ok(ez_compiled_ops)
}

fn compile_data_command(
    command: &link::DataCommand,
    stack_list: &Arc<ez::List>,
    stdout_list: &Arc<ez::List>,
) -> ez::Op {
    match command {
        link::DataCommand::Set(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
            list: Arc::clone(stack_list),
            index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                &args.dest,
            ))))),
            item: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(&args.val))))),
        }),
        link::DataCommand::Move(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
            list: Arc::clone(stack_list),
            index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                &args.dest,
            ))))),
            item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                list: Arc::clone(stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &args.val,
                ))))),
            })))),
        }),
        link::DataCommand::MoveDerefDest(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
            list: Arc::clone(stack_list),
            index: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                list: Arc::clone(stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &args.dest,
                ))))),
            })))),
            item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                list: Arc::clone(stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &args.val,
                ))))),
            })))),
        }),
        link::DataCommand::MoveDerefSrc(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
            list: Arc::clone(stack_list),
            index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                &args.dest,
            ))))),
            item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                list: Arc::clone(stack_list),
                index: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                    ez::DataOp::ItemOfList {
                        list: Arc::clone(stack_list),
                        index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            Arc::clone(&args.val),
                        )))),
                    },
                )))),
            })))),
        }),
        link::DataCommand::Add(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
            list: Arc::clone(stack_list),
            index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                &args.dest,
            ))))),
            item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Add {
                num_a: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                    ez::DataOp::ItemOfList {
                        list: Arc::clone(stack_list),
                        index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            Arc::clone(&args.left),
                        )))),
                    },
                )))),
                num_b: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                    ez::DataOp::ItemOfList {
                        list: Arc::clone(stack_list),
                        index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            Arc::clone(&args.right),
                        )))),
                    },
                )))),
            })))),
        }),
        link::DataCommand::Sub(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
            list: Arc::clone(stack_list),
            index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                &args.dest,
            ))))),
            item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(
                ez::OperatorOp::Subtract {
                    num_a: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                        ez::DataOp::ItemOfList {
                            list: Arc::clone(stack_list),
                            index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                Arc::clone(&args.left),
                            )))),
                        },
                    )))),
                    num_b: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                        ez::DataOp::ItemOfList {
                            list: Arc::clone(stack_list),
                            index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                Arc::clone(&args.right),
                            )))),
                        },
                    )))),
                },
            )))),
        }),
        link::DataCommand::Out(args) => {
            let addr =
                Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(&args.val)))));

            let value = Arc::new(ez::Expr::Derived(Arc::new(inter::ez::Op::Data(
                ez::DataOp::ItemOfList { list: Arc::clone(stack_list), index: addr },
            ))));

            ez::Op::Data(ez::DataOp::AddToList { list: Arc::clone(stdout_list), item: value })
        },
        link::DataCommand::Eq(args) => {
            let dest_addr =
                Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(&args.dest)))));

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
                    index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                        &args.left,
                    ))))),
                }))));

            let right_op =
                Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                    list: Arc::clone(stack_list),
                    index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                        &args.right,
                    ))))),
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
        link::DataCommand::Not(args) => {
            let dest_addr =
                Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(&args.dest)))));

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
                    index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                        &args.val,
                    ))))),
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
    }
}

// fn compile_branch_command(command: &ast::ControlCommand, stack_list: &Arc<ez::List>) -> ez::Op {
//     match command {
//         ast::ControlCommand::Jump(args) => ez::Op::Event(ez::EventOp::BroadcastAndWait {
//             input: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
//                 list: Arc::clone(stack_list),
//                 index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
//                     &args.val,
//                 ))))),
//             })))),
//         }),
//         ast::ControlCommand::BranchEq(args) => {
//             let broadcast_op = Arc::new(ez::Op::Event(ez::EventOp::BroadcastAndWait {
//                 input: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
//                     ez::DataOp::ItemOfList {
//                         list: Arc::clone(stack_list),
//                         index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
//                             Arc::clone(&args.dest),
//                         )))),
//                     },
//                 )))),
//             }));

//             let left_op =
//                 Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
//                     list: Arc::clone(stack_list),
//                     index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
//                         &args.left,
//                     ))))),
//                 }))));

//             let right_op =
//                 Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
//                     list: Arc::clone(stack_list),
//                     index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
//                         &args.right,
//                     ))))),
//                 }))));

//             let condition_expr =
//                 ez::Expr::derived(&Arc::new(ez::Op::Operator(ez::OperatorOp::Equals {
//                     operand_a: left_op,
//                     operand_b: right_op,
//                 })));

//             ez::Op::Control(ez::ControlOp::If {
//                 condition: condition_expr,
//                 then_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
//                     root: broadcast_op,
//                     rest: Default::default(),
//                 }))),
//             })
//         },
//     }
// }

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
    };

    Ok(ez_op)
}
