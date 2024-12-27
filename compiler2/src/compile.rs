use std::{collections::HashMap, sync::Arc};

use anyhow::bail;
use inter::ez;
use uuid::Uuid;

use crate::ast;

pub fn compile(ast: &[&ast::Procedure]) -> anyhow::Result<ez::Program> {
    let stack_list = Arc::new(ez::List { uuid: Uuid::new_v4(), name: "stack".into() });

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
            ast::Command::Load(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
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
            ast::Command::Store(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
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
                            Arc::clone(&args.src.str),
                        )))),
                    },
                )))),
            }),
        };

        ez_compiled_ops.push(Arc::new(ez_op));
    }

    Ok(ez_compiled_ops)
}
