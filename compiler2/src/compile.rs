use std::sync::Arc;

use inter::ez;
use uuid::Uuid;

use crate::ast;

pub fn compile<'a>(ast: impl Iterator<Item = &'a ast::Command>) -> ez::Program {
    let stack_list = Arc::new(ez::List { uuid: Uuid::new_v4(), name: "stack".into() });

    let mut ez_compiled_ops = Vec::<Arc<ez::Op>>::new();
    for command in ast {
        let ez_op = match command {
            ast::Command::Set(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(&stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &args.dest.str,
                ))))),
                item: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &args.val.str,
                ))))),
            }),
            ast::Command::Load(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(&stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &args.dest.str,
                ))))),
                item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                    list: Arc::clone(&stack_list),
                    index: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                        ez::DataOp::ItemOfList {
                            list: Arc::clone(&stack_list),
                            index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                Arc::clone(&args.val.str),
                            )))),
                        },
                    )))),
                })))),
            }),
            ast::Command::Store(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(&stack_list),
                index: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                    ez::DataOp::ItemOfList {
                        list: Arc::clone(&stack_list),
                        index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            Arc::clone(&args.dest.str),
                        )))),
                    },
                )))),
                item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                    list: Arc::clone(&stack_list),
                    index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                        &args.val.str,
                    ))))),
                })))),
            }),
            ast::Command::Add(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(&stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &args.dest.str,
                ))))),
                item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(
                    ez::OperatorOp::Add {
                        num_a: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                            ez::DataOp::ItemOfList {
                                list: Arc::clone(&stack_list),
                                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                    Arc::clone(&args.left.str),
                                )))),
                            },
                        )))),
                        num_b: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                            ez::DataOp::ItemOfList {
                                list: Arc::clone(&stack_list),
                                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                    Arc::clone(&args.right.str),
                                )))),
                            },
                        )))),
                    },
                )))),
            }),
            ast::Command::Sub(args) => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(&stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &args.dest.str,
                ))))),
                item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(
                    ez::OperatorOp::Subtract {
                        num_a: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                            ez::DataOp::ItemOfList {
                                list: Arc::clone(&stack_list),
                                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                    Arc::clone(&args.left.str),
                                )))),
                            },
                        )))),
                        num_b: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                            ez::DataOp::ItemOfList {
                                list: Arc::clone(&stack_list),
                                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                    Arc::clone(&args.right.str),
                                )))),
                            },
                        )))),
                    },
                )))),
            }),
        };

        ez_compiled_ops.push(Arc::new(ez_op));
    }

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

    let stack = Arc::new(ez::Stack {
        root: stack_root,
        rest: Arc::new(reset_ops.into_iter().chain(ez_compiled_ops).collect()),
    });

    let stage = Arc::new(ez::Stage {
        broadcasts: Arc::new(Vec::new()),
        lists: Arc::new(Vec::from([stack_list])),
        stack,
    });

    ez::Program { monitors: Arc::new(Vec::new()), stages: Arc::new(Vec::from([stage])) }
}
