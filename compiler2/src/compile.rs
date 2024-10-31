use std::sync::Arc;

use inter::ez;
use uuid::Uuid;

use crate::ast;

pub fn compile<'a>(ast: impl Iterator<Item = &'a ast::Command>) -> ez::Program {
    let stack_list = Arc::new(ez::List { uuid: Uuid::new_v4(), name: "stack".into() });

    let mut ez_compiled_ops = Vec::<Arc<ez::Op>>::new();
    for command in ast {
        let ez_op = match command {
            ast::Command::Push { val } => ez::Op::Data(ez::DataOp::AddToList {
                list: Arc::clone(&stack_list),
                item: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &val.str,
                ))))),
            }),
            ast::Command::Set { dest, val } => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(&stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &dest.str,
                ))))),
                item: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &val.str,
                ))))),
            }),
            ast::Command::Load { dest, src } => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(&stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                    &dest.str,
                ))))),
                item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                    list: Arc::clone(&stack_list),
                    index: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                        ez::DataOp::ItemOfList {
                            list: Arc::clone(&stack_list),
                            index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                Arc::clone(&src.str),
                            )))),
                        },
                    )))),
                })))),
            }),
            ast::Command::Store { dest, src } => ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: Arc::clone(&stack_list),
                index: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                    ez::DataOp::ItemOfList {
                        list: Arc::clone(&stack_list),
                        index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            Arc::clone(&dest.str),
                        )))),
                    },
                )))),
                item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                    list: Arc::clone(&stack_list),
                    index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                        &src.str,
                    ))))),
                })))),
            }),
            ast::Command::Add { dest, left, right } => {
                ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                    list: Arc::clone(&stack_list),
                    index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                        &dest.str,
                    ))))),
                    item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(
                        ez::OperatorOp::Add {
                            num_a: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                                ez::DataOp::ItemOfList {
                                    list: Arc::clone(&stack_list),
                                    index: Arc::new(ez::Expr::Literal(Arc::new(
                                        ez::Literal::String(Arc::clone(&left.str)),
                                    ))),
                                },
                            )))),
                            num_b: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                                ez::DataOp::ItemOfList {
                                    list: Arc::clone(&stack_list),
                                    index: Arc::new(ez::Expr::Literal(Arc::new(
                                        ez::Literal::String(Arc::clone(&right.str)),
                                    ))),
                                },
                            )))),
                        },
                    )))),
                })
            },
            ast::Command::Sub { dest, left, right } => {
                ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                    list: Arc::clone(&stack_list),
                    index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(Arc::clone(
                        &dest.str,
                    ))))),
                    item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(
                        ez::OperatorOp::Subtract {
                            num_a: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                                ez::DataOp::ItemOfList {
                                    list: Arc::clone(&stack_list),
                                    index: Arc::new(ez::Expr::Literal(Arc::new(
                                        ez::Literal::String(Arc::clone(&left.str)),
                                    ))),
                                },
                            )))),
                            num_b: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(
                                ez::DataOp::ItemOfList {
                                    list: Arc::clone(&stack_list),
                                    index: Arc::new(ez::Expr::Literal(Arc::new(
                                        ez::Literal::String(Arc::clone(&right.str)),
                                    ))),
                                },
                            )))),
                        },
                    )))),
                })
            },
        };

        ez_compiled_ops.push(Arc::new(ez_op));
    }

    let stack_root = Arc::new(ez::Op::Event(ez::EventOp::WhenFlagClicked));
    let reset_ops = Vec::from([Arc::new(ez::Op::Data(ez::DataOp::DeleteAllOfList {
        list: Arc::clone(&stack_list),
    }))]);

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
