use std::sync::{Arc, LazyLock};

use crate::{
    logic_ast as l,
    srced::{SrcRange, Srced},
};

pub fn add_builtins(l: &l::Ref<l::Program>) -> l::Ref<l::Program> {
    let mut top_items = l.val.items.val.to_vec();

    static SRC_RANGE: LazyLock<SrcRange> = LazyLock::new(SrcRange::default);

    fn rf<T>(t: T) -> l::Ref<T> {
        Arc::new(Srced { val: t, range: *SRC_RANGE })
    }

    fn decl(name: &str, ty: l::TypeHint) -> l::Ref<l::IdentDeclaration> {
        rf(l::IdentDeclaration { name: rf(l::Name { str: name.into() }), ty: rf(ty) })
    }

    fn place(ident_name: &str) -> l::Ref<l::Place> {
        rf(l::Place {
            head: rf(l::PlaceHead::Ident(rf(l::Ident { name: name(ident_name) }))),
            index_chain: rf(Vec::new()),
        })
    }

    fn deref(addr: l::Ref<l::Expr>) -> l::Ref<l::Place> {
        rf(l::Place {
            head: rf(l::PlaceHead::Deref(rf(l::Deref { addr }))),
            index_chain: rf(Vec::new()),
        })
    }

    fn name(name: &str) -> l::Ref<l::Name> {
        rf(l::Name { str: name.into() })
    }

    fn stmt(statement: l::Statement) -> l::Ref<l::BodyItem> {
        rf(l::BodyItem::Statement(rf(statement)))
    }

    fn body(items: impl IntoIterator<Item = l::Ref<l::BodyItem>>) -> l::Ref<l::Body> {
        rf(l::Body { items: rf(items.into_iter().collect()) })
    }

    fn func(func: l::Func) -> l::Ref<l::TopItem> {
        rf(l::TopItem::Exe(rf(l::ExeItem::Func(rf(func)))))
    }

    fn ref_ty(ty: l::TypeHint) -> l::TypeHint {
        l::TypeHint::Ref(rf(ty))
    }

    // add built-in native functions
    top_items.push(func(l::Func {
        name: name("out"),
        params: rf(Vec::from([decl("val", l::TypeHint::Any)])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([])),
            body: body([stmt(l::Statement::Native(rf(l::NativeOperation::Out {
                place: place("val"),
            })))]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("in"),
        params: rf(Vec::from([decl(
            "return",
            ref_ty(l::TypeHint::Primitive(l::PrimitiveType::Val)),
        )])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([decl("answer", l::TypeHint::Primitive(l::PrimitiveType::Val))])),
            body: body([
                stmt(l::Statement::Native(rf(l::NativeOperation::In {
                    dest_place: place("answer"),
                }))),
                stmt(l::Statement::Assign(rf(l::Assign {
                    place: deref(rf(l::Expr::Place(place("return")))),
                    expr: rf(l::AssignExpr::Expr(rf(l::Expr::Place(place("answer"))))),
                }))),
            ]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("random_num"),
        params: rf(Vec::from([
            decl("return", ref_ty(l::TypeHint::Primitive(l::PrimitiveType::Num))),
            decl("min", l::TypeHint::Primitive(l::PrimitiveType::Num)),
            decl("max", l::TypeHint::Primitive(l::PrimitiveType::Num)),
        ])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([decl(
                "generated",
                l::TypeHint::Primitive(l::PrimitiveType::Num),
            )])),
            body: body([
                stmt(l::Statement::Native(rf(l::NativeOperation::Random {
                    dest_place: place("generated"),
                    min: rf(l::Expr::Literal(rf(l::Literal::Num(0.)))),
                    max: rf(l::Expr::Literal(rf(l::Literal::Num(1.)))),
                }))),
                stmt(l::Statement::Assign(rf(l::Assign {
                    place: deref(rf(l::Expr::Place(place("return")))),
                    expr: rf(l::AssignExpr::Expr(rf(l::Expr::Paren(rf(l::ParenExpr::Binary(
                        rf(l::BinaryParenExpr {
                            left: rf(l::Expr::Place(place("min"))),
                            op: rf(l::BinaryParenExprOp::Add),
                            right: rf(l::Expr::Paren(rf(l::ParenExpr::Binary(rf(
                                l::BinaryParenExpr {
                                    left: rf(l::Expr::Place(place("generated"))),
                                    op: rf(l::BinaryParenExprOp::Mul),
                                    right: rf(l::Expr::Paren(rf(l::ParenExpr::Binary(rf(
                                        l::BinaryParenExpr {
                                            left: rf(l::Expr::Place(place("max"))),
                                            op: rf(l::BinaryParenExprOp::Sub),
                                            right: rf(l::Expr::Place(place("min"))),
                                        },
                                    ))))),
                                },
                            ))))),
                        }),
                    )))))),
                }))),
            ]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("random_int"),
        params: rf(Vec::from([
            decl("return", ref_ty(l::TypeHint::Primitive(l::PrimitiveType::Int))),
            decl("min", l::TypeHint::Primitive(l::PrimitiveType::Int)),
            decl("max", l::TypeHint::Primitive(l::PrimitiveType::Int)),
        ])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([decl(
                "generated",
                l::TypeHint::Primitive(l::PrimitiveType::Int),
            )])),
            body: body([
                stmt(l::Statement::Native(rf(l::NativeOperation::Random {
                    dest_place: place("generated"),
                    min: rf(l::Expr::Place(place("min"))),
                    max: rf(l::Expr::Place(place("max"))),
                }))),
                stmt(l::Statement::Assign(rf(l::Assign {
                    place: deref(rf(l::Expr::Place(place("return")))),
                    expr: rf(l::AssignExpr::Expr(rf(l::Expr::Place(place("generated"))))),
                }))),
            ]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("random_uint"),
        params: rf(Vec::from([
            decl("return", ref_ty(l::TypeHint::Primitive(l::PrimitiveType::Uint))),
            decl("min", l::TypeHint::Primitive(l::PrimitiveType::Uint)),
            decl("max", l::TypeHint::Primitive(l::PrimitiveType::Uint)),
        ])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([decl(
                "generated",
                l::TypeHint::Primitive(l::PrimitiveType::Uint),
            )])),
            body: body([
                stmt(l::Statement::Native(rf(l::NativeOperation::Random {
                    dest_place: place("generated"),
                    min: rf(l::Expr::Place(place("min"))),
                    max: rf(l::Expr::Place(place("max"))),
                }))),
                stmt(l::Statement::Assign(rf(l::Assign {
                    place: deref(rf(l::Expr::Place(place("return")))),
                    expr: rf(l::AssignExpr::Expr(rf(l::Expr::Place(place("generated"))))),
                }))),
            ]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("stdout_clear"),
        params: rf(Vec::from([])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([])),
            body: body([stmt(l::Statement::Native(rf(l::NativeOperation::StdoutClear)))]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("stdout_read"),
        params: rf(Vec::from([
            decl("return", l::TypeHint::Ref(rf(l::TypeHint::Primitive(l::PrimitiveType::Val)))),
            decl("index", l::TypeHint::Primitive(l::PrimitiveType::Uint)),
        ])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([])),
            body: body([stmt(l::Statement::Native(rf(l::NativeOperation::StdoutRead {
                dest_place: deref(rf(l::Expr::Place(place("return")))),
                index: rf(l::Expr::Paren(rf(l::ParenExpr::Binary(rf(l::BinaryParenExpr {
                    left: rf(l::Expr::Place(place("index"))),
                    op: rf(l::BinaryParenExprOp::Add),
                    right: rf(l::Expr::Literal(rf(l::Literal::Int(1.)))),
                }))))),
            })))]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("stdout_write"),
        params: rf(Vec::from([
            decl("val", l::TypeHint::Primitive(l::PrimitiveType::Val)),
            decl("index", l::TypeHint::Primitive(l::PrimitiveType::Uint)),
        ])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([decl("len", l::TypeHint::Primitive(l::PrimitiveType::Uint))])),
            body: body([
                stmt(l::Statement::Call(rf(l::FunctionCall {
                    func_name: name("stdout_len"),
                    param_exprs: rf(Vec::from([rf(l::AssignExpr::Expr(rf(l::Expr::Ref(place(
                        "len",
                    )))))])),
                }))),
                rf(l::BodyItem::While(rf(l::WhileItem {
                    condition: rf(l::Expr::Paren(rf(l::ParenExpr::Binary(rf(
                        l::BinaryParenExpr {
                            left: rf(l::Expr::Place(place("len"))),
                            op: rf(l::BinaryParenExprOp::Lte),
                            right: rf(l::Expr::Place(place("index"))),
                        },
                    ))))),
                    body: body([
                        stmt(l::Statement::Call(rf(l::FunctionCall {
                            func_name: name("out"),
                            param_exprs: rf(Vec::from([rf(l::AssignExpr::Expr(rf(
                                l::Expr::Literal(rf(l::Literal::Val("".into()))),
                            )))])),
                        }))),
                        stmt(l::Statement::Call(rf(l::FunctionCall {
                            func_name: name("stdout_len"),
                            param_exprs: rf(Vec::from([rf(l::AssignExpr::Expr(rf(
                                l::Expr::Ref(place("len")),
                            )))])),
                        }))),
                    ]),
                }))),
                stmt(l::Statement::Native(rf(l::NativeOperation::StdoutWrite {
                    val: rf(l::Expr::Place(place("val"))),
                    index: rf(l::Expr::Paren(rf(l::ParenExpr::Binary(rf(l::BinaryParenExpr {
                        left: rf(l::Expr::Place(place("index"))),
                        op: rf(l::BinaryParenExprOp::Add),
                        right: rf(l::Expr::Literal(rf(l::Literal::Int(1.)))),
                    }))))),
                }))),
            ]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("stdout_len"),
        params: rf(Vec::from([decl(
            "return",
            l::TypeHint::Ref(rf(l::TypeHint::Primitive(l::PrimitiveType::Uint))),
        )])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([])),
            body: body([stmt(l::Statement::Native(rf(l::NativeOperation::StdoutLen {
                dest_place: deref(rf(l::Expr::Place(place("return")))),
            })))]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("wait_s"),
        params: rf(Vec::from([decl("duration_s", l::TypeHint::Primitive(l::PrimitiveType::Num))])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([])),
            body: body([stmt(l::Statement::Native(rf(l::NativeOperation::Wait {
                duration_s: rf(l::Expr::Place(place("duration_s"))),
            })))]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("timer_s"),
        params: rf(Vec::from([decl(
            "return",
            l::TypeHint::Ref(rf(l::TypeHint::Primitive(l::PrimitiveType::Num))),
        )])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([])),
            body: body([stmt(l::Statement::Native(rf(l::NativeOperation::TimerGet {
                dest_place: deref(rf(l::Expr::Place(place("return")))),
            })))]),
        }),
    }));

    rf(l::Program { items: rf(top_items) })
}
