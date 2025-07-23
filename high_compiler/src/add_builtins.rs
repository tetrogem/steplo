use std::sync::Arc;

use crate::logic_ast as l;

pub fn add_builtins(mut l: l::Program) -> l::Program {
    let mut top_items: Vec<_> = l.items.iter().cloned().collect();

    fn decl(name: &str, ty: l::Type) -> Arc<l::IdentDeclaration> {
        Arc::new(l::IdentDeclaration {
            name: Arc::new(l::Name { str: name.into() }),
            ty: Arc::new(ty),
        })
    }

    fn ident(ident_name: &str) -> Arc<l::Place> {
        Arc::new(l::Place {
            head: Arc::new(l::PlaceHead::Ident(Arc::new(l::Ident { name: name(ident_name) }))),
            offset: None,
        })
    }

    fn deref(addr: Arc<l::Expr>) -> Arc<l::Place> {
        Arc::new(l::Place {
            head: Arc::new(l::PlaceHead::Deref(Arc::new(l::Deref { addr }))),
            offset: None,
        })
    }

    fn name(name: &str) -> Arc<l::Name> {
        Arc::new(l::Name { str: name.into() })
    }

    fn stmt(statement: l::Statement) -> Arc<l::BodyItem> {
        Arc::new(l::BodyItem::Statement(Arc::new(statement)))
    }

    fn body(items: impl IntoIterator<Item = Arc<l::BodyItem>>) -> Arc<l::Body> {
        Arc::new(l::Body { items: Arc::new(items.into_iter().collect()) })
    }

    fn func(func: l::Func) -> Arc<l::TopItem> {
        Arc::new(l::TopItem::Func(Arc::new(func)))
    }

    fn ref_ty(ty: l::Type) -> l::Type {
        l::Type::Ref(Arc::new(ty))
    }

    // add built-in native functions
    top_items.push(func(l::Func {
        name: name("out"),
        params: Arc::new(Vec::from([decl("val", l::Type::Base(Arc::new(l::BaseType::Any)))])),
        proc: Arc::new(l::Proc {
            idents: Arc::new(Vec::from([])),
            body: body([stmt(l::Statement::Native(Arc::new(l::NativeOperation::Out {
                ident: ident("val"),
            })))]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("in"),
        params: Arc::new(Vec::from([decl(
            "dest_ref",
            ref_ty(l::Type::Base(Arc::new(l::BaseType::Val))),
        )])),
        proc: Arc::new(l::Proc {
            idents: Arc::new(Vec::from([decl(
                "answer",
                l::Type::Base(Arc::new(l::BaseType::Val)),
            )])),
            body: body([
                stmt(l::Statement::Native(Arc::new(l::NativeOperation::In {
                    dest_ident: ident("answer"),
                }))),
                stmt(l::Statement::Assign(Arc::new(l::Assign {
                    place: deref(Arc::new(l::Expr::Place(ident("dest_ref")))),
                    expr: Arc::new(l::AssignExpr::Expr(Arc::new(l::Expr::Place(ident("answer"))))),
                }))),
            ]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("random_num"),
        params: Arc::new(Vec::from([
            decl("dest_ref", ref_ty(l::Type::Base(Arc::new(l::BaseType::Num)))),
            decl("min", l::Type::Base(Arc::new(l::BaseType::Num))),
            decl("max", l::Type::Base(Arc::new(l::BaseType::Num))),
        ])),
        proc: Arc::new(l::Proc {
            idents: Arc::new(Vec::from([decl(
                "generated",
                l::Type::Base(Arc::new(l::BaseType::Num)),
            )])),
            body: body([
                stmt(l::Statement::Native(Arc::new(l::NativeOperation::Random {
                    dest_ident: ident("generated"),
                    min: Arc::new(l::Expr::Literal(Arc::new(l::Literal::Num(0.)))),
                    max: Arc::new(l::Expr::Literal(Arc::new(l::Literal::Num(1.)))),
                }))),
                stmt(l::Statement::Assign(Arc::new(l::Assign {
                    place: deref(Arc::new(l::Expr::Place(ident("dest_ref")))),
                    expr: Arc::new(l::AssignExpr::Expr(Arc::new(l::Expr::Paren(Arc::new(
                        l::ParenExpr::Binary(Arc::new(l::BinaryParenExpr {
                            left: Arc::new(l::Expr::Place(ident("min"))),
                            op: l::BinaryParenExprOp::Add,
                            right: Arc::new(l::Expr::Paren(Arc::new(l::ParenExpr::Binary(
                                Arc::new(l::BinaryParenExpr {
                                    left: Arc::new(l::Expr::Place(ident("generated"))),
                                    op: l::BinaryParenExprOp::Mul,
                                    right: Arc::new(l::Expr::Paren(Arc::new(
                                        l::ParenExpr::Binary(Arc::new(l::BinaryParenExpr {
                                            left: Arc::new(l::Expr::Place(ident("max"))),
                                            op: l::BinaryParenExprOp::Sub,
                                            right: Arc::new(l::Expr::Place(ident("min"))),
                                        })),
                                    ))),
                                }),
                            )))),
                        })),
                    ))))),
                }))),
            ]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("random_int"),
        params: Arc::new(Vec::from([
            decl("dest_ref", ref_ty(l::Type::Base(Arc::new(l::BaseType::Int)))),
            decl("min", l::Type::Base(Arc::new(l::BaseType::Int))),
            decl("max", l::Type::Base(Arc::new(l::BaseType::Int))),
        ])),
        proc: Arc::new(l::Proc {
            idents: Arc::new(Vec::from([decl(
                "generated",
                l::Type::Base(Arc::new(l::BaseType::Int)),
            )])),
            body: body([
                stmt(l::Statement::Native(Arc::new(l::NativeOperation::Random {
                    dest_ident: ident("generated"),
                    min: Arc::new(l::Expr::Place(ident("min"))),
                    max: Arc::new(l::Expr::Place(ident("max"))),
                }))),
                stmt(l::Statement::Assign(Arc::new(l::Assign {
                    place: deref(Arc::new(l::Expr::Place(ident("dest_ref")))),
                    expr: Arc::new(l::AssignExpr::Expr(Arc::new(l::Expr::Place(ident(
                        "generated",
                    ))))),
                }))),
            ]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("random_uint"),
        params: Arc::new(Vec::from([
            decl("dest_ref", ref_ty(l::Type::Base(Arc::new(l::BaseType::Uint)))),
            decl("min", l::Type::Base(Arc::new(l::BaseType::Uint))),
            decl("max", l::Type::Base(Arc::new(l::BaseType::Uint))),
        ])),
        proc: Arc::new(l::Proc {
            idents: Arc::new(Vec::from([decl(
                "generated",
                l::Type::Base(Arc::new(l::BaseType::Uint)),
            )])),
            body: body([
                stmt(l::Statement::Native(Arc::new(l::NativeOperation::Random {
                    dest_ident: ident("generated"),
                    min: Arc::new(l::Expr::Place(ident("min"))),
                    max: Arc::new(l::Expr::Place(ident("max"))),
                }))),
                stmt(l::Statement::Assign(Arc::new(l::Assign {
                    place: deref(Arc::new(l::Expr::Place(ident("dest_ref")))),
                    expr: Arc::new(l::AssignExpr::Expr(Arc::new(l::Expr::Place(ident(
                        "generated",
                    ))))),
                }))),
            ]),
        }),
    }));

    l.items = Arc::new(top_items);

    l
}
