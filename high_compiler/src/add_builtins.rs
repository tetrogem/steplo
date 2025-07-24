use std::sync::{Arc, LazyLock};

use crate::{
    logic_ast as l,
    srced::{SrcRange, Srced},
};

pub fn add_builtins(l: &l::Ref<l::Program>) -> l::Ref<l::Program> {
    let mut top_items: Vec<_> = l.val.items.val.iter().cloned().collect();

    static SRC_RANGE: LazyLock<SrcRange> = LazyLock::new(|| SrcRange::default());

    fn rf<T>(t: T) -> l::Ref<T> {
        Arc::new(Srced { val: t, range: *SRC_RANGE })
    }

    fn decl(name: &str, ty: l::Type) -> l::Ref<l::IdentDeclaration> {
        rf(l::IdentDeclaration { name: rf(l::Name { str: name.into() }), ty: Arc::new(ty) })
    }

    fn ident(ident_name: &str) -> l::Ref<l::Place> {
        rf(l::Place {
            head: rf(l::PlaceHead::Ident(rf(l::Ident { name: name(ident_name) }))),
            offset: None,
        })
    }

    fn deref(addr: l::Ref<l::Expr>) -> l::Ref<l::Place> {
        rf(l::Place { head: rf(l::PlaceHead::Deref(rf(l::Deref { addr }))), offset: None })
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
        rf(l::TopItem::Func(rf(func)))
    }

    fn ref_ty(ty: l::Type) -> l::Type {
        l::Type::Ref(Arc::new(ty))
    }

    // add built-in native functions
    top_items.push(func(l::Func {
        name: name("out"),
        params: rf(Vec::from([decl("val", l::Type::Base(Arc::new(l::BaseType::Any)))])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([])),
            body: body([stmt(l::Statement::Native(rf(l::NativeOperation::Out {
                ident: ident("val"),
            })))]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("in"),
        params: rf(Vec::from([decl(
            "dest_ref",
            ref_ty(l::Type::Base(Arc::new(l::BaseType::Val))),
        )])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([decl("answer", l::Type::Base(Arc::new(l::BaseType::Val)))])),
            body: body([
                stmt(l::Statement::Native(rf(l::NativeOperation::In {
                    dest_ident: ident("answer"),
                }))),
                stmt(l::Statement::Assign(rf(l::Assign {
                    place: deref(rf(l::Expr::Place(ident("dest_ref")))),
                    expr: rf(l::AssignExpr::Expr(rf(l::Expr::Place(ident("answer"))))),
                }))),
            ]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("random_num"),
        params: rf(Vec::from([
            decl("dest_ref", ref_ty(l::Type::Base(Arc::new(l::BaseType::Num)))),
            decl("min", l::Type::Base(Arc::new(l::BaseType::Num))),
            decl("max", l::Type::Base(Arc::new(l::BaseType::Num))),
        ])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([decl("generated", l::Type::Base(Arc::new(l::BaseType::Num)))])),
            body: body([
                stmt(l::Statement::Native(rf(l::NativeOperation::Random {
                    dest_ident: ident("generated"),
                    min: rf(l::Expr::Literal(rf(l::Literal::Num(0.)))),
                    max: rf(l::Expr::Literal(rf(l::Literal::Num(1.)))),
                }))),
                stmt(l::Statement::Assign(rf(l::Assign {
                    place: deref(rf(l::Expr::Place(ident("dest_ref")))),
                    expr: rf(l::AssignExpr::Expr(rf(l::Expr::Paren(rf(l::ParenExpr::Binary(
                        rf(l::BinaryParenExpr {
                            left: rf(l::Expr::Place(ident("min"))),
                            op: rf(l::BinaryParenExprOp::Add),
                            right: rf(l::Expr::Paren(rf(l::ParenExpr::Binary(rf(
                                l::BinaryParenExpr {
                                    left: rf(l::Expr::Place(ident("generated"))),
                                    op: rf(l::BinaryParenExprOp::Mul),
                                    right: rf(l::Expr::Paren(rf(l::ParenExpr::Binary(rf(
                                        l::BinaryParenExpr {
                                            left: rf(l::Expr::Place(ident("max"))),
                                            op: rf(l::BinaryParenExprOp::Sub),
                                            right: rf(l::Expr::Place(ident("min"))),
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
            decl("dest_ref", ref_ty(l::Type::Base(Arc::new(l::BaseType::Int)))),
            decl("min", l::Type::Base(Arc::new(l::BaseType::Int))),
            decl("max", l::Type::Base(Arc::new(l::BaseType::Int))),
        ])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([decl("generated", l::Type::Base(Arc::new(l::BaseType::Int)))])),
            body: body([
                stmt(l::Statement::Native(rf(l::NativeOperation::Random {
                    dest_ident: ident("generated"),
                    min: rf(l::Expr::Place(ident("min"))),
                    max: rf(l::Expr::Place(ident("max"))),
                }))),
                stmt(l::Statement::Assign(rf(l::Assign {
                    place: deref(rf(l::Expr::Place(ident("dest_ref")))),
                    expr: rf(l::AssignExpr::Expr(rf(l::Expr::Place(ident("generated"))))),
                }))),
            ]),
        }),
    }));

    top_items.push(func(l::Func {
        name: name("random_uint"),
        params: rf(Vec::from([
            decl("dest_ref", ref_ty(l::Type::Base(Arc::new(l::BaseType::Uint)))),
            decl("min", l::Type::Base(Arc::new(l::BaseType::Uint))),
            decl("max", l::Type::Base(Arc::new(l::BaseType::Uint))),
        ])),
        proc: rf(l::Proc {
            idents: rf(Vec::from([decl("generated", l::Type::Base(Arc::new(l::BaseType::Uint)))])),
            body: body([
                stmt(l::Statement::Native(rf(l::NativeOperation::Random {
                    dest_ident: ident("generated"),
                    min: rf(l::Expr::Place(ident("min"))),
                    max: rf(l::Expr::Place(ident("max"))),
                }))),
                stmt(l::Statement::Assign(rf(l::Assign {
                    place: deref(rf(l::Expr::Place(ident("dest_ref")))),
                    expr: rf(l::AssignExpr::Expr(rf(l::Expr::Place(ident("generated"))))),
                }))),
            ]),
        }),
    }));

    rf(l::Program { items: rf(top_items) })
}
