use std::sync::{Arc, LazyLock};

use crate::{
    high_compiler::logic_ast::{
        integer_type_hint, num_type_hint, str_type_hint, uinteger_type_hint, unit_type_hint,
        val_type_hint,
    },
    logic_ast as l,
    srced::{SrcRange, Srced},
};

pub fn add_builtins(l: &l::Ref<l::Program>) -> l::Ref<l::Program> {
    let mut top_items = l.val.items.val.to_vec();

    static SRC_RANGE: LazyLock<SrcRange> = LazyLock::new(SrcRange::default);

    fn rf<T>(t: T) -> l::Ref<T> {
        Arc::new(Srced { val: t, range: *SRC_RANGE })
    }

    fn ident_def(name: &str, ty: l::TypeHint) -> l::Ref<l::IdentDef> {
        rf(l::IdentDef {
            ident: rf(l::Ident::User { name: rf(l::Name { str: name.into() }) }),
            ty: rf(ty),
        })
    }

    fn let_stmt(name: &str, ty: l::TypeHint, expr: l::Expr) -> l::Statement {
        l::Statement::Let(rf(l::Let { ident_init: ident_init(name, ty, expr) }))
    }

    fn ident_init(name: &str, ty: l::TypeHint, expr: l::Expr) -> l::Ref<l::IdentInit> {
        rf(l::IdentInit { def: ident_def(name, ty), expr: rf(expr) })
    }

    fn place(ident_name: &str) -> l::Ref<l::Place> {
        rf(l::Place {
            head: rf(l::PlaceHead::Ident(rf(l::Ident::User { name: name(ident_name) }))),
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

    fn stmt(statement: l::Statement) -> l::Ref<l::Expr> {
        rf(l::Expr::Statement(rf(statement)))
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    enum BodyTrailing {
        True,
        False,
    }

    fn block(
        items: impl IntoIterator<Item = l::Ref<l::Expr>>,
        trailing: BodyTrailing,
    ) -> l::Ref<l::Expr> {
        let trailing = match trailing {
            BodyTrailing::True => true,
            BodyTrailing::False => false,
        };

        let block = rf(l::Block {
            items: rf(l::Trail { items: rf(items.into_iter().collect()), trailing }),
        });

        rf(l::Expr::Block(block))
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
        params: rf(Vec::from([ident_def("val", val_type_hint(*SRC_RANGE))])),
        return_ty: rf(unit_type_hint(*SRC_RANGE)),
        body: block(
            [stmt(l::Statement::Native(rf(l::NativeOperation::Out {
                val: rf(l::Expr::Place(place("val"))),
            })))],
            BodyTrailing::True,
        ),
    }));

    top_items.push(func(l::Func {
        name: name("in"),
        params: rf(Vec::from([ident_def("return", ref_ty(str_type_hint(*SRC_RANGE)))])),
        return_ty: rf(unit_type_hint(*SRC_RANGE)),
        body: block(
            [
                stmt(let_stmt("answer", str_type_hint(*SRC_RANGE), l::Expr::Undefined)),
                stmt(l::Statement::Native(rf(l::NativeOperation::In {
                    dest_place: place("answer"),
                }))),
                stmt(l::Statement::Assign(rf(l::Assign {
                    place: deref(rf(l::Expr::Place(place("return")))),
                    expr: rf(l::Expr::Place(place("answer"))),
                }))),
            ],
            BodyTrailing::True,
        ),
    }));

    top_items.push(func(l::Func {
        name: name("random_num"),
        params: rf(Vec::from([
            ident_def("return", ref_ty(num_type_hint(*SRC_RANGE))),
            ident_def("min", num_type_hint(*SRC_RANGE)),
            ident_def("max", num_type_hint(*SRC_RANGE)),
        ])),
        return_ty: rf(unit_type_hint(*SRC_RANGE)),
        body: block(
            [
                stmt(let_stmt("generated", num_type_hint(*SRC_RANGE), l::Expr::Undefined)),
                stmt(l::Statement::Native(rf(l::NativeOperation::Random {
                    dest_place: place("generated"),
                    min: rf(l::Expr::Literal(rf(l::Literal::Num(0.)))),
                    max: rf(l::Expr::Literal(rf(l::Literal::Num(1.)))),
                }))),
                stmt(l::Statement::Assign(rf(l::Assign {
                    place: deref(rf(l::Expr::Place(place("return")))),
                    expr: rf(l::Expr::Paren(rf(l::ParenExpr::Binary(rf(l::BinaryParenExpr {
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
                    }))))),
                }))),
            ],
            BodyTrailing::True,
        ),
    }));

    top_items.push(func(l::Func {
        name: name("random_int"),
        params: rf(Vec::from([
            ident_def("return", ref_ty(integer_type_hint(*SRC_RANGE))),
            ident_def("min", integer_type_hint(*SRC_RANGE)),
            ident_def("max", integer_type_hint(*SRC_RANGE)),
        ])),
        return_ty: rf(unit_type_hint(*SRC_RANGE)),
        body: block(
            [
                stmt(let_stmt("generated", integer_type_hint(*SRC_RANGE), l::Expr::Undefined)),
                stmt(l::Statement::Native(rf(l::NativeOperation::Random {
                    dest_place: place("generated"),
                    min: rf(l::Expr::Place(place("min"))),
                    max: rf(l::Expr::Place(place("max"))),
                }))),
                stmt(l::Statement::Assign(rf(l::Assign {
                    place: deref(rf(l::Expr::Place(place("return")))),
                    expr: rf(l::Expr::Place(place("generated"))),
                }))),
            ],
            BodyTrailing::True,
        ),
    }));

    top_items.push(func(l::Func {
        name: name("random_uint"),
        params: rf(Vec::from([
            ident_def("return", ref_ty(uinteger_type_hint(*SRC_RANGE))),
            ident_def("min", uinteger_type_hint(*SRC_RANGE)),
            ident_def("max", uinteger_type_hint(*SRC_RANGE)),
        ])),
        return_ty: rf(unit_type_hint(*SRC_RANGE)),
        body: block(
            [
                stmt(let_stmt("generated", uinteger_type_hint(*SRC_RANGE), l::Expr::Undefined)),
                stmt(l::Statement::Native(rf(l::NativeOperation::Random {
                    dest_place: place("generated"),
                    min: rf(l::Expr::Place(place("min"))),
                    max: rf(l::Expr::Place(place("max"))),
                }))),
                stmt(l::Statement::Assign(rf(l::Assign {
                    place: deref(rf(l::Expr::Place(place("return")))),
                    expr: rf(l::Expr::Place(place("generated"))),
                }))),
            ],
            BodyTrailing::True,
        ),
    }));

    top_items.push(func(l::Func {
        name: name("stdout_clear"),
        params: rf(Vec::from([])),
        return_ty: rf(unit_type_hint(*SRC_RANGE)),
        body: block(
            [stmt(l::Statement::Native(rf(l::NativeOperation::StdoutClear)))],
            BodyTrailing::True,
        ),
    }));

    top_items.push(func(l::Func {
        name: name("stdout_read"),
        params: rf(Vec::from([
            ident_def("return", ref_ty(str_type_hint(*SRC_RANGE))),
            ident_def("index", uinteger_type_hint(*SRC_RANGE)),
        ])),
        return_ty: rf(unit_type_hint(*SRC_RANGE)),

        body: block(
            [stmt(l::Statement::Native(rf(l::NativeOperation::StdoutRead {
                dest_place: deref(rf(l::Expr::Place(place("return")))),
                index: rf(l::Expr::Paren(rf(l::ParenExpr::Binary(rf(l::BinaryParenExpr {
                    left: rf(l::Expr::Place(place("index"))),
                    op: rf(l::BinaryParenExprOp::Add),
                    right: rf(l::Expr::Literal(rf(l::Literal::Int(1.)))),
                }))))),
            })))],
            BodyTrailing::True,
        ),
    }));

    top_items.push(func(l::Func {
        name: name("stdout_write"),
        params: rf(Vec::from([
            ident_def("val", val_type_hint(*SRC_RANGE)),
            ident_def("index", uinteger_type_hint(*SRC_RANGE)),
        ])),
        return_ty: rf(unit_type_hint(*SRC_RANGE)),
        body: block(
            [
                stmt(let_stmt("len", uinteger_type_hint(*SRC_RANGE), l::Expr::Undefined)),
                rf(l::Expr::Call(rf(l::FunctionCall {
                    func_name: name("stdout_len"),
                    param_exprs: rf(Vec::from([rf(l::Expr::Ref(place("len")))])),
                }))),
                rf(l::Expr::While(rf(l::WhileItem {
                    condition: rf(l::Expr::Paren(rf(l::ParenExpr::Binary(rf(
                        l::BinaryParenExpr {
                            left: rf(l::Expr::Place(place("len"))),
                            op: rf(l::BinaryParenExprOp::Lte),
                            right: rf(l::Expr::Place(place("index"))),
                        },
                    ))))),
                    body: block(
                        [
                            rf(l::Expr::Call(rf(l::FunctionCall {
                                func_name: name("out"),
                                param_exprs: rf(Vec::from([rf(l::Expr::Literal(rf(
                                    l::Literal::Str("".into()),
                                )))])),
                            }))),
                            rf(l::Expr::Call(rf(l::FunctionCall {
                                func_name: name("stdout_len"),
                                param_exprs: rf(Vec::from([rf(l::Expr::Ref(place("len")))])),
                            }))),
                        ],
                        BodyTrailing::True,
                    ),
                }))),
                stmt(l::Statement::Native(rf(l::NativeOperation::StdoutWrite {
                    val: rf(l::Expr::Place(place("val"))),
                    index: rf(l::Expr::Paren(rf(l::ParenExpr::Binary(rf(l::BinaryParenExpr {
                        left: rf(l::Expr::Place(place("index"))),
                        op: rf(l::BinaryParenExprOp::Add),
                        right: rf(l::Expr::Literal(rf(l::Literal::Int(1.)))),
                    }))))),
                }))),
            ],
            BodyTrailing::True,
        ),
    }));

    top_items.push(func(l::Func {
        name: name("stdout_len"),
        params: rf(Vec::from([ident_def("return", ref_ty(uinteger_type_hint(*SRC_RANGE)))])),
        return_ty: rf(unit_type_hint(*SRC_RANGE)),
        body: block(
            [stmt(l::Statement::Native(rf(l::NativeOperation::StdoutLen {
                dest_place: deref(rf(l::Expr::Place(place("return")))),
            })))],
            BodyTrailing::True,
        ),
    }));

    top_items.push(func(l::Func {
        name: name("wait_s"),
        params: rf(Vec::from([ident_def("duration_s", num_type_hint(*SRC_RANGE))])),
        return_ty: rf(unit_type_hint(*SRC_RANGE)),
        body: block(
            [stmt(l::Statement::Native(rf(l::NativeOperation::Wait {
                duration_s: rf(l::Expr::Place(place("duration_s"))),
            })))],
            BodyTrailing::True,
        ),
    }));

    top_items.push(func(l::Func {
        name: name("timer_s"),
        params: rf(Vec::from([ident_def("return", ref_ty(num_type_hint(*SRC_RANGE)))])),
        return_ty: rf(unit_type_hint(*SRC_RANGE)),
        body: block(
            [stmt(l::Statement::Native(rf(l::NativeOperation::TimerGet {
                dest_place: deref(rf(l::Expr::Place(place("return")))),
            })))],
            BodyTrailing::True,
        ),
    }));

    top_items.push(func(l::Func {
        name: name("since_2000_days"),
        params: rf(Vec::from([ident_def("return", ref_ty(num_type_hint(*SRC_RANGE)))])),
        return_ty: rf(unit_type_hint(*SRC_RANGE)),
        body: block(
            [stmt(l::Statement::Native(rf(l::NativeOperation::DaysSince2000Get {
                dest_place: deref(rf(l::Expr::Place(place("return")))),
            })))],
            BodyTrailing::True,
        ),
    }));

    rf(l::Program { items: rf(top_items) })
}
