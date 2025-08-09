use std::sync::Arc;

use crate::inline::{
    ast::{BinaryArgs, Expr, Loc, Value},
    opt::{MaybeOptimized, tracked_optimize},
};

pub fn optimize_expr(expr: &Arc<Expr>) -> MaybeOptimized<Arc<Expr>> {
    let mut optimized = false;

    macro_rules! expr {
        ($expr:expr) => {
            tracked_optimize(&mut optimized, $expr.clone(), |expr| optimize_expr(&expr))
        };
    }

    macro_rules! args {
        ($args:expr) => {
            Arc::new(BinaryArgs { left: expr!(&$args.left), right: expr!(&$args.right) })
        };
    }

    let expr = match expr.as_ref() {
        Expr::Loc(loc) => {
            let loc = match loc.as_ref() {
                Loc::Temp(temp) => Loc::Temp(temp.clone()),
                Loc::Deref(addr) => Loc::Deref(expr!(addr)),
            };

            Arc::new(Expr::Loc(Arc::new(loc)))
        },
        Expr::StackAddr(addr) => Arc::new(Expr::StackAddr(addr.clone())),
        Expr::Value(value) => Arc::new(Expr::Value(value.clone())),
        Expr::StdoutDeref(expr) => Arc::new(Expr::StdoutDeref(expr!(expr))),
        Expr::StdoutLen => Arc::new(Expr::StdoutLen),
        Expr::Timer => Arc::new(Expr::Timer),
        Expr::Add(args) => Arc::new(Expr::Add(args!(args))),
        Expr::Sub(args) => Arc::new(Expr::Sub(args!(args))),
        Expr::Mul(args) => Arc::new(Expr::Mul(args!(args))),
        Expr::Div(args) => Arc::new(Expr::Div(args!(args))),
        Expr::Mod(args) => Arc::new(Expr::Mod(args!(args))),
        Expr::Eq(args) => Arc::new(Expr::Eq(args!(args))),
        Expr::Lt(args) => Arc::new(Expr::Lt(args!(args))),
        Expr::Gt(args) => Arc::new(Expr::Gt(args!(args))),
        Expr::Not(expr) => Arc::new(Expr::Not(expr!(expr))),
        Expr::Or(args) => Arc::new(Expr::Or(args!(args))),
        Expr::And(args) => Arc::new(Expr::And(args!(args))),
        Expr::InAnswer => Arc::new(Expr::InAnswer),
        Expr::Join(args) => Arc::new(Expr::Join(args!(args))),
        Expr::Random(args) => Arc::new(Expr::Random(args!(args))),
    };

    let expr = tracked_optimize(&mut optimized, expr.clone(), |expr| {
        optimization_const_evaluate_exprs(&expr)
    });

    MaybeOptimized { optimized, val: expr }
}

fn optimization_const_evaluate_exprs(expr: &Arc<Expr>) -> MaybeOptimized<Arc<Expr>> {
    let mut optimized = false;

    let expr = match expr.as_ref() {
        Expr::Loc(loc) => Arc::new(Expr::Loc(loc.clone())),
        Expr::StackAddr(addr) => Arc::new(Expr::StackAddr(addr.clone())),
        Expr::Value(value) => Arc::new(Expr::Value(value.clone())),
        Expr::StdoutDeref(expr) => Arc::new(Expr::StdoutDeref(expr.clone())),
        Expr::StdoutLen => Arc::new(Expr::StdoutLen),
        Expr::Timer => Arc::new(Expr::Timer),
        Expr::Add(args) => 'add: {
            if let Some(other) = other_side_if(args, is_literal("0")) {
                optimized = true;
                break 'add other.clone();
            }

            Arc::new(Expr::Add(args.clone()))
        },
        Expr::Sub(args) => Arc::new(Expr::Sub(args.clone())),
        Expr::Mul(args) => Arc::new(Expr::Mul(args.clone())),
        Expr::Div(args) => Arc::new(Expr::Div(args.clone())),
        Expr::Mod(args) => Arc::new(Expr::Mod(args.clone())),
        Expr::Eq(args) => Arc::new(Expr::Eq(args.clone())),
        Expr::Lt(args) => Arc::new(Expr::Lt(args.clone())),
        Expr::Gt(args) => Arc::new(Expr::Gt(args.clone())),
        Expr::Not(expr) => Arc::new(Expr::Not(expr.clone())),
        Expr::Or(args) => Arc::new(Expr::Or(args.clone())),
        Expr::And(args) => Arc::new(Expr::And(args.clone())),
        Expr::InAnswer => Arc::new(Expr::InAnswer),
        Expr::Join(args) => Arc::new(Expr::Join(args.clone())),
        Expr::Random(args) => Arc::new(Expr::Random(args.clone())),
    };

    MaybeOptimized { optimized, val: expr }
}

fn other_side_if(args: &BinaryArgs, predicate: impl Fn(&Expr) -> bool) -> Option<&Arc<Expr>> {
    if predicate(&args.left) {
        return Some(&args.right);
    }

    if predicate(&args.right) {
        return Some(&args.left);
    }

    None
}

fn is_literal(expected: &str) -> impl Fn(&Expr) -> bool {
    move |expr| {
        let Expr::Value(value) = expr else { return false };
        let Value::Literal(lit) = value.as_ref() else { return false };
        lit.as_ref() == expected
    }
}
