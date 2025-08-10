use std::sync::Arc;

use crate::inline::{
    ast::{ArgAssignment, Call, Expr, Value},
    opt::{MaybeOptimized, OptimizationFn, expr::optimize_expr, tracked_exhaust_optimize},
};

pub fn optimize_call(call: &Arc<Call>) -> MaybeOptimized<Arc<Call>> {
    let mut optimized = false;

    macro_rules! expr {
        ($expr:expr) => {
            tracked_exhaust_optimize(&mut optimized, $expr.clone(), |expr| optimize_expr(&expr))
        };
    }

    let mut call = Arc::new(match call.as_ref() {
        Call::Exit => Call::Exit,
        Call::Jump { to } => Call::Jump { to: expr!(to) },
        Call::Branch { cond, then_to, else_to } => {
            Call::Branch { cond: expr!(cond), then_to: expr!(then_to), else_to: expr!(else_to) }
        },
        Call::Func { to_func_name, arg_assignments } => {
            let arg_assignments = arg_assignments
                .iter()
                .map(|aa| ArgAssignment {
                    arg_uuid: aa.arg_uuid,
                    arg_offset: aa.arg_offset,
                    expr: expr!(aa.expr),
                })
                .collect();

            Call::Func {
                to_func_name: to_func_name.clone(),
                arg_assignments: Arc::new(arg_assignments),
            }
        },
        Call::Return { to } => Call::Return { to: expr!(to) },
    });

    const OPTIMIZATIONS: [OptimizationFn<Arc<Call>>; 1] = [optimization_inline_consistent_branch];

    for optimization in OPTIMIZATIONS {
        call = tracked_exhaust_optimize(&mut optimized, call, |call| optimization(&call));
    }

    MaybeOptimized { optimized, val: call }
}

fn optimization_inline_consistent_branch(call: &Arc<Call>) -> MaybeOptimized<Arc<Call>> {
    match maybe_inline_consistent_branch(call) {
        None => MaybeOptimized { optimized: false, val: call.clone() },
        Some(call) => MaybeOptimized { optimized: true, val: Arc::new(call) },
    }
}

fn maybe_inline_consistent_branch(call: &Call) -> Option<Call> {
    let Call::Branch { cond, then_to, else_to } = call else { return None };
    let Expr::Value(cond) = cond.as_ref() else { return None };
    let Value::Literal(cond) = cond.as_ref() else { return None };

    let to = match cond.as_ref() {
        "true" => then_to,
        "false" => else_to,
        _ => return None,
    };

    Some(Call::Jump { to: to.clone() })
}
