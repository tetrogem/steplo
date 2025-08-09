use std::sync::Arc;

use crate::inline::{
    ast::{ArgAssignment, Call},
    opt::{MaybeOptimized, expr::optimize_expr, tracked_optimize},
};

pub fn optimize_call(call: &Arc<Call>) -> MaybeOptimized<Arc<Call>> {
    let mut optimized = false;

    macro_rules! expr {
        ($expr:expr) => {
            tracked_optimize(&mut optimized, $expr.clone(), |expr| optimize_expr(&expr))
        };
    }

    let call = match call.as_ref() {
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
    };

    MaybeOptimized { optimized, val: Arc::new(call) }
}
