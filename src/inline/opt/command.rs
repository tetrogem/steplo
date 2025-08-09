use std::sync::Arc;

use crate::inline::{
    ast::{Command, Loc},
    opt::{MaybeOptimized, expr::optimize_expr, tracked_optimize},
};

pub fn optimize_command(command: &Arc<Command>) -> MaybeOptimized<Arc<Command>> {
    let mut optimized = false;

    macro_rules! expr {
        ($expr:expr) => {
            tracked_optimize(&mut optimized, $expr.clone(), |expr| optimize_expr(&expr))
        };
    }

    let command = match command.as_ref() {
        Command::In => Command::In,
        Command::ClearStdout => Command::ClearStdout,
        Command::Out(expr) => Command::Out(expr!(expr)),
        Command::Wait { duration_s } => Command::Wait { duration_s: expr!(duration_s) },
        Command::WriteStdout { index, val } => {
            Command::WriteStdout { index: expr!(index), val: expr!(val) }
        },
        Command::SetLoc { loc, val } => {
            let loc = match loc.as_ref() {
                Loc::Temp(temp) => Loc::Temp(temp.clone()),
                Loc::Deref(addr) => Loc::Deref(expr!(addr)),
            };

            Command::SetLoc { loc: Arc::new(loc), val: expr!(val) }
        },
    };

    MaybeOptimized { optimized, val: Arc::new(command) }
}
