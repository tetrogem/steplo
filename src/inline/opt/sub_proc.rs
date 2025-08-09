use std::sync::Arc;

use crate::inline::{
    ast::SubProc,
    opt::{MaybeOptimized, call::optimize_call, command::optimize_command, tracked_optimize},
};

pub fn optimize_sub_proc(sp: &Arc<SubProc>) -> MaybeOptimized<Arc<SubProc>> {
    let mut optimized = false;

    let commands = tracked_optimize(&mut optimized, sp.commands.as_ref().clone(), |commands| {
        commands.into_iter().map(|command| optimize_command(&command)).collect()
    });

    let call = tracked_optimize(&mut optimized, sp.call.clone(), |call| optimize_call(&call));

    MaybeOptimized {
        optimized,
        val: Arc::new(SubProc { uuid: sp.uuid, commands: Arc::new(commands), call }),
    }
}
