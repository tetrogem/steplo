use std::sync::Arc;

use crate::inline::{
    ast::Proc,
    opt::{MaybeOptimized, sub_proc::optimize_sub_proc, tracked_optimize},
};

pub fn optimize_proc(proc: &Arc<Proc>) -> MaybeOptimized<Arc<Proc>> {
    let mut optimized = false;

    let sub_procs = tracked_optimize(&mut optimized, proc.sub_procs.as_ref().clone(), |sps| {
        sps.into_iter().map(|sp| optimize_sub_proc(&sp)).collect()
    });

    MaybeOptimized {
        val: Arc::new(Proc {
            kind: proc.kind.clone(),
            sub_procs: Arc::new(sub_procs),
            ordered_local_infos: proc.ordered_local_infos.clone(),
            ordered_arg_infos: proc.ordered_arg_infos.clone(),
        }),
        optimized,
    }
}
