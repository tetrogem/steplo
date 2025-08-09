mod call;
mod command;
mod expr;

use std::{ops::Not, sync::Arc};

use itertools::Itertools;

use crate::inline::{
    ast::{Command, Proc, SubProc},
    opt::{call::optimize_call, command::optimize_command},
};

pub fn optimize(procs: &[Arc<Proc>]) -> Vec<Arc<Proc>> {
    let procs = procs.iter().cloned().collect_vec();
    let maybe_optimized = exhaust_optimizations(procs, |procs| {
        procs.into_iter().map(|proc| optimize_proc(&proc)).collect()
    });

    maybe_optimized.val
}

pub struct MaybeOptimized<T> {
    optimized: bool,
    val: T,
}

impl<Collection, T> FromIterator<MaybeOptimized<T>> for MaybeOptimized<Collection>
where
    Collection: FromIterator<T>,
{
    fn from_iter<I: IntoIterator<Item = MaybeOptimized<T>>>(iter: I) -> Self {
        let mut optimized = false;

        let collection = iter
            .into_iter()
            .map(|o| {
                optimized = optimized || o.optimized;
                o.val
            })
            .collect();

        MaybeOptimized { val: collection, optimized }
    }
}

fn exhaust_optimizations<T>(
    mut val: T,
    optimizer: impl Fn(T) -> MaybeOptimized<T>,
) -> MaybeOptimized<T> {
    let mut optimized = false;

    loop {
        let maybe_optimized = optimizer(val);

        val = maybe_optimized.val;
        optimized = optimized || maybe_optimized.optimized;

        if maybe_optimized.optimized.not() {
            break;
        }
    }

    MaybeOptimized { val, optimized }
}

fn tracked_optimize<T>(
    optimized: &mut bool,
    val: T,
    optimizer: impl Fn(T) -> MaybeOptimized<T>,
) -> T {
    let maybe_optimized = exhaust_optimizations(val, optimizer);
    *optimized = *optimized || maybe_optimized.optimized;
    maybe_optimized.val
}

fn optimize_proc(proc: &Arc<Proc>) -> MaybeOptimized<Arc<Proc>> {
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

fn optimize_sub_proc(sp: &Arc<SubProc>) -> MaybeOptimized<Arc<SubProc>> {
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
