mod call;
mod command;
mod expr;
mod proc;
mod procs;
mod sub_proc;
mod util;

use std::{ops::Not, sync::Arc};

use itertools::Itertools;

use crate::inline::{ast::Proc, opt::procs::optimize_procs};

pub fn optimize(procs: &[Arc<Proc>]) -> Vec<Arc<Proc>> {
    exhaust_optimizations(procs.iter().cloned().collect_vec(), |procs| optimize_procs(&procs)).val
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

fn tracked_exhaust_optimize<T>(
    optimized: &mut bool,
    val: T,
    optimizer: impl Fn(T) -> MaybeOptimized<T>,
) -> T {
    let maybe_optimized = exhaust_optimizations(val, optimizer);
    track_optimize(optimized, maybe_optimized)
}

fn track_optimize<T>(optimized: &mut bool, maybe_optimized: MaybeOptimized<T>) -> T {
    *optimized = *optimized || maybe_optimized.optimized;
    maybe_optimized.val
}

type OptimizationFn<T> = fn(&T) -> MaybeOptimized<T>;
