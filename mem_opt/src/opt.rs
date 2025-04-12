use itertools::Itertools;
use uuid::Uuid;

use std::{
    collections::{BTreeMap, BTreeSet},
    mem,
    ops::Not,
    sync::Arc,
};

use crate::ast::{BinaryArgs, Call, Command, Expr, Proc, SubProc, TempVar, UMemLoc, Value};

const DEBUG: bool = false;

macro_rules! opt_debug {
    ($($tokens:tt)*) => {
        if DEBUG {
            println!("[opt] {}", format!($($tokens)*));
        }
    };
}

pub fn optimize(procs: impl Iterator<Item = Arc<Proc<UMemLoc>>>) -> Vec<Arc<Proc<UMemLoc>>> {
    let mut procs = procs.collect_vec();
    let mut tracker = Tracker::default();

    loop {
        opt_debug!("root");
        procs = tracker.record(optimize_procs(procs));

        if tracker.take_pass_optimized().not() {
            break;
        }
    }

    procs
}

fn optimize_procs(
    mut procs: Vec<Arc<Proc<UMemLoc>>>,
) -> OptimizationReport<Vec<Arc<Proc<UMemLoc>>>> {
    let optimizer = Optimizer {
        optimizations: Vec::from([Optimization {
            f: optimization_inline_pure_redirects,
            name: "inline_pure_redirects",
        }]),
    };

    let mut tracker = Tracker::default();

    loop {
        opt_debug!("procs");
        procs = procs.into_iter().map(|proc| tracker.record(optimize_proc(proc))).collect();

        procs = tracker.record(optimizer.optimize(procs));

        if tracker.take_pass_optimized().not() {
            break;
        }
    }

    OptimizationReport { optimized: tracker.optimized(), val: procs }
}

fn optimize_proc(mut proc: Arc<Proc<UMemLoc>>) -> OptimizationReport<Arc<Proc<UMemLoc>>> {
    let optimizer = Optimizer { optimizations: Vec::from([]) };
    let mut tracker = Tracker::default();

    loop {
        opt_debug!("proc {:?}", proc.kind);
        proc = Arc::new(Proc {
            kind: proc.kind.clone(),
            sub_procs: Arc::new(
                proc.sub_procs
                    .iter()
                    .map(|sp| tracker.record(optimize_sub_proc(sp.clone())))
                    .collect(),
            ),
        });

        proc = tracker.record(optimizer.optimize(proc));

        if tracker.take_pass_optimized().not() {
            break;
        }
    }

    OptimizationReport { optimized: tracker.optimized(), val: proc }
}

fn optimize_sub_proc(mut sp: Arc<SubProc<UMemLoc>>) -> OptimizationReport<Arc<SubProc<UMemLoc>>> {
    let optimizer = Optimizer {
        optimizations: Vec::from([
            Optimization { f: optimization_inline_trivial_temps, name: "inline_trivial_temps" },
            Optimization { f: optimization_remove_unused_temps, name: "remove_unused_temps" },
        ]),
    };

    let mut tracker = Tracker::default();

    loop {
        opt_debug!("sub_proc {}", sp.uuid);
        sp = tracker.record(optimizer.optimize(sp));

        if tracker.take_pass_optimized().not() {
            break;
        }
    }

    OptimizationReport { optimized: tracker.optimized(), val: sp }
}

type OptimizeFn<T> = fn(sp: T) -> OptimizationReport<T>;

struct Optimization<T> {
    f: OptimizeFn<T>,
    name: &'static str,
}

struct Optimizer<T> {
    optimizations: Vec<Optimization<T>>,
}

impl<T> Optimizer<T> {
    pub fn optimize(&self, mut val: T) -> OptimizationReport<T> {
        let mut optimized = false;
        for optimization in &self.optimizations {
            let report = (optimization.f)(val);
            if report.optimized {
                optimized = true;
            }

            val = report.val;

            opt_debug!("{}: {}", optimization.name, report.optimized);
        }

        OptimizationReport { optimized, val }
    }
}

#[derive(Default)]
struct Tracker {
    optimized: bool,
    pass_optimized: bool,
}

impl Tracker {
    pub fn record<T>(&mut self, report: OptimizationReport<T>) -> T {
        self.optimized = self.optimized || report.optimized;
        self.pass_optimized = self.pass_optimized || report.optimized;
        report.val
    }

    pub fn optimized(&self) -> bool {
        self.optimized
    }

    pub fn take_pass_optimized(&mut self) -> bool {
        mem::take(&mut self.pass_optimized)
    }
}

struct OptimizationReport<T> {
    optimized: bool,
    val: T,
}

fn optimization_inline_trivial_temps(
    sp: Arc<SubProc<UMemLoc>>,
) -> OptimizationReport<Arc<SubProc<UMemLoc>>> {
    let trivial_temp_to_expr = find_trivial_temps(&sp);
    let mut opt_commands = Vec::new();
    for command in sp.commands.as_ref() {
        match command.as_ref() {
            Command::SetMemLoc { mem_loc, val } => {
                opt_commands.push(Arc::new(Command::SetMemLoc {
                    mem_loc: mem_loc.clone(),
                    val: expr_replace_trivial_temps(val, &trivial_temp_to_expr),
                }))
            },
            Command::SetStack { addr, val } => opt_commands.push(Arc::new(Command::SetStack {
                addr: expr_replace_trivial_temps(addr, &trivial_temp_to_expr),
                val: expr_replace_trivial_temps(val, &trivial_temp_to_expr),
            })),
            Command::In => opt_commands.push(Arc::new(Command::In)),
            Command::Out(expr) => opt_commands.push(Arc::new(Command::Out(
                expr_replace_trivial_temps(expr, &trivial_temp_to_expr),
            ))),
        }
    }

    let opt_call = match sp.call.as_ref() {
        Call::Branch { cond, then_to, else_to } => Call::Branch {
            cond: expr_replace_trivial_temps(cond, &trivial_temp_to_expr),
            then_to: expr_replace_trivial_temps(then_to, &trivial_temp_to_expr),
            else_to: expr_replace_trivial_temps(else_to, &trivial_temp_to_expr),
        },
        Call::Jump(to) => Call::Jump(expr_replace_trivial_temps(to, &trivial_temp_to_expr)),
        Call::Exit => Call::Exit,
    };

    let optimized = trivial_temp_to_expr.is_empty().not();

    OptimizationReport {
        optimized,
        val: Arc::new(SubProc {
            uuid: sp.uuid,
            commands: Arc::new(opt_commands),
            call: Arc::new(opt_call),
        }),
    }
}

fn optimization_remove_unused_temps(
    sp: Arc<SubProc<UMemLoc>>,
) -> OptimizationReport<Arc<SubProc<UMemLoc>>> {
    let unused_temps = find_unused_temps(&sp);
    let opt_commands = sp
        .commands
        .iter()
        .filter(|command| {
            let Command::SetMemLoc { mem_loc, .. } = command.as_ref() else { return true };
            let UMemLoc::Temp(temp) = mem_loc.as_ref() else { return true };
            unused_temps.contains(temp).not()
        })
        .cloned()
        .collect();

    let optimized = unused_temps.is_empty().not();

    OptimizationReport {
        optimized,
        val: Arc::new(SubProc {
            uuid: sp.uuid,
            commands: Arc::new(opt_commands),
            call: sp.call.clone(),
        }),
    }
}

fn optimization_inline_pure_redirects(
    procs: Vec<Arc<Proc<UMemLoc>>>,
) -> OptimizationReport<Vec<Arc<Proc<UMemLoc>>>> {
    let mut pure_redirect_label_to_call = BTreeMap::new();

    // find pure redirects
    for sp in procs.iter().flat_map(|proc| proc.sub_procs.iter()) {
        if sp.commands.is_empty() {
            pure_redirect_label_to_call.insert(sp.uuid, sp.call.clone());
        }
    }

    let mut optimized = false;

    let optimize_jump = |to: &Expr<UMemLoc>| -> Option<Arc<Call<UMemLoc>>> {
        let Expr::Value(value) = to else { return None };
        let Value::Label(label) = value.as_ref() else { return None };

        let redirect_call = pure_redirect_label_to_call.get(label)?;
        Some(redirect_call.clone())
    };

    let call_to_single_to = |call: &Call<UMemLoc>| -> Option<Arc<Expr<UMemLoc>>> {
        match call {
            Call::Exit => None,
            Call::Branch { .. } => None,
            Call::Jump(to) => Some(to.clone()),
        }
    };

    let optimize_call = |call: &Call<UMemLoc>| -> Option<Arc<Call<UMemLoc>>> {
        match call {
            Call::Exit => None,
            Call::Jump(to) => optimize_jump(to),
            Call::Branch { cond, then_to, else_to } => {
                let then_to_redirect = optimize_jump(then_to);
                let else_to_redirect = optimize_jump(else_to);

                let then_to_optimized = then_to_redirect.and_then(|call| call_to_single_to(&call));
                let else_to_optimized = else_to_redirect.and_then(|call| call_to_single_to(&call));

                if then_to_optimized.is_none() && else_to_optimized.is_none() {
                    return None;
                }

                Some(Arc::new(Call::Branch {
                    cond: cond.clone(),
                    then_to: then_to_optimized.unwrap_or_else(|| then_to.clone()),
                    else_to: else_to_optimized.unwrap_or_else(|| else_to.clone()),
                }))
            },
        }
    };

    let procs = procs
        .iter()
        .map(|proc| {
            Arc::new(Proc {
                kind: proc.kind.clone(),
                sub_procs: Arc::new(
                    proc.sub_procs
                        .iter()
                        .map(|sp| {
                            Arc::new(SubProc {
                                uuid: sp.uuid,
                                commands: sp.commands.clone(),
                                call: match optimize_call(&sp.call) {
                                    Some(call) => {
                                        optimized = true;
                                        call
                                    },
                                    None => sp.call.clone(),
                                },
                            })
                        })
                        .collect(),
                ),
            })
        })
        .collect();

    OptimizationReport { optimized, val: procs }
}

fn find_trivial_temps(sp: &SubProc<UMemLoc>) -> BTreeMap<Arc<TempVar>, Arc<Expr<UMemLoc>>> {
    let mut trivial_temp_to_expr = BTreeMap::new();

    for command in sp.commands.as_ref() {
        if let Command::SetMemLoc { mem_loc, val } = command.as_ref() {
            if is_trivial_expr(val) {
                if let UMemLoc::Temp(temp) = mem_loc.as_ref() {
                    trivial_temp_to_expr.insert(temp.clone(), val.clone());
                }
            }
        }
    }

    trivial_temp_to_expr
}

fn is_trivial_expr(expr: &Expr<UMemLoc>) -> bool {
    // match expr {
    //     Expr::MemLoc(_) => true,
    //     Expr::Value(_) => true,
    //     _ => false,
    // }
    true
}

fn expr_replace_trivial_temps(
    expr: &Expr<UMemLoc>,
    trivial_temp_to_expr: &BTreeMap<Arc<TempVar>, Arc<Expr<UMemLoc>>>,
) -> Arc<Expr<UMemLoc>> {
    match expr {
        Expr::MemLoc(mem_loc) => match mem_loc.as_ref() {
            UMemLoc::StackPointer => Arc::new(Expr::MemLoc(Arc::new(UMemLoc::StackPointer))),
            UMemLoc::Temp(temp) => match trivial_temp_to_expr.get(temp) {
                Some(expr) => expr.clone(),
                None => Arc::new(Expr::MemLoc(Arc::new(UMemLoc::Temp(temp.clone())))),
            },
        },
        Expr::Value(value) => Arc::new(Expr::Value(value.clone())),
        Expr::Deref(expr) => {
            Arc::new(Expr::Deref(expr_replace_trivial_temps(expr, trivial_temp_to_expr)))
        },
        Expr::Add(args) => {
            Arc::new(Expr::Add(binary_args_replace_trivial_temps(args, trivial_temp_to_expr)))
        },
        Expr::Sub(args) => {
            Arc::new(Expr::Sub(binary_args_replace_trivial_temps(args, trivial_temp_to_expr)))
        },
        Expr::Mul(args) => {
            Arc::new(Expr::Mul(binary_args_replace_trivial_temps(args, trivial_temp_to_expr)))
        },
        Expr::Div(args) => {
            Arc::new(Expr::Div(binary_args_replace_trivial_temps(args, trivial_temp_to_expr)))
        },
        Expr::Mod(args) => {
            Arc::new(Expr::Mod(binary_args_replace_trivial_temps(args, trivial_temp_to_expr)))
        },
        Expr::Eq(args) => {
            Arc::new(Expr::Eq(binary_args_replace_trivial_temps(args, trivial_temp_to_expr)))
        },
        Expr::Lte(args) => {
            Arc::new(Expr::Lte(binary_args_replace_trivial_temps(args, trivial_temp_to_expr)))
        },
        Expr::Neq(args) => {
            Arc::new(Expr::Neq(binary_args_replace_trivial_temps(args, trivial_temp_to_expr)))
        },
        Expr::Not(expr) => {
            Arc::new(Expr::Not(expr_replace_trivial_temps(expr, trivial_temp_to_expr)))
        },
        Expr::InAnswer => Arc::new(Expr::InAnswer),
    }
}

fn binary_args_replace_trivial_temps(
    args: &BinaryArgs<UMemLoc>,
    trivial_temp_to_expr: &BTreeMap<Arc<TempVar>, Arc<Expr<UMemLoc>>>,
) -> Arc<BinaryArgs<UMemLoc>> {
    Arc::new(BinaryArgs {
        left: expr_replace_trivial_temps(&args.left, trivial_temp_to_expr),
        right: expr_replace_trivial_temps(&args.right, trivial_temp_to_expr),
    })
}

fn find_unused_temps(sp: &SubProc<UMemLoc>) -> BTreeSet<Arc<TempVar>> {
    let mut unused_temps = BTreeSet::new();
    let mut used_temps = BTreeSet::new();

    for command in sp.commands.as_ref() {
        if let Command::SetMemLoc { mem_loc, .. } = command.as_ref() {
            if let UMemLoc::Temp(temp) = mem_loc.as_ref() {
                unused_temps.insert(temp.clone());
            }
        }

        match command.as_ref() {
            Command::SetMemLoc { val, .. } => used_temps.extend(expr_get_used_temps(val)),
            Command::SetStack { addr, val } => {
                used_temps.extend(expr_get_used_temps(addr));
                used_temps.extend(expr_get_used_temps(val));
            },
            Command::In => {},
            Command::Out(expr) => used_temps.extend(expr_get_used_temps(expr)),
        }
    }

    match sp.call.as_ref() {
        Call::Branch { cond, then_to, else_to } => {
            used_temps.extend(expr_get_used_temps(cond));
            used_temps.extend(expr_get_used_temps(then_to));
            used_temps.extend(expr_get_used_temps(else_to));
        },
        Call::Jump(to) => used_temps.extend(expr_get_used_temps(to)),
        Call::Exit => {},
    }

    unused_temps.difference(&used_temps).cloned().collect()
}

fn expr_get_used_temps(expr: &Expr<UMemLoc>) -> BTreeSet<Arc<TempVar>> {
    match expr {
        Expr::MemLoc(mem_loc) => match mem_loc.as_ref() {
            UMemLoc::StackPointer => Default::default(),
            UMemLoc::Temp(temp) => BTreeSet::from([temp.clone()]),
        },
        Expr::Value(_) => Default::default(),
        Expr::Deref(expr) => expr_get_used_temps(expr),
        Expr::Add(args) => binary_args_get_used_temps(args),
        Expr::Sub(args) => binary_args_get_used_temps(args),
        Expr::Mul(args) => binary_args_get_used_temps(args),
        Expr::Div(args) => binary_args_get_used_temps(args),
        Expr::Mod(args) => binary_args_get_used_temps(args),
        Expr::Eq(args) => binary_args_get_used_temps(args),
        Expr::Lte(args) => binary_args_get_used_temps(args),
        Expr::Neq(args) => binary_args_get_used_temps(args),
        Expr::Not(expr) => expr_get_used_temps(expr),
        Expr::InAnswer => Default::default(),
    }
}

fn binary_args_get_used_temps(args: &BinaryArgs<UMemLoc>) -> BTreeSet<Arc<TempVar>> {
    let mut used_temps = expr_get_used_temps(&args.left);
    used_temps.extend(expr_get_used_temps(&args.right));
    used_temps
}
