use itertools::{Itertools, chain};
use uuid::Uuid;

use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    mem,
    ops::Not,
    sync::Arc,
};

use super::ast::{
    BinaryArgs, Call, Command, Expr, Proc, ProcKind, SubProc, TempVar, UMemLoc, Value,
};

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
        optimizations: Vec::from([
            Optimization {
                f: optimization_inline_pure_redirect_calls,
                name: "inline_pure_redirect_calls",
            },
            Optimization {
                f: optimization_inline_pure_redirect_labels,
                name: "inline_pure_redirect_labels",
            },
            Optimization {
                f: optimization_remove_unused_sub_procs,
                name: "remove_unused_sub_procs",
            },
            Optimization { f: optimization_inline_sub_procs, name: "inline_sub_procs" },
        ]),
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
            // Optimization { f: optimization_inline_assignments, name: "inline_assignments" },
        ]),
    };

    let mut tracker = Tracker::default();

    loop {
        opt_debug!("sub_proc {}", sp.uuid);

        sp = Arc::new(SubProc {
            uuid: sp.uuid,
            call: tracker.record(optimize_call(sp.call.clone())),
            commands: tracker.record(optimize_commands(sp.commands.clone())),
        });

        sp = tracker.record(optimizer.optimize(sp));

        if tracker.take_pass_optimized().not() {
            break;
        }
    }

    OptimizationReport { optimized: tracker.optimized(), val: sp }
}

fn optimize_commands(
    mut commands: Arc<Vec<Arc<Command<UMemLoc>>>>,
) -> OptimizationReport<Arc<Vec<Arc<Command<UMemLoc>>>>> {
    let optimizer = Optimizer { optimizations: Vec::new() };
    let mut tracker = Tracker::default();

    loop {
        opt_debug!("commands");

        commands = Arc::new(
            commands
                .iter()
                .map(|command| tracker.record(optimize_command(command.clone())))
                .collect(),
        );

        commands = tracker.record(optimizer.optimize(commands));

        if tracker.take_pass_optimized().not() {
            break;
        }
    }

    OptimizationReport { optimized: tracker.optimized(), val: commands }
}

fn optimize_command(
    mut command: Arc<Command<UMemLoc>>,
) -> OptimizationReport<Arc<Command<UMemLoc>>> {
    let optimizer = Optimizer { optimizations: Vec::new() };
    let mut tracker = Tracker::default();

    loop {
        opt_debug!("command");

        command = Arc::new(match command.as_ref() {
            Command::SetMemLoc { mem_loc, val } => Command::SetMemLoc {
                mem_loc: mem_loc.clone(),
                val: tracker.record(optimize_expr(val.clone())),
            },
            Command::SetStack { addr, val } => Command::SetStack {
                addr: tracker.record(optimize_expr(addr.clone())),
                val: tracker.record(optimize_expr(val.clone())),
            },
            Command::In => Command::In,
            Command::Out(expr) => Command::Out(tracker.record(optimize_expr(expr.clone()))),
            Command::ClearStdout => Command::ClearStdout,
            Command::WriteStdout { index, val } => Command::WriteStdout {
                index: tracker.record(optimize_expr(index.clone())),
                val: tracker.record(optimize_expr(val.clone())),
            },
            Command::Wait { duration_s } => {
                Command::Wait { duration_s: tracker.record(optimize_expr(duration_s.clone())) }
            },
        });

        command = tracker.record(optimizer.optimize(command));

        if tracker.take_pass_optimized().not() {
            break;
        }
    }

    OptimizationReport { optimized: tracker.optimized(), val: command }
}

fn optimize_call(mut call: Arc<Call<UMemLoc>>) -> OptimizationReport<Arc<Call<UMemLoc>>> {
    let optimizer = Optimizer { optimizations: Vec::new() };
    let mut tracker = Tracker::default();

    loop {
        opt_debug!("call");

        call = Arc::new(match call.as_ref() {
            Call::Exit => Call::Exit,
            Call::Jump(to) => Call::Jump(tracker.record(optimize_expr(to.clone()))),
            Call::Branch { cond, then_to, else_to } => Call::Branch {
                cond: tracker.record(optimize_expr(cond.clone())),
                then_to: tracker.record(optimize_expr(then_to.clone())),
                else_to: tracker.record(optimize_expr(else_to.clone())),
            },
        });

        call = tracker.record(optimizer.optimize(call));

        if tracker.take_pass_optimized().not() {
            break;
        }
    }

    OptimizationReport { optimized: tracker.optimized(), val: call }
}

fn optimize_expr(mut expr: Arc<Expr<UMemLoc>>) -> OptimizationReport<Arc<Expr<UMemLoc>>> {
    let optimizer = Optimizer {
        optimizations: Vec::from([Optimization {
            f: optimization_eval_well_known_exprs,
            name: "eval_well_known_exprs",
        }]),
    };
    let mut tracker = Tracker::default();

    loop {
        opt_debug!("expr");

        expr = Arc::new(match expr.as_ref() {
            Expr::Value(value) => Expr::Value(value.clone()),
            Expr::MemLoc(mem_loc) => Expr::MemLoc(mem_loc.clone()),
            Expr::StackDeref(expr) => Expr::StackDeref(tracker.record(optimize_expr(expr.clone()))),
            Expr::StdoutDeref(expr) => {
                Expr::StdoutDeref(tracker.record(optimize_expr(expr.clone())))
            },
            Expr::StdoutLen => Expr::StdoutLen,
            Expr::Timer => Expr::Timer,
            Expr::Add(args) => Expr::Add(tracker.record(optimize_binary_args(args.clone()))),
            Expr::Sub(args) => Expr::Sub(tracker.record(optimize_binary_args(args.clone()))),
            Expr::Mul(args) => Expr::Mul(tracker.record(optimize_binary_args(args.clone()))),
            Expr::Div(args) => Expr::Div(tracker.record(optimize_binary_args(args.clone()))),
            Expr::Mod(args) => Expr::Mod(tracker.record(optimize_binary_args(args.clone()))),
            Expr::Eq(args) => Expr::Eq(tracker.record(optimize_binary_args(args.clone()))),
            Expr::Gt(args) => Expr::Gt(tracker.record(optimize_binary_args(args.clone()))),
            Expr::Lt(args) => Expr::Lt(tracker.record(optimize_binary_args(args.clone()))),
            Expr::Not(expr) => Expr::Not(tracker.record(optimize_expr(expr.clone()))),
            Expr::Or(args) => Expr::Or(tracker.record(optimize_binary_args(args.clone()))),
            Expr::And(args) => Expr::And(tracker.record(optimize_binary_args(args.clone()))),
            Expr::InAnswer => Expr::InAnswer,
            Expr::Join(args) => Expr::Join(tracker.record(optimize_binary_args(args.clone()))),
            Expr::Random(args) => Expr::Random(tracker.record(optimize_binary_args(args.clone()))),
        });

        expr = tracker.record(optimizer.optimize(expr));

        if tracker.take_pass_optimized().not() {
            break;
        }
    }

    OptimizationReport { optimized: tracker.optimized(), val: expr }
}

fn optimize_binary_args(
    mut args: Arc<BinaryArgs<UMemLoc>>,
) -> OptimizationReport<Arc<BinaryArgs<UMemLoc>>> {
    let optimizer = Optimizer { optimizations: Vec::new() };
    let mut tracker = Tracker::default();

    loop {
        opt_debug!("binary args");

        args = Arc::new(BinaryArgs {
            left: tracker.record(optimize_expr(args.left.clone())),
            right: tracker.record(optimize_expr(args.right.clone())),
        });

        args = tracker.record(optimizer.optimize(args));

        if tracker.take_pass_optimized().not() {
            break;
        }
    }

    OptimizationReport { optimized: tracker.optimized(), val: args }
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
        let opt_command = match command.as_ref() {
            Command::SetMemLoc { mem_loc, val } => Command::SetMemLoc {
                mem_loc: mem_loc.clone(),
                val: expr_replace_trivial_temps(val, &trivial_temp_to_expr),
            },
            Command::SetStack { addr, val } => Command::SetStack {
                addr: expr_replace_trivial_temps(addr, &trivial_temp_to_expr),
                val: expr_replace_trivial_temps(val, &trivial_temp_to_expr),
            },
            Command::In => Command::In,
            Command::Out(expr) => {
                Command::Out(expr_replace_trivial_temps(expr, &trivial_temp_to_expr))
            },
            Command::ClearStdout => Command::ClearStdout,
            Command::WriteStdout { index, val } => Command::WriteStdout {
                index: expr_replace_trivial_temps(index, &trivial_temp_to_expr),
                val: expr_replace_trivial_temps(val, &trivial_temp_to_expr),
            },
            Command::Wait { duration_s } => Command::Wait {
                duration_s: expr_replace_trivial_temps(duration_s, &trivial_temp_to_expr),
            },
        };

        opt_commands.push(Arc::new(opt_command));
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

#[deprecated]
fn optimization_inline_assignments(
    sp: Arc<SubProc<UMemLoc>>,
) -> OptimizationReport<Arc<SubProc<UMemLoc>>> {
    let mut optimized = false;

    #[derive(PartialEq, Eq, PartialOrd, Ord)]
    enum Loc {
        Temp { loc: Arc<UMemLoc> },
        Stack { addr: Arc<Expr<UMemLoc>> },
    }

    let mut loc_to_val: BTreeMap<Loc, Arc<Expr<UMemLoc>>> = Default::default();

    fn expr_inline_loc(
        expr: &Expr<UMemLoc>,
        loc_to_val: &BTreeMap<Loc, Arc<Expr<UMemLoc>>>,
        optimized: &mut bool,
    ) -> Arc<Expr<UMemLoc>> {
        match expr {
            Expr::MemLoc(mem_loc) => match loc_to_val.get(&Loc::Temp { loc: mem_loc.clone() }) {
                Some(val) => {
                    println!("{:?} => {:?}", mem_loc, val);
                    *optimized = true;
                    val.clone()
                },
                None => Arc::new(Expr::MemLoc(mem_loc.clone())),
            },
            Expr::StackDeref(addr) => match loc_to_val.get(&Loc::Stack { addr: addr.clone() }) {
                Some(val) => {
                    *optimized = true;
                    val.clone()
                },
                None => Arc::new(Expr::StackDeref(expr_inline_loc(addr, loc_to_val, optimized))),
            },
            Expr::Add(args) => Arc::new(Expr::Add(args_inline_locs(args, loc_to_val, optimized))),
            Expr::And(args) => Arc::new(Expr::And(args_inline_locs(args, loc_to_val, optimized))),
            Expr::Div(args) => Arc::new(Expr::Div(args_inline_locs(args, loc_to_val, optimized))),
            Expr::Eq(args) => Arc::new(Expr::Eq(args_inline_locs(args, loc_to_val, optimized))),
            Expr::Gt(args) => Arc::new(Expr::Gt(args_inline_locs(args, loc_to_val, optimized))),
            Expr::InAnswer => Arc::new(Expr::InAnswer),
            Expr::Join(args) => Arc::new(Expr::Join(args_inline_locs(args, loc_to_val, optimized))),
            Expr::Lt(args) => Arc::new(Expr::Lt(args_inline_locs(args, loc_to_val, optimized))),
            Expr::Mod(args) => Arc::new(Expr::Mod(args_inline_locs(args, loc_to_val, optimized))),
            Expr::Mul(args) => Arc::new(Expr::Mul(args_inline_locs(args, loc_to_val, optimized))),
            Expr::Not(expr) => Arc::new(Expr::Not(expr_inline_loc(expr, loc_to_val, optimized))),
            Expr::Or(args) => Arc::new(Expr::Or(args_inline_locs(args, loc_to_val, optimized))),
            Expr::Random(args) => {
                Arc::new(Expr::Random(args_inline_locs(args, loc_to_val, optimized)))
            },
            Expr::StdoutDeref(addr) => {
                Arc::new(Expr::StdoutDeref(expr_inline_loc(addr, loc_to_val, optimized)))
            },
            Expr::StdoutLen => Arc::new(Expr::StdoutLen),
            Expr::Sub(args) => Arc::new(Expr::Sub(args_inline_locs(args, loc_to_val, optimized))),
            Expr::Timer => Arc::new(Expr::Timer),
            Expr::Value(value) => Arc::new(Expr::Value(value.clone())),
        }
    }

    fn args_inline_locs(
        args: &BinaryArgs<UMemLoc>,
        loc_to_val: &BTreeMap<Loc, Arc<Expr<UMemLoc>>>,
        optimized: &mut bool,
    ) -> Arc<BinaryArgs<UMemLoc>> {
        Arc::new(BinaryArgs {
            left: expr_inline_loc(&args.left, loc_to_val, optimized),
            right: expr_inline_loc(&args.right, loc_to_val, optimized),
        })
    }

    fn loc_contains_mem_loc(loc: &Loc, mem_loc: &UMemLoc) -> bool {
        match loc {
            Loc::Temp { loc } => loc.as_ref() == mem_loc,
            Loc::Stack { addr } => expr_contains_mem_loc(addr, mem_loc),
        }
    }

    fn expr_contains_mem_loc(expr: &Expr<UMemLoc>, mem_loc: &UMemLoc) -> bool {
        match expr {
            Expr::MemLoc(ml) => ml.as_ref() == mem_loc,
            Expr::Add(args) => args_contains_mem_loc(args, mem_loc),
            Expr::And(args) => args_contains_mem_loc(args, mem_loc),
            Expr::Div(args) => args_contains_mem_loc(args, mem_loc),
            Expr::Eq(args) => args_contains_mem_loc(args, mem_loc),
            Expr::Gt(args) => args_contains_mem_loc(args, mem_loc),
            Expr::InAnswer => false,
            Expr::Join(args) => args_contains_mem_loc(args, mem_loc),
            Expr::Lt(args) => args_contains_mem_loc(args, mem_loc),
            Expr::Mod(args) => args_contains_mem_loc(args, mem_loc),
            Expr::Mul(args) => args_contains_mem_loc(args, mem_loc),
            Expr::Not(expr) => expr_contains_mem_loc(expr, mem_loc),
            Expr::Or(args) => args_contains_mem_loc(args, mem_loc),
            Expr::Random(args) => args_contains_mem_loc(args, mem_loc),
            Expr::StackDeref(expr) => expr_contains_mem_loc(expr, mem_loc),
            Expr::StdoutDeref(expr) => expr_contains_mem_loc(expr, mem_loc),
            Expr::StdoutLen => false,
            Expr::Sub(args) => args_contains_mem_loc(args, mem_loc),
            Expr::Timer => false,
            Expr::Value(_) => false,
        }
    }

    fn args_contains_mem_loc(args: &BinaryArgs<UMemLoc>, mem_loc: &UMemLoc) -> bool {
        expr_contains_mem_loc(&args.left, mem_loc) || expr_contains_mem_loc(&args.right, mem_loc)
    }

    fn loc_contains_stack_deref(loc: &Loc, deref_addr: &Expr<UMemLoc>) -> bool {
        match loc {
            Loc::Temp { .. } => false,
            Loc::Stack { addr } => {
                addr.as_ref() == deref_addr || expr_contains_stack_deref(addr, deref_addr)
            },
        }
    }

    fn expr_contains_stack_deref(expr: &Expr<UMemLoc>, deref_addr: &Expr<UMemLoc>) -> bool {
        match expr {
            Expr::StackDeref(expr) => {
                expr.as_ref() == deref_addr || expr_contains_stack_deref(expr, deref_addr)
            },
            Expr::MemLoc(_) => false,
            Expr::Add(args) => args_contains_stack_deref(args, deref_addr),
            Expr::And(args) => args_contains_stack_deref(args, deref_addr),
            Expr::Div(args) => args_contains_stack_deref(args, deref_addr),
            Expr::Eq(args) => args_contains_stack_deref(args, deref_addr),
            Expr::Gt(args) => args_contains_stack_deref(args, deref_addr),
            Expr::InAnswer => false,
            Expr::Join(args) => args_contains_stack_deref(args, deref_addr),
            Expr::Lt(args) => args_contains_stack_deref(args, deref_addr),
            Expr::Mod(args) => args_contains_stack_deref(args, deref_addr),
            Expr::Mul(args) => args_contains_stack_deref(args, deref_addr),
            Expr::Not(expr) => expr_contains_stack_deref(expr, deref_addr),
            Expr::Or(args) => args_contains_stack_deref(args, deref_addr),
            Expr::Random(args) => args_contains_stack_deref(args, deref_addr),
            Expr::StdoutDeref(expr) => expr_contains_stack_deref(expr, deref_addr),
            Expr::StdoutLen => false,
            Expr::Sub(args) => args_contains_stack_deref(args, deref_addr),
            Expr::Timer => false,
            Expr::Value(_) => false,
        }
    }

    fn args_contains_stack_deref(args: &BinaryArgs<UMemLoc>, deref_addr: &Expr<UMemLoc>) -> bool {
        expr_contains_stack_deref(&args.left, deref_addr)
            || expr_contains_stack_deref(&args.right, deref_addr)
    }

    fn is_inlineable_val(val: &Expr<UMemLoc>) -> bool {
        match val {
            Expr::Value(_) => true,
            _ => false,
        }
    }

    let sub_proc = Arc::new(SubProc {
        uuid: sp.uuid,
        commands: Arc::new(
            sp.commands
                .iter()
                .map(|command| {
                    let inlined_command = match command.as_ref() {
                        Command::In => Command::In,
                        Command::ClearStdout => Command::ClearStdout,
                        Command::Out(expr) => {
                            Command::Out(expr_inline_loc(expr, &loc_to_val, &mut optimized))
                        },
                        Command::Wait { duration_s } => Command::Wait {
                            duration_s: expr_inline_loc(duration_s, &loc_to_val, &mut optimized),
                        },
                        Command::WriteStdout { index, val } => Command::WriteStdout {
                            index: expr_inline_loc(index, &loc_to_val, &mut optimized),
                            val: expr_inline_loc(val, &loc_to_val, &mut optimized),
                        },
                        Command::SetMemLoc { mem_loc, val } => Command::SetMemLoc {
                            mem_loc: mem_loc.clone(),
                            val: expr_inline_loc(val, &loc_to_val, &mut optimized),
                        },
                        Command::SetStack { addr, val } => Command::SetStack {
                            addr: expr_inline_loc(addr, &loc_to_val, &mut optimized),
                            val: expr_inline_loc(val, &loc_to_val, &mut optimized),
                        },
                    };

                    match &inlined_command {
                        Command::In => {},
                        Command::ClearStdout => {},
                        Command::Out(_expr) => {},
                        Command::Wait { duration_s: _ } => {},
                        Command::WriteStdout { index: _, val: _ } => {},
                        Command::SetMemLoc { mem_loc, val } => {
                            // remove all cached locs dependent on this mem_loc
                            loc_to_val.retain(|loc, _| loc_contains_mem_loc(loc, mem_loc).not());

                            if is_inlineable_val(val) {
                                loc_to_val.insert(Loc::Temp { loc: mem_loc.clone() }, val.clone());
                            }
                        },
                        Command::SetStack { addr, val } => {
                            // remove all cached locs dependent on this mem_loc
                            loc_to_val.retain(|loc, _| loc_contains_stack_deref(loc, addr).not());

                            if is_inlineable_val(val) {
                                loc_to_val.insert(Loc::Stack { addr: addr.clone() }, val.clone());
                            }
                        },
                    }

                    Arc::new(inlined_command)
                })
                .collect(),
        ),
        call: Arc::new(match sp.call.as_ref() {
            Call::Exit => Call::Exit,
            Call::Jump(expr) => Call::Jump(expr_inline_loc(expr, &loc_to_val, &mut optimized)),
            Call::Branch { cond, then_to, else_to } => Call::Branch {
                cond: expr_inline_loc(cond, &loc_to_val, &mut optimized),
                then_to: expr_inline_loc(then_to, &loc_to_val, &mut optimized),
                else_to: expr_inline_loc(else_to, &loc_to_val, &mut optimized),
            },
        }),
    });

    OptimizationReport { optimized, val: sub_proc }
}

fn optimization_inline_pure_redirect_calls(
    procs: Vec<Arc<Proc<UMemLoc>>>,
) -> OptimizationReport<Vec<Arc<Proc<UMemLoc>>>> {
    let pure_redirect_label_to_call = find_pure_redirects(procs.iter());

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

fn optimization_inline_pure_redirect_labels(
    procs: Vec<Arc<Proc<UMemLoc>>>,
) -> OptimizationReport<Vec<Arc<Proc<UMemLoc>>>> {
    let pure_redirect_label_to_call = find_pure_redirects(procs.iter());

    let pure_redirect_label_to_to_label = pure_redirect_label_to_call
        .into_iter()
        .filter_map(|(redirect_label, call)| match call.as_ref() {
            Call::Exit => None,
            Call::Branch { .. } => None,
            Call::Jump(to) => {
                let Expr::Value(value) = to.as_ref() else { return None };
                let Value::Label(to_label) = value.as_ref() else { return None };
                Some((redirect_label, *to_label))
            },
        })
        .collect::<BTreeMap<_, _>>();

    fn expr_replace_pure_redirect_labels(
        optimized: &mut bool,
        // pure_redirect_label_to_to_label
        rlabel_to_tlabel: &BTreeMap<Uuid, Uuid>,
        expr: &Expr<UMemLoc>,
    ) -> Arc<Expr<UMemLoc>> {
        let expr = match expr {
            Expr::MemLoc(mem_loc) => Expr::MemLoc(mem_loc.clone()),
            Expr::Value(value) => match value.as_ref() {
                Value::Literal(literal) => Expr::Value(Arc::new(Value::Literal(literal.clone()))),
                Value::Label(label) => {
                    let inlined_label = rlabel_to_tlabel.get(label);

                    *optimized = *optimized || inlined_label.is_some();

                    let label = *inlined_label.unwrap_or(label);
                    Expr::Value(Arc::new(Value::Label(label)))
                },
            },
            Expr::StackDeref(expr) => Expr::StackDeref(expr_replace_pure_redirect_labels(
                optimized,
                rlabel_to_tlabel,
                expr,
            )),
            Expr::StdoutDeref(expr) => Expr::StdoutDeref(expr_replace_pure_redirect_labels(
                optimized,
                rlabel_to_tlabel,
                expr,
            )),
            Expr::StdoutLen => Expr::StdoutLen,
            Expr::Timer => Expr::Timer,
            Expr::Add(args) => Expr::Add(binary_args_replace_pure_redirect_labels(
                optimized,
                rlabel_to_tlabel,
                args,
            )),
            Expr::Sub(args) => Expr::Sub(binary_args_replace_pure_redirect_labels(
                optimized,
                rlabel_to_tlabel,
                args,
            )),
            Expr::Mul(args) => Expr::Mul(binary_args_replace_pure_redirect_labels(
                optimized,
                rlabel_to_tlabel,
                args,
            )),
            Expr::Div(args) => Expr::Div(binary_args_replace_pure_redirect_labels(
                optimized,
                rlabel_to_tlabel,
                args,
            )),
            Expr::Mod(args) => Expr::Mod(binary_args_replace_pure_redirect_labels(
                optimized,
                rlabel_to_tlabel,
                args,
            )),
            Expr::Eq(args) => Expr::Eq(binary_args_replace_pure_redirect_labels(
                optimized,
                rlabel_to_tlabel,
                args,
            )),
            Expr::Gt(args) => Expr::Gt(binary_args_replace_pure_redirect_labels(
                optimized,
                rlabel_to_tlabel,
                args,
            )),
            Expr::Lt(args) => Expr::Lt(binary_args_replace_pure_redirect_labels(
                optimized,
                rlabel_to_tlabel,
                args,
            )),
            Expr::Not(expr) => {
                Expr::Not(expr_replace_pure_redirect_labels(optimized, rlabel_to_tlabel, expr))
            },
            Expr::Or(args) => Expr::Or(binary_args_replace_pure_redirect_labels(
                optimized,
                rlabel_to_tlabel,
                args,
            )),
            Expr::And(args) => Expr::And(binary_args_replace_pure_redirect_labels(
                optimized,
                rlabel_to_tlabel,
                args,
            )),
            Expr::InAnswer => Expr::InAnswer,
            Expr::Join(args) => Expr::Join(binary_args_replace_pure_redirect_labels(
                optimized,
                rlabel_to_tlabel,
                args,
            )),
            Expr::Random(args) => Expr::Random(binary_args_replace_pure_redirect_labels(
                optimized,
                rlabel_to_tlabel,
                args,
            )),
        };

        Arc::new(expr)
    }

    fn binary_args_replace_pure_redirect_labels(
        optimized: &mut bool,
        // pure_redirect_label_to_to_label
        rlabel_to_tlabel: &BTreeMap<Uuid, Uuid>,
        args: &BinaryArgs<UMemLoc>,
    ) -> Arc<BinaryArgs<UMemLoc>> {
        Arc::new(BinaryArgs {
            left: expr_replace_pure_redirect_labels(optimized, rlabel_to_tlabel, &args.left),
            right: expr_replace_pure_redirect_labels(optimized, rlabel_to_tlabel, &args.right),
        })
    }

    let mut optimized = false;

    let procs = procs
        .into_iter()
        .map(|proc| {
            Arc::new(Proc {
                kind: proc.kind.clone(),
                sub_procs: Arc::new(
                    proc.sub_procs
                        .iter()
                        .map(|sp| {
                            Arc::new(SubProc {
                                uuid: sp.uuid,
                                call: sp.call.clone(),
                                commands: Arc::new(
                                    sp.commands
                                        .iter()
                                        .map(|command| {
                                            let command = match command.as_ref() {
                                                Command::SetMemLoc { mem_loc, val } => {
                                                    Command::SetMemLoc {
                                                        mem_loc: mem_loc.clone(),
                                                        val: expr_replace_pure_redirect_labels(
                                                            &mut optimized,
                                                            &pure_redirect_label_to_to_label,
                                                            val,
                                                        ),
                                                    }
                                                },
                                                Command::SetStack { addr, val } => {
                                                    Command::SetStack {
                                                        addr: expr_replace_pure_redirect_labels(
                                                            &mut optimized,
                                                            &pure_redirect_label_to_to_label,
                                                            addr,
                                                        ),
                                                        val: expr_replace_pure_redirect_labels(
                                                            &mut optimized,
                                                            &pure_redirect_label_to_to_label,
                                                            val,
                                                        ),
                                                    }
                                                },
                                                Command::In => Command::In,
                                                Command::Out(expr) => {
                                                    Command::Out(expr_replace_pure_redirect_labels(
                                                        &mut optimized,
                                                        &pure_redirect_label_to_to_label,
                                                        expr,
                                                    ))
                                                },
                                                Command::ClearStdout => Command::ClearStdout,
                                                Command::WriteStdout { index, val } => {
                                                    Command::WriteStdout {
                                                        index: expr_replace_pure_redirect_labels(
                                                            &mut optimized,
                                                            &pure_redirect_label_to_to_label,
                                                            index,
                                                        ),
                                                        val: expr_replace_pure_redirect_labels(
                                                            &mut optimized,
                                                            &pure_redirect_label_to_to_label,
                                                            val,
                                                        ),
                                                    }
                                                },
                                                Command::Wait { duration_s } => Command::Wait {
                                                    duration_s: expr_replace_pure_redirect_labels(
                                                        &mut optimized,
                                                        &pure_redirect_label_to_to_label,
                                                        duration_s,
                                                    ),
                                                },
                                            };

                                            Arc::new(command)
                                        })
                                        .collect(),
                                ),
                            })
                        })
                        .collect(),
                ),
            })
        })
        .collect();

    OptimizationReport { optimized, val: procs }
}

fn optimization_inline_sub_procs(
    procs: Vec<Arc<Proc<UMemLoc>>>,
) -> OptimizationReport<Vec<Arc<Proc<UMemLoc>>>> {
    let label_to_sp = procs
        .iter()
        .flat_map(|proc| proc.sub_procs.iter())
        .map(|sp| (sp.uuid, sp))
        .collect::<BTreeMap<_, _>>();

    let mut optimized = false;

    let procs = procs
        .iter()
        .map(|proc| {
            Arc::new(Proc {
                kind: proc.kind.clone(),
                sub_procs: Arc::new(
                    proc.sub_procs
                        .iter()
                        .map(|sp| {
                            if let Call::Jump(to_label) = sp.call.as_ref()
                                && let Expr::Value(to_label) = to_label.as_ref()
                                && let Value::Label(to_label) = to_label.as_ref()
                            {
                                let to_sp =
                                    label_to_sp.get(to_label).expect("no sp with label found");

                                let commands = Arc::new(
                                    sp.commands
                                        .iter()
                                        .chain(to_sp.commands.iter())
                                        .cloned()
                                        .collect(),
                                );

                                let call = to_sp.call.clone();

                                optimized = true;

                                return Arc::new(SubProc { uuid: sp.uuid, commands, call });
                            }

                            sp.clone()
                        })
                        .collect(),
                ),
            })
        })
        .collect();

    OptimizationReport { optimized, val: procs }
}

fn optimization_remove_unused_sub_procs(
    procs: Vec<Arc<Proc<UMemLoc>>>,
) -> OptimizationReport<Vec<Arc<Proc<UMemLoc>>>> {
    // first main label is the start of the program...
    // ...always used, but never called by anything else, so we have to add it manually
    let main_proc = procs
        .iter()
        .find(|proc| matches!(proc.kind.as_ref(), ProcKind::Main))
        .expect("main proc should exist");

    let first_main_sp =
        main_proc.sub_procs.first().expect("a sub proc for the main proc should exist");

    let label_to_sp = procs
        .iter()
        .flat_map(|proc| proc.sub_procs.iter())
        .map(|sp| (sp.uuid, sp))
        .collect::<BTreeMap<_, _>>();

    let mut used_labels = BTreeSet::new();
    used_labels.insert(first_main_sp.uuid);

    // find all other used labels
    call_graph_search_for_used_labels(first_main_sp, &mut used_labels, &label_to_sp);

    fn call_graph_search_for_used_labels(
        sp: &Arc<SubProc<UMemLoc>>,
        used_labels: &mut BTreeSet<Uuid>,
        label_to_sp: &BTreeMap<Uuid, &Arc<SubProc<UMemLoc>>>,
    ) {
        let sp_used_labels = chain!(
            sp.commands.iter().flat_map(|command| command_find_used_labels(command)),
            call_find_used_labels(&sp.call)
        )
        .collect::<BTreeSet<_>>();

        for sp_label in sp_used_labels {
            if used_labels.insert(sp_label) {
                let sp = label_to_sp.get(&sp_label).expect("no sp found for label");
                call_graph_search_for_used_labels(sp, used_labels, label_to_sp);
            }
        }
    }

    fn command_find_used_labels(command: &Command<UMemLoc>) -> BTreeSet<Uuid> {
        match command {
            Command::SetMemLoc { val, mem_loc: _mem_loc } => expr_find_used_labels(val),
            Command::SetStack { addr, val } => {
                chain!(expr_find_used_labels(addr), expr_find_used_labels(val)).collect()
            },
            Command::In => Default::default(),
            Command::Out(expr) => expr_find_used_labels(expr),
            Command::ClearStdout => Default::default(),
            Command::WriteStdout { index, val } => {
                chain!(expr_find_used_labels(index), expr_find_used_labels(val)).collect()
            },
            Command::Wait { duration_s } => expr_find_used_labels(duration_s),
        }
    }

    fn call_find_used_labels(call: &Call<UMemLoc>) -> BTreeSet<Uuid> {
        match call {
            Call::Exit => Default::default(),
            Call::Jump(to) => expr_find_used_labels(to),
            Call::Branch { cond, then_to, else_to } => chain!(
                expr_find_used_labels(cond),
                expr_find_used_labels(then_to),
                expr_find_used_labels(else_to)
            )
            .collect(),
        }
    }

    fn expr_find_used_labels(expr: &Expr<UMemLoc>) -> BTreeSet<Uuid> {
        match expr {
            Expr::MemLoc(_) => Default::default(),
            Expr::Value(value) => match value.as_ref() {
                Value::Literal(_) => Default::default(),
                Value::Label(label) => BTreeSet::from([*label]),
            },
            Expr::StackDeref(expr) => expr_find_used_labels(expr),
            Expr::StdoutDeref(expr) => expr_find_used_labels(expr),
            Expr::StdoutLen => Default::default(),
            Expr::Timer => Default::default(),
            Expr::Add(args) => binary_args_find_used_labels(args),
            Expr::Sub(args) => binary_args_find_used_labels(args),
            Expr::Mul(args) => binary_args_find_used_labels(args),
            Expr::Div(args) => binary_args_find_used_labels(args),
            Expr::Mod(args) => binary_args_find_used_labels(args),
            Expr::Eq(args) => binary_args_find_used_labels(args),
            Expr::Gt(args) => binary_args_find_used_labels(args),
            Expr::Lt(args) => binary_args_find_used_labels(args),
            Expr::Not(expr) => expr_find_used_labels(expr),
            Expr::Or(args) => binary_args_find_used_labels(args),
            Expr::And(args) => binary_args_find_used_labels(args),
            Expr::InAnswer => Default::default(),
            Expr::Join(args) => binary_args_find_used_labels(args),
            Expr::Random(args) => binary_args_find_used_labels(args),
        }
    }

    fn binary_args_find_used_labels(args: &BinaryArgs<UMemLoc>) -> BTreeSet<Uuid> {
        chain!(expr_find_used_labels(&args.left), expr_find_used_labels(&args.right)).collect()
    }

    // remove procs/sub procs that go unused
    let optimized_procs = procs
        .iter()
        .map(|proc| {
            let sub_procs = proc
                .sub_procs
                .iter()
                .filter(|sp| used_labels.contains(&sp.uuid))
                .cloned()
                .collect();

            Arc::new(Proc { kind: proc.kind.clone(), sub_procs: Arc::new(sub_procs) })
        })
        .filter(|proc| proc.sub_procs.is_empty().not())
        .collect::<Vec<_>>();

    let procs_were_optimized = optimized_procs.len() != procs.len();
    let sub_procs_were_optimized =
        optimized_procs.iter().flat_map(|proc| proc.sub_procs.iter()).count()
            != procs.iter().flat_map(|proc| proc.sub_procs.iter()).count();

    OptimizationReport {
        optimized: procs_were_optimized || sub_procs_were_optimized,
        val: optimized_procs,
    }
}

fn optimization_eval_well_known_exprs(
    expr: Arc<Expr<UMemLoc>>,
) -> OptimizationReport<Arc<Expr<UMemLoc>>> {
    fn eval_well_known_expr(expr: &Expr<UMemLoc>) -> Option<Arc<Expr<UMemLoc>>> {
        match expr {
            Expr::Add(args) => {
                let BinaryArgs { left, right } = args.as_ref();

                if let Expr::Value(left) = left.as_ref() {
                    if let Value::Literal(left) = left.as_ref() {
                        if left.as_ref() == "0" {
                            return Some(right.clone());
                        }
                    }
                }

                if let Expr::Value(right) = right.as_ref() {
                    if let Value::Literal(right) = right.as_ref() {
                        if right.as_ref() == "0" {
                            return Some(left.clone());
                        }
                    }
                }
            },
            Expr::Sub(args) => {
                let BinaryArgs { left, right } = args.as_ref();

                if let Expr::Value(right) = right.as_ref() {
                    if let Value::Literal(right) = right.as_ref() {
                        if right.as_ref() == "0" {
                            return Some(left.clone());
                        }
                    }
                }
            },
            Expr::Mul(args) => {
                let BinaryArgs { left, right } = args.as_ref();

                if let Expr::Value(left) = left.as_ref() {
                    if let Value::Literal(left) = left.as_ref() {
                        if left.as_ref() == "0" {
                            return Some(Arc::new(Expr::Value(Arc::new(Value::Literal(
                                "0".into(),
                            )))));
                        }

                        if left.as_ref() == "1" {
                            return Some(right.clone());
                        }
                    }
                }

                if let Expr::Value(right) = right.as_ref() {
                    if let Value::Literal(right) = right.as_ref() {
                        if right.as_ref() == "0" {
                            return Some(Arc::new(Expr::Value(Arc::new(Value::Literal(
                                "0".into(),
                            )))));
                        }

                        if right.as_ref() == "1" {
                            return Some(left.clone());
                        }
                    }
                }
            },
            Expr::Div(args) => {
                let BinaryArgs { left, right } = args.as_ref();

                if let Expr::Value(right) = right.as_ref() {
                    if let Value::Literal(right) = right.as_ref() {
                        if right.as_ref() == "1" {
                            return Some(left.clone());
                        }
                    }
                }
            },
            Expr::Join(args) => {
                let BinaryArgs { left, right } = args.as_ref();

                if let Expr::Value(left) = left.as_ref() {
                    if let Value::Literal(left) = left.as_ref() {
                        if left.as_ref() == "" {
                            return Some(right.clone());
                        }
                    }
                }

                if let Expr::Value(right) = right.as_ref() {
                    if let Value::Literal(right) = right.as_ref() {
                        if right.as_ref() == "" {
                            return Some(left.clone());
                        }
                    }
                }
            },
            _ => {},
        }

        None
    }

    let optimized = eval_well_known_expr(&expr);
    OptimizationReport { optimized: optimized.is_some(), val: optimized.unwrap_or(expr) }
}

// a "pure redirect" is a sub proc where its only command is a call to another sub proc
fn find_pure_redirects<'a>(
    procs: impl Iterator<Item = &'a Arc<Proc<UMemLoc>>>,
) -> BTreeMap<Uuid, Arc<Call<UMemLoc>>> {
    let mut pure_redirect_label_to_call = BTreeMap::new();

    for sp in procs.flat_map(|proc| proc.sub_procs.iter()) {
        if sp.commands.is_empty() {
            pure_redirect_label_to_call.insert(sp.uuid, sp.call.clone());
        }
    }

    pure_redirect_label_to_call
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
    match expr {
        Expr::MemLoc(_) => true,
        Expr::Value(_) => true,
        Expr::StackDeref(_) => false,
        Expr::StdoutDeref(_) => false,
        Expr::StdoutLen => true,
        Expr::Timer => true,
        Expr::Add(_) => false,
        Expr::Sub(_) => false,
        Expr::Mul(_) => false,
        Expr::Div(_) => false,
        Expr::Mod(_) => false,
        Expr::Eq(_) => false,
        Expr::Lt(_) => false,
        Expr::Gt(_) => false,
        Expr::Not(_) => false,
        Expr::Or(_) => false,
        Expr::And(_) => false,
        Expr::InAnswer => true,
        Expr::Join(_) => false,
        Expr::Random(_) => false,
    }
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
        Expr::StackDeref(expr) => {
            Arc::new(Expr::StackDeref(expr_replace_trivial_temps(expr, trivial_temp_to_expr)))
        },
        Expr::StdoutDeref(expr) => {
            Arc::new(Expr::StdoutDeref(expr_replace_trivial_temps(expr, trivial_temp_to_expr)))
        },
        Expr::StdoutLen => Arc::new(Expr::StdoutLen),
        Expr::Timer => Arc::new(Expr::Timer),
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
        Expr::Gt(args) => {
            Arc::new(Expr::Gt(binary_args_replace_trivial_temps(args, trivial_temp_to_expr)))
        },
        Expr::Lt(args) => {
            Arc::new(Expr::Lt(binary_args_replace_trivial_temps(args, trivial_temp_to_expr)))
        },
        Expr::Not(expr) => {
            Arc::new(Expr::Not(expr_replace_trivial_temps(expr, trivial_temp_to_expr)))
        },
        Expr::Or(args) => {
            Arc::new(Expr::Or(binary_args_replace_trivial_temps(args, trivial_temp_to_expr)))
        },
        Expr::And(args) => {
            Arc::new(Expr::And(binary_args_replace_trivial_temps(args, trivial_temp_to_expr)))
        },
        Expr::InAnswer => Arc::new(Expr::InAnswer),
        Expr::Join(args) => {
            Arc::new(Expr::Join(binary_args_replace_trivial_temps(args, trivial_temp_to_expr)))
        },
        Expr::Random(args) => {
            Arc::new(Expr::Random(binary_args_replace_trivial_temps(args, trivial_temp_to_expr)))
        },
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
            Command::SetMemLoc { val, mem_loc: _ } => used_temps.extend(expr_get_used_temps(val)),
            Command::SetStack { addr, val } => {
                used_temps.extend(expr_get_used_temps(addr));
                used_temps.extend(expr_get_used_temps(val));
            },
            Command::In => {},
            Command::Out(expr) => used_temps.extend(expr_get_used_temps(expr)),
            Command::ClearStdout => {},
            Command::WriteStdout { index, val } => {
                used_temps.extend(expr_get_used_temps(index));
                used_temps.extend(expr_get_used_temps(val));
            },
            Command::Wait { duration_s } => used_temps.extend(expr_get_used_temps(duration_s)),
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
        Expr::StackDeref(expr) => expr_get_used_temps(expr),
        Expr::StdoutDeref(expr) => expr_get_used_temps(expr),
        Expr::StdoutLen => Default::default(),
        Expr::Timer => Default::default(),
        Expr::Add(args) => binary_args_get_used_temps(args),
        Expr::Sub(args) => binary_args_get_used_temps(args),
        Expr::Mul(args) => binary_args_get_used_temps(args),
        Expr::Div(args) => binary_args_get_used_temps(args),
        Expr::Mod(args) => binary_args_get_used_temps(args),
        Expr::Eq(args) => binary_args_get_used_temps(args),
        Expr::Gt(args) => binary_args_get_used_temps(args),
        Expr::Lt(args) => binary_args_get_used_temps(args),
        Expr::Not(expr) => expr_get_used_temps(expr),
        Expr::Or(args) => binary_args_get_used_temps(args),
        Expr::And(args) => binary_args_get_used_temps(args),
        Expr::InAnswer => Default::default(),
        Expr::Join(args) => binary_args_get_used_temps(args),
        Expr::Random(args) => binary_args_get_used_temps(args),
    }
}

fn binary_args_get_used_temps(args: &BinaryArgs<UMemLoc>) -> BTreeSet<Arc<TempVar>> {
    let mut used_temps = expr_get_used_temps(&args.left);
    used_temps.extend(expr_get_used_temps(&args.right));
    used_temps
}
