use std::{
    collections::{BTreeMap, BTreeSet},
    ops::Not,
    sync::Arc,
};

use uuid::Uuid;

use crate::ast::{BinaryArgs, Call, Command, Expr, Proc, SubProc, TempVar, UMemLoc};

pub fn optimize<'a>(procs: impl Iterator<Item = &'a Proc<UMemLoc>>) -> Vec<Arc<Proc<UMemLoc>>> {
    procs.map(|proc| Arc::new(optimize_proc(proc))).collect()
}

fn optimize_proc(proc: &Proc<UMemLoc>) -> Proc<UMemLoc> {
    Proc {
        kind: proc.kind.clone(),
        sub_procs: Arc::new(
            proc.sub_procs.iter().map(|sp| optimize_sub_proc(sp.clone())).collect(),
        ),
    }
}

fn optimize_sub_proc(mut sp: Arc<SubProc<UMemLoc>>) -> Arc<SubProc<UMemLoc>> {
    let optimizer = Optimizer {
        optimizations: Vec::from([
            optimization_inline_trivial_temps,
            optimization_remove_unused_temps,
        ]),
    };

    loop {
        let report = optimizer.optimize(sp);
        sp = report.sp;
        if report.optimized.not() {
            break;
        }
    }

    sp
}

struct Optimizer {
    optimizations: Vec<fn(sp: Arc<SubProc<UMemLoc>>) -> OptimizationReport>,
}

impl Optimizer {
    pub fn optimize(&self, mut sp: Arc<SubProc<UMemLoc>>) -> OptimizationReport {
        let mut optimized = false;
        for optimization in &self.optimizations {
            let report = optimization(sp);
            if report.optimized {
                optimized = true;
            }

            sp = report.sp;
        }

        OptimizationReport { optimized, sp }
    }
}

struct OptimizationReport {
    optimized: bool,
    sp: Arc<SubProc<UMemLoc>>,
}

fn optimization_inline_trivial_temps(sp: Arc<SubProc<UMemLoc>>) -> OptimizationReport {
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
    println!("[opt] inline_trivial_temps: {}", optimized);

    OptimizationReport {
        optimized,
        sp: Arc::new(SubProc {
            uuid: sp.uuid,
            commands: Arc::new(opt_commands),
            call: Arc::new(opt_call),
        }),
    }
}

fn optimization_remove_unused_temps(sp: Arc<SubProc<UMemLoc>>) -> OptimizationReport {
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
    println!("[opt] remove_unused_temps: {}", optimized);

    OptimizationReport {
        optimized,
        sp: Arc::new(SubProc {
            uuid: sp.uuid,
            commands: Arc::new(opt_commands),
            call: sp.call.clone(),
        }),
    }
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
