use std::{
    collections::{BTreeMap, BTreeSet},
    ops::Not,
    sync::Arc,
};

use crate::inline::{
    ast::{ArgAssignment, BinaryArgs, Call, Command, Expr, Loc, SubProc, TempVar},
    export,
    opt::{
        MaybeOptimized, OptimizationFn,
        call::optimize_call,
        command::optimize_command,
        track_optimize, tracked_exhaust_optimize,
        util::{
            find_addr_expr_used_local_vars::expr_find_addr_expr_used_vars,
            is_definitely_not_runtime_equal::expr_is_definitely_not_runtime_equal,
        },
    },
};

pub fn optimize_sub_proc(sp: &Arc<SubProc>) -> MaybeOptimized<Arc<SubProc>> {
    let mut optimized = false;

    let commands =
        tracked_exhaust_optimize(&mut optimized, sp.commands.as_ref().clone(), |commands| {
            commands.into_iter().map(|command| optimize_command(&command)).collect()
        });

    let call =
        tracked_exhaust_optimize(&mut optimized, sp.call.clone(), |call| optimize_call(&call));

    let mut sp = Arc::new(SubProc { uuid: sp.uuid, commands: Arc::new(commands), call });

    const OPTIMIZATIONS: [OptimizationFn<Arc<SubProc>>; 4] = [
        optimization_inline_constant_trivial_temp_exprs,
        optimization_inline_single_read_constant_temps,
        optimization_remove_zero_read_temps,
        optimization_inline_constant_trivial_derefs,
    ];

    for optimization in OPTIMIZATIONS {
        sp = tracked_exhaust_optimize(&mut optimized, sp, |sp| optimization(&sp));
    }

    MaybeOptimized { optimized, val: sp }
}

fn optimization_inline_constant_trivial_temp_exprs(
    sp: &Arc<SubProc>,
) -> MaybeOptimized<Arc<SubProc>> {
    let mut optimized = false;

    let temp_to_trivial_expr = sp
        .commands
        .iter()
        .filter_map(|command| {
            let Command::SetLoc { loc, val } = command.as_ref() else { return None };
            let Loc::Temp(temp) = loc.as_ref() else { return None };

            if (is_trivial_expr(val) && is_constant_expr(val)).not() {
                return None;
            }

            Some((temp.clone(), val.clone()))
        })
        .collect::<BTreeMap<_, _>>();

    let commands = sp
        .commands
        .iter()
        .map(|command| {
            Arc::new(track_optimize(
                &mut optimized,
                command_inline_temps(command, &temp_to_trivial_expr),
            ))
        })
        .collect();

    let call = track_optimize(&mut optimized, call_inline_temps(&sp.call, &temp_to_trivial_expr));

    let sp = SubProc { uuid: sp.uuid, commands: Arc::new(commands), call: Arc::new(call) };

    MaybeOptimized { optimized, val: Arc::new(sp) }
}

fn is_trivial_expr(expr: &Expr) -> bool {
    // match expr {
    //     Expr::Loc(loc) => match loc.as_ref() {
    //         Loc::Temp(_) => true,
    //         Loc::Deref(addr) => is_base_trivial_expr(addr),
    //     },
    //     expr => is_base_trivial_expr(expr),
    // }

    is_base_trivial_expr(expr)
}

fn is_base_trivial_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Value(_) | Expr::InAnswer | Expr::StackAddr(_) | Expr::StdoutLen | Expr::Timer => {
            true
        },
        Expr::Loc(loc) => matches!(loc.as_ref(), Loc::Temp(_)),
        _ => false,
    }
}

// a "constant" expr is one that's value will not change if it is moved to execute at a different
// time or place within the program; it's value will always remain the same no matter the circumstances
fn is_constant_expr(expr: &Expr) -> bool {
    macro_rules! args {
        ($args:expr) => {
            is_constant_expr(&$args.left) && is_constant_expr(&$args.right)
        };
    }

    match expr {
        Expr::Loc(loc) => match loc.as_ref() {
            // temp locs are single assign, as long as they are moved within the same sub proc
            // their value will be constant
            Loc::Temp(_) => true,
            // the value a stack addr points to can change if it is set at a different time
            Loc::Deref(_) => false,
        },
        Expr::StackAddr(_) => true,
        Expr::Value(_) => true,
        Expr::StdoutDeref(_) => false,
        Expr::StdoutLen => false,
        Expr::Timer => false,
        Expr::Add(args) => args!(args),
        Expr::Sub(args) => args!(args),
        Expr::Mul(args) => args!(args),
        Expr::Div(args) => args!(args),
        Expr::Mod(args) => args!(args),
        Expr::Eq(args) => args!(args),
        Expr::Lt(args) => args!(args),
        Expr::Gt(args) => args!(args),
        Expr::Not(expr) => is_constant_expr(expr),
        Expr::Or(args) => args!(args),
        Expr::And(args) => args!(args),
        Expr::InAnswer => false,
        Expr::Join(args) => args!(args),
        Expr::Random(_) => false,
    }
}

fn command_inline_temps(
    command: &Command,
    temp_to_expr: &BTreeMap<Arc<TempVar>, Arc<Expr>>,
) -> MaybeOptimized<Command> {
    let mut optimized = false;

    macro_rules! expr {
        ($expr:expr) => {
            track_optimize(&mut optimized, expr_inline_temps($expr, temp_to_expr))
        };
    }

    let command = match command {
        Command::In => Command::In,
        Command::ClearStdout => Command::ClearStdout,
        Command::Out(expr) => Command::Out(expr!(expr)),
        Command::WriteStdout { index, val } => {
            Command::WriteStdout { index: expr!(index), val: expr!(val) }
        },
        Command::SetLoc { loc, val } => {
            let loc = match loc.as_ref() {
                Loc::Temp(temp) => Loc::Temp(temp.clone()),
                Loc::Deref(addr) => Loc::Deref(track_optimize(
                    &mut optimized,
                    expr_inline_temps(addr, temp_to_expr),
                )),
            };

            Command::SetLoc { loc: Arc::new(loc), val: expr!(val) }
        },
    };

    MaybeOptimized { optimized, val: command }
}

fn call_inline_temps(
    call: &Call,
    temp_to_expr: &BTreeMap<Arc<TempVar>, Arc<Expr>>,
) -> MaybeOptimized<Call> {
    let mut optimized = false;

    macro_rules! expr {
        ($expr:expr) => {
            track_optimize(&mut optimized, expr_inline_temps($expr, temp_to_expr))
        };
    }

    let call = match call {
        Call::Exit => Call::Exit,
        Call::Jump { to } => Call::Jump { to: expr!(to) },
        Call::Branch { cond, then_to, else_to } => {
            Call::Branch { cond: expr!(cond), then_to: expr!(then_to), else_to: expr!(else_to) }
        },
        Call::Sleep { duration_s, to } => {
            Call::Sleep { duration_s: expr!(duration_s), to: expr!(to) }
        },
        Call::Return { to } => Call::Return { to: expr!(to) },
        Call::Func { to_func_name, arg_assignments } => Call::Func {
            to_func_name: to_func_name.clone(),
            arg_assignments: Arc::new(
                arg_assignments
                    .iter()
                    .map(|aa| {
                        Arc::new(ArgAssignment {
                            arg_uuid: aa.arg_uuid,
                            arg_offset: aa.arg_offset,
                            expr: expr!(&aa.expr),
                        })
                    })
                    .collect(),
            ),
        },
    };

    MaybeOptimized { optimized, val: call }
}

fn expr_inline_temps(
    expr: &Expr,
    temp_to_expr: &BTreeMap<Arc<TempVar>, Arc<Expr>>,
) -> MaybeOptimized<Arc<Expr>> {
    let mut optimized = false;

    macro_rules! expr {
        ($expr:expr) => {
            track_optimize(&mut optimized, expr_inline_temps($expr, temp_to_expr))
        };
    }

    macro_rules! args {
        ($args:expr) => {
            Arc::new(BinaryArgs { left: expr!(&$args.left), right: expr!(&$args.right) })
        };
    }

    let expr = match expr {
        Expr::Loc(loc) => match loc.as_ref() {
            Loc::Temp(temp) => match temp_to_expr.get(temp) {
                None => Arc::new(Expr::Loc(Arc::new(Loc::Temp(temp.clone())))),
                Some(expr) => expr!(expr),
            },
            Loc::Deref(addr) => Arc::new(Expr::Loc(Arc::new(Loc::Deref(expr!(addr))))),
        },
        Expr::StackAddr(addr) => Arc::new(Expr::StackAddr(addr.clone())),
        Expr::Value(value) => Arc::new(Expr::Value(value.clone())),
        Expr::StdoutDeref(expr) => Arc::new(Expr::StdoutDeref(expr!(expr))),
        Expr::StdoutLen => Arc::new(Expr::StdoutLen),
        Expr::Timer => Arc::new(Expr::Timer),
        Expr::Add(args) => Arc::new(Expr::Add(args!(args))),
        Expr::Sub(args) => Arc::new(Expr::Sub(args!(args))),
        Expr::Mul(args) => Arc::new(Expr::Mul(args!(args))),
        Expr::Div(args) => Arc::new(Expr::Div(args!(args))),
        Expr::Mod(args) => Arc::new(Expr::Mod(args!(args))),
        Expr::Eq(args) => Arc::new(Expr::Eq(args!(args))),
        Expr::Lt(args) => Arc::new(Expr::Lt(args!(args))),
        Expr::Gt(args) => Arc::new(Expr::Gt(args!(args))),
        Expr::Not(expr) => Arc::new(Expr::Not(expr!(expr))),
        Expr::Or(args) => Arc::new(Expr::Or(args!(args))),
        Expr::And(args) => Arc::new(Expr::And(args!(args))),
        Expr::InAnswer => Arc::new(Expr::InAnswer),
        Expr::Join(args) => Arc::new(Expr::Join(args!(args))),
        Expr::Random(args) => Arc::new(Expr::Random(args!(args))),
    };

    MaybeOptimized { optimized, val: expr }
}

fn optimization_inline_single_read_constant_temps(
    sp: &Arc<SubProc>,
) -> MaybeOptimized<Arc<SubProc>> {
    let mut optimized = false;

    let single_use_temp_to_expr = find_single_read_temps_to_constant_exprs(sp);

    let commands = sp
        .commands
        .iter()
        .map(|command| {
            Arc::new(track_optimize(
                &mut optimized,
                command_inline_temps(command, &single_use_temp_to_expr),
            ))
        })
        .collect();

    let call =
        track_optimize(&mut optimized, call_inline_temps(&sp.call, &single_use_temp_to_expr));

    let sp = SubProc { uuid: sp.uuid, commands: Arc::new(commands), call: Arc::new(call) };

    MaybeOptimized { optimized, val: Arc::new(sp) }
}

fn find_single_read_temps_to_constant_exprs(sp: &SubProc) -> BTreeMap<Arc<TempVar>, Arc<Expr>> {
    let mut temp_to_reads = BTreeMap::new();
    sp_count_temp_reads(sp, &mut temp_to_reads);

    sp.commands
        .iter()
        .filter_map(|command| {
            let Command::SetLoc { loc, val } = command.as_ref() else { return None };
            let Loc::Temp(temp) = loc.as_ref() else { return None };

            let reads = temp_to_reads.get(temp).copied().unwrap_or_default();
            if reads != 1 || is_constant_expr(val).not() {
                return None;
            }

            Some((temp.clone(), val.clone()))
        })
        .collect()
}

fn sp_count_temp_reads(sp: &SubProc, temp_to_reads: &mut BTreeMap<Arc<TempVar>, u32>) {
    for command in sp.commands.iter() {
        command_count_temp_reads(command, temp_to_reads);
    }

    call_count_temp_reads(&sp.call, temp_to_reads);
}

fn command_count_temp_reads(command: &Command, temp_to_reads: &mut BTreeMap<Arc<TempVar>, u32>) {
    match command {
        Command::In => {},
        Command::ClearStdout => {},
        Command::Out(expr) => expr_count_temp_reads(expr, temp_to_reads),
        Command::WriteStdout { index, val } => {
            expr_count_temp_reads(index, temp_to_reads);
            expr_count_temp_reads(val, temp_to_reads);
        },
        Command::SetLoc { loc, val } => {
            match loc.as_ref() {
                Loc::Temp(_) => {}, // this is a write, not a read
                Loc::Deref(addr) => expr_count_temp_reads(addr, temp_to_reads),
            }

            expr_count_temp_reads(val, temp_to_reads);
        },
    }
}

fn call_count_temp_reads(call: &Call, temp_to_reads: &mut BTreeMap<Arc<TempVar>, u32>) {
    match call {
        Call::Exit => {},
        Call::Jump { to } => expr_count_temp_reads(to, temp_to_reads),
        Call::Branch { cond, then_to, else_to } => {
            expr_count_temp_reads(cond, temp_to_reads);
            expr_count_temp_reads(then_to, temp_to_reads);
            expr_count_temp_reads(else_to, temp_to_reads);
        },
        Call::Sleep { duration_s, to } => {
            expr_count_temp_reads(duration_s, temp_to_reads);
            expr_count_temp_reads(to, temp_to_reads);
        },
        Call::Return { to } => expr_count_temp_reads(to, temp_to_reads),
        Call::Func { to_func_name: _, arg_assignments } => {
            for aa in arg_assignments.iter() {
                expr_count_temp_reads(&aa.expr, temp_to_reads);
            }
        },
    }
}

fn expr_count_temp_reads(expr: &Expr, temp_to_reads: &mut BTreeMap<Arc<TempVar>, u32>) {
    match expr {
        Expr::Loc(loc) => loc_count_temp_reads(loc, temp_to_reads),
        Expr::StackAddr(_) => {},
        Expr::Value(_) => {},
        Expr::StdoutDeref(expr) => expr_count_temp_reads(expr, temp_to_reads),
        Expr::StdoutLen => {},
        Expr::Timer => {},
        Expr::Add(args) => args_count_temp_reads(args, temp_to_reads),
        Expr::Sub(args) => args_count_temp_reads(args, temp_to_reads),
        Expr::Mul(args) => args_count_temp_reads(args, temp_to_reads),
        Expr::Div(args) => args_count_temp_reads(args, temp_to_reads),
        Expr::Mod(args) => args_count_temp_reads(args, temp_to_reads),
        Expr::Eq(args) => args_count_temp_reads(args, temp_to_reads),
        Expr::Lt(args) => args_count_temp_reads(args, temp_to_reads),
        Expr::Gt(args) => args_count_temp_reads(args, temp_to_reads),
        Expr::Not(expr) => expr_count_temp_reads(expr, temp_to_reads),
        Expr::Or(args) => args_count_temp_reads(args, temp_to_reads),
        Expr::And(args) => args_count_temp_reads(args, temp_to_reads),
        Expr::InAnswer => {},
        Expr::Join(args) => args_count_temp_reads(args, temp_to_reads),
        Expr::Random(args) => args_count_temp_reads(args, temp_to_reads),
    }
}

fn args_count_temp_reads(args: &BinaryArgs, temp_to_reads: &mut BTreeMap<Arc<TempVar>, u32>) {
    expr_count_temp_reads(&args.left, temp_to_reads);
    expr_count_temp_reads(&args.right, temp_to_reads);
}

fn loc_count_temp_reads(loc: &Loc, temp_to_reads: &mut BTreeMap<Arc<TempVar>, u32>) {
    match loc {
        Loc::Temp(temp) => {
            let reads = temp_to_reads.entry(temp.clone()).or_default();
            *reads += 1;
        },
        Loc::Deref(addr) => expr_count_temp_reads(addr, temp_to_reads),
    }
}

fn optimization_remove_zero_read_temps(sp: &Arc<SubProc>) -> MaybeOptimized<Arc<SubProc>> {
    let mut optimized = false;

    let mut temp_to_reads = BTreeMap::new();
    sp_count_temp_reads(sp, &mut temp_to_reads);
    let mut removed_temps = BTreeSet::new();

    let commands = sp
        .commands
        .iter()
        .filter(|command| {
            if let Command::SetLoc { loc, .. } = command.as_ref()
                && let Loc::Temp(temp) = loc.as_ref()
                && temp_to_reads.get(temp).copied().unwrap_or_default() == 0
            {
                removed_temps.insert(temp.clone());
                optimized = true;
                false
            } else {
                true
            }
        })
        .cloned()
        .collect();

    let optimized_sp =
        SubProc { uuid: sp.uuid, commands: Arc::new(commands), call: sp.call.clone() };

    // if optimized {
    //     println!("Before:");
    //     println!("{}", export::export_sub_proc(&mut export::NameManager::default(), sp));
    //     println!("After:");
    //     println!("{}", export::export_sub_proc(&mut export::NameManager::default(), &optimized_sp));
    //     println!("Removed:");
    //     println!("{:#?}", &removed_temps);
    // }

    MaybeOptimized { optimized, val: Arc::new(optimized_sp) }
}

fn optimization_inline_constant_trivial_derefs(sp: &Arc<SubProc>) -> MaybeOptimized<Arc<SubProc>> {
    let mut optimized = false;

    let mut deref_addr_to_constant_trivial_expr = BTreeMap::new();

    macro_rules! expr {
        ($expr:expr) => {
            track_optimize(
                &mut optimized,
                expr_inline_constant_trivial_derefs($expr, &deref_addr_to_constant_trivial_expr),
            )
        };
    }

    let commands = sp
        .commands
        .iter()
        .map(|command| {
            Arc::new(match command.as_ref() {
                Command::In => Command::In,
                Command::ClearStdout => Command::ClearStdout,
                Command::Out(expr) => Command::Out(expr!(expr)),
                Command::WriteStdout { index, val } => {
                    Command::WriteStdout { index: expr!(index), val: expr!(val) }
                },
                Command::SetLoc { loc, val } => {
                    let val = expr!(val);

                    // remove addrs which could be modified after this point due to deref assignment
                    let addrs_used_as_values = expr_find_addr_expr_used_vars(&val);
                    for addr in addrs_used_as_values {
                        let addr = Expr::StackAddr(Arc::new(addr));
                        deref_addr_to_constant_trivial_expr
                            .retain(|key, _| expr_is_definitely_not_runtime_equal(key, &addr));
                    }

                    if let Loc::Deref(addr) = loc.as_ref() {
                        if is_trivial_expr(&val) && is_constant_expr(&val) {
                            // mark addr as constant trivial expr
                            deref_addr_to_constant_trivial_expr.insert(addr.clone(), val.clone());
                        } else {
                            // remove addr if it was previously added but is no longer constant trivial
                            deref_addr_to_constant_trivial_expr.remove(addr);
                        }
                    }

                    Command::SetLoc { loc: loc.clone(), val }
                },
            })
        })
        .collect();

    let call = match sp.call.as_ref() {
        Call::Jump { to } => Call::Jump { to: expr!(to) },
        Call::Branch { cond, then_to, else_to } => {
            Call::Branch { cond: expr!(cond), then_to: expr!(then_to), else_to: expr!(else_to) }
        },
        Call::Sleep { duration_s, to } => {
            Call::Sleep { duration_s: expr!(duration_s), to: expr!(to) }
        },
        Call::Func { to_func_name, arg_assignments } => Call::Func {
            to_func_name: to_func_name.clone(),
            arg_assignments: Arc::new(
                arg_assignments
                    .iter()
                    .map(|aa| {
                        Arc::new(ArgAssignment {
                            arg_uuid: aa.arg_uuid,
                            arg_offset: aa.arg_offset,
                            expr: expr!(&aa.expr),
                        })
                    })
                    .collect(),
            ),
        },
        Call::Return { to } => Call::Return { to: expr!(to) },
        Call::Exit => Call::Exit,
    };

    let sp = SubProc { uuid: sp.uuid, commands: Arc::new(commands), call: Arc::new(call) };

    MaybeOptimized { optimized, val: Arc::new(sp) }
}

fn expr_inline_constant_trivial_derefs(
    expr: &Expr,
    deref_addr_to_constant_trivial_expr: &BTreeMap<Arc<Expr>, Arc<Expr>>,
) -> MaybeOptimized<Arc<Expr>> {
    let mut optimized = false;

    macro_rules! expr {
        ($expr:expr) => {
            track_optimize(
                &mut optimized,
                expr_inline_constant_trivial_derefs($expr, &deref_addr_to_constant_trivial_expr),
            )
        };
    }

    macro_rules! args {
        ($expr:expr) => {
            Arc::new(track_optimize(
                &mut optimized,
                args_inline_constant_trivial_derefs($expr, &deref_addr_to_constant_trivial_expr),
            ))
        };
    }

    let expr = match expr {
        Expr::Loc(loc) => match loc.as_ref() {
            Loc::Temp(temp) => Arc::new(Expr::Loc(Arc::new(Loc::Temp(temp.clone())))),
            Loc::Deref(addr) => match deref_addr_to_constant_trivial_expr.get(addr) {
                None => Arc::new(Expr::Loc(Arc::new(Loc::Deref(expr!(addr))))),
                Some(trivial_expr) => trivial_expr.clone(),
            },
        },
        Expr::StackAddr(addr) => Arc::new(Expr::StackAddr(addr.clone())),
        Expr::Value(value) => Arc::new(Expr::Value(value.clone())),
        Expr::StdoutDeref(expr) => Arc::new(Expr::StdoutDeref(expr!(expr))),
        Expr::StdoutLen => Arc::new(Expr::StdoutLen),
        Expr::Timer => Arc::new(Expr::Timer),
        Expr::Add(args) => Arc::new(Expr::Add(args!(args))),
        Expr::Sub(args) => Arc::new(Expr::Sub(args!(args))),
        Expr::Mul(args) => Arc::new(Expr::Mul(args!(args))),
        Expr::Div(args) => Arc::new(Expr::Div(args!(args))),
        Expr::Mod(args) => Arc::new(Expr::Mod(args!(args))),
        Expr::Eq(args) => Arc::new(Expr::Eq(args!(args))),
        Expr::Lt(args) => Arc::new(Expr::Lt(args!(args))),
        Expr::Gt(args) => Arc::new(Expr::Gt(args!(args))),
        Expr::Not(expr) => Arc::new(Expr::Not(expr!(expr))),
        Expr::Or(args) => Arc::new(Expr::Or(args!(args))),
        Expr::And(args) => Arc::new(Expr::And(args!(args))),
        Expr::InAnswer => Arc::new(Expr::InAnswer),
        Expr::Join(args) => Arc::new(Expr::Join(args!(args))),
        Expr::Random(args) => Arc::new(Expr::Random(args!(args))),
    };

    MaybeOptimized { optimized, val: expr }
}

fn args_inline_constant_trivial_derefs(
    args: &BinaryArgs,
    deref_addr_to_constant_trivial_expr: &BTreeMap<Arc<Expr>, Arc<Expr>>,
) -> MaybeOptimized<BinaryArgs> {
    let mut optimized = false;

    macro_rules! expr {
        ($expr:expr) => {
            track_optimize(
                &mut optimized,
                expr_inline_constant_trivial_derefs($expr, &deref_addr_to_constant_trivial_expr),
            )
        };
    }

    let args = BinaryArgs { left: expr!(&args.left), right: expr!(&args.right) };

    MaybeOptimized { optimized, val: args }
}
