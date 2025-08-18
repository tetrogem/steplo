use std::{
    collections::{BTreeMap, BTreeSet},
    ops::Not,
    sync::Arc,
};

use itertools::chain;
use uuid::Uuid;

use crate::inline::{
    ast::{
        ArgAssignment, BinaryArgs, Call, Command, Expr, Loc, Proc, StackAddr, SubProc, TempVar,
        Value,
    },
    opt::{MaybeOptimized, OptimizationFn, sub_proc::optimize_sub_proc, tracked_exhaust_optimize},
};

pub fn optimize_proc(proc: &Arc<Proc>) -> MaybeOptimized<Arc<Proc>> {
    let mut optimized = false;

    let sub_procs =
        tracked_exhaust_optimize(&mut optimized, proc.sub_procs.as_ref().clone(), |sps| {
            sps.into_iter().map(|sp| optimize_sub_proc(&sp)).collect()
        });

    let mut proc = Arc::new(Proc {
        kind: proc.kind.clone(),
        sub_procs: Arc::new(sub_procs),
        ordered_local_infos: proc.ordered_local_infos.clone(),
        ordered_arg_infos: proc.ordered_arg_infos.clone(),
    });

    const OPTIMIZATIONS: [OptimizationFn<Arc<Proc>>; 2] =
        [optimization_tempify_local_vars, optimization_inline_sp_jumps];

    for optimization in OPTIMIZATIONS {
        proc = tracked_exhaust_optimize(&mut optimized, proc, |proc| optimization(&proc));
    }

    MaybeOptimized { optimized, val: proc }
}

fn optimization_tempify_local_vars(proc: &Arc<Proc>) -> MaybeOptimized<Arc<Proc>> {
    let mut optimized = false;

    let tempifiable_stack_vars = find_tempifiable_local_vars(proc);

    let sub_procs = proc
        .sub_procs
        .iter()
        .map(|sp| {
            let mut tempifiable_local_addr_to_temp = BTreeMap::new();

            macro_rules! expr {
                ($expr:expr) => {
                    Arc::new(expr_replace_locals_with_temps($expr, &tempifiable_local_addr_to_temp))
                };
            }

            let commands = sp
                .commands
                .iter()
                .map(|command| {
                    let command = match command.as_ref() {
                        Command::In => Command::In,
                        Command::ClearStdout => Command::ClearStdout,
                        Command::Out(expr) => Command::Out(expr!(expr)),
                        Command::WriteStdout { index, val } => {
                            Command::WriteStdout { index: expr!(index), val: expr!(val) }
                        },
                        Command::SetLoc { loc, val } => {
                            // compute val using existing temp
                            // (in case local var used itself in its expr, we want...
                            // ...to use the previous temp)
                            let val = expr!(val);

                            // create new temp for new assignment for this local
                            // (since temps are single assignment)
                            let loc = match loc.as_ref() {
                                Loc::Temp(temp) => Loc::Temp(temp.clone()),
                                Loc::Deref(addr) => match addr.as_ref() {
                                    Expr::StackAddr(addr) => match addr.as_ref() {
                                        StackAddr::Arg { uuid } => {
                                            Loc::Deref(Arc::new(Expr::StackAddr(Arc::new(
                                                StackAddr::Arg { uuid: *uuid },
                                            ))))
                                        },
                                        StackAddr::Local { uuid } => {
                                            if tempifiable_stack_vars.contains(uuid) {
                                                let temp = Arc::new(TempVar::new());
                                                tempifiable_local_addr_to_temp
                                                    .insert(*uuid, temp.clone());

                                                optimized = true;

                                                Loc::Temp(temp)
                                            } else {
                                                Loc::Deref(Arc::new(Expr::StackAddr(Arc::new(
                                                    StackAddr::Local { uuid: *uuid },
                                                ))))
                                            }
                                        },
                                    },
                                    addr => Loc::Deref(expr!(addr)),
                                },
                            };

                            Command::SetLoc { loc: Arc::new(loc), val }
                        },
                    };

                    Arc::new(command)
                })
                .collect();

            let call = match sp.call.as_ref() {
                Call::Exit => Call::Exit,
                Call::Jump { to } => Call::Jump { to: expr!(to) },
                Call::Branch { cond, then_to, else_to } => Call::Branch {
                    cond: expr!(cond),
                    then_to: expr!(then_to),
                    else_to: expr!(else_to),
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
                            .map(|aa| ArgAssignment {
                                arg_uuid: aa.arg_uuid,
                                arg_offset: aa.arg_offset,
                                expr: expr!(&aa.expr),
                            })
                            .collect(),
                    ),
                },
            };

            Arc::new(SubProc { uuid: sp.uuid, commands: Arc::new(commands), call: Arc::new(call) })
        })
        .collect();

    let proc = Proc {
        kind: proc.kind.clone(),
        ordered_arg_infos: proc.ordered_arg_infos.clone(),
        ordered_local_infos: proc.ordered_local_infos.clone(),
        sub_procs: Arc::new(sub_procs),
    };

    MaybeOptimized { optimized, val: Arc::new(proc) }
}

fn find_tempifiable_local_vars(proc: &Arc<Proc>) -> BTreeSet<Uuid> {
    let read_before_write_local_vars = find_read_before_write_local_vars(proc);
    let addr_expr_used_local_vars = find_addr_expr_used_local_vars(proc);

    let uninlineable_local_vars =
        BTreeSet::from_iter(chain!(read_before_write_local_vars, addr_expr_used_local_vars));

    proc.ordered_local_infos
        .iter()
        .map(|info| info.uuid)
        .filter(|uuid| uninlineable_local_vars.contains(uuid).not())
        .collect()
}

// local vars who are read from in a sp before being written to in that sp
fn find_read_before_write_local_vars(proc: &Arc<Proc>) -> BTreeSet<Uuid> {
    let mut proc_read_before_write_local_vars = BTreeSet::new();

    for sp in proc.sub_procs.iter() {
        let mut written_local_vars = BTreeSet::<Uuid>::new();

        for command in sp.commands.iter() {
            let (command_written_local_vars, command_read_local_vars) = match command.as_ref() {
                Command::In => (Default::default(), Default::default()),
                Command::ClearStdout => (Default::default(), Default::default()),
                Command::Out(expr) => (Default::default(), expr_find_read_local_vars(expr)),
                Command::WriteStdout { index, val } => (
                    Default::default(),
                    chain!(expr_find_read_local_vars(index), expr_find_read_local_vars(val))
                        .collect(),
                ),
                Command::SetLoc { loc, val } => {
                    let val_read_local_vars = expr_find_read_local_vars(val);
                    let loc_written_and_read_local_vars = loc_find_written_and_read_local_vars(loc);

                    (
                        loc_written_and_read_local_vars.written,
                        chain!(val_read_local_vars, loc_written_and_read_local_vars.read).collect(),
                    )
                },
            };

            let command_read_before_write_local_vars = command_read_local_vars
                .into_iter()
                .filter(|info| written_local_vars.contains(info).not());

            proc_read_before_write_local_vars.extend(command_read_before_write_local_vars);
            written_local_vars.extend(command_written_local_vars);
        }

        let call_read_local_vars = match sp.call.as_ref() {
            Call::Exit => Default::default(),
            Call::Jump { to } => expr_find_read_local_vars(to),
            Call::Branch { cond, then_to, else_to } => chain!(
                expr_find_read_local_vars(cond),
                expr_find_read_local_vars(then_to),
                expr_find_read_local_vars(else_to)
            )
            .collect(),
            Call::Sleep { duration_s, to } => {
                chain!(expr_find_read_local_vars(duration_s), expr_find_read_local_vars(to))
                    .collect()
            },
            Call::Return { to } => expr_find_read_local_vars(to),
            Call::Func { to_func_name: _, arg_assignments } => {
                arg_assignments.iter().flat_map(|aa| expr_find_read_local_vars(&aa.expr)).collect()
            },
        };

        let call_read_before_write_local_vars =
            call_read_local_vars.into_iter().filter(|info| written_local_vars.contains(info).not());

        proc_read_before_write_local_vars.extend(call_read_before_write_local_vars);
    }

    proc_read_before_write_local_vars
}

#[derive(Default)]
struct WrittenAndReadLocalVars {
    written: BTreeSet<Uuid>,
    read: BTreeSet<Uuid>,
}

fn loc_find_written_and_read_local_vars(loc: &Loc) -> WrittenAndReadLocalVars {
    match loc {
        Loc::Temp(_) => Default::default(),
        Loc::Deref(addr) => expr_find_written_and_read_local_vars(addr),
    }
}

fn expr_find_written_and_read_local_vars(expr: &Expr) -> WrittenAndReadLocalVars {
    match expr {
        Expr::Loc(loc) => WrittenAndReadLocalVars {
            written: Default::default(),
            read: loc_find_read_local_vars(loc),
        },
        Expr::StackAddr(addr) => match addr.as_ref() {
            StackAddr::Arg { .. } => Default::default(),
            StackAddr::Local { uuid } => WrittenAndReadLocalVars {
                written: BTreeSet::from([*uuid]),
                read: Default::default(),
            },
        },
        Expr::Value(_) => Default::default(),
        Expr::StdoutDeref(expr) => WrittenAndReadLocalVars {
            written: Default::default(),
            read: expr_find_read_local_vars(expr),
        },
        Expr::StdoutLen => Default::default(),
        Expr::Timer => Default::default(),
        // add and sub can count towards writes for offsets e.g. (stack[local:ab + "2"]) is a write
        // but stack[stack[local:ab]] and stack[local:ab * 2] are not
        Expr::Add(args) => args_find_written_and_read_local_vars(args),
        Expr::Sub(args) => args_find_written_and_read_local_vars(args),
        Expr::Mul(args) => WrittenAndReadLocalVars {
            written: Default::default(),
            read: args_find_read_local_vars(args),
        },
        Expr::Div(args) => WrittenAndReadLocalVars {
            written: Default::default(),
            read: args_find_read_local_vars(args),
        },
        Expr::Mod(args) => WrittenAndReadLocalVars {
            written: Default::default(),
            read: args_find_read_local_vars(args),
        },
        Expr::Eq(args) => WrittenAndReadLocalVars {
            written: Default::default(),
            read: args_find_read_local_vars(args),
        },
        Expr::Lt(args) => WrittenAndReadLocalVars {
            written: Default::default(),
            read: args_find_read_local_vars(args),
        },
        Expr::Gt(args) => WrittenAndReadLocalVars {
            written: Default::default(),
            read: args_find_read_local_vars(args),
        },
        Expr::Not(expr) => WrittenAndReadLocalVars {
            written: Default::default(),
            read: expr_find_read_local_vars(expr),
        },
        Expr::Or(args) => WrittenAndReadLocalVars {
            written: Default::default(),
            read: args_find_read_local_vars(args),
        },
        Expr::And(args) => WrittenAndReadLocalVars {
            written: Default::default(),
            read: args_find_read_local_vars(args),
        },
        Expr::InAnswer => Default::default(),
        Expr::Join(args) => WrittenAndReadLocalVars {
            written: Default::default(),
            read: args_find_read_local_vars(args),
        },
        Expr::Random(args) => WrittenAndReadLocalVars {
            written: Default::default(),
            read: args_find_read_local_vars(args),
        },
    }
}

fn args_find_written_and_read_local_vars(args: &BinaryArgs) -> WrittenAndReadLocalVars {
    let left_written_and_read = expr_find_written_and_read_local_vars(&args.left);
    let right_written_and_read = expr_find_written_and_read_local_vars(&args.right);

    WrittenAndReadLocalVars {
        written: chain!(left_written_and_read.written, right_written_and_read.written).collect(),
        read: chain!(left_written_and_read.read, right_written_and_read.read).collect(),
    }
}

fn expr_find_read_local_vars(expr: &Expr) -> BTreeSet<Uuid> {
    match expr {
        Expr::Loc(loc) => loc_find_read_local_vars(loc),
        Expr::StackAddr(_) => Default::default(),
        Expr::Value(_) => Default::default(),
        Expr::StdoutDeref(expr) => expr_find_read_local_vars(expr),
        Expr::StdoutLen => Default::default(),
        Expr::Timer => Default::default(),
        Expr::Add(args) => args_find_read_local_vars(args),
        Expr::Sub(args) => args_find_read_local_vars(args),
        Expr::Mul(args) => args_find_read_local_vars(args),
        Expr::Div(args) => args_find_read_local_vars(args),
        Expr::Mod(args) => args_find_read_local_vars(args),
        Expr::Eq(args) => args_find_read_local_vars(args),
        Expr::Lt(args) => args_find_read_local_vars(args),
        Expr::Gt(args) => args_find_read_local_vars(args),
        Expr::Not(expr) => expr_find_read_local_vars(expr),
        Expr::Or(args) => args_find_read_local_vars(args),
        Expr::And(args) => args_find_read_local_vars(args),
        Expr::InAnswer => Default::default(),
        Expr::Join(args) => args_find_read_local_vars(args),
        Expr::Random(args) => args_find_read_local_vars(args),
    }
}

fn loc_find_read_local_vars(loc: &Loc) -> BTreeSet<Uuid> {
    match loc {
        Loc::Temp(_) => Default::default(),
        Loc::Deref(addr) => match addr.as_ref() {
            Expr::StackAddr(addr) => match addr.as_ref() {
                StackAddr::Arg { .. } => Default::default(),
                StackAddr::Local { uuid } => BTreeSet::from([*uuid]),
            },
            addr => expr_find_read_local_vars(addr),
        },
    }
}

fn args_find_read_local_vars(args: &BinaryArgs) -> BTreeSet<Uuid> {
    chain!(expr_find_read_local_vars(&args.left), expr_find_read_local_vars(&args.right)).collect()
}

// local vars who's addresses are used in expressions other than instantly dereffing
fn find_addr_expr_used_local_vars(proc: &Arc<Proc>) -> BTreeSet<Uuid> {
    let mut addr_expr_used_local_vars = BTreeSet::new();

    for sp in proc.sub_procs.iter() {
        for command in sp.commands.iter() {
            addr_expr_used_local_vars.extend(command_find_addr_expr_used_local_vars(command));
        }

        match sp.call.as_ref() {
            Call::Exit => {},
            Call::Jump { to } => {
                addr_expr_used_local_vars.extend(expr_find_addr_expr_used_local_vars(to))
            },
            Call::Branch { cond, then_to, else_to } => {
                addr_expr_used_local_vars.extend(expr_find_addr_expr_used_local_vars(cond));
                addr_expr_used_local_vars.extend(expr_find_addr_expr_used_local_vars(then_to));
                addr_expr_used_local_vars.extend(expr_find_addr_expr_used_local_vars(else_to));
            },
            Call::Sleep { duration_s, to } => {
                addr_expr_used_local_vars.extend(expr_find_addr_expr_used_local_vars(duration_s));
                addr_expr_used_local_vars.extend(expr_find_addr_expr_used_local_vars(to));
            },
            Call::Return { to } => {
                addr_expr_used_local_vars.extend(expr_find_addr_expr_used_local_vars(to));
            },
            Call::Func { to_func_name: _, arg_assignments } => {
                for aa in arg_assignments.iter() {
                    addr_expr_used_local_vars.extend(expr_find_addr_expr_used_local_vars(&aa.expr));
                }
            },
        }
    }

    addr_expr_used_local_vars
}

fn command_find_addr_expr_used_local_vars(command: &Command) -> BTreeSet<Uuid> {
    match command {
        Command::In => Default::default(),
        Command::ClearStdout => Default::default(),
        Command::Out(expr) => expr_find_addr_expr_used_local_vars(expr),
        Command::WriteStdout { index, val } => chain!(
            expr_find_addr_expr_used_local_vars(index),
            expr_find_addr_expr_used_local_vars(val)
        )
        .collect(),
        Command::SetLoc { loc, val } => chain!(
            loc_find_addr_expr_used_local_vars(loc),
            expr_find_addr_expr_used_local_vars(val)
        )
        .collect(),
    }
}

fn expr_find_addr_expr_used_local_vars(expr: &Expr) -> BTreeSet<Uuid> {
    match expr {
        Expr::Loc(loc) => loc_find_addr_expr_used_local_vars(loc),
        Expr::StackAddr(addr) => match addr.as_ref() {
            StackAddr::Arg { .. } => Default::default(),
            StackAddr::Local { uuid } => BTreeSet::from([*uuid]),
        },
        Expr::Value(_) => Default::default(),
        Expr::StdoutDeref(expr) => expr_find_addr_expr_used_local_vars(expr),
        Expr::StdoutLen => Default::default(),
        Expr::Timer => Default::default(),
        Expr::Add(args) => args_find_addr_expr_used_local_vars(args),
        Expr::Sub(args) => args_find_addr_expr_used_local_vars(args),
        Expr::Mul(args) => args_find_addr_expr_used_local_vars(args),
        Expr::Div(args) => args_find_addr_expr_used_local_vars(args),
        Expr::Mod(args) => args_find_addr_expr_used_local_vars(args),
        Expr::Eq(args) => args_find_addr_expr_used_local_vars(args),
        Expr::Lt(args) => args_find_addr_expr_used_local_vars(args),
        Expr::Gt(args) => args_find_addr_expr_used_local_vars(args),
        Expr::Not(expr) => expr_find_addr_expr_used_local_vars(expr),
        Expr::Or(args) => args_find_addr_expr_used_local_vars(args),
        Expr::And(args) => args_find_addr_expr_used_local_vars(args),
        Expr::InAnswer => Default::default(),
        Expr::Join(args) => args_find_addr_expr_used_local_vars(args),
        Expr::Random(args) => args_find_addr_expr_used_local_vars(args),
    }
}

fn args_find_addr_expr_used_local_vars(args: &BinaryArgs) -> BTreeSet<Uuid> {
    chain!(
        expr_find_addr_expr_used_local_vars(&args.left),
        expr_find_addr_expr_used_local_vars(&args.right)
    )
    .collect()
}

fn loc_find_addr_expr_used_local_vars(loc: &Loc) -> BTreeSet<Uuid> {
    match loc {
        Loc::Temp(_) => Default::default(),
        Loc::Deref(addr) => {
            match addr.as_ref() {
                // don't count immediately dereffed stack addrs
                Expr::StackAddr(_) => Default::default(),
                addr => expr_find_addr_expr_used_local_vars(addr),
            }
        },
    }
}

fn expr_replace_locals_with_temps(
    expr: &Expr,
    local_var_uuid_to_temp: &BTreeMap<Uuid, Arc<TempVar>>,
) -> Expr {
    match expr {
        Expr::Loc(loc) => {
            Expr::Loc(Arc::new(loc_replace_locals_with_temps(loc, local_var_uuid_to_temp)))
        },
        Expr::StackAddr(addr) => {
            if let StackAddr::Local { uuid } = addr.as_ref()
                && local_var_uuid_to_temp.get(uuid).is_some()
            {
                panic!("tried to inline local var which has its addr used in an expr");
            }

            Expr::StackAddr(addr.clone())
        },
        Expr::Value(value) => Expr::Value(value.clone()),
        Expr::StdoutDeref(expr) => Expr::StdoutDeref(Arc::new(expr_replace_locals_with_temps(
            expr,
            local_var_uuid_to_temp,
        ))),
        Expr::StdoutLen => Expr::StdoutLen,
        Expr::Timer => Expr::Timer,
        Expr::Add(args) => {
            Expr::Add(Arc::new(args_replace_locals_with_temps(args, local_var_uuid_to_temp)))
        },
        Expr::Sub(args) => {
            Expr::Sub(Arc::new(args_replace_locals_with_temps(args, local_var_uuid_to_temp)))
        },
        Expr::Mul(args) => {
            Expr::Mul(Arc::new(args_replace_locals_with_temps(args, local_var_uuid_to_temp)))
        },
        Expr::Div(args) => {
            Expr::Div(Arc::new(args_replace_locals_with_temps(args, local_var_uuid_to_temp)))
        },
        Expr::Mod(args) => {
            Expr::Mod(Arc::new(args_replace_locals_with_temps(args, local_var_uuid_to_temp)))
        },
        Expr::Eq(args) => {
            Expr::Eq(Arc::new(args_replace_locals_with_temps(args, local_var_uuid_to_temp)))
        },
        Expr::Lt(args) => {
            Expr::Lt(Arc::new(args_replace_locals_with_temps(args, local_var_uuid_to_temp)))
        },
        Expr::Gt(args) => {
            Expr::Gt(Arc::new(args_replace_locals_with_temps(args, local_var_uuid_to_temp)))
        },
        Expr::Not(expr) => {
            Expr::Not(Arc::new(expr_replace_locals_with_temps(expr, local_var_uuid_to_temp)))
        },
        Expr::Or(args) => {
            Expr::Or(Arc::new(args_replace_locals_with_temps(args, local_var_uuid_to_temp)))
        },
        Expr::And(args) => {
            Expr::And(Arc::new(args_replace_locals_with_temps(args, local_var_uuid_to_temp)))
        },
        Expr::InAnswer => Expr::InAnswer,
        Expr::Join(args) => {
            Expr::Join(Arc::new(args_replace_locals_with_temps(args, local_var_uuid_to_temp)))
        },
        Expr::Random(args) => {
            Expr::Random(Arc::new(args_replace_locals_with_temps(args, local_var_uuid_to_temp)))
        },
    }
}

fn args_replace_locals_with_temps(
    args: &BinaryArgs,
    local_var_uuid_to_temp: &BTreeMap<Uuid, Arc<TempVar>>,
) -> BinaryArgs {
    BinaryArgs {
        left: Arc::new(expr_replace_locals_with_temps(&args.left, local_var_uuid_to_temp)),
        right: Arc::new(expr_replace_locals_with_temps(&args.right, local_var_uuid_to_temp)),
    }
}

fn loc_replace_locals_with_temps(
    loc: &Loc,
    local_var_uuid_to_temp: &BTreeMap<Uuid, Arc<TempVar>>,
) -> Loc {
    match loc {
        Loc::Temp(temp) => Loc::Temp(temp.clone()),
        Loc::Deref(addr) => match addr.as_ref() {
            Expr::StackAddr(addr) => match addr.as_ref() {
                StackAddr::Arg { uuid } => {
                    Loc::Deref(Arc::new(Expr::StackAddr(Arc::new(StackAddr::Arg { uuid: *uuid }))))
                },
                StackAddr::Local { uuid } => match local_var_uuid_to_temp.get(uuid) {
                    Some(temp) => Loc::Temp(temp.clone()),
                    None => Loc::Deref(Arc::new(Expr::StackAddr(Arc::new(StackAddr::Local {
                        uuid: *uuid,
                    })))),
                },
            },
            addr => {
                Loc::Deref(Arc::new(expr_replace_locals_with_temps(addr, local_var_uuid_to_temp)))
            },
        },
    }
}

fn optimization_inline_sp_jumps(proc: &Arc<Proc>) -> MaybeOptimized<Arc<Proc>> {
    let mut optimized = false;

    let sp_uuid_to_sp =
        proc.sub_procs.iter().map(|sp| (sp.uuid, sp.clone())).collect::<BTreeMap<_, _>>();

    let sub_procs = proc
        .sub_procs
        .iter()
        .map(|sp| {
            let (commands, call) = if let Call::Jump { to } = sp.call.as_ref()
                && let Expr::Value(to_value) = to.as_ref()
                && let Value::Label(to_label) = to_value.as_ref()
                && let Some(to_sp) = sp_uuid_to_sp.get(to_label)
                // sub procs can't inline themselves -- avoid infinite loops
                && sp_can_jump_to_itself(to_sp).not()
            {
                // dbg!(to_sp.uuid, &to_sp.call, sp.uuid, &sp.call);
                optimized = true;

                let commands =
                    chain!(sp.commands.iter().cloned(), to_sp.commands.iter().cloned()).collect();

                (Arc::new(commands), to_sp.call.clone())
            } else {
                (sp.commands.clone(), sp.call.clone())
            };

            Arc::new(SubProc { uuid: sp.uuid, commands, call })
        })
        .collect();

    let proc = Proc {
        kind: proc.kind.clone(),
        sub_procs: Arc::new(sub_procs),
        ordered_arg_infos: proc.ordered_arg_infos.clone(),
        ordered_local_infos: proc.ordered_local_infos.clone(),
    };

    MaybeOptimized { optimized, val: Arc::new(proc) }
}

fn sp_can_jump_to_itself(sp: &SubProc) -> bool {
    let always_jump_labels = match sp.call.as_ref() {
        Call::Exit => Default::default(),
        Call::Jump { to } => Vec::from([to]),
        Call::Branch { cond: _, then_to, else_to } => Vec::from([then_to, else_to]),
        Call::Sleep { duration_s: _, to } => Vec::from([to]),
        Call::Return { to } => Vec::from([to]),
        Call::Func { .. } => Default::default(),
    };

    for label in always_jump_labels {
        let Expr::Value(label) = label.as_ref() else { continue };
        let Value::Label(label) = label.as_ref() else { continue };
        if *label == sp.uuid {
            return true;
        }
    }

    false
}
