use std::{
    collections::{BTreeMap, BTreeSet},
    ops::Not,
    sync::Arc,
};

use itertools::{Itertools, chain};
use uuid::Uuid;

use crate::inline::{
    ast::{
        ArgAssignment, BinaryArgs, Call, Command, Expr, Loc, Proc, ProcKind, StackAddr, SubProc,
        Value, VarInfo,
    },
    opt::{MaybeOptimized, OptimizationFn, proc::optimize_proc, tracked_exhaust_optimize},
};

#[expect(clippy::ptr_arg)]
pub fn optimize_procs(procs: &Vec<Arc<Proc>>) -> MaybeOptimized<Vec<Arc<Proc>>> {
    let mut optimized = false;

    let procs = procs.clone();
    let mut procs = tracked_exhaust_optimize(&mut optimized, procs, |procs| {
        procs.into_iter().map(|proc| optimize_proc(&proc)).collect()
    });

    const OPTIMIZATIONS: [OptimizationFn<Vec<Arc<Proc>>>; 2] =
        [optimization_inline_func_calls, optimization_remove_unused_stack_addrs];

    for optimization in OPTIMIZATIONS {
        procs = tracked_exhaust_optimize(&mut optimized, procs, |procs| optimization(&procs));
    }

    MaybeOptimized { optimized, val: procs }
}

#[expect(clippy::ptr_arg)]
fn optimization_inline_func_calls(procs: &Vec<Arc<Proc>>) -> MaybeOptimized<Vec<Arc<Proc>>> {
    let mut optimized = false;

    let inlineable_func_name_to_sp = find_inlineable_funcs(procs);

    let procs = procs
        .iter()
        .map(|proc| {
            let maybe_optimized_proc = proc_inline_funcs(proc, &inlineable_func_name_to_sp);
            optimized = optimized || maybe_optimized_proc.optimized;
            maybe_optimized_proc.val
        })
        .map(Arc::new)
        .collect();

    MaybeOptimized { optimized, val: procs }
}

fn proc_inline_funcs(
    proc: &Proc,
    inlineable_func_name_to_sp: &BTreeMap<Arc<str>, Arc<Proc>>,
) -> MaybeOptimized<Proc> {
    let mut optimized = false;

    let mut ordered_local_infos = proc.ordered_local_infos.as_ref().clone();

    let sub_procs = proc
        .sub_procs
        .iter()
        .flat_map(|sp| {
            // see if this sp can inline its function call
            let Call::Func { to_func_name, arg_assignments } = sp.call.as_ref() else {
                return Vec::from([sp.clone()]);
            };

            let Some(inlineable_func_proc) = inlineable_func_name_to_sp.get(to_func_name) else {
                return Vec::from([sp.clone()]);
            };

            // add all of the inlined function's stack to this proc's stack
            let func_arg_to_inlined = inlineable_func_proc
                .ordered_arg_infos
                .iter()
                .map(|info| (info.uuid, VarInfo { uuid: Uuid::new_v4(), size: info.size }))
                .collect::<BTreeMap<_, _>>();

            let func_local_to_inlined = inlineable_func_proc
                .ordered_local_infos
                .iter()
                .map(|info| (info.uuid, VarInfo { uuid: Uuid::new_v4(), size: info.size }))
                .collect::<BTreeMap<_, _>>();

            ordered_local_infos
                .extend(chain!(func_arg_to_inlined.values(), func_local_to_inlined.values()));

            let mut commands = sp.commands.as_ref().clone();

            // update argument assignments to be local stack var assignments
            for aa in arg_assignments.iter() {
                let inlined_addr = Expr::StackAddr(Arc::new(StackAddr::Local {
                    uuid: func_arg_to_inlined
                        .get(&aa.arg_uuid)
                        .expect("Couldn't find inlined func arg")
                        .uuid,
                }));

                let loc = Loc::Deref(Arc::new(Expr::Add(Arc::new(BinaryArgs {
                    left: Arc::new(inlined_addr),
                    right: Arc::new(Expr::Value(Arc::new(Value::Literal(
                        aa.arg_offset.to_string().into(),
                    )))),
                }))));

                commands
                    .push(Arc::new(Command::SetLoc { loc: Arc::new(loc), val: aa.expr.clone() }));
            }

            let func_label_to_inlined = inlineable_func_proc
                .sub_procs
                .iter()
                .map(|sp| (sp.uuid, Uuid::new_v4()))
                .collect::<BTreeMap<_, _>>();

            let inlined_func_sps = inlineable_func_proc
                .sub_procs
                .iter()
                .map(|sp| {
                    let commands = sp
                        .commands
                        .iter()
                        .map(|command| {
                            Arc::new(inline_command(
                                command,
                                &func_arg_to_inlined,
                                &func_local_to_inlined,
                                &func_label_to_inlined,
                            ))
                        })
                        .collect();

                    let call = match sp.call.as_ref() {
                        Call::Exit => Call::Exit,
                        Call::Return { to } => Call::Jump {
                            to: Arc::new(inline_expr(
                                to,
                                &func_arg_to_inlined,
                                &func_local_to_inlined,
                                &func_label_to_inlined,
                            )),
                        },
                        Call::Jump { to } => Call::Jump {
                            to: Arc::new(inline_expr(
                                to,
                                &func_arg_to_inlined,
                                &func_local_to_inlined,
                                &func_label_to_inlined,
                            )),
                        },
                        Call::Branch { cond, then_to, else_to } => Call::Branch {
                            cond: Arc::new(inline_expr(
                                cond,
                                &func_arg_to_inlined,
                                &func_local_to_inlined,
                                &func_label_to_inlined,
                            )),
                            then_to: Arc::new(inline_expr(
                                then_to,
                                &func_arg_to_inlined,
                                &func_local_to_inlined,
                                &func_label_to_inlined,
                            )),
                            else_to: Arc::new(inline_expr(
                                else_to,
                                &func_arg_to_inlined,
                                &func_local_to_inlined,
                                &func_label_to_inlined,
                            )),
                        },
                        Call::Func { to_func_name, arg_assignments } => {
                            let arg_assignments = arg_assignments
                                .iter()
                                .map(|aa| ArgAssignment {
                                    arg_uuid: aa.arg_uuid,
                                    arg_offset: aa.arg_offset,
                                    expr: Arc::new(inline_expr(
                                        &aa.expr,
                                        &func_arg_to_inlined,
                                        &func_local_to_inlined,
                                        &func_label_to_inlined,
                                    )),
                                })
                                .collect();

                            Call::Func {
                                to_func_name: to_func_name.clone(),
                                arg_assignments: Arc::new(arg_assignments),
                            }
                        },
                    };

                    SubProc {
                        uuid: *func_label_to_inlined
                            .get(&sp.uuid)
                            .expect("inlined sub proc was not assigned a new uuid"),
                        commands: Arc::new(commands),
                        call: Arc::new(call),
                    }
                })
                .map(Arc::new)
                .collect_vec();

            // call first sp of the inlined func
            let first_inlined_sp = inlined_func_sps.first().expect("inlined func has no sub procs");

            let call = Call::Jump {
                to: Arc::new(Expr::Value(Arc::new(Value::Label(first_inlined_sp.uuid)))),
            };

            optimized = true;

            chain!(
                [Arc::new(SubProc {
                    uuid: sp.uuid,
                    commands: Arc::new(commands),
                    call: Arc::new(call)
                }),],
                inlined_func_sps
            )
            .collect_vec()
        })
        .collect();

    let proc = Proc {
        kind: proc.kind.clone(),
        ordered_arg_infos: proc.ordered_arg_infos.clone(),
        ordered_local_infos: Arc::new(ordered_local_infos),
        sub_procs: Arc::new(sub_procs),
    };

    MaybeOptimized { optimized, val: proc }
}

fn find_inlineable_funcs(procs: &[Arc<Proc>]) -> BTreeMap<Arc<str>, Arc<Proc>> {
    // any function that does not call another function within it can currently be inlined

    let func_name_to_proc = procs
        .iter()
        .filter_map(|proc| match proc.kind.as_ref() {
            ProcKind::Main => None,
            ProcKind::Func { name } => Some((name.clone(), proc.clone())),
        })
        .collect::<BTreeMap<_, _>>();

    func_name_to_proc
        .iter()
        .filter(|(_, proc)| {
            proc.sub_procs
                .iter()
                .map(|sp| &sp.call)
                .any(|call| matches!(call.as_ref(), Call::Func { .. }))
                .not()
        })
        .map(|(name, proc)| (name.clone(), proc.clone()))
        .collect()
}

fn command_replace_stack_addrs(
    command: &Command,
    inlined_arg_to_local: &BTreeMap<Uuid, VarInfo>,
    inlined_local_to_local: &BTreeMap<Uuid, VarInfo>,
) -> Command {
    match command {
        Command::ClearStdout => Command::ClearStdout,
        Command::In => Command::In,
        Command::Out(expr) => Command::Out(Arc::new(expr_replace_stack_addrs(
            expr,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
        Command::Wait { duration_s } => Command::Wait {
            duration_s: Arc::new(expr_replace_stack_addrs(
                duration_s,
                inlined_arg_to_local,
                inlined_local_to_local,
            )),
        },
        Command::WriteStdout { index, val } => Command::WriteStdout {
            index: Arc::new(expr_replace_stack_addrs(
                index,
                inlined_arg_to_local,
                inlined_local_to_local,
            )),
            val: Arc::new(expr_replace_stack_addrs(
                val,
                inlined_arg_to_local,
                inlined_local_to_local,
            )),
        },
        Command::SetLoc { loc, val } => Command::SetLoc {
            loc: Arc::new(loc_replace_stack_addrs(
                loc,
                inlined_arg_to_local,
                inlined_local_to_local,
            )),
            val: Arc::new(expr_replace_stack_addrs(
                val,
                inlined_arg_to_local,
                inlined_local_to_local,
            )),
        },
    }
}

fn expr_replace_stack_addrs(
    expr: &Expr,
    inlined_arg_to_local: &BTreeMap<Uuid, VarInfo>,
    inlined_local_to_local: &BTreeMap<Uuid, VarInfo>,
) -> Expr {
    match expr {
        Expr::Loc(loc) => Expr::Loc(Arc::new(loc_replace_stack_addrs(
            loc,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
        Expr::StackAddr(addr) => {
            let uuid = match addr.as_ref() {
                StackAddr::Arg { uuid } => {
                    inlined_arg_to_local.get(uuid).map(|info| info.uuid).unwrap_or(*uuid)
                },
                StackAddr::Local { uuid } => {
                    inlined_local_to_local.get(uuid).map(|info| info.uuid).unwrap_or(*uuid)
                },
            };

            Expr::StackAddr(Arc::new(StackAddr::Local { uuid }))
        },
        Expr::Value(value) => Expr::Value(value.clone()),
        Expr::StdoutDeref(expr) => Expr::StdoutDeref(Arc::new(expr_replace_stack_addrs(
            expr,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
        Expr::StdoutLen => Expr::StdoutLen,
        Expr::Timer => Expr::Timer,
        Expr::Add(args) => Expr::Add(Arc::new(args_replace_stack_addrs(
            args,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
        Expr::Sub(args) => Expr::Sub(Arc::new(args_replace_stack_addrs(
            args,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
        Expr::Mul(args) => Expr::Mul(Arc::new(args_replace_stack_addrs(
            args,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
        Expr::Div(args) => Expr::Div(Arc::new(args_replace_stack_addrs(
            args,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
        Expr::Mod(args) => Expr::Mod(Arc::new(args_replace_stack_addrs(
            args,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
        Expr::Eq(args) => Expr::Eq(Arc::new(args_replace_stack_addrs(
            args,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
        Expr::Lt(args) => Expr::Lt(Arc::new(args_replace_stack_addrs(
            args,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
        Expr::Gt(args) => Expr::Gt(Arc::new(args_replace_stack_addrs(
            args,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
        Expr::Not(expr) => Expr::Not(Arc::new(expr_replace_stack_addrs(
            expr,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
        Expr::Or(args) => Expr::Or(Arc::new(args_replace_stack_addrs(
            args,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
        Expr::And(args) => Expr::And(Arc::new(args_replace_stack_addrs(
            args,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
        Expr::InAnswer => Expr::InAnswer,
        Expr::Join(args) => Expr::Join(Arc::new(args_replace_stack_addrs(
            args,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
        Expr::Random(args) => Expr::Random(Arc::new(args_replace_stack_addrs(
            args,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
    }
}

fn loc_replace_stack_addrs(
    loc: &Loc,
    inlined_arg_to_local: &BTreeMap<Uuid, VarInfo>,
    inlined_local_to_local: &BTreeMap<Uuid, VarInfo>,
) -> Loc {
    match loc {
        Loc::Temp(temp) => Loc::Temp(temp.clone()),
        Loc::Deref(addr) => Loc::Deref(Arc::new(expr_replace_stack_addrs(
            addr,
            inlined_arg_to_local,
            inlined_local_to_local,
        ))),
    }
}

fn args_replace_stack_addrs(
    args: &BinaryArgs,
    inlined_arg_to_local: &BTreeMap<Uuid, VarInfo>,
    inlined_local_to_local: &BTreeMap<Uuid, VarInfo>,
) -> BinaryArgs {
    BinaryArgs {
        left: Arc::new(expr_replace_stack_addrs(
            &args.left,
            inlined_arg_to_local,
            inlined_local_to_local,
        )),
        right: Arc::new(expr_replace_stack_addrs(
            &args.right,
            inlined_arg_to_local,
            inlined_local_to_local,
        )),
    }
}

fn command_replace_labels(
    command: &Command,
    func_label_to_inlined: &BTreeMap<Uuid, Uuid>,
) -> Command {
    match command {
        Command::In => Command::In,
        Command::ClearStdout => Command::ClearStdout,
        Command::Out(expr) => {
            Command::Out(Arc::new(expr_replace_labels(expr, func_label_to_inlined)))
        },
        Command::Wait { duration_s } => Command::Wait {
            duration_s: Arc::new(expr_replace_labels(duration_s, func_label_to_inlined)),
        },
        Command::WriteStdout { index, val } => Command::WriteStdout {
            index: Arc::new(expr_replace_labels(index, func_label_to_inlined)),
            val: Arc::new(expr_replace_labels(val, func_label_to_inlined)),
        },
        Command::SetLoc { loc, val } => Command::SetLoc {
            loc: Arc::new(loc_replace_labels(loc, func_label_to_inlined)),
            val: Arc::new(expr_replace_labels(val, func_label_to_inlined)),
        },
    }
}

fn expr_replace_labels(expr: &Expr, func_label_to_inlined: &BTreeMap<Uuid, Uuid>) -> Expr {
    match expr {
        Expr::Loc(loc) => Expr::Loc(Arc::new(loc_replace_labels(loc, func_label_to_inlined))),
        Expr::StackAddr(addr) => Expr::StackAddr(addr.clone()),
        Expr::Value(value) => {
            let value = match value.as_ref() {
                Value::Literal(lit) => Value::Literal(lit.clone()),
                Value::Label(label) => {
                    Value::Label(*func_label_to_inlined.get(label).unwrap_or(label))
                },
            };

            Expr::Value(Arc::new(value))
        },
        Expr::StdoutDeref(expr) => {
            Expr::StdoutDeref(Arc::new(expr_replace_labels(expr, func_label_to_inlined)))
        },
        Expr::StdoutLen => Expr::StdoutLen,
        Expr::Timer => Expr::Timer,
        Expr::Add(args) => Expr::Add(Arc::new(args_replace_labels(args, func_label_to_inlined))),
        Expr::Sub(args) => Expr::Sub(Arc::new(args_replace_labels(args, func_label_to_inlined))),
        Expr::Mul(args) => Expr::Mul(Arc::new(args_replace_labels(args, func_label_to_inlined))),
        Expr::Div(args) => Expr::Div(Arc::new(args_replace_labels(args, func_label_to_inlined))),
        Expr::Mod(args) => Expr::Mod(Arc::new(args_replace_labels(args, func_label_to_inlined))),
        Expr::Eq(args) => Expr::Eq(Arc::new(args_replace_labels(args, func_label_to_inlined))),
        Expr::Lt(args) => Expr::Lt(Arc::new(args_replace_labels(args, func_label_to_inlined))),
        Expr::Gt(args) => Expr::Gt(Arc::new(args_replace_labels(args, func_label_to_inlined))),
        Expr::Not(expr) => Expr::Not(Arc::new(expr_replace_labels(expr, func_label_to_inlined))),
        Expr::Or(args) => Expr::Or(Arc::new(args_replace_labels(args, func_label_to_inlined))),
        Expr::And(args) => Expr::And(Arc::new(args_replace_labels(args, func_label_to_inlined))),
        Expr::InAnswer => Expr::InAnswer,
        Expr::Join(args) => Expr::Join(Arc::new(args_replace_labels(args, func_label_to_inlined))),
        Expr::Random(args) => {
            Expr::Random(Arc::new(args_replace_labels(args, func_label_to_inlined)))
        },
    }
}

fn args_replace_labels(
    args: &BinaryArgs,
    func_label_to_inlined: &BTreeMap<Uuid, Uuid>,
) -> BinaryArgs {
    BinaryArgs {
        left: Arc::new(expr_replace_labels(&args.left, func_label_to_inlined)),
        right: Arc::new(expr_replace_labels(&args.right, func_label_to_inlined)),
    }
}

fn loc_replace_labels(loc: &Loc, func_label_to_inlined: &BTreeMap<Uuid, Uuid>) -> Loc {
    match loc {
        Loc::Temp(temp) => Loc::Temp(temp.clone()),
        Loc::Deref(addr) => Loc::Deref(Arc::new(expr_replace_labels(addr, func_label_to_inlined))),
    }
}

fn inline_command(
    command: &Command,
    func_arg_to_inlined: &BTreeMap<Uuid, VarInfo>,
    func_local_to_inlined: &BTreeMap<Uuid, VarInfo>,
    func_label_to_inlined: &BTreeMap<Uuid, Uuid>,
) -> Command {
    let command = command_replace_stack_addrs(command, func_arg_to_inlined, func_local_to_inlined);
    command_replace_labels(&command, func_label_to_inlined)
}

fn inline_expr(
    expr: &Expr,
    func_arg_to_inlined: &BTreeMap<Uuid, VarInfo>,
    func_local_to_inlined: &BTreeMap<Uuid, VarInfo>,
    func_label_to_inlined: &BTreeMap<Uuid, Uuid>,
) -> Expr {
    let expr = expr_replace_stack_addrs(expr, func_arg_to_inlined, func_local_to_inlined);
    expr_replace_labels(&expr, func_label_to_inlined)
}

fn optimization_remove_unused_stack_addrs(
    procs: &Vec<Arc<Proc>>,
) -> MaybeOptimized<Vec<Arc<Proc>>> {
    let mut optimized = false;

    let mut optimized_procs = Vec::new();
    let mut used_stack_addrs = BTreeSet::new();

    for proc in procs {
        let res = proc_remove_unused_stack_addrs(proc);

        used_stack_addrs.extend(res.used_stack_addrs);

        optimized_procs.push(res.maybe_optimized_proc.val);
        optimized = optimized || res.maybe_optimized_proc.optimized;
    }

    let optimized_procs = optimized_procs
        .into_iter()
        .map(|proc| {
            let sub_procs = proc
                .sub_procs
                .iter()
                .map(|sp| {
                    let commands = sp
                        .commands
                        .iter()
                        .filter(|command| {
                            if let Command::SetLoc { loc, .. } = command.as_ref()
                                && let Loc::Deref(addr) = loc.as_ref()
                                && let Expr::StackAddr(addr) = addr.as_ref()
                                && used_stack_addrs.contains(addr).not()
                            {
                                false
                            } else {
                                true
                            }
                        })
                        .cloned()
                        .collect();

                    Arc::new(SubProc {
                        uuid: sp.uuid,
                        commands: Arc::new(commands),
                        call: sp.call.clone(),
                    })
                })
                .collect();

            Arc::new(Proc {
                kind: proc.kind.clone(),
                ordered_arg_infos: proc.ordered_arg_infos.clone(),
                ordered_local_infos: proc.ordered_local_infos.clone(),
                sub_procs: Arc::new(sub_procs),
            })
        })
        .collect();

    MaybeOptimized { optimized, val: optimized_procs }
}

struct RemoveUnusedStackAddrsRes {
    maybe_optimized_proc: MaybeOptimized<Proc>,
    used_stack_addrs: BTreeSet<StackAddr>,
}

fn proc_remove_unused_stack_addrs(proc: &Proc) -> RemoveUnusedStackAddrsRes {
    let mut optimized = false;

    let mut used_stack_addrs = BTreeSet::<StackAddr>::new();

    for sp in proc.sub_procs.iter() {
        for command in sp.commands.iter() {
            used_stack_addrs.extend(command_find_used_stack_addrs(command));
        }

        used_stack_addrs.extend(call_find_used_stack_addrs(&sp.call));
    }

    let filtered_args = proc
        .ordered_arg_infos
        .iter()
        .filter(|info| used_stack_addrs.contains(&StackAddr::Arg { uuid: info.uuid }))
        .copied()
        .collect_vec();

    let filtered_locals = proc
        .ordered_local_infos
        .iter()
        .filter(|info| used_stack_addrs.contains(&StackAddr::Local { uuid: info.uuid }))
        .copied()
        .collect_vec();

    optimized = optimized
        || filtered_args.len() != proc.ordered_arg_infos.len()
        || filtered_locals.len() != proc.ordered_local_infos.len();

    let proc = Proc {
        kind: proc.kind.clone(),
        sub_procs: proc.sub_procs.clone(),
        ordered_arg_infos: Arc::new(filtered_args),
        ordered_local_infos: Arc::new(filtered_locals),
    };

    RemoveUnusedStackAddrsRes {
        maybe_optimized_proc: MaybeOptimized { optimized, val: proc },
        used_stack_addrs,
    }
}

fn command_find_used_stack_addrs(command: &Command) -> BTreeSet<StackAddr> {
    match command {
        Command::In => Default::default(),
        Command::ClearStdout => Default::default(),
        Command::Out(expr) => expr_find_used_stack_addrs(expr),
        Command::Wait { duration_s } => expr_find_used_stack_addrs(duration_s),
        Command::WriteStdout { index, val } => {
            chain!(expr_find_used_stack_addrs(index), expr_find_used_stack_addrs(val)).collect()
        },
        Command::SetLoc { loc, val } => {
            chain!(loc_find_used_stack_addrs(loc), expr_find_used_stack_addrs(val)).collect()
        },
    }
}

fn call_find_used_stack_addrs(call: &Call) -> BTreeSet<StackAddr> {
    match call {
        Call::Exit => Default::default(),
        Call::Jump { to } => expr_find_used_stack_addrs(to),
        Call::Branch { cond, then_to, else_to } => chain!(
            expr_find_used_stack_addrs(cond),
            expr_find_used_stack_addrs(then_to),
            expr_find_used_stack_addrs(else_to)
        )
        .collect(),
        Call::Return { to } => expr_find_used_stack_addrs(to),
        Call::Func { to_func_name: _, arg_assignments } => {
            arg_assignments.iter().flat_map(|aa| expr_find_used_stack_addrs(&aa.expr)).collect()
        },
    }
}

fn expr_find_used_stack_addrs(expr: &Expr) -> BTreeSet<StackAddr> {
    match expr {
        Expr::Loc(loc) => loc_find_used_stack_addrs(loc),
        Expr::StackAddr(addr) => BTreeSet::from([*addr.as_ref()]),
        Expr::Value(_) => Default::default(),
        Expr::StdoutDeref(expr) => expr_find_used_stack_addrs(expr),
        Expr::StdoutLen => Default::default(),
        Expr::Timer => Default::default(),
        Expr::Add(args) => args_find_used_stack_addrs(args),
        Expr::Sub(args) => args_find_used_stack_addrs(args),
        Expr::Mul(args) => args_find_used_stack_addrs(args),
        Expr::Div(args) => args_find_used_stack_addrs(args),
        Expr::Mod(args) => args_find_used_stack_addrs(args),
        Expr::Eq(args) => args_find_used_stack_addrs(args),
        Expr::Lt(args) => args_find_used_stack_addrs(args),
        Expr::Gt(args) => args_find_used_stack_addrs(args),
        Expr::Not(expr) => expr_find_used_stack_addrs(expr),
        Expr::Or(args) => args_find_used_stack_addrs(args),
        Expr::And(args) => args_find_used_stack_addrs(args),
        Expr::InAnswer => Default::default(),
        Expr::Join(args) => args_find_used_stack_addrs(args),
        Expr::Random(args) => args_find_used_stack_addrs(args),
    }
}

fn args_find_used_stack_addrs(args: &BinaryArgs) -> BTreeSet<StackAddr> {
    chain!(expr_find_used_stack_addrs(&args.left), expr_find_used_stack_addrs(&args.right))
        .collect()
}

fn loc_find_used_stack_addrs(loc: &Loc) -> BTreeSet<StackAddr> {
    match loc {
        Loc::Temp(_) => Default::default(),
        Loc::Deref(expr) => expr_find_used_stack_addrs(expr),
    }
}
