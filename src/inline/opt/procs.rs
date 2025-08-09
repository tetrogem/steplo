use std::{collections::BTreeMap, ops::Not, sync::Arc};

use itertools::chain;
use uuid::Uuid;

use crate::inline::{
    ast::{
        ArgAssignment, BinaryArgs, Call, Command, Expr, Loc, Proc, ProcKind, StackAddr, SubProc,
        Value, VarInfo,
    },
    opt::{MaybeOptimized, proc::optimize_proc, tracked_optimize},
};

#[expect(clippy::ptr_arg)]
pub fn optimize_procs(procs: &Vec<Arc<Proc>>) -> MaybeOptimized<Vec<Arc<Proc>>> {
    let mut optimized = false;

    let procs = procs.clone();
    let procs = tracked_optimize(&mut optimized, procs, |procs| {
        procs.into_iter().map(|proc| optimize_proc(&proc)).collect()
    });

    let procs =
        tracked_optimize(&mut optimized, procs, |procs| optimization_inline_func_calls(&procs));

    MaybeOptimized { optimized, val: procs }
}

fn optimization_inline_func_calls(procs: &[Arc<Proc>]) -> MaybeOptimized<Vec<Arc<Proc>>> {
    let mut optimized = false;

    let inlineable_func_name_to_sp = find_inlineable_funcs(procs);

    let procs = procs
        .into_iter()
        .map(|proc| {
            let mut ordered_local_infos = proc.ordered_local_infos.as_ref().clone();

            let sub_procs = proc
                .sub_procs
                .iter()
                .map(|sp| {
                    // see if this sp can inline its function call
                    let Call::Func { to_func_name, arg_assignments } = sp.call.as_ref() else {
                        return sp.clone();
                    };

                    let Some(inlineable_func_proc) = inlineable_func_name_to_sp.get(to_func_name)
                    else {
                        return sp.clone();
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

                    ordered_local_infos.extend(chain!(
                        func_arg_to_inlined.values(),
                        func_local_to_inlined.values()
                    ));

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

                        commands.push(Arc::new(Command::SetLoc {
                            loc: Arc::new(loc),
                            val: aa.expr.clone(),
                        }));
                    }

                    let inlined_func_sp = inlineable_func_proc
                        .sub_procs
                        .iter()
                        .next()
                        .expect("inlined func has no sub procs");

                    // inline func's commands
                    for command in inlined_func_sp.commands.iter() {
                        let inlined_command = command_replace_stack_addrs(
                            command,
                            &func_arg_to_inlined,
                            &func_local_to_inlined,
                        );

                        commands.push(Arc::new(inlined_command));
                    }

                    // inline func's call
                    let call = match inlined_func_sp.call.as_ref() {
                        Call::Exit => Call::Exit,
                        Call::Return { to } => Call::Jump {
                            to: Arc::new(expr_replace_stack_addrs(
                                to,
                                &func_arg_to_inlined,
                                &func_local_to_inlined,
                            )),
                        },
                        Call::Jump { to } => Call::Jump {
                            to: Arc::new(expr_replace_stack_addrs(
                                to,
                                &func_arg_to_inlined,
                                &func_local_to_inlined,
                            )),
                        },
                        Call::Branch { cond, then_to, else_to } => Call::Branch {
                            cond: Arc::new(expr_replace_stack_addrs(
                                cond,
                                &func_arg_to_inlined,
                                &func_local_to_inlined,
                            )),
                            then_to: Arc::new(expr_replace_stack_addrs(
                                then_to,
                                &func_arg_to_inlined,
                                &func_local_to_inlined,
                            )),
                            else_to: Arc::new(expr_replace_stack_addrs(
                                else_to,
                                &func_arg_to_inlined,
                                &func_local_to_inlined,
                            )),
                        },
                        Call::Func { to_func_name, arg_assignments } => {
                            let arg_assignments = arg_assignments
                                .iter()
                                .map(|aa| ArgAssignment {
                                    arg_uuid: aa.arg_uuid,
                                    arg_offset: aa.arg_offset,
                                    expr: Arc::new(expr_replace_stack_addrs(
                                        &aa.expr,
                                        &func_arg_to_inlined,
                                        &func_local_to_inlined,
                                    )),
                                })
                                .collect();

                            Call::Func {
                                to_func_name: to_func_name.clone(),
                                arg_assignments: Arc::new(arg_assignments),
                            }
                        },
                    };

                    optimized = true;

                    Arc::new(SubProc {
                        uuid: sp.uuid,
                        commands: Arc::new(commands),
                        call: Arc::new(call),
                    })
                })
                .collect();

            Proc {
                kind: proc.kind.clone(),
                ordered_arg_infos: proc.ordered_arg_infos.clone(),
                ordered_local_infos: Arc::new(ordered_local_infos),
                sub_procs: Arc::new(sub_procs),
            }
        })
        .map(Arc::new)
        .collect();

    MaybeOptimized { optimized, val: procs }
}

fn find_inlineable_funcs(procs: &[Arc<Proc>]) -> BTreeMap<Arc<str>, Arc<Proc>> {
    // a function is currently inlinable if it contains a single sub proc
    procs
        .iter()
        .filter(|proc| proc.sub_procs.len() == 1)
        .filter_map(|proc| match proc.kind.as_ref() {
            ProcKind::Main => None,
            ProcKind::Func { name } => Some((name.clone(), proc.clone())),
        })
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
