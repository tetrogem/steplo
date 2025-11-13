pub mod find_addr_expr_used_local_vars {
    use std::{collections::BTreeSet, sync::Arc};

    use itertools::chain;

    use crate::inline::ast::{BinaryArgs, Call, Command, Expr, Loc, Proc, StackAddr};

    // local vars who's addresses are used in expressions other than instantly dereffing
    pub fn proc_find_addr_expr_used_vars(proc: &Arc<Proc>) -> BTreeSet<StackAddr> {
        let mut addr_expr_used_local_vars = BTreeSet::new();

        for sp in proc.sub_procs.iter() {
            for command in sp.commands.iter() {
                addr_expr_used_local_vars.extend(command_find_addr_expr_used_vars(command));
            }

            match sp.call.as_ref() {
                Call::Exit => {},
                Call::Jump { to } => {
                    addr_expr_used_local_vars.extend(expr_find_addr_expr_used_vars(to))
                },
                Call::Branch { cond, then_to, else_to } => {
                    addr_expr_used_local_vars.extend(expr_find_addr_expr_used_vars(cond));
                    addr_expr_used_local_vars.extend(expr_find_addr_expr_used_vars(then_to));
                    addr_expr_used_local_vars.extend(expr_find_addr_expr_used_vars(else_to));
                },
                Call::Sleep { duration_s, to } => {
                    addr_expr_used_local_vars.extend(expr_find_addr_expr_used_vars(duration_s));
                    addr_expr_used_local_vars.extend(expr_find_addr_expr_used_vars(to));
                },
                Call::Return { to } => {
                    addr_expr_used_local_vars.extend(expr_find_addr_expr_used_vars(to));
                },
                Call::Func { to_func_name: _, arg_assignments } => {
                    for aa in arg_assignments.iter() {
                        addr_expr_used_local_vars.extend(expr_find_addr_expr_used_vars(&aa.expr));
                    }
                },
            }
        }

        addr_expr_used_local_vars
    }

    pub fn command_find_addr_expr_used_vars(command: &Command) -> BTreeSet<StackAddr> {
        match command {
            Command::In => Default::default(),
            Command::ClearStdout => Default::default(),
            Command::ClearKeyEventsKeyQueue => Default::default(),
            Command::ClearKeyEventsTimeQueue => Default::default(),
            Command::Out(expr) => expr_find_addr_expr_used_vars(expr),
            Command::WriteStdout { index, val } => {
                chain!(expr_find_addr_expr_used_vars(index), expr_find_addr_expr_used_vars(val))
                    .collect()
            },
            Command::DeleteKeyEventsKeyQueue { index } => expr_find_addr_expr_used_vars(index),
            Command::DeleteKeyEventsTimeQueue { index } => expr_find_addr_expr_used_vars(index),
            Command::SetLoc { loc, val } => {
                chain!(loc_find_addr_expr_used_vars(loc), expr_find_addr_expr_used_vars(val))
                    .collect()
            },
        }
    }

    pub fn expr_find_addr_expr_used_vars(expr: &Expr) -> BTreeSet<StackAddr> {
        match expr {
            Expr::Loc(loc) => loc_find_addr_expr_used_vars(loc),

            Expr::StackAddr(addr) => BTreeSet::from([*addr.as_ref()]),

            Expr::Value(_)
            | Expr::StdoutLen
            | Expr::KeyEventsKeyQueueLen
            | Expr::KeyEventsTimeQueueLen
            | Expr::Timer
            | Expr::DaysSince2000
            | Expr::InAnswer => Default::default(),

            Expr::StdoutDeref(expr)
            | Expr::KeyEventsKeyQueueDeref(expr)
            | Expr::KeyEventsTimeQueueDeref(expr)
            | Expr::Not(expr)
            | Expr::Round(expr)
            | Expr::Floor(expr)
            | Expr::Ceil(expr)
            | Expr::Abs(expr) => expr_find_addr_expr_used_vars(expr),

            Expr::Add(args)
            | Expr::Sub(args)
            | Expr::Mul(args)
            | Expr::Div(args)
            | Expr::Mod(args)
            | Expr::Eq(args)
            | Expr::Lt(args)
            | Expr::Gt(args)
            | Expr::Or(args)
            | Expr::And(args)
            | Expr::Join(args)
            | Expr::Random(args) => args_find_addr_expr_used_vars(args),
        }
    }

    pub fn args_find_addr_expr_used_vars(args: &BinaryArgs) -> BTreeSet<StackAddr> {
        chain!(
            expr_find_addr_expr_used_vars(&args.left),
            expr_find_addr_expr_used_vars(&args.right)
        )
        .collect()
    }

    pub fn loc_find_addr_expr_used_vars(loc: &Loc) -> BTreeSet<StackAddr> {
        match loc {
            Loc::Temp(_) => Default::default(),
            Loc::Deref(addr) => {
                match addr.as_ref() {
                    // don't count immediately dereffed stack addrs
                    Expr::StackAddr(_) => Default::default(),
                    addr => expr_find_addr_expr_used_vars(addr),
                }
            },
        }
    }
}

pub mod is_definitely_not_runtime_equal {
    use crate::inline::ast::Expr;

    pub fn expr_is_definitely_not_runtime_equal(_left: &Expr, _right: &Expr) -> bool {
        false
    }
}
