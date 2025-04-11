// use std::{collections::BTreeMap, sync::Arc};

// use uuid::Uuid;

// use crate::ast::{Command, Expr, SubProc, TempVar, UMemLoc};

// fn optimize_sub_proc(sp: &SubProc<UMemLoc>) -> SubProc<UMemLoc> {
//     let trivial_temp_to_expr = find_trivial_temps(sp);
//     let mut opt_commands = Vec::new();
//     for command in sp.commands.as_ref() {
//         match command
//     }
// }

// fn find_trivial_temps(sp: &SubProc<UMemLoc>) -> BTreeMap<Arc<TempVar>, Arc<Expr<UMemLoc>>> {
//     let mut trivial_temp_to_expr = BTreeMap::new();

//     for command in sp.commands.as_ref() {
//         match command.as_ref() {
//             Command::SetMemLoc(args) => {
//                 if is_trivial_expr(&args.src) {
//                     if let UMemLoc::Temp(temp) = args.dest.as_ref() {
//                         trivial_temp_to_expr.insert(temp.clone(), args.src.clone());
//                     }
//                 }
//             },
//             _ => {},
//         }
//     }

//     trivial_temp_to_expr
// }

// fn is_trivial_expr(expr: &Expr<UMemLoc>) -> bool {
//     match expr {
//         Expr::MemLoc(_) => true,
//         Expr::Value(_) => true,
//         _ => false,
//     }
// }
