use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use itertools::Itertools;
use uuid::Uuid;

use super::ast::{
    BinaryArgs, Call, Command, Expr, Proc, ProcKind, SubProc, TempVar, UMemLoc, Value,
};

fn indent(str: impl AsRef<str>) -> String {
    str.as_ref().lines().map(|l| format!("    {l}")).join("\n")
}

pub fn export<'a>(procs: impl Iterator<Item = &'a Proc<UMemLoc>>) -> String {
    let mut name_m = NameManager::default();
    procs.map(|proc| export_proc(&mut name_m, proc)).join("\n\n")
}

fn export_proc(name_m: &mut NameManager, proc: &Proc<UMemLoc>) -> String {
    let mut lines = Vec::new();

    let header_name = match proc.kind.as_ref() {
        ProcKind::Main => "main".into(),
        ProcKind::Func { name } => format!("func {name}"),
    };

    lines.push(format!("{header_name} {{"));

    lines
        .push(proc.sub_procs.iter().map(|sp| export_sub_proc(name_m, sp)).map(indent).join("\n\n"));

    lines.push("}".into());

    lines.join("\n")
}

fn export_sub_proc(name_m: &mut NameManager, sp: &SubProc<UMemLoc>) -> String {
    let mut lines = Vec::new();

    lines.push(format!("proc {} {{", export_label(name_m, sp.uuid)));

    lines.extend(sp.commands.iter().map(|command| export_command(name_m, command)).map(indent));
    lines.push(indent(export_call(name_m, &sp.call)));

    lines.push("}".into());

    lines.join("\n")
}

fn export_command(name_m: &mut NameManager, command: &Command<UMemLoc>) -> String {
    let command_name = match command {
        Command::SetMemLoc { mem_loc, val } => {
            format!("{} = {}", export_mem_loc(name_m, mem_loc), export_expr(name_m, val))
        },
        Command::SetStack { addr, val } => {
            format!("stack[{}] = {}", export_expr(name_m, addr), export_expr(name_m, val))
        },
        Command::In => "in".into(),
        Command::Out(val) => format!("out {}", export_expr(name_m, val)),
    };

    format!("{command_name};")
}

fn export_call(name_m: &mut NameManager, call: &Call<UMemLoc>) -> String {
    let call_name = match call {
        Call::Jump(to) => format!("jump {}", export_expr(name_m, to)),
        Call::Exit => "exit".into(),
        Call::Branch { cond, then_to, else_to } => {
            format!(
                "if {} then jump {} else jump {}",
                export_expr(name_m, cond),
                export_expr(name_m, then_to),
                export_expr(name_m, else_to)
            )
        },
    };

    format!("{call_name};")
}

fn export_mem_loc(name_m: &mut NameManager, mem_loc: &UMemLoc) -> String {
    match mem_loc {
        UMemLoc::StackPointer => "$sp".into(),
        UMemLoc::Temp(temp) => export_temp(name_m, temp),
    }
}

fn export_expr(name_m: &mut NameManager, expr: &Expr<UMemLoc>) -> String {
    match expr {
        Expr::MemLoc(mem_loc) => export_mem_loc(name_m, mem_loc),
        Expr::Value(value) => match value.as_ref() {
            Value::Label(label) => export_label(name_m, *label),
            Value::Literal(literal) => format!("\"{literal}\""),
        },
        Expr::Deref(expr) => format!("stack[{}]", export_expr(name_m, expr)),
        Expr::Add(args) => export_binary_op_expr(name_m, args, "+"),
        Expr::Sub(args) => export_binary_op_expr(name_m, args, "-"),
        Expr::Mul(args) => export_binary_op_expr(name_m, args, "*"),
        Expr::Div(args) => export_binary_op_expr(name_m, args, "/"),
        Expr::Mod(args) => export_binary_op_expr(name_m, args, "%"),
        Expr::Eq(args) => export_binary_op_expr(name_m, args, "=="),
        Expr::Gt(args) => export_binary_op_expr(name_m, args, ">"),
        Expr::Lt(args) => export_binary_op_expr(name_m, args, "<"),
        Expr::Not(expr) => format!("(!{})", export_expr(name_m, expr)),
        Expr::Or(args) => export_binary_op_expr(name_m, args, "||"),
        Expr::And(args) => export_binary_op_expr(name_m, args, "&&"),
        Expr::InAnswer => "$answer".into(),
        Expr::Join(args) => export_binary_op_expr(name_m, args, "~"),
        Expr::Random(args) => export_binary_op_expr(name_m, args, "<random>"),
    }
}

fn export_binary_op_expr(name_m: &mut NameManager, args: &BinaryArgs<UMemLoc>, op: &str) -> String {
    format!("({} {} {})", export_expr(name_m, &args.left), op, export_expr(name_m, &args.right))
}

fn export_label(name_m: &mut NameManager, uuid: Uuid) -> String {
    format!("#{}", name_m.get_name(uuid))
}

fn export_temp(name_m: &mut NameManager, temp: &TempVar) -> String {
    format!("%{}", name_m.get_name(temp.uuid))
}

#[derive(Default)]
struct NameManager {
    used_names: HashSet<Arc<str>>,
    uuid_to_name: HashMap<Uuid, Arc<str>>,
}

impl NameManager {
    pub fn get_name(&mut self, uuid: Uuid) -> &str {
        self.uuid_to_name.entry(uuid).or_insert_with(|| {
            let full_name = uuid.to_string();
            let mut available_chars = full_name.chars();
            let mut current_name: String = available_chars
                .next_array::<2>()
                .expect("2 chars should exist in UUID")
                .into_iter()
                .collect();

            loop {
                if !self.used_names.contains(current_name.as_str()) {
                    let current_name: Arc<str> = current_name.into();
                    self.used_names.insert(current_name.clone());
                    return current_name;
                }

                let Some(char) = available_chars.next() else {
                    panic!("Could not find available name for UUID `{uuid}`")
                };

                current_name.push(char);
            }
        })
    }
}
