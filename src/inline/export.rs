use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use itertools::Itertools;
use uuid::Uuid;

use crate::inline::ast::{
    BinaryArgs, Call, Command, Expr, Loc, Proc, ProcKind, Program, StackAddr, SubProc, TempVar,
    Value, VarInfo,
};

fn indent(str: impl AsRef<str>) -> String {
    str.as_ref().lines().map(|l| format!("    {l}")).join("\n")
}

pub fn export(program: &Program) -> String {
    let mut name_m = NameManager::default();
    let statics = program.statics.iter().map(|x| export_static(&mut name_m, **x)).join("\n");
    let procs = program.procs.iter().map(|proc| export_proc(&mut name_m, proc)).join("\n\n");
    format!("{statics}\n\n{procs}")
}

fn export_static(name_m: &mut NameManager, var_info: VarInfo) -> String {
    format!("static:{} ({})", name_m.get_name(var_info.uuid), var_info.size)
}

fn export_proc(name_m: &mut NameManager, proc: &Proc) -> String {
    let mut lines = Vec::new();

    let header_name = match proc.kind.as_ref() {
        ProcKind::Main => "main".into(),
        ProcKind::Func { name } => format!("func {name}"),
    };

    lines.push(header_name);

    lines.extend(
        proc.ordered_arg_infos
            .iter()
            .map(|info| format!("| arg:{} ({})", name_m.get_name(info.uuid), info.size)),
    );

    lines.extend(
        proc.ordered_local_infos
            .iter()
            .map(|info| format!("| local:{} ({})", name_m.get_name(info.uuid), info.size)),
    );

    lines.push("{".into());

    lines
        .push(proc.sub_procs.iter().map(|sp| export_sub_proc(name_m, sp)).map(indent).join("\n\n"));

    lines.push("}".into());

    lines.join("\n")
}

pub fn export_sub_proc(name_m: &mut NameManager, sp: &SubProc) -> String {
    let mut lines = Vec::new();

    lines.push(format!("proc {} {{", export_label(name_m, sp.uuid)));

    lines.extend(sp.commands.iter().map(|command| export_command(name_m, command)).map(indent));
    lines.push(indent(export_call(name_m, &sp.call)));

    lines.push("}".into());

    lines.join("\n")
}

fn export_command(name_m: &mut NameManager, command: &Command) -> String {
    let command_name = match command {
        Command::SetLoc { loc, val } => {
            format!("{} = {}", export_loc(name_m, loc), export_expr(name_m, val))
        },
        Command::In => "in".into(),
        Command::Out(val) => format!("out {}", export_expr(name_m, val)),
        Command::ClearStdout => "stdout::clear".into(),
        Command::WriteStdout { index, val } => {
            format!("stdout[{}] = {}", export_expr(name_m, index), export_expr(name_m, val))
        },
    };

    format!("{command_name};")
}

fn export_call(name_m: &mut NameManager, call: &Call) -> String {
    let call_name = match call {
        Call::Jump { to } => format!("jump {}", export_expr(name_m, to)),
        Call::Exit => "exit".into(),
        Call::Branch { cond, then_to, else_to } => {
            format!(
                "if {} then jump {} else jump {}",
                export_expr(name_m, cond),
                export_expr(name_m, then_to),
                export_expr(name_m, else_to)
            )
        },
        Call::Sleep { duration_s, to } => {
            format!(
                "sleep_s {} then jump {}",
                export_expr(name_m, duration_s),
                export_expr(name_m, to)
            )
        },
        Call::Return { to } => format!("return {}", export_expr(name_m, to)),
        Call::Func { to_func_name, arg_assignments: args } => {
            let mut lines = Vec::new();

            lines.push(format!("exec {to_func_name} {{"));

            let assignment_lines = args.iter().map(|aa| {
                format!(
                    "stack[(arg:{} + \"{}\")] = {};",
                    String::from(name_m.get_name(aa.arg_uuid)),
                    aa.arg_offset,
                    export_expr(name_m, &aa.expr)
                )
            });

            lines.extend(assignment_lines.map(indent));

            lines.push("}".into());

            lines.join("\n")
        },
    };

    format!("{call_name};")
}

fn export_loc(name_m: &mut NameManager, loc: &Loc) -> String {
    match loc {
        Loc::Deref(addr) => format!("stack[{}]", export_expr(name_m, addr)),
        Loc::Temp(temp) => export_temp(name_m, temp),
    }
}

fn export_stack_addr(name_m: &mut NameManager, stack_addr: &StackAddr) -> String {
    let (kind, uuid) = match stack_addr {
        StackAddr::Local { uuid } => ("local", uuid),
        StackAddr::Arg { uuid } => ("arg", uuid),
        StackAddr::Static { uuid } => ("static", uuid),
    };

    let name = name_m.get_name(*uuid);

    format!("{kind}:{name}")
}

fn export_expr(name_m: &mut NameManager, expr: &Expr) -> String {
    match expr {
        Expr::StackAddr(addr) => export_stack_addr(name_m, addr),
        Expr::Loc(loc) => export_loc(name_m, loc),
        Expr::Value(value) => match value.as_ref() {
            Value::Label(label) => export_label(name_m, *label),
            Value::Literal(literal) => format!("\"{literal}\""),
        },
        Expr::StdoutDeref(expr) => format!("stdout[{}]", export_expr(name_m, expr)),
        Expr::StdoutLen => "stdout.len".into(),
        Expr::Timer => "timer".into(),
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
        Expr::InAnswer => "answer".into(),
        Expr::Join(args) => export_binary_op_expr(name_m, args, "~"),
        Expr::Random(args) => export_binary_op_expr(name_m, args, "<random>"),
    }
}

fn export_binary_op_expr(name_m: &mut NameManager, args: &BinaryArgs, op: &str) -> String {
    format!("({} {} {})", export_expr(name_m, &args.left), op, export_expr(name_m, &args.right))
}

fn export_label(name_m: &mut NameManager, uuid: Uuid) -> String {
    format!("#{}", name_m.get_name(uuid))
}

fn export_temp(name_m: &mut NameManager, temp: &TempVar) -> String {
    format!("%{}", name_m.get_name(temp.uuid))
}

#[derive(Default)]
pub struct NameManager {
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
