use itertools::{chain, Itertools};

use crate::ast::{
    BinaryArgs, Command, ControlCommand, DataCommand, Procedure, ProcedureKind, Program,
    TernaryArgs, UnaryArgs, Value,
};

pub fn export(program: &Program) -> String {
    let proc_exports = program.procedures.iter().map(|proc| export_proc(proc)).collect_vec();
    proc_exports.join("\n\n")
}

fn export_proc(proc: &Procedure) -> String {
    let label_export = match &proc.kind {
        ProcedureKind::Main => None,
        ProcedureKind::Sub { name } => Some(export_label(name)),
    };

    let command_exports = proc.commands.iter().map(|c| export_command(c)).collect_vec();

    chain!(label_export, command_exports).join("\n")
}

fn export_command(command: &Command) -> String {
    match command {
        Command::Data(data) => match data.as_ref() {
            DataCommand::Set(args) => build_command_export("lit", args),
            DataCommand::Move(args) => build_command_export("mv", args),
            DataCommand::MoveDerefDest(args) => build_command_export("mvdd", args),
            DataCommand::MoveDerefSrc(args) => build_command_export("mvds", args),
            DataCommand::In(args) => build_command_export("in", args),
            DataCommand::Out(args) => build_command_export("out", args),
            DataCommand::Add(args) => build_command_export("add", args),
            DataCommand::Sub(args) => build_command_export("sub", args),
            DataCommand::Mul(args) => build_command_export("mul", args),
            DataCommand::Div(args) => build_command_export("div", args),
            DataCommand::Mod(args) => build_command_export("mod", args),
            DataCommand::Eq(args) => build_command_export("eq", args),
            DataCommand::Neq(args) => build_command_export("neq", args),
            DataCommand::Gt(args) => build_command_export("gt", args),
            DataCommand::Lt(args) => build_command_export("lt", args),
            DataCommand::Gte(args) => build_command_export("gte", args),
            DataCommand::Lte(args) => build_command_export("lte", args),
            DataCommand::And(args) => build_command_export("and", args),
            DataCommand::Or(args) => build_command_export("or", args),
            DataCommand::Xor(args) => build_command_export("xor", args),
            DataCommand::Not(args) => build_command_export("not", args),
            DataCommand::Join(args) => build_command_export("join", args),
        },
        Command::Control(control) => match control.as_ref() {
            ControlCommand::Jump(args) => build_command_export("jmp", args),
            ControlCommand::Branch(args) => build_command_export("brc", args),
            ControlCommand::Exit => build_command_export("exit", &()),
        },
    }
}

trait ExportArgs {
    fn export(&self) -> Vec<String>;
}

impl ExportArgs for () {
    fn export(&self) -> Vec<String> {
        Vec::new()
    }
}

impl ExportArgs for UnaryArgs {
    fn export(&self) -> Vec<String> {
        export_args(&[&self.val])
    }
}

impl ExportArgs for BinaryArgs {
    fn export(&self) -> Vec<String> {
        export_args(&[&self.dest, &self.val])
    }
}

impl ExportArgs for TernaryArgs {
    fn export(&self) -> Vec<String> {
        export_args(&[&self.dest, &self.left, &self.right])
    }
}

fn build_command_export<Args: ExportArgs>(op: &str, args: &Args) -> String {
    chain!([op.to_string()], args.export()).join(" ")
}

fn export_args(args: &[&Value]) -> Vec<String> {
    args.iter().map(|arg| export_value(arg)).collect()
}

fn export_value(value: &Value) -> String {
    match value {
        Value::Literal(literal) => export_literal(&literal.val),
        Value::Label(label) => export_label(&label.name),
        Value::Register(register) => export_register(&register.name),
    }
}

fn export_literal(val: &str) -> String {
    format!(r#""{}""#, val)
}

fn export_label(name: &str) -> String {
    format!(r#"#"{}""#, name)
}

fn export_register(name: &str) -> String {
    format!("${}", name)
}
