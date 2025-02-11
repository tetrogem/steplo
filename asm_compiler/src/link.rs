use std::{mem, sync::Arc};

use crate::ast;

#[derive(Debug)]
pub struct Program {
    pub registers: Arc<Vec<Arc<str>>>,
    pub procedures: Arc<Vec<Arc<Procedure>>>,
}

#[derive(Debug)]
pub struct Procedure {
    pub kind: ProcedureKind,
    pub body: Arc<Body>,
}

#[derive(Debug)]
pub enum ProcedureKind {
    Main,
    Sub { name: Arc<str> },
}

#[derive(Debug)]
pub struct Body {
    pub commands: Vec<Arc<DataCommand>>,
    pub next_call: Arc<Call>,
}

#[derive(Debug)]
pub enum Call {
    Jump { proc_name_addr: Arc<Value> },
    Branch { proc_name_addr: Arc<Value>, cond_addr: Arc<Value> },
    Passthrough,
    Exit,
}

#[derive(Debug)]
pub enum DataCommand {
    // memory
    Set(BinaryArgs),
    Move(BinaryArgs),
    MoveDerefDest(BinaryArgs),
    MoveDerefSrc(BinaryArgs),
    // io
    In(UnaryArgs),
    Out(UnaryArgs),
    // math
    Add(TernaryArgs),
    Sub(TernaryArgs),
    Mul(TernaryArgs),
    Div(TernaryArgs),
    Mod(TernaryArgs),
    // inequality
    Eq(TernaryArgs),
    Neq(TernaryArgs),
    Gt(TernaryArgs),
    Lt(TernaryArgs),
    Gte(TernaryArgs),
    Lte(TernaryArgs),
    // boolean
    And(TernaryArgs),
    Or(TernaryArgs),
    Xor(TernaryArgs),
    Not(BinaryArgs),
    // string
    Join(TernaryArgs),
}

#[derive(Debug)]
pub struct UnaryArgs {
    pub val: Arc<Value>,
}

impl From<&ast::UnaryArgs> for UnaryArgs {
    fn from(value: &ast::UnaryArgs) -> Self {
        UnaryArgs { val: link_value(&value.val) }
    }
}

#[derive(Debug)]
pub struct BinaryArgs {
    pub dest: Arc<Value>,
    pub val: Arc<Value>,
}

impl From<&ast::BinaryArgs> for BinaryArgs {
    fn from(value: &ast::BinaryArgs) -> Self {
        BinaryArgs { dest: link_value(&value.dest), val: link_value(&value.val) }
    }
}

#[derive(Debug)]
pub struct TernaryArgs {
    pub dest: Arc<Value>,
    pub left: Arc<Value>,
    pub right: Arc<Value>,
}

impl From<&ast::TernaryArgs> for TernaryArgs {
    fn from(value: &ast::TernaryArgs) -> Self {
        TernaryArgs {
            dest: link_value(&value.dest),
            left: link_value(&value.left),
            right: link_value(&value.right),
        }
    }
}

fn link_value(value: &ast::Value) -> Arc<Value> {
    let value = match value {
        ast::Value::Literal(literal) => Value::Literal { val: Arc::clone(&literal.val) },
        ast::Value::Label(label) => {
            Value::Literal { val: format!("sub_{}.{}", label.name, 0).into() }
        },
        ast::Value::Register(static_var) => Value::Register { name: Arc::clone(&static_var.name) },
    };

    Arc::new(value)
}

#[derive(Debug)]
pub enum Value {
    Literal { val: Arc<str> },
    Register { name: Arc<str> },
}

pub fn link(ast: &ast::Program) -> Program {
    let procs = link_procs(&ast.procedures);

    Program { registers: Arc::clone(&ast.registers), procedures: Arc::new(procs) }
}

fn link_procs(ast_procs: &Vec<Arc<ast::Procedure>>) -> Vec<Arc<Procedure>> {
    let mut procs = Vec::<Arc<Procedure>>::new();

    for ast_proc in ast_procs {
        let linked_bodies = link_proc(ast_proc);
        for (i, body) in linked_bodies.into_iter().enumerate() {
            let proc_kind = match &ast_proc.kind {
                ast::ProcedureKind::Main => match i {
                    0 => ProcedureKind::Main,
                    i => ProcedureKind::Sub { name: format!("main.{}", i).into() },
                },
                ast::ProcedureKind::Sub { name } => {
                    ProcedureKind::Sub { name: format!("sub_{}.{}", name, i).into() }
                },
            };

            let proc = Procedure { kind: proc_kind, body: Arc::new(body) };

            procs.push(Arc::new(proc));
        }
    }

    procs
}

fn link_proc(ast_proc: &Arc<ast::Procedure>) -> Vec<Body> {
    let mut linked_bodies = Vec::<Body>::new();
    let mut linking = Vec::<Arc<DataCommand>>::new();

    for ast_command in &ast_proc.commands {
        match ast_command.as_ref() {
            ast::Command::Data(data_command) => {
                macro_rules! ast_to_link {
                    ($val:expr => $($com:ident),* $(,)?) => {
                        match $val.as_ref() {
                            $(
                                ast::DataCommand:: $com (args) => DataCommand:: $com (args.into()),
                            )*
                        }
                    };
                }

                let data_command = ast_to_link!(
                    data_command =>
                        Set,
                        Move,
                        MoveDerefDest,
                        MoveDerefSrc,
                        In,
                        Out,
                        Add,
                        Sub,
                        Mul,
                        Div,
                        Mod,
                        Eq,
                        Neq,
                        Gt,
                        Lt,
                        Gte,
                        Lte,
                        And,
                        Or,
                        Xor,
                        Not,
                        Join,
                );

                linking.push(Arc::new(data_command));
            },
            ast::Command::Control(branch_command) => match branch_command.as_ref() {
                ast::ControlCommand::Jump(args) => {
                    let commands = mem::take(&mut linking);

                    let body = Body {
                        commands,
                        next_call: Arc::new(Call::Jump { proc_name_addr: link_value(&args.val) }),
                    };

                    linked_bodies.push(body);
                },
                ast::ControlCommand::Branch(args) => {
                    let commands = mem::take(&mut linking);

                    let body = Body {
                        commands,
                        next_call: Arc::new(Call::Branch {
                            proc_name_addr: link_value(&args.dest),
                            cond_addr: link_value(&args.val),
                        }),
                    };

                    linked_bodies.push(body);
                },
                ast::ControlCommand::Exit => {
                    let commands = mem::take(&mut linking);

                    let body = Body { commands, next_call: Arc::new(Call::Exit) };

                    linked_bodies.push(body);
                },
            },
        }
    }

    let final_body = Body { commands: linking, next_call: Arc::new(Call::Passthrough) };
    linked_bodies.push(final_body);
    linked_bodies
}
