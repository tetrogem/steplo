use std::{mem, sync::Arc};

use crate::ast;

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
    Jump { proc_name_addr: Arc<str> },
    Branch { proc_name_addr: Arc<str>, cond_addr: Arc<str> },
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
    pub val: Arc<str>,
}

impl From<&ast::UnaryArgs> for UnaryArgs {
    fn from(value: &ast::UnaryArgs) -> Self {
        UnaryArgs { val: value_to_str(&value.val) }
    }
}

#[derive(Debug)]
pub struct BinaryArgs {
    pub dest: Arc<str>,
    pub val: Arc<str>,
}

impl From<&ast::BinaryArgs> for BinaryArgs {
    fn from(value: &ast::BinaryArgs) -> Self {
        BinaryArgs { dest: value_to_str(&value.dest), val: value_to_str(&value.val) }
    }
}

#[derive(Debug)]
pub struct TernaryArgs {
    pub dest: Arc<str>,
    pub left: Arc<str>,
    pub right: Arc<str>,
}

impl From<&ast::TernaryArgs> for TernaryArgs {
    fn from(value: &ast::TernaryArgs) -> Self {
        TernaryArgs {
            dest: value_to_str(&value.dest),
            left: value_to_str(&value.left),
            right: value_to_str(&value.right),
        }
    }
}

fn value_to_str(value: &ast::Value) -> Arc<str> {
    match value {
        ast::Value::Literal(literal) => Arc::clone(&literal.val),
        ast::Value::Label(label) => format!("sub_{}.{}", label.name, 0).into(),
    }
}

pub fn link(ast: &[Arc<ast::Procedure>]) -> Vec<Arc<Procedure>> {
    let mut procs = Vec::<Arc<Procedure>>::new();

    for ast_proc in ast {
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
                        next_call: Arc::new(Call::Jump { proc_name_addr: value_to_str(&args.val) }),
                    };

                    linked_bodies.push(body);
                },
                ast::ControlCommand::Branch(args) => {
                    let commands = mem::take(&mut linking);

                    let body = Body {
                        commands,
                        next_call: Arc::new(Call::Branch {
                            proc_name_addr: value_to_str(&args.dest),
                            cond_addr: value_to_str(&args.val),
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
