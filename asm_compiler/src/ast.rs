use std::{iter::Peekable, mem::replace, sync::Arc};

use anyhow::{bail, Context};

use crate::token;

#[derive(Debug)]
pub enum Command {
    Data(Arc<DataCommand>),
    Control(Arc<ControlCommand>),
}

#[derive(Debug)]
pub enum DataCommand {
    Set(BinaryArgs),
    Move(BinaryArgs),
    MoveDerefDest(BinaryArgs),
    MoveDerefSrc(BinaryArgs),
    Add(TernaryArgs),
    Sub(TernaryArgs),
    Out(UnaryArgs),
    Eq(TernaryArgs),
    Not(BinaryArgs),
    In(UnaryArgs),
}

#[derive(Debug)]
pub enum ControlCommand {
    Jump(UnaryArgs),
    Branch(BinaryArgs),
    Exit,
}

#[derive(Debug)]
pub struct UnaryArgs {
    pub val: Arc<Value>,
}

#[derive(Debug)]
pub struct BinaryArgs {
    pub dest: Arc<Value>,
    pub val: Arc<Value>,
}

#[derive(Debug)]
pub struct TernaryArgs {
    pub dest: Arc<Value>,
    pub left: Arc<Value>,
    pub right: Arc<Value>,
}

#[derive(Debug)]
pub enum Value {
    Literal(Arc<Literal>),
    Label(Arc<Label>),
}

#[derive(Debug)]
pub struct Literal {
    pub val: Arc<str>,
}

#[derive(Debug)]
pub struct Label {
    pub name: Arc<str>,
}

#[derive(Debug)]
pub struct Procedure {
    pub kind: ProcedureKind,
    pub commands: Vec<Arc<Command>>,
}

#[derive(Debug)]
pub enum ProcedureKind {
    Main,
    Sub { name: Arc<str> },
}

fn parse_value<'a>(
    tokens: &mut Peekable<impl Iterator<Item = &'a token::Token>>,
) -> anyhow::Result<Value> {
    let value = match tokens.peek() {
        Some(token::Token::Hashtag) => Value::Label(Arc::new(parse_label(tokens)?)),
        _ => Value::Literal(Arc::new(parse_literal(tokens)?)),
    };

    Ok(value)
}

fn parse_literal<'a>(
    tokens: &mut impl Iterator<Item = &'a token::Token>,
) -> anyhow::Result<Literal> {
    let Some(token::Token::Value(value)) = tokens.next() else { bail!("Expected value") };
    Ok(Literal { val: Arc::clone(value) })
}

fn parse_label<'a>(tokens: &mut impl Iterator<Item = &'a token::Token>) -> anyhow::Result<Label> {
    let Some(token::Token::Hashtag) = tokens.next() else { bail!("Expected proc name hashtag") };
    let Some(token::Token::Value(name)) = tokens.next() else { bail!("Expected proc name value") };
    Ok(Label { name: Arc::clone(name) })
}

fn parse_unary_args<'a>(
    tokens: &mut Peekable<impl Iterator<Item = &'a token::Token>>,
) -> anyhow::Result<UnaryArgs> {
    let val = Arc::new(parse_value(tokens).with_context(|| "For arg [val]")?);
    Ok(UnaryArgs { val })
}

fn parse_binary_args<'a>(
    tokens: &mut Peekable<impl Iterator<Item = &'a token::Token>>,
) -> anyhow::Result<BinaryArgs> {
    let dest = Arc::new(parse_value(tokens).with_context(|| "For arg [dest]")?);
    let val = Arc::new(parse_value(tokens).with_context(|| "For arg [val]")?);
    Ok(BinaryArgs { dest, val })
}

fn parse_ternary_args<'a>(
    tokens: &mut Peekable<impl Iterator<Item = &'a token::Token>>,
) -> anyhow::Result<TernaryArgs> {
    let dest = Arc::new(parse_value(tokens).with_context(|| "For arg [dest]")?);
    let left = Arc::new(parse_value(tokens).with_context(|| "For arg [left]")?);
    let right = Arc::new(parse_value(tokens).with_context(|| "For arg [right]")?);
    Ok(TernaryArgs { dest, left, right })
}

pub fn parse<'a>(
    tokens: impl Iterator<Item = &'a token::Token>,
) -> anyhow::Result<Vec<Arc<Procedure>>> {
    let mut parsed_procs = Vec::new();

    let mut proc = Procedure { kind: ProcedureKind::Main, commands: Vec::new() };

    let mut tokens = tokens.into_iter().peekable();
    while let Some(next) = tokens.peek() {
        let command = match next {
            token::Token::Op(_) => {
                let Some(token::Token::Op(op_token)) = tokens.next() else { bail!("Expected op") };

                let command = match op_token {
                    token::Op::Lit => {
                        Command::Data(Arc::new(DataCommand::Set(parse_binary_args(&mut tokens)?)))
                    },
                    token::Op::Move => {
                        Command::Data(Arc::new(DataCommand::Move(parse_binary_args(&mut tokens)?)))
                    },
                    token::Op::MoveDerefDest => Command::Data(Arc::new(
                        DataCommand::MoveDerefDest(parse_binary_args(&mut tokens)?),
                    )),
                    token::Op::MoveDerefSrc => Command::Data(Arc::new(DataCommand::MoveDerefSrc(
                        parse_binary_args(&mut tokens)?,
                    ))),
                    token::Op::Add => {
                        Command::Data(Arc::new(DataCommand::Add(parse_ternary_args(&mut tokens)?)))
                    },
                    token::Op::Sub => {
                        Command::Data(Arc::new(DataCommand::Sub(parse_ternary_args(&mut tokens)?)))
                    },
                    token::Op::Jump => Command::Control(Arc::new(ControlCommand::Jump(
                        parse_unary_args(&mut tokens)?,
                    ))),
                    token::Op::Branch => Command::Control(Arc::new(ControlCommand::Branch(
                        parse_binary_args(&mut tokens)?,
                    ))),
                    token::Op::Out => {
                        Command::Data(Arc::new(DataCommand::Out(parse_unary_args(&mut tokens)?)))
                    },
                    token::Op::Eq => {
                        Command::Data(Arc::new(DataCommand::Eq(parse_ternary_args(&mut tokens)?)))
                    },
                    token::Op::Not => {
                        Command::Data(Arc::new(DataCommand::Not(parse_binary_args(&mut tokens)?)))
                    },
                    token::Op::Exit => Command::Control(Arc::new(ControlCommand::Exit)),
                    token::Op::In => {
                        Command::Data(Arc::new(DataCommand::In(parse_unary_args(&mut tokens)?)))
                    },
                };

                Some(command)
            },
            token::Token::Value(_) => bail!("Found unexpected value"),
            token::Token::Comment(_) => {
                // comments do not generate code
                tokens.next();
                None
            },
            token::Token::Eol => {
                // ignore extra EOLs
                tokens.next();
                None
            },
            token::Token::Hashtag => {
                let Some(token::Token::Hashtag) = tokens.next() else {
                    bail!("Expected proc header")
                };

                let Some(token::Token::Value(name)) = tokens.next() else {
                    bail!("Expected value [proc name]");
                };

                let parsed_proc = replace(
                    &mut proc,
                    Procedure {
                        kind: ProcedureKind::Sub { name: Arc::clone(name) },
                        commands: Vec::new(),
                    },
                );

                parsed_procs.push(Arc::new(parsed_proc));
                None
            },
        };

        if let Some(command) = command {
            proc.commands.push(Arc::new(command));
        }
    }

    // push final proc
    parsed_procs.push(Arc::new(proc));

    Ok(parsed_procs)
}
