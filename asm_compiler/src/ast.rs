use std::{mem::replace, sync::Arc};

use anyhow::{bail, Context};

use crate::token;

#[derive(Debug)]
pub enum Command {
    Set(BinaryArgs),
    Move(BinaryArgs),
    MoveDerefDest(BinaryArgs),
    MoveDerefSrc(BinaryArgs),
    Add(TernaryArgs),
    Sub(TernaryArgs),
    Jump(UnaryArgs),
    BranchEq(TernaryArgs),
    Out(UnaryArgs),
    Eq(TernaryArgs),
    Not(BinaryArgs),
}

#[derive(Debug)]
pub struct BinaryArgs {
    pub dest: Value,
    pub val: Value,
}

#[derive(Debug)]
pub struct TernaryArgs {
    pub dest: Value,
    pub left: Value,
    pub right: Value,
}

#[derive(Debug)]
pub struct UnaryArgs {
    pub val: Value,
}

#[derive(Debug)]
pub struct BranchArgs {
    pub src: Value,
    pub left: Value,
    pub right: Value,
}

#[derive(Debug)]
pub struct Value {
    pub str: Arc<str>,
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

fn parse_value<'a>(tokens: &mut impl Iterator<Item = &'a token::Token>) -> anyhow::Result<Value> {
    let Some(token::Token::Value(value)) = tokens.next() else { bail!("Expected value") };
    Ok(Value { str: Arc::clone(value) })
}

fn parse_binary_args<'a>(
    tokens: &mut impl Iterator<Item = &'a token::Token>,
) -> anyhow::Result<BinaryArgs> {
    let dest = parse_value(tokens).with_context(|| "For arg [dest]")?;
    let val = parse_value(tokens).with_context(|| "For arg [val]")?;
    Ok(BinaryArgs { dest, val })
}

fn parse_ternary_args<'a>(
    tokens: &mut impl Iterator<Item = &'a token::Token>,
) -> anyhow::Result<TernaryArgs> {
    let dest = parse_value(tokens).with_context(|| "For arg [dest]")?;
    let left = parse_value(tokens).with_context(|| "For arg [left]")?;
    let right = parse_value(tokens).with_context(|| "For arg [right]")?;
    Ok(TernaryArgs { dest, left, right })
}

fn parse_unary_args<'a>(
    tokens: &mut impl Iterator<Item = &'a token::Token>,
) -> anyhow::Result<UnaryArgs> {
    let src = parse_value(tokens).with_context(|| "For arg [val]")?;
    Ok(UnaryArgs { val: src })
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
                    token::Op::Lit => Command::Set(parse_binary_args(&mut tokens)?),
                    token::Op::Move => Command::Move(parse_binary_args(&mut tokens)?),
                    token::Op::MoveDerefDest => {
                        Command::MoveDerefDest(parse_binary_args(&mut tokens)?)
                    },
                    token::Op::MoveDerefSrc => {
                        Command::MoveDerefSrc(parse_binary_args(&mut tokens)?)
                    },
                    token::Op::Add => Command::Add(parse_ternary_args(&mut tokens)?),
                    token::Op::Sub => Command::Sub(parse_ternary_args(&mut tokens)?),
                    token::Op::Jump => Command::Jump(parse_unary_args(&mut tokens)?),
                    token::Op::BranchEq => Command::BranchEq(parse_ternary_args(&mut tokens)?),
                    token::Op::Out => Command::Out(parse_unary_args(&mut tokens)?),
                    token::Op::Eq => Command::Eq(parse_ternary_args(&mut tokens)?),
                    token::Op::Not => Command::Not(parse_binary_args(&mut tokens)?),
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
            token::Token::ProcHeader => {
                let Some(token::Token::ProcHeader) = tokens.next() else {
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
