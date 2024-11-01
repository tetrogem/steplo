use std::sync::Arc;

use anyhow::{bail, Context};

use crate::token;

#[derive(Debug)]
pub enum Command {
    Set(BinaryArgs),
    Load(BinaryArgs),
    Store(BinaryArgs),
    Add(TernaryArgs),
    Sub(TernaryArgs),
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
pub struct Value {
    pub str: Arc<str>,
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

pub fn parse<'a>(
    tokens: impl Iterator<Item = &'a token::Token>,
) -> anyhow::Result<Vec<Arc<Command>>> {
    let mut commands = Vec::<Arc<Command>>::new();

    let mut tokens = tokens.into_iter().peekable();
    while let Some(next) = tokens.peek() {
        let command = match next {
            token::Token::Op(_) => {
                let Some(token::Token::Op(op_token)) = tokens.next() else { bail!("Expected op") };

                let command = match op_token {
                    token::Op::Lit => Command::Set(parse_binary_args(&mut tokens)?),
                    token::Op::Load => Command::Load(parse_binary_args(&mut tokens)?),
                    token::Op::Store => Command::Store(parse_binary_args(&mut tokens)?),
                    token::Op::Add => Command::Add(parse_ternary_args(&mut tokens)?),
                    token::Op::Sub => Command::Sub(parse_ternary_args(&mut tokens)?),
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
        };

        if let Some(command) = command {
            commands.push(Arc::new(command));
        }
    }

    Ok(commands)
}
