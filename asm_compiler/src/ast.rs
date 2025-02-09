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

                macro_rules! com {
                    ($kind:ident $com:ident 0) => {
                        com!(@@ $kind $com => [])
                    };
                    ($kind:ident $com:ident 1) => {
                        com!(@ $kind $com => parse_unary_args)
                    };
                    ($kind:ident $com:ident 2) => {
                        com!(@ $kind $com => parse_binary_args)
                    };
                    ($kind:ident $com:ident 3) => {
                        com!(@ $kind $com => parse_ternary_args)
                    };
                    (@ $kind:ident $com:ident => $parse_args:ident) => {{
                        let args = $parse_args(&mut tokens)?;
                        com!(@@ $kind $com => [(args)])
                    }};
                    (@@ $kind:ident $com:ident => [$($args:tt)?]) => {{
                        paste::paste! {
                            let com = [< $kind Command >] :: $com $($args)?;
                            let com = Arc::new(com);
                            Command :: $kind (com)
                        }
                    }}
                }

                let command = match op_token {
                    token::Op::Lit => com!(Data Set 2),
                    token::Op::Move => com!(Data Move 2),
                    token::Op::MoveDerefDest => com!(Data MoveDerefDest 2),
                    token::Op::MoveDerefSrc => com!(Data MoveDerefSrc 2),
                    token::Op::Add => com!(Data Add 3),
                    token::Op::Sub => com!(Data Sub 3),
                    token::Op::Jump => com!(Control Jump 1),
                    token::Op::Branch => com!(Control Branch 2),
                    token::Op::Out => com!(Data Out 1),
                    token::Op::Eq => com!(Data Eq 3),
                    token::Op::Not => com!(Data Not 2),
                    token::Op::Exit => com!(Control Exit 0),
                    token::Op::In => com!(Data In 1),
                    token::Op::Mul => com!(Data Mul 3),
                    token::Op::Div => com!(Data Div 3),
                    token::Op::Mod => com!(Data Mod 3),
                    token::Op::Neq => com!(Data Neq 3),
                    token::Op::Gt => com!(Data Gt 3),
                    token::Op::Lt => com!(Data Lt 3),
                    token::Op::Gte => com!(Data Gte 3),
                    token::Op::Lte => com!(Data Lte 3),
                    token::Op::And => com!(Data And 3),
                    token::Op::Or => com!(Data Or 3),
                    token::Op::Xor => com!(Data Xor 3),
                    token::Op::Join => com!(Data Join 3),
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
