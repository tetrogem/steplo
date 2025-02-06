use std::{iter::Peekable, ops::Not, sync::Arc};

use anyhow::bail;

use crate::token::{Comword, Token};

#[derive(Debug, Clone)]
pub enum Item {
    Main(Arc<Main>),
    Func(Arc<Func>),
}

#[derive(Debug, Clone)]
pub struct Main {
    pub proc: Arc<Proc>,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: Arc<str>,
    pub params: Arc<Vec<Arc<str>>>,
    pub proc: Arc<Proc>,
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub vars: Arc<Vec<Arc<str>>>,
    pub statements: Arc<Vec<Arc<Statement>>>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assign { deref_var: bool, var: Arc<str>, command: Arc<Command> },
    Call { func_item: Arc<str>, param_vars: Arc<Vec<Arc<str>>>, cond_var: Option<Arc<str>> },
    Native { command: NativeCommand }, // not compiled to by source code, internal/built-ins only
}

#[derive(Debug, Clone)]
pub enum Command {
    Literal(Arc<str>),
    Add { left: Arc<str>, right: Arc<str> },
    Ref { var: Arc<str> },
    CopyDeref { var: Arc<str> },
    Copy { var: Arc<str> },
    Eq { left: Arc<str>, right: Arc<str> },
    Not { var: Arc<str> },
}

#[derive(Debug, Clone)]
pub enum NativeCommand {
    Out { var: Arc<str> },
}

pub fn parse(tokens: Vec<Token>) -> anyhow::Result<Vec<Arc<Item>>> {
    let mut tokens = tokens.into_iter().peekable();
    let mut items = Vec::new();

    while let Some(token) = tokens.peek() {
        let item = match token {
            Token::Main => Item::Main(Arc::new(parse_main(&mut tokens)?)),
            Token::Func => Item::Func(Arc::new(parse_func(&mut tokens)?)),
            _ => bail!("Expected main or func"),
        };

        items.push(Arc::new(item));
    }

    Ok(items)
}

macro_rules! parse_var_list {
    (
        $tokens:expr,
        $opener:pat = $opener_name:expr,
        $closer:pat = $closer_name:expr $(,)?
    ) => {
        (|| {
            let Some($opener) = $tokens.next() else { bail!("Expected opening {}", $opener_name) };

            let mut vars = Vec::<Arc<str>>::new();
            loop {
                if let Some($closer) = $tokens.peek() {
                    break;
                };

                let Some(Token::Name(var)) = $tokens.next() else { bail!("Expected var name") };
                vars.push(var.into());

                if matches!($tokens.peek(), Some(Token::Comma)).not() {
                    break;
                }

                let Some(Token::Comma) = $tokens.next() else { bail!("Expected comma") };
            }

            let Some($closer) = $tokens.next() else { bail!("Expected closing {}", $closer_name) };

            Ok(vars)
        })()
    };
}

fn parse_main(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> anyhow::Result<Main> {
    let Some(Token::Main) = tokens.next() else { bail!("Expected main") };

    let proc = parse_proc(tokens)?;
    let main = Main { proc: Arc::new(proc) };
    Ok(main)
}

fn parse_func(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> anyhow::Result<Func> {
    let Some(Token::Func) = tokens.next() else { bail!("Expected func") };
    let Some(Token::Name(name)) = tokens.next() else { bail!("Expected func name") };
    let params = parse_var_list!(tokens, Token::LeftParen = "(", Token::RightParen = ")")?;
    let proc = parse_proc(tokens)?;

    let func = Func { name: name.into(), params: Arc::new(params), proc: Arc::new(proc) };
    Ok(func)
}

fn parse_proc(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> anyhow::Result<Proc> {
    let vars = parse_var_list!(tokens, Token::Pipe = "pipe", Token::Pipe = "pipe")?;

    let Some(Token::LeftBrace) = tokens.next() else { bail!("Expected left brace") };

    // parse statements
    let mut statements = Vec::<Arc<Statement>>::new();
    while let Some(token) = tokens.peek() {
        if let &Token::RightBrace = token {
            break;
        }

        let statement = parse_statement(tokens)?;
        statements.push(Arc::new(statement));
    }

    let Some(Token::RightBrace) = tokens.next() else { bail!("Expected right brace") };
    let proc = Proc { vars: Arc::new(vars), statements: Arc::new(statements) };
    Ok(proc)
}

fn parse_statement(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
) -> anyhow::Result<Statement> {
    let deref_var = match tokens.peek() {
        Some(Token::Deref) => {
            let Some(Token::Deref) = tokens.next() else { bail!("Expected deref") };
            true
        },
        _ => false,
    };

    let Some(Token::Name(var)) = tokens.next() else { bail!("Expected var name in statement") };
    let var: Arc<str> = var.into();

    let statement = match tokens.peek() {
        Some(Token::Eq) => parse_assign(tokens, deref_var, var)?,
        Some(Token::LeftParen) => {
            if deref_var {
                bail!("Cannot deref call");
            }

            parse_call(tokens, var)?
        },
        _ => bail!("Expected = or ("),
    };

    let Some(Token::Semi) = tokens.next() else { bail!("Expected semicolon") };

    Ok(statement)
}

fn parse_assign(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    deref_var: bool,
    var: Arc<str>,
) -> anyhow::Result<Statement> {
    let Some(Token::Eq) = tokens.next() else { bail!("Expected =") };

    let Some(Token::Comword(comword)) = tokens.next() else { bail!("Expected comword") };

    let command = match comword {
        Comword::Literal => {
            let Some(Token::Literal(value)) = tokens.next() else { bail!("Expected literal") };
            Command::Literal(value.into())
        },
        Comword::Add => {
            let Some(Token::Name(left)) = tokens.next() else { bail!("Expected var") };
            let Some(Token::Name(right)) = tokens.next() else { bail!("Expected var") };
            Command::Add { left: left.into(), right: right.into() }
        },
        Comword::Ref => {
            let Some(Token::Name(var)) = tokens.next() else { bail!("Expected var") };
            Command::Ref { var: var.into() }
        },
        Comword::CopyDeref => {
            let Some(Token::Name(var)) = tokens.next() else { bail!("Expected var") };
            Command::CopyDeref { var: var.into() }
        },
        Comword::Copy => {
            let Some(Token::Name(var)) = tokens.next() else { bail!("Expected var") };
            Command::Copy { var: var.into() }
        },
        Comword::Eq => {
            let Some(Token::Name(left)) = tokens.next() else { bail!("Expected var") };
            let Some(Token::Name(right)) = tokens.next() else { bail!("Expected var") };
            Command::Eq { left: left.into(), right: right.into() }
        },
        Comword::Not => {
            let Some(Token::Name(var)) = tokens.next() else { bail!("Expected var") };
            Command::Not { var: var.into() }
        },
    };

    let statement = Statement::Assign { deref_var, var, command: Arc::new(command) };

    Ok(statement)
}

fn parse_call(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
    var: Arc<str>,
) -> anyhow::Result<Statement> {
    let param_vars = parse_var_list!(tokens, Token::LeftParen = "(", Token::RightParen = ")")?;

    let statement =
        Statement::Call { func_item: var, param_vars: Arc::new(param_vars), cond_var: None };

    Ok(statement)
}
