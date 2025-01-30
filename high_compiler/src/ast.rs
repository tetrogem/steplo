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
    Assign { ref_var: bool, var: Arc<str>, command: Arc<Command> },
    Call { func_item: Arc<str>, cond_var: Arc<str> },
}

#[derive(Debug, Clone)]
pub enum Command {
    Literal(Arc<str>),
    Add { left: Arc<str>, right: Arc<str> },
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

fn parse_main(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> anyhow::Result<Main> {
    let Some(Token::Main) = tokens.next() else { bail!("Expected main") };
    let Some(Token::Pipe) = tokens.next() else { bail!("Expected opening pipe") };

    let mut vars = Vec::<Arc<str>>::new();
    loop {
        let Some(Token::Name(var)) = tokens.next() else { bail!("Expected var name") };
        vars.push(var.into());

        if matches!(tokens.peek(), Some(Token::Comma)).not() {
            break;
        }

        let Some(Token::Comma) = tokens.next() else { bail!("Expected comma") };
    }

    let Some(Token::Pipe) = tokens.next() else { bail!("Expected closing pipe") };
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
    let main = Main { proc: Arc::new(proc) };
    Ok(main)
}

fn parse_func(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> anyhow::Result<Func> {
    todo!()
}

fn parse_statement(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
) -> anyhow::Result<Statement> {
    let Some(Token::Name(var)) = tokens.next() else { bail!("Expected var name") };
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
        _ => todo!(),
    };

    let Some(Token::Semi) = tokens.next() else { bail!("Expected semicolon") };

    let statement =
        Statement::Assign { ref_var: false, var: var.into(), command: Arc::new(command) };

    Ok(statement)
}
