use std::{iter::Peekable, ops::Not, sync::Arc};

use anyhow::bail;
use itertools::{Itertools, MultiPeek};

use crate::token::{Comword, Token};

#[derive(Debug, Clone)]
pub enum TopItem {
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
    pub body: Arc<Vec<Arc<BodyItem>>>,
}

#[derive(Debug, Clone)]
pub enum BodyItem {
    Statement(Arc<Statement>),
    If {
        cond_var: Arc<str>,
        then_body: Arc<Vec<Arc<BodyItem>>>,
        else_body: Option<Arc<Vec<Arc<BodyItem>>>>,
    },
    While {
        cond_var: Arc<str>,
        body: Arc<Vec<Arc<BodyItem>>>,
    },
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(Arc<Assign>),
    Call { func_name: Arc<str>, param_vars: Arc<Vec<Arc<str>>> },
    Native(Arc<NativeCommand>), // not compiled to by source code, internal/built-ins only
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub deref_var: bool,
    pub var: Arc<str>,
    pub command: Arc<Command>,
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
    Sub { left: Arc<str>, right: Arc<str> },
}

#[derive(Debug, Clone)]
pub enum NativeCommand {
    Out { var: Arc<str> },
}

pub fn parse(tokens: Vec<Token>) -> anyhow::Result<Vec<Arc<TopItem>>> {
    let mut tokens = tokens.into_iter().multipeek();
    let mut items = Vec::new();

    tokens.reset_peek();
    while let Some(token) = tokens.peek() {
        let item = match token {
            Token::Main => TopItem::Main(Arc::new(parse_main(&mut tokens)?)),
            Token::Func => TopItem::Func(Arc::new(parse_func(&mut tokens)?)),
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
                $tokens.reset_peek();
                if let Some($closer) = $tokens.peek() {
                    break;
                };

                let Some(Token::Name(var)) = $tokens.next() else { bail!("Expected var name") };
                vars.push(var.into());

                $tokens.reset_peek();
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

fn parse_main(tokens: &mut MultiPeek<impl Iterator<Item = Token>>) -> anyhow::Result<Main> {
    let Some(Token::Main) = tokens.next() else { bail!("Expected main") };

    let proc = parse_proc(tokens)?;
    let main = Main { proc: Arc::new(proc) };
    Ok(main)
}

fn parse_func(tokens: &mut MultiPeek<impl Iterator<Item = Token>>) -> anyhow::Result<Func> {
    let Some(Token::Func) = tokens.next() else { bail!("Expected func") };
    let Some(Token::Name(name)) = tokens.next() else { bail!("Expected func name") };
    let params = parse_var_list!(tokens, Token::LeftParen = "(", Token::RightParen = ")")?;
    let proc = parse_proc(tokens)?;

    let func = Func { name: name.into(), params: Arc::new(params), proc: Arc::new(proc) };
    Ok(func)
}

fn parse_proc(tokens: &mut MultiPeek<impl Iterator<Item = Token>>) -> anyhow::Result<Proc> {
    let vars = parse_var_list!(tokens, Token::Pipe = "pipe", Token::Pipe = "pipe")?;

    let Some(Token::LeftBrace) = tokens.next() else { bail!("Expected left brace") };

    // parse statements
    let mut body_items = Vec::<Arc<BodyItem>>::new();
    tokens.reset_peek();
    while let Some(token) = tokens.peek() {
        if let &Token::RightBrace = token {
            break;
        }

        let body_item = parse_body_item(tokens)?;
        body_items.push(Arc::new(body_item));
        tokens.reset_peek();
    }

    let Some(Token::RightBrace) = tokens.next() else { bail!("Expected right brace") };
    let proc = Proc { vars: Arc::new(vars), body: Arc::new(body_items) };
    Ok(proc)
}

fn parse_body_item(
    tokens: &mut MultiPeek<impl Iterator<Item = Token>>,
) -> anyhow::Result<BodyItem> {
    tokens.reset_peek();
    let Some(peek_token) = tokens.peek() else { bail!("Expected statement") };

    let body_item = match peek_token {
        Token::Deref => BodyItem::Statement(Arc::new(parse_assign(tokens)?)),
        Token::Name(_) => {
            let Some(peek_token) = tokens.peek() else {
                bail!("Unexpected end after var name in statement")
            };

            match peek_token {
                Token::Eq => BodyItem::Statement(Arc::new(parse_assign(tokens)?)),
                Token::LeftParen => BodyItem::Statement(Arc::new(parse_call(tokens)?)),
                _ => bail!("Expected ( or ="),
            }
        },
        Token::If => parse_if(tokens)?,
        Token::While => parse_while(tokens)?,
        t => bail!("Expected deref, var name, or if at start of statement (Found: {:?})", t),
    };

    Ok(body_item)
}

fn parse_assign(tokens: &mut MultiPeek<impl Iterator<Item = Token>>) -> anyhow::Result<Statement> {
    tokens.reset_peek();
    let deref_var = match tokens.peek() {
        Some(Token::Deref) => {
            let Some(Token::Deref) = tokens.next() else { bail!("Expected deref") };
            true
        },
        _ => false,
    };

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
        Comword::Sub => {
            let Some(Token::Name(left)) = tokens.next() else { bail!("Expected var") };
            let Some(Token::Name(right)) = tokens.next() else { bail!("Expected var") };
            Command::Sub { left: left.into(), right: right.into() }
        },
    };

    let Some(Token::Semi) = tokens.next() else { bail!("Expected semicolon") };

    let assign = Assign { deref_var, var: var.into(), command: Arc::new(command) };
    let statement = Statement::Assign(Arc::new(assign));

    Ok(statement)
}

fn parse_call(tokens: &mut MultiPeek<impl Iterator<Item = Token>>) -> anyhow::Result<Statement> {
    let Some(Token::Name(var)) = tokens.next() else { bail!("Expected var name") };
    let param_vars = parse_var_list!(tokens, Token::LeftParen = "(", Token::RightParen = ")")?;
    let Some(Token::Semi) = tokens.next() else { bail!("Expected semicolon") };

    let statement = Statement::Call { func_name: var.into(), param_vars: Arc::new(param_vars) };

    Ok(statement)
}

fn parse_if(tokens: &mut MultiPeek<impl Iterator<Item = Token>>) -> anyhow::Result<BodyItem> {
    let Some(Token::If) = tokens.next() else { bail!("Expected if") };
    let Some(Token::Name(cond_var)) = tokens.next() else { bail!("Expected condition var for if") };

    let then_body_items = Arc::new(parse_body(tokens)?);

    tokens.reset_peek();
    let else_body_items = match tokens.peek() {
        Some(Token::Else) => {
            let Some(Token::Else) = tokens.next() else { bail!("Expected else") };
            Some(Arc::new(parse_body(tokens)?))
        },
        _ => None,
    };

    let statement = BodyItem::If {
        cond_var: cond_var.into(),
        then_body: then_body_items,
        else_body: else_body_items,
    };
    Ok(statement)
}

fn parse_while(tokens: &mut MultiPeek<impl Iterator<Item = Token>>) -> anyhow::Result<BodyItem> {
    let Some(Token::While) = tokens.next() else { bail!("Expected while") };
    let Some(Token::Name(cond_var)) = tokens.next() else {
        bail!("Expected condition var for while")
    };

    let body_items = Arc::new(parse_body(tokens)?);

    let statement = BodyItem::While { cond_var: cond_var.into(), body: body_items };
    Ok(statement)
}

fn parse_body(
    tokens: &mut MultiPeek<impl Iterator<Item = Token>>,
) -> anyhow::Result<Vec<Arc<BodyItem>>> {
    let Some(Token::LeftBrace) = tokens.next() else { bail!("Expected {{") };

    let mut body_items = Vec::new();
    loop {
        tokens.reset_peek();
        if let Some(Token::RightBrace) = tokens.peek() {
            break;
        }

        let body_item = parse_body_item(tokens)?;
        body_items.push(Arc::new(body_item));
    }

    let Some(Token::RightBrace) = tokens.next() else { bail!("Expected }}") };

    Ok(body_items)
}
