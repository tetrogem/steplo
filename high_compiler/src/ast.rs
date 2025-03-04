use std::{iter::Peekable, ops::Not, sync::Arc};

use anyhow::bail;
use itertools::{Itertools, MultiPeek};

use crate::token::{Opword, Token};

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
    pub params: Arc<Vec<Arc<IdentDeclaration>>>,
    pub proc: Arc<Proc>,
}

#[derive(Debug, Clone)]
pub enum IdentDeclaration {
    Value { name: Arc<str> },
    Array { name: Arc<str>, length: usize },
}

impl IdentDeclaration {
    pub fn name(&self) -> &Arc<str> {
        match self {
            Self::Value { name } => name,
            Self::Array { name, .. } => name,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Ident {
    Var { name: Arc<str> },
    Array { name: Arc<str>, index: Arc<Pipeline> },
}

impl Ident {
    pub fn name(&self) -> &Arc<str> {
        match self {
            Self::Var { name } => name,
            Self::Array { name, .. } => name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub idents: Arc<Vec<Arc<IdentDeclaration>>>,
    pub body: Arc<Vec<Arc<BodyItem>>>,
}

#[derive(Debug, Clone)]
pub enum BodyItem {
    Statement(Arc<Statement>),
    If {
        cond_pipeline: Arc<Pipeline>,
        then_body: Arc<Vec<Arc<BodyItem>>>,
        else_body: Option<Arc<Vec<Arc<BodyItem>>>>,
    },
    While {
        cond_pipeline: Arc<Pipeline>,
        body: Arc<Vec<Arc<BodyItem>>>,
    },
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(Arc<Assign>),
    Call { func_name: Arc<str>, param_pipelines: Arc<Vec<Arc<Pipeline>>> },
    Native(Arc<NativeOperation>), // not compiled to by source code, internal/built-ins only
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub deref_ident: bool,
    pub ident: Arc<Ident>,
    pub pipeline: Arc<Pipeline>,
}

#[derive(Debug, Clone)]
pub enum Value {
    Literal(Arc<str>),
    Ident(Arc<Ident>),
    Ref(Arc<Ident>),
}

#[derive(Debug, Clone)]
pub struct Pipeline {
    pub initial_val: Arc<Value>,
    pub operations: Arc<Vec<Arc<Operation>>>,
}

#[derive(Debug, Clone)]
pub enum Operation {
    // memory
    Deref,
    // math
    Add { operand: Arc<Value> },
    Sub { operand: Arc<Value> },
    Mul { operand: Arc<Value> },
    Div { operand: Arc<Value> },
    Mod { operand: Arc<Value> },
    // inequality
    Eq { operand: Arc<Value> },
    Neq { operand: Arc<Value> },
    Gt { operand: Arc<Value> },
    Lt { operand: Arc<Value> },
    Gte { operand: Arc<Value> },
    Lte { operand: Arc<Value> },
    // boolean
    And { operand: Arc<Value> },
    Or { operand: Arc<Value> },
    Xor { operand: Arc<Value> },
    Not,
    // string
    Join { operand: Arc<Value> },
}

#[derive(Debug, Clone)]
pub enum NativeOperation {
    Out { ident: Arc<Ident> },
    In { dest_ident: Arc<Ident> },
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

macro_rules! parse_ident_declaration_list {
    (
        $tokens:expr,
        $opener:pat = $opener_name:expr,
        $closer:pat = $closer_name:expr $(,)?
    ) => {
        (|| {
            let Some($opener) = $tokens.next() else { bail!("Expected opening {}", $opener_name) };

            let mut idents = Vec::<Arc<IdentDeclaration>>::new();
            loop {
                $tokens.reset_peek();
                if let Some($closer) = $tokens.peek() {
                    break;
                };

                let ident = parse_ident_declaration($tokens)?;
                idents.push(Arc::new(ident));

                $tokens.reset_peek();
                if matches!($tokens.peek(), Some(Token::Comma)).not() {
                    break;
                }

                let Some(Token::Comma) = $tokens.next() else { bail!("Expected comma") };
            }

            let Some($closer) = $tokens.next() else { bail!("Expected closing {}", $closer_name) };

            Ok(idents)
        })()
    };
}

macro_rules! parse_pipeline_list {
    (
        $tokens:expr,
        $opener:pat = $opener_name:expr,
        $closer:pat = $closer_name:expr $(,)?
    ) => {
        (|| {
            let Some($opener) = $tokens.next() else { bail!("Expected opening {}", $opener_name) };

            let mut pipelines = Vec::<Arc<Pipeline>>::new();
            loop {
                $tokens.reset_peek();
                if let Some($closer) = $tokens.peek() {
                    break;
                };

                let pipeline = parse_pipeline($tokens)?;
                pipelines.push(Arc::new(pipeline));

                $tokens.reset_peek();
                if matches!($tokens.peek(), Some(Token::Comma)).not() {
                    break;
                }

                let Some(Token::Comma) = $tokens.next() else { bail!("Expected comma") };
            }

            let Some($closer) = $tokens.next() else { bail!("Expected closing {}", $closer_name) };

            Ok(pipelines)
        })()
    };
}

fn parse_ident_declaration(
    tokens: &mut MultiPeek<impl Iterator<Item = Token>>,
) -> anyhow::Result<IdentDeclaration> {
    let Some(Token::Name(name)) = tokens.next() else { bail!("Expected var name") };
    let name: Arc<str> = name.into();

    tokens.reset_peek();
    let ident = match tokens.peek() {
        Some(Token::LeftBracket) => {
            let Some(Token::LeftBracket) = tokens.next() else { bail!("Expected left bracket") };

            let Some(Token::Literal(length)) = tokens.next() else {
                bail!("Expected array length")
            };

            let Ok(length) = length.parse() else {
                bail!("Array length must be a positive integer")
            };

            let Some(Token::RightBracket) = tokens.next() else { bail!("Expected right bracket") };

            IdentDeclaration::Array { name, length }
        },
        _ => IdentDeclaration::Value { name },
    };

    Ok(ident)
}

fn parse_ident(tokens: &mut MultiPeek<impl Iterator<Item = Token>>) -> anyhow::Result<Ident> {
    let Some(Token::Name(name)) = tokens.next() else { bail!("Expected var name") };
    let name: Arc<str> = name.into();

    tokens.reset_peek();
    let ident = match tokens.peek() {
        Some(Token::LeftBracket) => {
            let Some(Token::LeftBracket) = tokens.next() else { bail!("Expected left bracket") };
            let index = parse_pipeline(tokens)?;
            let Some(Token::RightBracket) = tokens.next() else { bail!("Expected right bracket") };

            Ident::Array { name, index: Arc::new(index) }
        },
        _ => Ident::Var { name },
    };

    Ok(ident)
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
    let params =
        parse_ident_declaration_list!(tokens, Token::LeftParen = "(", Token::RightParen = ")")?;
    let proc = parse_proc(tokens)?;

    let func = Func { name: name.into(), params: Arc::new(params), proc: Arc::new(proc) };
    Ok(func)
}

fn parse_proc(tokens: &mut MultiPeek<impl Iterator<Item = Token>>) -> anyhow::Result<Proc> {
    let idents = parse_ident_declaration_list!(tokens, Token::Pipe = "pipe", Token::Pipe = "pipe")?;

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
    let proc = Proc { idents: Arc::new(idents), body: Arc::new(body_items) };
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
                Token::Eq | Token::LeftBracket => {
                    BodyItem::Statement(Arc::new(parse_assign(tokens)?))
                },
                Token::LeftParen => BodyItem::Statement(Arc::new(parse_call(tokens)?)),
                _ => bail!("Expected =, [, or ("),
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

    let ident = parse_ident(tokens)?;
    let Some(Token::Eq) = tokens.next() else { bail!("Expected =") };
    let pipeline = parse_pipeline(tokens)?;

    let Some(Token::Semi) = tokens.next() else { bail!("Expected semicolon") };

    let assign =
        Assign { deref_ident: deref_var, ident: Arc::new(ident), pipeline: Arc::new(pipeline) };
    let statement = Statement::Assign(Arc::new(assign));

    Ok(statement)
}

fn parse_pipeline(tokens: &mut MultiPeek<impl Iterator<Item = Token>>) -> anyhow::Result<Pipeline> {
    let initial_val = parse_value(tokens)?;

    let mut operations = Vec::<Arc<Operation>>::new();
    loop {
        tokens.reset_peek();
        if !matches!(tokens.peek(), Some(Token::Pipe)) {
            break;
        }

        let Some(Token::Pipe) = tokens.next() else { bail!("Expected pipe") };

        let opword = match tokens.next() {
            Some(Token::Deref) => Opword::Deref,
            Some(Token::Comword(comword)) => comword,
            _ => bail!("Expected comword"),
        };

        macro_rules! opword_to_op {
            ($opword:expr, [$($unary:ident),* $(,)?], [$($binary:ident),* $(,)?] $(,)?) => {
                match $opword {
                    $(
                        Opword:: $unary => Operation:: $unary,
                    )*
                    $(
                        Opword:: $binary =>
                        Operation:: $binary { operand: Arc::new(parse_value(tokens)?) },
                    )*
                }
            };
        }

        let operation = opword_to_op!(
            opword,
            [Deref, Not],
            [Add, Sub, Mul, Div, Mod, Eq, Neq, Gt, Lt, Gte, Lte, And, Or, Xor, Join],
        );

        operations.push(Arc::new(operation));
    }

    let pipeline =
        Pipeline { initial_val: Arc::new(initial_val), operations: Arc::new(operations) };

    Ok(pipeline)
}

fn parse_value(tokens: &mut MultiPeek<impl Iterator<Item = Token>>) -> anyhow::Result<Value> {
    tokens.reset_peek();
    let value = match tokens.peek() {
        Some(Token::Literal(_)) => {
            let Some(Token::Literal(val)) = tokens.next() else { bail!("Expected literal") };
            Value::Literal(val.into())
        },
        Some(Token::Ref) => {
            let Some(Token::Ref) = tokens.next() else { bail!("Expected ref") };
            let ident = parse_ident(tokens)?;
            Value::Ref(Arc::new(ident))
        },
        _ => {
            let ident = parse_ident(tokens)?;
            Value::Ident(Arc::new(ident))
        },
    };

    Ok(value)
}

fn parse_call(tokens: &mut MultiPeek<impl Iterator<Item = Token>>) -> anyhow::Result<Statement> {
    let Some(Token::Name(var)) = tokens.next() else { bail!("Expected var name") };
    let param_pipelines =
        parse_pipeline_list!(tokens, Token::LeftParen = "(", Token::RightParen = ")")?;
    let Some(Token::Semi) = tokens.next() else { bail!("Expected semicolon") };

    let statement =
        Statement::Call { func_name: var.into(), param_pipelines: Arc::new(param_pipelines) };

    Ok(statement)
}

fn parse_if(tokens: &mut MultiPeek<impl Iterator<Item = Token>>) -> anyhow::Result<BodyItem> {
    let Some(Token::If) = tokens.next() else { bail!("Expected if") };
    let cond_pipeline = parse_pipeline(tokens)?;

    let then_body_items = Arc::new(parse_body(tokens)?);

    tokens.reset_peek();
    let else_body_items = match tokens.peek() {
        Some(Token::Else) => {
            let Some(Token::Else) = tokens.next() else { bail!("Expected else") };

            tokens.reset_peek();
            let body_items = match tokens.peek() {
                Some(Token::If) => {
                    let chained_if_item = parse_if(tokens)?;
                    Vec::from([Arc::new(chained_if_item)])
                },
                _ => parse_body(tokens)?,
            };

            Some(Arc::new(body_items))
        },
        _ => None,
    };

    let statement = BodyItem::If {
        cond_pipeline: Arc::new(cond_pipeline),
        then_body: then_body_items,
        else_body: else_body_items,
    };
    Ok(statement)
}

fn parse_while(tokens: &mut MultiPeek<impl Iterator<Item = Token>>) -> anyhow::Result<BodyItem> {
    let Some(Token::While) = tokens.next() else { bail!("Expected while") };
    let cond_pipeline = parse_pipeline(tokens)?;

    let body_items = Arc::new(parse_body(tokens)?);

    let statement = BodyItem::While { cond_pipeline: Arc::new(cond_pipeline), body: body_items };
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
