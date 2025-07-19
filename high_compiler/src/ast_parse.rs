use std::{ops::Not, sync::Arc};

use anyhow::{anyhow, bail, Context};

use crate::{
    ast::{
        AddOp, AndOp, Assign, AssignExpr, BinaryParenExpr, BinaryParenExprOp, Body, BodyItem,
        CommaSeparated, Comment, Deref, DivOp, ElseItem, EqOp, Expr, Func, FunctionCall, GtOp,
        GteOp, Ident, IdentDeclaration, IfItem, JoinOp, Literal, LtOp, LteOp, Main, ModOp, MulOp,
        Name, NeqOp, NotOp, Offset, OrOp, ParenExpr, Place, PlaceHead, Proc, Program, RefExpr,
        SemiSeparated, SingleIdentDeclaration, Slice, Span, SpanIdentDeclaration, Statement, SubOp,
        TopItem, UnaryParenExpr, UnaryParenExprOp, WhileItem,
    },
    token::Token,
    token_feed::{Parse, TokenFeed},
};

macro_rules! token {
    ($token_pat:pat => $token_use:expr, $expected:expr) => {
        |t| match t {
            Some($token_pat) => Ok($token_use),
            t => Err(::anyhow::anyhow!("{}, found: {:?}", $expected, t)),
        }
    };
}

macro_rules! parse_alt {
    {
        parse $tokens:expr => $x:ident {
            $(
                $parser:expr
            ),* $(,)?
        } else $else_msg:expr
    } => {
        let mut errors = Vec::<::anyhow::Error>::new();

        $(
            match $tokens.parse() {
                Ok($x) => return Ok($parser),
                Err(e) => errors.push(e),
            }
        )*

        Err(::anyhow::anyhow!($else_msg).context(
            errors.into_iter().map(|e| e.to_string()).collect::<Vec<_>>().join(" or ").to_string(),
        ))
    };
}

impl Parse for Program {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let mut items = Vec::new();
        while tokens.is_finished().not() {
            match tokens.parse::<TopItem>() {
                Ok(item) => {
                    items.push(Arc::new(item));
                    continue;
                },
                Err(e) => {
                    return Err(anyhow!("Expected top item").context(e));
                },
            }
        }

        Ok(Self { items: Arc::new(items) })
    }
}

impl Parse for Comment {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let text = tokens
            .try_next(token!(Token::Comment(text) => text.as_str().into(), "Expected comment"))?;

        Ok(Self { text })
    }
}

impl Parse for TopItem {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        parse_alt! {
            parse tokens => x {
                Self::Main(Arc::new(x)),
                Self::Func(Arc::new(x)),
            } else {
                "Expected top item"
            }
        }
    }
}

impl Parse for Main {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Main => (), "Expected `main`"))?;
        let proc = tokens.parse::<Proc>()?;
        Ok(Self { proc: Arc::new(proc) })
    }
}

impl Parse for Func {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Func => (), "Expected `func`"))?;

        let name = tokens.parse()?;

        tokens.try_next(token!(Token::LeftParen => (), "Expected opening parenthesis"))?;
        let params = tokens.parse()?;
        tokens.try_next(token!(Token::RightParen => (), "Expected closing parenthesis"))?;

        let proc = tokens.parse()?;

        Ok(Self { name: Arc::new(name), params: Arc::new(params), proc: Arc::new(proc) })
    }
}

impl Parse for Name {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let str =
            tokens.try_next(token!(Token::Name(name) => name.as_str().into(), "Expected name"))?;
        Ok(Self { str })
    }
}

impl<T: Parse> Parse for CommaSeparated<T>
where
    anyhow::Error: From<T::Error>,
{
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let mut elements = Vec::new();

        loop {
            let Ok(element) = tokens.parse() else { break };

            elements.push(Arc::new(element));

            if tokens.try_next(token!(Token::Comma => (), "Expected comma")).is_err() {
                break;
            }
        }

        Ok(Self { elements: Arc::new(elements) })
    }
}

impl Parse for IdentDeclaration {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        parse_alt! {
            parse tokens => x {
                Self::Span(Arc::new(x)),
                Self::Single(Arc::new(x)),
            } else {
                "Expected identifier declaration"
            }
        }
    }
}

impl Parse for SingleIdentDeclaration {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let name = tokens.parse()?;
        Ok(Self { name: Arc::new(name) })
    }
}

impl Parse for SpanIdentDeclaration {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let name = tokens.parse()?;
        tokens.try_next(token!(Token::LeftBracket => (), "Expected opening bracket"))?;
        let size = tokens.parse::<Literal>()?;
        let Ok(size) = size.str.parse() else {
            return Err(anyhow!("Span identifier size must be a positive integer"));
        };

        tokens.try_next(token!(Token::RightBracket => (), "Expected closing bracket"))?;
        Ok(Self { name: Arc::new(name), size })
    }
}

impl Parse for Literal {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let str = tokens
            .try_next(token!(Token::Literal(str) => str.as_str().into(), "Expected literal"))?;
        Ok(Self { str })
    }
}

impl Parse for Proc {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Pipe => (), "Expected opening pipe"))?;
        let idents = tokens.parse()?;
        tokens.try_next(token!(Token::Pipe => (), "Expected closing pipe"))?;
        let body = tokens.parse()?;
        Ok(Self { idents: Arc::new(idents), body: Arc::new(body) })
    }
}

impl Parse for Body {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftBrace => (), "Expected opening brace"))?;

        let res = tokens.parse::<SemiSeparatedParseResult<_>>()?;

        tokens
            .try_next(token!(Token::RightBrace => (), "Expected closing brace"))
            .context(res.last_err)?;

        let items = res.semi_separated;
        Ok(Self { items: Arc::new(items) })
    }
}

struct SemiSeparatedParseResult<T> {
    semi_separated: SemiSeparated<T>,
    last_err: anyhow::Error,
}

impl<T: Parse> Parse for SemiSeparatedParseResult<T>
where
    anyhow::Error: From<T::Error>,
{
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let mut elements = Vec::new();
        let last_err = loop {
            let element = match tokens.parse() {
                Ok(element) => element,
                Err(err) => break anyhow::Error::from(err),
            };

            elements.push(Arc::new(element));
            if let Err(err) = tokens.try_next(token!(Token::Semi => (), "Expected semicolon")) {
                break err;
            }
        };

        let semi_separated = SemiSeparated { elements: Arc::new(elements) };

        Ok(Self { semi_separated, last_err })
    }
}

impl Parse for BodyItem {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        parse_alt! {
            parse tokens => x {
                Self::Statement(Arc::new(x)),
                Self::If(Arc::new(x)),
                Self::While(Arc::new(x)),
            } else {
                "Expected body item"
            }
        }
    }
}

impl Parse for IfItem {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::If => (), "Expected `if`"))?;
        let condition = tokens.parse()?;
        let then_body = tokens.parse()?;
        let else_item = tokens.parse().ok();
        Ok(Self {
            condition: Arc::new(condition),
            then_body: Arc::new(then_body),
            else_item: else_item.map(Arc::new),
        })
    }
}

impl Parse for ElseItem {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Else => (), "Expected `else`"))?;
        let body = tokens.parse()?;
        Ok(Self { body: Arc::new(body) })
    }
}

impl Parse for WhileItem {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::While => (), "Expected `while`"))?;
        let condition = tokens.parse()?;
        let body = tokens.parse()?;

        Ok(Self { condition: Arc::new(condition), body: Arc::new(body) })
    }
}

impl Parse for Statement {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        parse_alt! {
            parse tokens => x {
                Self::Assign(Arc::new(x)),
                Self::Call(Arc::new(x)),
            } else {
                "Expected statement"
            }
        }
    }
}

impl Parse for Assign {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let loc = tokens.parse()?;
        tokens.try_next(token!(Token::Eq => (), "Expected ="))?;
        let expr = tokens.parse()?;
        Ok(Self { loc: Arc::new(loc), expr: Arc::new(expr) })
    }
}

impl Parse for FunctionCall {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let func_name = tokens.parse()?;
        tokens.try_next(token!(Token::LeftParen => (), "Expected opening parenthesis"))?;
        let param_exprs = tokens.parse()?;
        tokens.try_next(token!(Token::RightParen => (), "Expected closing parenthesis"))?;
        Ok(Self { func_name: Arc::new(func_name), param_exprs: Arc::new(param_exprs) })
    }
}

impl Parse for Place {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let head = tokens.parse()?;
        let offset = tokens.parse().ok();
        Ok(Self { head: Arc::new(head), offset: offset.map(Arc::new) })
    }
}

impl Parse for PlaceHead {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        parse_alt! {
            parse tokens => x {
                Self::Ident(Arc::new(x)),
                Self::Deref(Arc::new(x)),
            } else {
                "Expected place head"
            }
        }
    }
}

impl Parse for Offset {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftBracket => (), "Expected opening bracket"))?;
        let expr = tokens.parse()?;
        tokens.try_next(token!(Token::RightBracket => (), "Expected closing bracket"))?;
        Ok(Self { expr: Arc::new(expr) })
    }
}

impl Parse for Ident {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        match tokens.try_next(token!(Token::LeftParen => (), "Expected opening parenthesis")) {
            Ok(_) => {
                let ident = tokens.parse()?;
                tokens.try_next(token!(Token::RightParen => (), "Expected closing parenthesis"))?;
                Ok(ident)
            },
            Err(_) => {
                let name = tokens.parse()?;
                Ok(Self { name: Arc::new(name) })
            },
        }
    }
}

impl Parse for Deref {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        match tokens.try_next(token!(Token::LeftParen => (), "Expected opening parenthesis")) {
            Ok(_) => {
                let deref = tokens.parse()?;
                tokens.try_next(token!(Token::RightParen => (), "Expected closing parenthesis"))?;
                Ok(deref)
            },
            Err(_) => {
                tokens.try_next(token!(Token::Asterisk => (), "Expected *"))?;
                let addr = tokens.parse()?;
                Ok(Self { addr: Arc::new(addr) })
            },
        }
    }
}

impl Parse for AssignExpr {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        parse_alt! {
            parse tokens => x {
                Self::Expr(Arc::new(x)),
                Self::Span(Arc::new(x)),
                Self::Slice(Arc::new(x)),
            } else {
                "Expected assign expression"
            }
        }
    }
}

impl Parse for Span {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftBracket => (), "Expected opening bracket"))?;
        let elements = tokens.parse()?;
        tokens.try_next(token!(Token::RightBracket => (), "Expected closing bracket"))?;
        Ok(Self { elements: Arc::new(elements) })
    }
}

impl Parse for Slice {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let place = tokens.parse()?;
        let start_in = tokens.parse::<Literal>()?;
        let Ok(start_in) = start_in.str.parse() else {
            bail!("Inclusive start of range must be a positive integer")
        };

        tokens.try_next(token!(Token::Period => (), "Expected .."))?;
        tokens.try_next(token!(Token::Period => (), "Expected .."))?;

        let end_ex = tokens.parse::<Literal>()?;
        let Ok(end_ex) = end_ex.str.parse() else {
            bail!("Exclusive end of range must be a positive integer")
        };

        Ok(Self { place: Arc::new(place), start_in, end_ex })
    }
}

impl Parse for Expr {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        parse_alt! {
            parse tokens => x {
                Self::Paren(Arc::new(x)),
                Self::Ref(Arc::new(x)),
                Self::Place(Arc::new(x)),
                Self::Literal(Arc::new(x)),
            } else {
                "Expected expression"
            }
        }
    }
}

impl Parse for RefExpr {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Ampersand => (), "Expected &"))?;
        let place = tokens.parse()?;
        Ok(Self { place: Arc::new(place) })
    }
}

impl Parse for ParenExpr {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        if let Ok(unary) = tokens.parse() {
            return Ok(Self::Unary(Arc::new(unary)));
        }

        if let Ok(binary) = tokens.parse() {
            return Ok(Self::Binary(Arc::new(binary)));
        }

        bail!("Expected unary or binary expression");
    }
}

impl Parse for UnaryParenExpr {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftParen => (), "Expected opening parenthesis"))?;
        let op = tokens.parse()?;
        let operand = tokens.parse()?;
        tokens.try_next(token!(Token::RightParen => (), "Expected closing parenthesis"))?;
        Ok(Self { op, operand: Arc::new(operand) })
    }
}

impl Parse for BinaryParenExpr {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftParen => (), "Expected opening parenthesis"))?;
        let left = tokens.parse()?;
        let op = tokens.parse()?;
        let right = tokens.parse()?;
        tokens.try_next(token!(Token::RightParen => (), "Expected closing parenthesis"))?;
        Ok(Self { op, left: Arc::new(left), right: Arc::new(right) })
    }
}

impl Parse for UnaryParenExprOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        parse_alt! {
            parse tokens => x {
                Self::Not(x),
            } else {
                "Expected unary operator"
            }
        }
    }
}

impl Parse for NotOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Bang => (), "Expected !"))?;
        Ok(Self)
    }
}

impl Parse for BinaryParenExprOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        parse_alt! {
            parse tokens => x {
                // == two chars ==

                // inequality
                Self::Eq(x),
                Self::Neq(x),
                Self::Gte(x),
                Self::Lte(x),
                // boolean
                Self::And(x),
                Self::Or(x),

                // == one char ==

                // math
                Self::Add(x),
                Self::Sub(x),
                Self::Mul(x),
                Self::Div(x),
                Self::Mod(x),
                // inequality
                Self::Gt(x),
                Self::Lt(x),
                // string
                Self::Join(x),
            } else {
                "Expected binary operator"
            }
        }
    }
}

impl Parse for AddOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Plus => (), "Expected +"))?;
        Ok(Self)
    }
}

impl Parse for SubOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Dash => (), "Expected -"))?;
        Ok(Self)
    }
}

impl Parse for MulOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Asterisk => (), "Expected *"))?;
        Ok(Self)
    }
}

impl Parse for DivOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Slash => (), "Expected /"))?;
        Ok(Self)
    }
}

impl Parse for ModOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Percent => (), "Expected %"))?;
        Ok(Self)
    }
}

impl Parse for EqOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Eq => (), "Expected =="))?;
        tokens.try_next(token!(Token::Eq => (), "Expected =="))?;
        Ok(Self)
    }
}

impl Parse for NeqOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Bang => (), "Expected !="))?;
        tokens.try_next(token!(Token::Eq => (), "Expected !="))?;
        Ok(Self)
    }
}

impl Parse for GtOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::RightAngle => (), "Expected >"))?;
        Ok(Self)
    }
}

impl Parse for LtOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftAngle => (), "Expected <"))?;
        Ok(Self)
    }
}

impl Parse for GteOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::RightAngle => (), "Expected >="))?;
        tokens.try_next(token!(Token::Eq => (), "Expected >="))?;
        Ok(Self)
    }
}

impl Parse for LteOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftAngle => (), "Expected <="))?;
        tokens.try_next(token!(Token::Eq => (), "Expected <="))?;
        Ok(Self)
    }
}

impl Parse for AndOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Ampersand => (), "Expected &&"))?;
        tokens.try_next(token!(Token::Ampersand => (), "Expected &&"))?;
        Ok(Self)
    }
}

impl Parse for OrOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Pipe => (), "Expected ||"))?;
        tokens.try_next(token!(Token::Pipe => (), "Expected ||"))?;
        Ok(Self)
    }
}

impl Parse for JoinOp {
    type Error = anyhow::Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Tilde => (), "Expected ~"))?;
        Ok(Self)
    }
}
