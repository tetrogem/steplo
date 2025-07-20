use std::{ops::Not, sync::Arc};

use crate::{
    ast::{
        AddOp, AndOp, Assign, AssignExpr, BinaryParenExpr, BinaryParenExprOp, Body, BodyItem,
        CommaSeparated, Comment, Deref, DivOp, ElseItem, EqOp, Expr, Func, FunctionCall, GtOp,
        GteOp, Ident, IdentDeclaration, IfItem, JoinOp, Literal, LtOp, LteOp, Main, ModOp, MulOp,
        Name, NeqOp, NotOp, Offset, OrOp, ParenExpr, Place, PlaceHead, Proc, Program, RefExpr,
        SemiSeparated, SingleIdentDeclaration, Slice, Span, SpanIdentDeclaration, Statement, SubOp,
        TopItem, UnaryParenExpr, UnaryParenExprOp, WhileItem,
    },
    ast_error::AstErrorSet,
    src_pos::SrcRange,
    token::Token,
    token_feed::{Parse, TokenFeed},
};

macro_rules! token {
    ($token_pat:pat => $token_use:expr, $expected:expr) => {
        |cell| match cell.res {
            Some(t) => match &t.token {
                $token_pat => Ok($token_use),
                token => Err(AstErrorSet::new_error(
                    t.range,
                    format!("{}, found: {:?}", $expected, token),
                )),
            },
            None => Err(AstErrorSet::new_error(
                SrcRange::new_zero_len(cell.pos),
                format!("{}, found end of document", $expected),
            )),
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
        let mut error = AstErrorSet::new();

        $(
            match $tokens.parse().res {
                Ok($x) => return Ok($parser),
                Err(e) => error = error.merge(e),
            }
        )*

        Err(error)
    };
}

macro_rules! parse_optional_parens {
    ($tokens:expr => $code:expr) => {
        let tokens = $tokens;

        let mut errors = AstErrorSet::new();

        // we need to check unwrapped first, otherwise some valid trees will error
        let unwrapped_res = tokens.try_match(|tokens| {
            $code(tokens)
        });

        match unwrapped_res {
            Ok(value) => return Ok(value),
            Err(e) => errors = errors.merge(e),
        }

        let wrapped_res = tokens.try_match(|tokens| {
            tokens.try_next(token!(Token::LeftParen => (), "Expected opening parenthesis"))?;
            let place = tokens.parse().res?;
            tokens.try_next(token!(Token::RightParen => (), "Expected closing parenthesis"))?;
            Ok(place)
        });

        match wrapped_res {
            Ok(value) => return Ok(value),
            Err(e) => errors = errors.merge(e),
        }

        Err(errors)
    };
}

impl Parse for Program {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let mut items = Vec::new();
        while tokens.is_finished().not() {
            let item_cell = tokens.parse::<TopItem>();
            match item_cell.res {
                Ok(item) => {
                    items.push(Arc::new(item));
                    continue;
                },
                Err(e) => {
                    return Err(e.merge(AstErrorSet::new_error(
                        SrcRange::new_zero_len(item_cell.pos),
                        "Expected top item",
                    )));
                },
            }
        }

        Ok(Self { items: Arc::new(items) })
    }
}

impl Parse for Comment {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let text = tokens
            .try_next(token!(Token::Comment(text) => text.as_str().into(), "Expected comment"))?;
        Ok(Self { text })
    }
}

impl Parse for TopItem {
    type Error = AstErrorSet;

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
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Main => (), "Expected `main`"))?;
        let proc = tokens.parse::<Proc>().res?;
        Ok(Self { proc: Arc::new(proc) })
    }
}

impl Parse for Func {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Func => (), "Expected `func`"))?;

        let name = tokens.parse().res?;

        tokens.try_next(token!(Token::LeftParen => (), "Expected opening parenthesis"))?;
        let params = tokens.parse().res?;
        tokens.try_next(token!(Token::RightParen => (), "Expected closing parenthesis"))?;

        let proc = tokens.parse().res?;

        Ok(Self { name: Arc::new(name), params: Arc::new(params), proc: Arc::new(proc) })
    }
}

impl Parse for Name {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let str =
            tokens.try_next(token!(Token::Name(name) => name.as_str().into(), "Expected name"))?;
        Ok(Self { str })
    }
}

impl<T: Parse> Parse for CommaSeparated<T>
where
    AstErrorSet: From<T::Error>,
{
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let mut elements = Vec::new();

        loop {
            let Ok(element) = tokens.parse().res else { break };

            elements.push(Arc::new(element));

            if tokens.try_next(token!(Token::Comma => (), "Expected comma")).is_err() {
                break;
            }
        }

        Ok(Self { elements: Arc::new(elements) })
    }
}

impl Parse for IdentDeclaration {
    type Error = AstErrorSet;

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
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let name = tokens.parse().res?;
        Ok(Self { name: Arc::new(name) })
    }
}

impl Parse for SpanIdentDeclaration {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let name = tokens.parse().res?;
        tokens.try_next(token!(Token::LeftBracket => (), "Expected opening bracket"))?;

        let size_cell = tokens.parse::<Literal>();
        let size = size_cell.res?;
        let Ok(size) = size.str.parse() else {
            return Err(AstErrorSet::new_error(
                SrcRange::new_zero_len(size_cell.pos),
                "Span identifier size must be a positive integer",
            ));
        };

        tokens.try_next(token!(Token::RightBracket => (), "Expected closing bracket"))?;
        Ok(Self { name: Arc::new(name), size })
    }
}

impl Parse for Literal {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let str = tokens
            .try_next(token!(Token::Literal(str) => str.as_str().into(), "Expected literal"))?;
        Ok(Self { str })
    }
}

impl Parse for Proc {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Pipe => (), "Expected opening pipe"))?;
        let idents = tokens.parse().res?;
        tokens.try_next(token!(Token::Pipe => (), "Expected closing pipe"))?;
        let body = tokens.parse().res?;
        Ok(Self { idents: Arc::new(idents), body: Arc::new(body) })
    }
}

impl Parse for Body {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftBrace => (), "Expected opening brace"))?;

        let res = tokens.parse::<SemiSeparatedParseResult<_>>().res?;

        tokens
            .try_next(token!(Token::RightBrace => (), "Expected closing brace"))
            .map_err(|e| e.merge(res.last_err))?;

        let items = res.semi_separated;
        Ok(Self { items: Arc::new(items) })
    }
}

struct SemiSeparatedParseResult<T> {
    semi_separated: SemiSeparated<T>,
    last_err: AstErrorSet,
}

impl<T: Parse> Parse for SemiSeparatedParseResult<T>
where
    AstErrorSet: From<T::Error>,
{
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let mut elements = Vec::new();
        let last_err = loop {
            let element = match tokens.parse().res {
                Ok(element) => element,
                Err(err) => break AstErrorSet::from(err),
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
    type Error = AstErrorSet;

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
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::If => (), "Expected `if`"))?;
        let condition = tokens.parse().res?;
        let then_body = tokens.parse().res?;
        let else_item = tokens.parse().res.ok();
        Ok(Self {
            condition: Arc::new(condition),
            then_body: Arc::new(then_body),
            else_item: else_item.map(Arc::new),
        })
    }
}

impl Parse for ElseItem {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Else => (), "Expected `else`"))?;
        let body = tokens.parse().res?;
        Ok(Self { body: Arc::new(body) })
    }
}

impl Parse for WhileItem {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::While => (), "Expected `while`"))?;
        let condition = tokens.parse().res?;
        let body = tokens.parse().res?;

        Ok(Self { condition: Arc::new(condition), body: Arc::new(body) })
    }
}

impl Parse for Statement {
    type Error = AstErrorSet;

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
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let loc = tokens.parse().res?;
        tokens.try_next(token!(Token::Eq => (), "Expected ="))?;
        let expr = tokens.parse().res?;
        Ok(Self { loc: Arc::new(loc), expr: Arc::new(expr) })
    }
}

impl Parse for FunctionCall {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let func_name = tokens.parse().res?;
        tokens.try_next(token!(Token::LeftParen => (), "Expected opening parenthesis"))?;
        let param_exprs = tokens.parse().res?;
        tokens.try_next(token!(Token::RightParen => (), "Expected closing parenthesis"))?;
        Ok(Self { func_name: Arc::new(func_name), param_exprs: Arc::new(param_exprs) })
    }
}

impl Parse for Place {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        parse_optional_parens! { tokens => |tokens: &mut TokenFeed| {
            let head = tokens.parse().res?;
            let offset = tokens.parse().res.ok();
            Ok(Self { head: Arc::new(head), offset: offset.map(Arc::new) })
        }}
    }
}

impl Parse for PlaceHead {
    type Error = AstErrorSet;

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
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftBracket => (), "Expected opening bracket"))?;
        let expr = tokens.parse().res?;
        tokens.try_next(token!(Token::RightBracket => (), "Expected closing bracket"))?;
        Ok(Self { expr: Arc::new(expr) })
    }
}

impl Parse for Ident {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        parse_optional_parens! { tokens => |tokens: &mut TokenFeed| {
            let name = tokens.parse().res?;
            Ok(Self { name: Arc::new(name) })
        }}
    }
}

impl Parse for Deref {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        parse_optional_parens! { tokens => |tokens: &mut TokenFeed| {
            tokens.try_next(token!(Token::Asterisk => (), "Expected *"))?;
            let addr = tokens.parse().res?;
            Ok(Self { addr: Arc::new(addr) })
        }}
    }
}

impl Parse for AssignExpr {
    type Error = AstErrorSet;

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
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftBracket => (), "Expected opening bracket"))?;
        let elements = tokens.parse().res?;
        tokens.try_next(token!(Token::RightBracket => (), "Expected closing bracket"))?;
        Ok(Self { elements: Arc::new(elements) })
    }
}

impl Parse for Slice {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let place = tokens.parse().res?;

        let start_in_cell = tokens.parse::<Literal>();
        let start_in = start_in_cell.res?;
        let Ok(start_in) = start_in.str.parse() else {
            return Err(AstErrorSet::new_error(
                SrcRange::new_zero_len(start_in_cell.pos),
                "Inclusive start of range must be a positive integer",
            ));
        };

        tokens.try_next(token!(Token::Period => (), "Expected .."))?;
        tokens.try_next(token!(Token::Period => (), "Expected .."))?;

        let end_ex_cell = tokens.parse::<Literal>();
        let end_ex = end_ex_cell.res?;
        let Ok(end_ex) = end_ex.str.parse() else {
            return Err(AstErrorSet::new_error(
                SrcRange::new_zero_len(end_ex_cell.pos),
                "Exclusive end of range must be a positive integer",
            ));
        };

        Ok(Self { place: Arc::new(place), start_in, end_ex })
    }
}

impl Parse for Expr {
    type Error = AstErrorSet;

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
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Ampersand => (), "Expected &"))?;
        let place = tokens.parse().res?;
        Ok(Self { place: Arc::new(place) })
    }
}

impl Parse for ParenExpr {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        parse_alt! {
            parse tokens => x {
                Self::Unary(Arc::new(x)),
                Self::Binary(Arc::new(x))
            } else {
                "Expected unary or binary expression"
            }
        }
    }
}

impl Parse for UnaryParenExpr {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftParen => (), "Expected opening parenthesis"))?;
        let op = tokens.parse().res?;
        let operand = tokens.parse().res?;
        tokens.try_next(token!(Token::RightParen => (), "Expected closing parenthesis"))?;
        Ok(Self { op, operand: Arc::new(operand) })
    }
}

impl Parse for BinaryParenExpr {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftParen => (), "Expected opening parenthesis"))?;
        let left = tokens.parse().res?;
        let op = tokens.parse().res?;
        let right = tokens.parse().res?;
        tokens.try_next(token!(Token::RightParen => (), "Expected closing parenthesis"))?;
        Ok(Self { op, left: Arc::new(left), right: Arc::new(right) })
    }
}

impl Parse for UnaryParenExprOp {
    type Error = AstErrorSet;

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
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Bang => (), "Expected !"))?;
        Ok(Self)
    }
}

impl Parse for BinaryParenExprOp {
    type Error = AstErrorSet;

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
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Plus => (), "Expected +"))?;
        Ok(Self)
    }
}

impl Parse for SubOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Dash => (), "Expected -"))?;
        Ok(Self)
    }
}

impl Parse for MulOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Asterisk => (), "Expected *"))?;
        Ok(Self)
    }
}

impl Parse for DivOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Slash => (), "Expected /"))?;
        Ok(Self)
    }
}

impl Parse for ModOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Percent => (), "Expected %"))?;
        Ok(Self)
    }
}

impl Parse for EqOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Eq => (), "Expected =="))?;
        tokens.try_next(token!(Token::Eq => (), "Expected =="))?;
        Ok(Self)
    }
}

impl Parse for NeqOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Bang => (), "Expected !="))?;
        tokens.try_next(token!(Token::Eq => (), "Expected !="))?;
        Ok(Self)
    }
}

impl Parse for GtOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::RightAngle => (), "Expected >"))?;
        Ok(Self)
    }
}

impl Parse for LtOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftAngle => (), "Expected <"))?;
        Ok(Self)
    }
}

impl Parse for GteOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::RightAngle => (), "Expected >="))?;
        tokens.try_next(token!(Token::Eq => (), "Expected >="))?;
        Ok(Self)
    }
}

impl Parse for LteOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftAngle => (), "Expected <="))?;
        tokens.try_next(token!(Token::Eq => (), "Expected <="))?;
        Ok(Self)
    }
}

impl Parse for AndOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Ampersand => (), "Expected &&"))?;
        tokens.try_next(token!(Token::Ampersand => (), "Expected &&"))?;
        Ok(Self)
    }
}

impl Parse for OrOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Pipe => (), "Expected ||"))?;
        tokens.try_next(token!(Token::Pipe => (), "Expected ||"))?;
        Ok(Self)
    }
}

impl Parse for JoinOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Tilde => (), "Expected ~"))?;
        Ok(Self)
    }
}
