use std::{ops::Not, sync::Arc};

use asm_compiler::token;

use crate::{
    ast::{
        AddOp, AndOp, ArrayType, Assign, AssignExpr, BaseType, BinaryParenExpr, BinaryParenExprOp,
        Body, BodyItem, CommaSeparated, Comment, Deref, DivOp, ElseItem, EqOp, Expr, Func,
        FunctionCall, GtOp, GteOp, Ident, IdentDeclaration, IfItem, JoinOp, Literal, LtOp, LteOp,
        Main, ModOp, MulOp, Name, NeqOp, NotOp, Offset, OrOp, ParenExpr, Place, PlaceHead, Proc,
        Program, RefExpr, RefType, SemiSeparated, Slice, Span, Statement, SubOp, TopItem, Type,
        UnaryParenExpr, UnaryParenExprOp, WhileItem,
    },
    ast_error::{AstErrorKind, AstErrorSet},
    src_pos::SrcRange,
    token::{Token, TokenKind},
    token_feed::{Parse, TokenFeed},
};

macro_rules! token {
    ($token_pat:pat => $token_use:expr, $expected:expr) => {{
        |cell| match cell.res {
            Some(t) => match &t.token {
                $token_pat => Ok($token_use),
                token => Err(AstErrorSet::new_error(
                    t.range,
                    AstErrorKind::MismatchedTokenString {
                        expected: $expected.into(),
                        found: token.into(),
                    },
                )),
            },
            None => Err(AstErrorSet::new_error(
                cell.range,
                AstErrorKind::ExpectedTokenString { expected: $expected.into() },
            )),
        }
    }};
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
            tokens.try_next(token!(Token::LeftParen => (), [TokenKind::LeftParen]))?;
            let place = tokens.parse().res?;
            tokens.try_next(token!(Token::RightParen => (), [TokenKind::RightParen]))?;
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
                        item_cell.range,
                        AstErrorKind::ExpectedTopItem,
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
            .try_next(token!(Token::Comment(text) => text.as_str().into(), [TokenKind::Comment]))?;
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
        tokens.try_next(token!(Token::Main => (), [TokenKind::Main]))?;
        let proc = tokens.parse::<Proc>().res?;
        Ok(Self { proc: Arc::new(proc) })
    }
}

impl Parse for Func {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Func => (), [TokenKind::Func]))?;

        let name = tokens.parse().res?;

        tokens.try_next(token!(Token::LeftParen => (), [TokenKind::LeftParen]))?;
        let params_res = tokens.parse::<ZeroOrManyParseResult<_>>().res?;

        let proc: Proc = (|| {
            tokens.try_next(token!(Token::RightParen => (), [TokenKind::RightParen]))?;
            tokens.parse().res
        })()
        .map_err(|e: AstErrorSet| e.merge(params_res.last_err))?;

        Ok(Self {
            name: Arc::new(name),
            params: Arc::new(params_res.zero_or_many),
            proc: Arc::new(proc),
        })
    }
}

impl Parse for Name {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let str = tokens
            .try_next(token!(Token::Name(name) => name.as_str().into(), [TokenKind::Name]))?;
        Ok(Self { str })
    }
}

impl Parse for IdentDeclaration {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let name = tokens.parse().res?;
        tokens.try_next(token!(Token::Colon => (), [TokenKind::Colon]))?;
        let ty = tokens.parse().res?;
        Ok(Self { name: Arc::new(name), ty: Arc::new(ty) })
    }
}

impl Parse for Type {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        parse_alt! {
            parse tokens => x {
                Self::Base(Arc::new(x)),
                Self::Ref(Arc::new(x)),
                Self::Array(Arc::new(x)),
            } else {
                "Expected type"
            }
        }
    }
}

impl Parse for BaseType {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let name = tokens.parse().res?;
        Ok(Self { name: Arc::new(name) })
    }
}

impl Parse for RefType {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Ampersand => (), [TokenKind::Ampersand]))?;
        let ty = tokens.parse().res?;
        Ok(Self { ty: Arc::new(ty) })
    }
}

impl Parse for ArrayType {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftBracket => (), [TokenKind::LeftBracket]))?;
        let ty = tokens.parse().res?;
        tokens.try_next(token!(Token::Semi => (), [TokenKind::Semi]))?;
        let len_token = tokens.parse::<Literal>();
        let len = len_token.res?.str.parse().map_err(|_| {
            AstErrorSet::new_error(len_token.range, AstErrorKind::InvalidArrayLength)
        })?;

        tokens.try_next(token!(Token::RightBracket => (), [TokenKind::RightBracket]))?;
        Ok(Self { ty: Arc::new(ty), len })
    }
}

impl Parse for Literal {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let str = tokens
            .try_next(token!(Token::Literal(str) => str.as_str().into(), [TokenKind::Literal]))?;
        Ok(Self { str })
    }
}

impl Parse for Proc {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Pipe => (), [TokenKind::Pipe]))?;
        let idents_res = tokens.parse::<ZeroOrManyParseResult<_>>().res?;

        let body: Body = (|| {
            tokens.try_next(token!(Token::Pipe => (), [TokenKind::Pipe]))?;
            tokens.parse().res
        })()
        .map_err(|e: AstErrorSet| e.merge(idents_res.last_err))?;

        Ok(Self { idents: Arc::new(idents_res.zero_or_many), body: Arc::new(body) })
    }
}

impl Parse for Body {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftBrace => (), [TokenKind::LeftBrace]))?;

        let res = tokens.parse::<ZeroOrManyParseResult<_>>().res?;

        tokens
            .try_next(token!(Token::RightBrace => (), [TokenKind::RightBrace]))
            .map_err(|e| e.merge(res.last_err))?;

        let items = res.zero_or_many;
        Ok(Self { items: Arc::new(items) })
    }
}

struct ZeroOrManyParseResult<T> {
    zero_or_many: T,
    last_err: AstErrorSet,
}

impl<T: Parse> Parse for ZeroOrManyParseResult<SemiSeparated<T>>
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
            if let Err(err) = tokens.try_next(token!(Token::Semi => (), [TokenKind::Semi])) {
                break err;
            }
        };

        let semi_separated = SemiSeparated { elements: Arc::new(elements) };

        Ok(Self { zero_or_many: semi_separated, last_err })
    }
}

impl<T: Parse> Parse for ZeroOrManyParseResult<CommaSeparated<T>>
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
            if let Err(err) = tokens.try_next(token!(Token::Comma => (), [TokenKind::Comma])) {
                break err;
            }
        };

        let comma_separated = CommaSeparated { elements: Arc::new(elements) };

        Ok(Self { zero_or_many: comma_separated, last_err })
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
        tokens.try_next(token!(Token::If => (), [TokenKind::If]))?;
        let condition = tokens.parse().res?;
        let then_body = tokens.parse().res?;
        let else_item: Option<ElseItem> = tokens.parse().res?;
        Ok(Self {
            condition: Arc::new(condition),
            then_body: Arc::new(then_body),
            else_item: else_item.map(Arc::new),
        })
    }
}

impl Parse for Option<ElseItem> {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let Ok(_) = tokens.try_next(token!(Token::Else => (), [TokenKind::Else])) else {
            return Ok(None);
        };

        parse_alt! {
            parse tokens => x {
                Some(ElseItem { body: Arc::new(Body { items: Arc::new(SemiSeparated { elements: Arc::new(Vec::from([Arc::new(BodyItem::If(Arc::new(x)))])) }) }) }),
                Some(ElseItem { body: Arc::new(x) })
            } else {
                "Expected else item"
            }
        }
    }
}

impl Parse for WhileItem {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::While => (), [TokenKind::While]))?;
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
        tokens.try_next(token!(Token::Eq => (), [TokenKind::Eq]))?;
        let expr = tokens.parse().res?;
        Ok(Self { loc: Arc::new(loc), expr: Arc::new(expr) })
    }
}

impl Parse for FunctionCall {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let func_name = tokens.parse().res?;
        tokens.try_next(token!(Token::LeftParen => (), [TokenKind::LeftParen]))?;
        let param_exprs_res = tokens.parse::<ZeroOrManyParseResult<_>>().res?;

        tokens
            .try_next(token!(Token::RightParen => (), [TokenKind::RightParen]))
            .map_err(|e| e.merge(param_exprs_res.last_err))?;

        Ok(Self {
            func_name: Arc::new(func_name),
            param_exprs: Arc::new(param_exprs_res.zero_or_many),
        })
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
        tokens.try_next(token!(Token::LeftBracket => (), [TokenKind::LeftBracket]))?;
        let expr = tokens.parse().res?;
        tokens.try_next(token!(Token::RightBracket => (), [TokenKind::RightBracket]))?;
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
            tokens.try_next(token!(Token::Asterisk => (), [TokenKind::Asterisk]))?;
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
                Self::Slice(Arc::new(x)),
                Self::Span(Arc::new(x)),
                Self::Expr(Arc::new(x)),
            } else {
                "Expected assign expression"
            }
        }
    }
}

impl Parse for Span {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftBracket => (), [TokenKind::LeftBracket]))?;
        let elements_res = tokens.parse::<ZeroOrManyParseResult<_>>().res?;
        tokens
            .try_next(token!(Token::RightBracket => (), [TokenKind::RightBracket]))
            .map_err(|e| e.merge(elements_res.last_err))?;

        Ok(Self { elements: Arc::new(elements_res.zero_or_many) })
    }
}

impl Parse for Slice {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        let place = tokens.parse().res?;

        tokens.try_next(token!(Token::LeftBracket => (), [TokenKind::LeftBracket]))?;

        let start_in_cell = tokens.parse::<Literal>();

        let start_in = match start_in_cell.res {
            Err(_) => 0,
            Ok(start_in) => match start_in.str.parse() {
                Ok(start_in) => start_in,
                Err(_) => {
                    return Err(AstErrorSet::new_error(
                        start_in_cell.range,
                        AstErrorKind::InvalidInclRangeStart,
                    ))
                },
            },
        };

        tokens.try_next(token!(Token::Period => (), [TokenKind::Period, TokenKind::Period]))?;
        tokens.try_next(token!(Token::Period => (), [TokenKind::Period, TokenKind::Period]))?;

        let end_ex_cell = tokens.parse::<Literal>();
        let end_ex = end_ex_cell.res?;
        let Ok(end_ex) = end_ex.str.parse() else {
            return Err(AstErrorSet::new_error(
                end_ex_cell.range,
                AstErrorKind::InvalidExclRangeEnd,
            ));
        };

        tokens.try_next(token!(Token::RightBracket => (), [TokenKind::RightBracket]))?;

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
        tokens.try_next(token!(Token::Ampersand => (), [TokenKind::Ampersand]))?;
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
        tokens.try_next(token!(Token::LeftParen => (), [TokenKind::LeftParen]))?;
        let op = tokens.parse().res?;
        let operand = tokens.parse().res?;
        tokens.try_next(token!(Token::RightParen => (), [TokenKind::RightParen]))?;
        Ok(Self { op, operand: Arc::new(operand) })
    }
}

impl Parse for BinaryParenExpr {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftParen => (), [TokenKind::LeftParen]))?;
        let left = tokens.parse().res?;
        let op = tokens.parse().res?;
        let right = tokens.parse().res?;
        tokens.try_next(token!(Token::RightParen => (), [TokenKind::RightParen]))?;
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
        tokens.try_next(token!(Token::Bang => (), [TokenKind::Bang]))?;
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
        tokens.try_next(token!(Token::Plus => (), [TokenKind::Plus]))?;
        Ok(Self)
    }
}

impl Parse for SubOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Dash => (), [TokenKind::Dash]))?;
        Ok(Self)
    }
}

impl Parse for MulOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Asterisk => (), [TokenKind::Asterisk]))?;
        Ok(Self)
    }
}

impl Parse for DivOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Slash => (), [TokenKind::Slash]))?;
        Ok(Self)
    }
}

impl Parse for ModOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Percent => (), [TokenKind::Percent]))?;
        Ok(Self)
    }
}

impl Parse for EqOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Eq => (), [TokenKind::Eq, TokenKind::Eq]))?;
        tokens.try_next(token!(Token::Eq => (), [TokenKind::Eq, TokenKind::Eq]))?;
        Ok(Self)
    }
}

impl Parse for NeqOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Bang => (), [TokenKind::Bang, TokenKind::Eq]))?;
        tokens.try_next(token!(Token::Eq => (), [TokenKind::Bang, TokenKind::Eq]))?;
        Ok(Self)
    }
}

impl Parse for GtOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::RightAngle => (), [TokenKind::RightAngle]))?;
        Ok(Self)
    }
}

impl Parse for LtOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftAngle => (), [TokenKind::LeftAngle]))?;
        Ok(Self)
    }
}

impl Parse for GteOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::RightAngle => (), [TokenKind::RightAngle, TokenKind::Eq]))?;
        tokens.try_next(token!(Token::Eq => (), [TokenKind::RightAngle, TokenKind::Eq]))?;
        Ok(Self)
    }
}

impl Parse for LteOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::LeftAngle => (), [TokenKind::LeftAngle, TokenKind::Eq]))?;
        tokens.try_next(token!(Token::Eq => (), [TokenKind::LeftAngle, TokenKind::Eq]))?;
        Ok(Self)
    }
}

impl Parse for AndOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(
            token!(Token::Ampersand => (), [TokenKind::Ampersand, TokenKind::Ampersand]),
        )?;
        tokens.try_next(
            token!(Token::Ampersand => (), [TokenKind::Ampersand, TokenKind::Ampersand]),
        )?;
        Ok(Self)
    }
}

impl Parse for OrOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Pipe => (), [TokenKind::Pipe, TokenKind::Pipe]))?;
        tokens.try_next(token!(Token::Pipe => (), [TokenKind::Pipe, TokenKind::Pipe]))?;
        Ok(Self)
    }
}

impl Parse for JoinOp {
    type Error = AstErrorSet;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error> {
        tokens.try_next(token!(Token::Tilde => (), [TokenKind::Tilde]))?;
        Ok(Self)
    }
}
