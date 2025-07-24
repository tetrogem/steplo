use std::sync::Arc;

use crate::{
    compile_error::{CompileError, CompileErrorSet, GrammarError},
    grammar_ast::{
        AddOp, AndOp, ArrayType, Assign, AssignExpr, BaseType, BinaryParenExpr, BinaryParenExprOp,
        Body, BodyItem, BoolLiteral, CastExpr, CommaList, CommaListLink, Comment, Decimal, Deref,
        Digits, DivOp, ElseBodyItem, ElseIfItem, ElseItem, Empty, EqOp, Expr, FalseLiteral, Func,
        FunctionCall, GtOp, GteOp, Ident, IdentDeclaration, IfItem, JoinOp, List, ListLink,
        Literal, LtOp, LteOp, Main, Maybe, ModOp, MulOp, Name, Negative, NeqOp, NotOp, NumLiteral,
        Offset, OrOp, ParenExpr, ParensNest, ParensWrapped, Place, PlaceHead, Proc, Program,
        RefExpr, RefType, SemiList, SemiListLink, Slice, Span, Statement, StrLiteral, SubOp,
        TopItem, TransmuteExpr, TrueLiteral, Type, UnaryParenExpr, UnaryParenExprOp, WhileItem,
    },
    srced::{SrcRange, Srced},
    token::{Token, TokenKind},
    token_feed::TokenFeed,
};

trait AstParse: Sized {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self>;
}

#[derive(Debug)]
struct AstParseRes<T> {
    item: Result<Srced<T>, ()>,
    errors: CompileErrorSet,
}

macro_rules! parse_enum {
    {
        parse $tokens:ident => $x:ident {
            $(
                $parser:expr
            ),* $(,)?
        } else $else_msg:expr
    } => {
        let (item, errors) = match $tokens.try_match(|tokens| {
            let mut errors = CompileErrorSet::new();

            $(
                let res = <_ as AstParse>::parse(tokens);
                errors = errors.merge(res.errors);
                if let Ok($x) = res.item {
                    let range = $x.range;
                    return Ok(($parser, errors, range));
                }
            )*

            Err(errors)
        }) {
            Ok((item, errors, range)) => (Ok(Srced { val: item, range }), errors),
            Err(errors) => (Err(()), errors),
        };

        AstParseRes { item, errors }
    };
}

macro_rules! parse_helper {
    ([$tokens:ident, $errors:ident, $range:ident] struct $ident:ident) => {
        let $ident = {
            let res = <_ as AstParse>::parse($tokens);
            $errors = $errors.merge(res.errors);
            match res.item {
                Ok(val) => {
                    $range = Some(match $range {
                        None => val.range,
                        Some(prev) => prev.merge(val.range),
                    });

                    val
                },
                Err(()) => return Err($errors),
            }
        };
    };
    ([$tokens:ident, $errors:ident, $range:ident] match $ident:pat = $token_pat:pat => $token_use:expr; as $expected:expr) => {
        #[allow(clippy::let_unit_value)]
        let $ident = {
            let cur_range = $tokens.cur_range();
            match $tokens.try_next(|cell| match cell.res {
                Some(t) => match &t.val {
                    $token_pat => Ok(($token_use, t.range)),
                    token => Err(CompileErrorSet::new_error(
                        SrcRange::guess_pos(cur_range.end()),
                        CompileError::Grammar(GrammarError::MismatchedTokenString {
                            expected: $expected.into(),
                            found: token.into(),
                        }),
                    )),
                },
                None => Err(CompileErrorSet::new_error(
                    cell.range,
                    CompileError::Grammar(GrammarError::ExpectedTokenString {
                        expected: $expected.into(),
                    }),
                )),
            }) {
                Ok((val, range)) => {
                    $range = Some(match $range {
                        None => range,
                        Some(prev) => prev.merge(range),
                    });

                    val
                },
                Err(errors) => {
                    $errors = $errors.merge(errors);
                    return Err($errors);
                },
            }
        };
    };
    ([$tokens:ident, $errors:ident, $range:ident] return $expr:expr) => {
        return Ok(($expr, $errors, $range));
    };
}

macro_rules! parse_struct {
    {
        parse $tokens:ident;
        $([$($stuff:tt)*]);* $(;)?
    } => {
        let (item, errors) = match $tokens.try_match(|tokens| {
            let mut errors = CompileErrorSet::new();
            let mut range: Option<SrcRange> = None;

            $(
                parse_helper!([tokens, errors, range] $($stuff)*);
            )*
        }) {
            Ok((item, errors, range)) => {
                let range = range.unwrap_or_else(|| SrcRange::guess_pos($tokens.cur_range().end()));
                (Ok(Srced { val: item, range }), errors)
            },
            Err(errors) => (Err(()), errors),
        };

        AstParseRes { item, errors }
    };
}

pub fn parse_ast(tokens: &mut TokenFeed) -> Result<Srced<Program>, CompileErrorSet> {
    let res = Program::parse(tokens);

    match res.item {
        Ok(item) if tokens.is_finished() => Ok(item),
        _ => Err(res.errors),
    }
}

impl AstParse for Program {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [struct items];
            [return Self { items: Arc::new(items) }];
        }
    }
}

impl AstParse for Comment {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match text = Token::Comment(text) => text.as_str().into(); as [TokenKind::Comment]];
            [return Self { text }];
        }
    }
}

impl AstParse for TopItem {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
            parse tokens => x {
                Self::Main(Arc::new(x)),
                Self::Func(Arc::new(x)),
            } else {
                "Expected top item"
            }
        }
    }
}

impl AstParse for Main {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Main => (); as [TokenKind::Main]];
            [struct proc];
            [return Self { proc: Arc::new(proc)} ];
        }
    }
}

impl AstParse for Func {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Func => (); as [TokenKind::Func]];
            [struct name];
            [match _ = Token::LeftParen => (); as [TokenKind::LeftParen]];
            [struct params];
            [match _ = Token::RightParen => (); as [TokenKind::RightParen]];
            [struct proc];
            [return Self { name: Arc::new(name), params: Arc::new(params), proc: Arc::new(proc) }];
        }
    }
}

impl AstParse for Name {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match str = Token::Name(name) => name.as_str().into(); as [TokenKind::Name]];
            [return Self { str }]
        }
    }
}

impl AstParse for IdentDeclaration {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [struct name];
            [match _ = Token::Colon => (); as [TokenKind::Colon]];
            [struct ty];
            [return Self { name: Arc::new(name), ty: Arc::new(ty) }];
        }
    }
}

impl AstParse for Type {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
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

impl AstParse for BaseType {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [struct name];
            [return Self { name: Arc::new(name) }];
        }
    }
}

impl AstParse for RefType {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Ampersand => (); as [TokenKind::Ampersand]];
            [struct ty];
            [return Self { ty: Arc::new(ty) }];
        }
    }
}

impl AstParse for ArrayType {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::LeftBracket => (); as [TokenKind::LeftBracket]];
            [struct ty];
            [match _ = Token::Semi => (); as [TokenKind::Semi]];
            [struct len];
            [match _ = Token::RightBracket => (); as [TokenKind::RightBracket]];
            [return Self { ty: Arc::new(ty), len: Arc::new(len) }];
        }
    }
}

impl AstParse for Literal {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
            parse tokens => x {
                Self::Str(Arc::new(x)),
                Self::Num(Arc::new(x)),
                Self::Bool(Arc::new(x)),
            } else {
                "Expected literal"
            }
        }
    }
}

impl AstParse for StrLiteral {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match str = Token::String(str) => str.as_str().into(); as [TokenKind::String]];
            [return Self { str }];
        }
    }
}

impl AstParse for NumLiteral {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [struct negative];
            [struct int];
            [struct dec];
            [return Self { negative: Arc::new(negative), int: Arc::new(int), dec: Arc::new(dec) }];
        }
    }
}

impl AstParse for Decimal {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Period => (); as [TokenKind::Period]];
            [struct digits];
            [return Self { digits: Arc::new(digits) }];
        }
    }
}

impl AstParse for Digits {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match digits = Token::Digits(str) => str.as_str().into(); as [TokenKind::Digits]];
            [return Self { digits }];
        }
    }
}

impl AstParse for Negative {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Dash => (); as [TokenKind::Dash]];
            [return Self];
        }
    }
}

impl AstParse for BoolLiteral {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
            parse tokens => x {
                Self::True(Arc::new(x)),
                Self::False(Arc::new(x))
            } else {
                "Expected bool"
            }
        }
    }
}

impl AstParse for TrueLiteral {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::True => (); as [TokenKind::True]];
            [return Self];
        }
    }
}

impl AstParse for FalseLiteral {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::False => (); as [TokenKind::False]];
            [return Self];
        }
    }
}

impl AstParse for Proc {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Pipe => (); as [TokenKind::Pipe]];
            [struct idents];
            [match _ = Token::Pipe => (); as [TokenKind::Pipe]];
            [struct body];
            [return Self { idents: Arc::new(idents), body: Arc::new(body) }];
        }
    }
}

impl AstParse for Body {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::LeftBrace => (); as [TokenKind::LeftBrace]];
            [struct items];
            [match _ = Token::RightBrace => (); as [TokenKind::RightBrace]];
            [return Self { items: Arc::new(items) }];
        }
    }
}

impl<T: AstParse> AstParse for CommaList<T> {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
            parse tokens => x {
                Self::Link(Arc::new(x)),
                Self::Tail(Arc::new(x)),
                Self::Empty(Arc::new(x)),
            } else {
                "Expected comma list"
            }
        }
    }
}

impl<T: AstParse> AstParse for CommaListLink<T> {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [struct item];
            [match _ = Token::Comma => (); as [TokenKind::Comma]];
            [struct next];
            [return Self { item: Arc::new(item), next: Arc::new(next) }];
        }
    }
}

impl AstParse for Empty {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        #![expect(unused_variables, unused_mut)]
        parse_struct! {
            parse tokens;
            [return Self];
        }
    }
}

impl<T: AstParse> AstParse for SemiList<T> {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
            parse tokens => x {
                Self::Link(Arc::new(x)),
                Self::Tail(Arc::new(x)),
                Self::Empty(Arc::new(x)),
            } else {
                "Expected semi list"
            }
        }
    }
}

impl<T: AstParse> AstParse for SemiListLink<T> {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [struct item];
            [match _ = Token::Semi => (); as [TokenKind::Semi]];
            [struct next];
            [return Self { item: Arc::new(item), next: Arc::new(next) }];
        }
    }
}

impl<T: AstParse> AstParse for List<T> {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
            parse tokens => x {
                Self::Link(Arc::new(x)),
                Self::Empty(Arc::new(x)),
            } else {
                "Expected list"
            }
        }
    }
}

impl<T: AstParse> AstParse for ListLink<T> {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [struct item];
            [struct next];
            [return Self { item: Arc::new(item), next: Arc::new(next) }];
        }
    }
}

impl AstParse for BodyItem {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
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

impl AstParse for IfItem {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::If => (); as [TokenKind::If]];
            [struct condition];
            [struct then_body];
            [struct else_item];
            [return Self {
                condition: Arc::new(condition),
                then_body: Arc::new(then_body),
                else_item: Arc::new(else_item),
            }]
        }
    }
}

impl AstParse for ElseItem {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
            parse tokens => x {
                Self::Body(Arc::new(x)),
                Self::If(Arc::new(x)),
            } else {
                "Expected else item"
            }
        }
    }
}

impl AstParse for ElseBodyItem {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Else => (); as [TokenKind::Else]];
            [struct body];
            [return Self { body: Arc::new(body) }];
        }
    }
}

impl AstParse for ElseIfItem {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Else => (); as [TokenKind::Else]];
            [struct if_item];
            [return Self { if_item: Arc::new(if_item) }];
        }
    }
}

impl AstParse for WhileItem {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::While => (); as [TokenKind::While]];
            [struct condition];
            [struct body];
            [return Self { condition: Arc::new(condition), body: Arc::new(body) }];
        }
    }
}

impl AstParse for Statement {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
            parse tokens => x {
                Self::Assign(Arc::new(x)),
                Self::Call(Arc::new(x)),
            } else {
                "Expected statement"
            }
        }
    }
}

impl AstParse for Assign {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [struct place];
            [match _ = Token::Eq => (); as [TokenKind::Eq]];
            [struct expr];
            [return Self { place: Arc::new(place), expr: Arc::new(expr) }];
        }
    }
}

impl AstParse for FunctionCall {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [struct func_name];
            [match _ = Token::LeftParen => (); as [TokenKind::LeftParen]];
            [struct param_exprs];
            [match _ = Token::RightParen => (); as [TokenKind::RightParen]];
            [return Self { func_name: Arc::new(func_name), param_exprs: Arc::new(param_exprs) }];
        }
    }
}

impl AstParse for Place {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [struct head];
            [struct offset];
            [return Self { head: Arc::new(head), offset: Arc::new(offset) }];
        }
    }
}

impl<T: AstParse> AstParse for ParensNest<T> {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
            parse tokens => x {
                Self::Root(Arc::new(x)),
                Self::Wrapped(Arc::new(x)),
            } else {
                "Expected parentheses nest"
            }
        }
    }
}

impl<T: AstParse> AstParse for ParensWrapped<T> {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::LeftParen => (); as [TokenKind::LeftParen]];
            [struct item];
            [match _ = Token::RightParen => (); as [TokenKind::RightParen]];
            [return Self { item: Arc::new(item) }]
        }
    }
}

impl AstParse for PlaceHead {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
            parse tokens => x {
                Self::Ident(Arc::new(x)),
                Self::Deref(Arc::new(x)),
            } else {
                "Expected place head"
            }
        }
    }
}

impl AstParse for Offset {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::LeftBracket => (); as [TokenKind::LeftBracket]];
            [struct expr];
            [match _ = Token::RightBracket => (); as [TokenKind::RightBracket]];
            [return Self { expr: Arc::new(expr) }];
        }
    }
}

impl<T: AstParse> AstParse for Maybe<T> {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
            parse tokens => x {
                Self::Item(Arc::new(x)),
                Self::Empty(Arc::new(x)),
            } else {
                "Expected optional item"
            }
        }
    }
}

impl AstParse for Ident {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [struct name];
            [return Self { name: Arc::new(name) }];
        }
    }
}

impl AstParse for Deref {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Asterisk => (); as [TokenKind::Asterisk]];
            [struct addr];
            [return Self { addr: Arc::new(addr) }];
        }
    }
}

impl AstParse for AssignExpr {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
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

impl AstParse for Span {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::LeftBracket => (); as [TokenKind::LeftBracket]];
            [struct elements];
            [match _ = Token::RightBracket => (); as [TokenKind::RightBracket]];
            [return Self { elements: Arc::new(elements) }];
        }
    }
}

impl AstParse for Slice {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [struct place];
            [match _ = Token::LeftBracket => (); as [TokenKind::LeftBracket]];
            [struct start_in];
            [match _ = Token::Period => (); as [TokenKind::Period, TokenKind::Period]];
            [match _ = Token::Period => (); as [TokenKind::Period, TokenKind::Period]];
            [struct end_ex];
            [match _ = Token::RightBracket => (); as [TokenKind::RightBracket]];
            [return Self {
                place: Arc::new(place),
                start_in: Arc::new(start_in),
                end_ex: Arc::new(end_ex)
            }]
        }
    }
}

impl AstParse for Expr {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
            parse tokens => x {
                Self::Transmute(Arc::new(x)),
                Self::Cast(Arc::new(x)),
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

impl<T: AstParse> AstParse for TransmuteExpr<T> {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::LeftAngle => (); as [TokenKind::LeftAngle, TokenKind::LeftAngle]];
            [match _ = Token::LeftAngle => (); as [TokenKind::LeftAngle, TokenKind::LeftAngle]];
            [struct ty];
            [match _ = Token::RightAngle => (); as [TokenKind::RightAngle, TokenKind::RightAngle]];
            [match _ = Token::RightAngle => (); as [TokenKind::RightAngle, TokenKind::RightAngle]];
            [struct item];
            [return Self { ty: Arc::new(ty), item: Arc::new(item) }]
        }
    }
}

impl<T: AstParse> AstParse for CastExpr<T> {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::LeftAngle => (); as [TokenKind::LeftAngle]];
            [struct ty];
            [match _ = Token::RightAngle => (); as [TokenKind::RightAngle]];
            [struct item];
            [return Self { ty: Arc::new(ty), item: Arc::new(item) }]
        }
    }
}

impl AstParse for RefExpr {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Ampersand => (); as [TokenKind::Ampersand]];
            [struct place];
            [return Self { place: Arc::new(place) }];
        }
    }
}

impl AstParse for ParenExpr {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
            parse tokens => x {
                Self::Unary(Arc::new(x)),
                Self::Binary(Arc::new(x))
            } else {
                "Expected unary or binary expression"
            }
        }
    }
}

impl AstParse for UnaryParenExpr {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::LeftParen => (); as [TokenKind::LeftParen]];
            [struct op];
            [struct operand];
            [match _ = Token::RightParen => (); as [TokenKind::RightParen]];
            [return Self { op: Arc::new(op), operand: Arc::new(operand) }];
        }
    }
}

impl AstParse for BinaryParenExpr {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::LeftParen => (); as [TokenKind::LeftParen]];
            [struct left];
            [struct op];
            [struct right];
            [match _ = Token::RightParen => (); as [TokenKind::RightParen]];
            [return Self { op: Arc::new(op), left: Arc::new(left), right: Arc::new(right) }];
        }
    }
}

impl AstParse for UnaryParenExprOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
            parse tokens => x {
                Self::Not(Arc::new(x)),
            } else {
                "Expected unary operator"
            }
        }
    }
}

impl AstParse for NotOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Bang => (); as [TokenKind::Bang]];
            [return Self];
        }
    }
}

impl AstParse for BinaryParenExprOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_enum! {
            parse tokens => x {
                // == two chars ==

                // inequality
                Self::Eq(Arc::new(x)),
                Self::Neq(Arc::new(x)),
                Self::Gte(Arc::new(x)),
                Self::Lte(Arc::new(x)),
                // boolean
                Self::And(Arc::new(x)),
                Self::Or(Arc::new(x)),

                // == one char ==

                // math
                Self::Add(Arc::new(x)),
                Self::Sub(Arc::new(x)),
                Self::Mul(Arc::new(x)),
                Self::Div(Arc::new(x)),
                Self::Mod(Arc::new(x)),
                // inequality
                Self::Gt(Arc::new(x)),
                Self::Lt(Arc::new(x)),
                // string
                Self::Join(Arc::new(x)),
            } else {
                "Expected binary operator"
            }
        }
    }
}

impl AstParse for AddOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Plus => (); as [TokenKind::Plus]];
            [return Self];
        }
    }
}

impl AstParse for SubOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Dash => (); as [TokenKind::Dash]];
            [return Self];
        }
    }
}

impl AstParse for MulOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Asterisk => (); as [TokenKind::Asterisk]];
            [return Self];
        }
    }
}

impl AstParse for DivOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Slash => (); as [TokenKind::Slash]];
            [return Self];
        }
    }
}

impl AstParse for ModOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Percent => (); as [TokenKind::Percent]];
            [return Self];
        }
    }
}

impl AstParse for EqOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Eq => (); as [TokenKind::Eq, TokenKind::Eq]];
            [match _ = Token::Eq => (); as [TokenKind::Eq, TokenKind::Eq]];
            [return Self];
        }
    }
}

impl AstParse for NeqOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Bang => (); as [TokenKind::Bang, TokenKind::Eq]];
            [match _ = Token::Eq => (); as [TokenKind::Bang, TokenKind::Eq]];
            [return Self];
        }
    }
}

impl AstParse for GtOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::RightAngle => (); as [TokenKind::RightAngle]];
            [return Self];
        }
    }
}

impl AstParse for LtOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::LeftAngle => (); as [TokenKind::LeftAngle]];
            [return Self];
        }
    }
}

impl AstParse for GteOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::RightAngle => (); as [TokenKind::RightAngle, TokenKind::Eq]];
            [match _ = Token::Eq => (); as [TokenKind::RightAngle, TokenKind::Eq]];
            [return Self];
        }
    }
}

impl AstParse for LteOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::LeftAngle => (); as [TokenKind::LeftAngle, TokenKind::Eq]];
            [match _ = Token::Eq => (); as [TokenKind::LeftAngle, TokenKind::Eq]];
            [return Self];
        }
    }
}

impl AstParse for AndOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Ampersand => (); as [TokenKind::Ampersand, TokenKind::Ampersand]];
            [match _ = Token::Ampersand => (); as [TokenKind::Ampersand, TokenKind::Ampersand]];
            [return Self];
        }
    }
}

impl AstParse for OrOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Pipe => (); as [TokenKind::Pipe, TokenKind::Pipe]];
            [match _ = Token::Pipe => (); as [TokenKind::Pipe, TokenKind::Pipe]];
            [return Self];
        }
    }
}

impl AstParse for JoinOp {
    fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
        parse_struct! {
            parse tokens;
            [match _ = Token::Tilde => (); as [TokenKind::Tilde]];
            [return Self];
        }
    }
}
