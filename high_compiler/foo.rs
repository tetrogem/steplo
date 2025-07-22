#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
use std::{
    fs::{self, File},
    io::Read, path::Path,
};
use clap::Parser;
use compile::compile;
use grammar_ast::parse;
use link::link;
use shared::{time, time_total, write_json};
use token::tokenize;
use crate::ast_error::report_ast_errors;
mod ast_error {
    use std::{
        collections::{BTreeSet, HashSet},
        path::Path, sync::Arc,
    };
    use colored::Colorize;
    use itertools::Itertools;
    use crate::{src_pos::SrcRange, token::TokenKind};
    pub struct AstErrorSet {
        errors: Vec<AstError>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for AstErrorSet {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "AstErrorSet",
                "errors",
                &&self.errors,
            )
        }
    }
    #[automatically_derived]
    impl ::core::default::Default for AstErrorSet {
        #[inline]
        fn default() -> AstErrorSet {
            AstErrorSet {
                errors: ::core::default::Default::default(),
            }
        }
    }
    pub struct AstError {
        kind: AstErrorKind,
        range: SrcRange,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for AstError {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "AstError",
                "kind",
                &self.kind,
                "range",
                &&self.range,
            )
        }
    }
    pub enum AstErrorKind {
        MismatchedTokenString { expected: Arc<[TokenKind]>, found: TokenKind },
        ExpectedTokenString { expected: Arc<[TokenKind]> },
        ExpectedTopItem,
        InvalidArrayLength,
        InvalidInclRangeStart,
        InvalidExclRangeEnd,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for AstErrorKind {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                AstErrorKind::MismatchedTokenString {
                    expected: __self_0,
                    found: __self_1,
                } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "MismatchedTokenString",
                        "expected",
                        __self_0,
                        "found",
                        &__self_1,
                    )
                }
                AstErrorKind::ExpectedTokenString { expected: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "ExpectedTokenString",
                        "expected",
                        &__self_0,
                    )
                }
                AstErrorKind::ExpectedTopItem => {
                    ::core::fmt::Formatter::write_str(f, "ExpectedTopItem")
                }
                AstErrorKind::InvalidArrayLength => {
                    ::core::fmt::Formatter::write_str(f, "InvalidArrayLength")
                }
                AstErrorKind::InvalidInclRangeStart => {
                    ::core::fmt::Formatter::write_str(f, "InvalidInclRangeStart")
                }
                AstErrorKind::InvalidExclRangeEnd => {
                    ::core::fmt::Formatter::write_str(f, "InvalidExclRangeEnd")
                }
            }
        }
    }
    impl AstErrorSet {
        pub fn new() -> Self {
            Self::default()
        }
        pub fn new_error(range: SrcRange, kind: AstErrorKind) -> Self {
            let err = AstError { range, kind };
            AstErrorSet {
                errors: Vec::from([err]),
            }
        }
        pub fn merge(self, other: AstErrorSet) -> Self {
            Self {
                errors: self.errors.into_iter().chain(other.errors).collect(),
            }
        }
    }
    enum CollapsedAstErrorKind {
        MismatchedTokenString { expected: HashSet<Arc<[TokenKind]>>, found: TokenKind },
        ExpectedTokenString { expected: HashSet<Arc<[TokenKind]>> },
        ExpectedTopItem,
        InvalidArrayLength,
        InvalidInclRangeStart,
        InvalidExclRangeEnd,
    }
    impl CollapsedAstErrorKind {
        pub fn try_collapse(&mut self, other: AstErrorKind) -> Result<(), AstErrorKind> {
            match (self, &other) {
                (
                    Self::MismatchedTokenString { expected, found },
                    AstErrorKind::MismatchedTokenString {
                        expected: other_expected,
                        found: other_found,
                    },
                ) => {
                    if *found != *other_found {
                        return Err(other);
                    }
                    expected.insert(other_expected.clone());
                    return Ok(());
                }
                (
                    Self::ExpectedTokenString { expected },
                    AstErrorKind::ExpectedTokenString { expected: other_expected },
                ) => {
                    expected.insert(other_expected.clone());
                    return Ok(());
                }
                (Self::ExpectedTopItem, AstErrorKind::ExpectedTopItem) => return Ok(()),
                (Self::InvalidArrayLength, AstErrorKind::InvalidArrayLength) => {
                    return Ok(());
                }
                (Self::InvalidInclRangeStart, AstErrorKind::InvalidInclRangeStart) => {
                    return Ok(());
                }
                (Self::InvalidExclRangeEnd, AstErrorKind::InvalidExclRangeEnd) => {
                    return Ok(());
                }
                _ => {}
            }
            Err(other)
        }
        pub fn to_msg(&self) -> String {
            fn tokens_to_string(tokens: &[TokenKind]) -> String {
                enum TokenString {
                    Keyword(&'static str),
                    Punctuation(&'static str),
                    Generic(&'static str),
                }
                fn token_to_string(token: &TokenKind) -> TokenString {
                    match token {
                        TokenKind::Ampersand => TokenString::Punctuation("&"),
                        TokenKind::Asterisk => TokenString::Punctuation("*"),
                        TokenKind::Bang => TokenString::Punctuation("!"),
                        TokenKind::Colon => TokenString::Punctuation(":"),
                        TokenKind::Comma => TokenString::Punctuation(","),
                        TokenKind::Comment => TokenString::Generic("comment"),
                        TokenKind::Dash => TokenString::Punctuation("-"),
                        TokenKind::Else => TokenString::Keyword("else"),
                        TokenKind::Eq => TokenString::Punctuation("="),
                        TokenKind::Func => TokenString::Keyword("func"),
                        TokenKind::If => TokenString::Keyword("if"),
                        TokenKind::LeftAngle => TokenString::Punctuation("<"),
                        TokenKind::LeftBrace => TokenString::Punctuation("{"),
                        TokenKind::LeftBracket => TokenString::Punctuation("["),
                        TokenKind::LeftParen => TokenString::Punctuation("("),
                        TokenKind::Literal => TokenString::Generic("literal"),
                        TokenKind::Main => TokenString::Keyword("main"),
                        TokenKind::Name => TokenString::Generic("name"),
                        TokenKind::Semi => TokenString::Punctuation(";"),
                        TokenKind::RightParen => TokenString::Punctuation(")"),
                        TokenKind::RightBrace => TokenString::Punctuation("}"),
                        TokenKind::Pipe => TokenString::Punctuation("|"),
                        TokenKind::While => TokenString::Keyword("while"),
                        TokenKind::RightBracket => TokenString::Punctuation("]"),
                        TokenKind::Period => TokenString::Punctuation("."),
                        TokenKind::Slash => TokenString::Punctuation("/"),
                        TokenKind::Plus => TokenString::Punctuation("+"),
                        TokenKind::Tilde => TokenString::Punctuation("~"),
                        TokenKind::RightAngle => TokenString::Punctuation(">"),
                        TokenKind::Percent => TokenString::Punctuation("%"),
                    }
                }
                enum FmtTokenString {
                    Verbatim(String),
                    Generic(String),
                }
                fn tokens_to_fts(tokens: &[TokenKind]) -> Vec<FmtTokenString> {
                    let mut fmt_token_strings = Vec::new();
                    let mut cur_token_string: Option<FmtTokenString> = None;
                    for ts in tokens.iter().map(token_to_string) {
                        cur_token_string = Some(
                            match cur_token_string {
                                None => {
                                    match ts {
                                        TokenString::Generic(s) => FmtTokenString::Generic(s.into()),
                                        TokenString::Keyword(s) => {
                                            FmtTokenString::Verbatim(s.into())
                                        }
                                        TokenString::Punctuation(s) => {
                                            FmtTokenString::Verbatim(s.into())
                                        }
                                    }
                                }
                                Some(generic @ FmtTokenString::Generic(_)) => {
                                    fmt_token_strings.push(generic);
                                    match ts {
                                        TokenString::Generic(s) => FmtTokenString::Generic(s.into()),
                                        TokenString::Keyword(s) => {
                                            FmtTokenString::Verbatim(s.into())
                                        }
                                        TokenString::Punctuation(s) => {
                                            FmtTokenString::Verbatim(s.into())
                                        }
                                    }
                                }
                                Some(FmtTokenString::Verbatim(verbatim)) => {
                                    match ts {
                                        TokenString::Generic(s) => {
                                            fmt_token_strings.push(FmtTokenString::Verbatim(verbatim));
                                            FmtTokenString::Generic(s.into())
                                        }
                                        TokenString::Keyword(s) => {
                                            FmtTokenString::Verbatim(
                                                ::alloc::__export::must_use({
                                                    let res = ::alloc::fmt::format(
                                                        format_args!("{0} {1}", verbatim, s),
                                                    );
                                                    res
                                                }),
                                            )
                                        }
                                        TokenString::Punctuation(s) => {
                                            FmtTokenString::Verbatim(
                                                ::alloc::__export::must_use({
                                                    let res = ::alloc::fmt::format(
                                                        format_args!("{0}{1}", verbatim, s),
                                                    );
                                                    res
                                                }),
                                            )
                                        }
                                    }
                                }
                            },
                        );
                    }
                    if let Some(ts) = cur_token_string {
                        fmt_token_strings.push(ts);
                    }
                    fmt_token_strings
                }
                let token_string = tokens_to_fts(tokens)
                    .into_iter()
                    .map(|ts| match ts {
                        FmtTokenString::Generic(s) => s,
                        FmtTokenString::Verbatim(s) => {
                            ::alloc::__export::must_use({
                                let res = ::alloc::fmt::format(format_args!("`{0}`", s));
                                res
                            })
                        }
                    })
                    .join("");
                token_string
            }
            fn token_strings_to_string<'a>(
                strings: impl Iterator<Item = &'a [TokenKind]>,
            ) -> String {
                strings.map(tokens_to_string).sorted().join(", ")
            }
            match self {
                Self::MismatchedTokenString { expected, found } => {
                    ::alloc::__export::must_use({
                        let res = ::alloc::fmt::format(
                            format_args!(
                                "Expected {0}; Found: {1}",
                                token_strings_to_string(expected.iter().map(AsRef::as_ref)),
                                tokens_to_string(&[*found]),
                            ),
                        );
                        res
                    })
                }
                Self::ExpectedTokenString { expected } => {
                    ::alloc::__export::must_use({
                        let res = ::alloc::fmt::format(
                            format_args!(
                                "Expected {0}; Found end of document",
                                token_strings_to_string(expected.iter().map(AsRef::as_ref)),
                            ),
                        );
                        res
                    })
                }
                Self::ExpectedTopItem => "Expected top item".into(),
                Self::InvalidArrayLength => {
                    "Array length should be a positive integer".into()
                }
                Self::InvalidInclRangeStart => {
                    "Inclusive start of range must be a positive integer".into()
                }
                Self::InvalidExclRangeEnd => {
                    "Exclusive end of range must be a positive integer".into()
                }
            }
        }
    }
    impl From<AstErrorKind> for CollapsedAstErrorKind {
        fn from(value: AstErrorKind) -> Self {
            match value {
                AstErrorKind::MismatchedTokenString { expected, found } => {
                    Self::MismatchedTokenString {
                        expected: HashSet::from([expected]),
                        found,
                    }
                }
                AstErrorKind::ExpectedTokenString { expected } => {
                    Self::ExpectedTokenString {
                        expected: HashSet::from([expected]),
                    }
                }
                AstErrorKind::ExpectedTopItem => Self::ExpectedTopItem,
                AstErrorKind::InvalidArrayLength => Self::InvalidArrayLength,
                AstErrorKind::InvalidInclRangeStart => Self::InvalidInclRangeStart,
                AstErrorKind::InvalidExclRangeEnd => Self::InvalidExclRangeEnd,
            }
        }
    }
    struct CollapsedAstError {
        kinds: Vec<CollapsedAstErrorKind>,
        range: SrcRange,
    }
    fn collapse_errors(errors: Vec<AstError>) -> Option<CollapsedAstError> {
        let range = errors.first().map(|e| e.range)?;
        let mut kinds = Vec::<CollapsedAstErrorKind>::new();
        'error: for error in errors {
            let mut kind = error.kind;
            for collapsed in &mut kinds {
                match collapsed.try_collapse(kind) {
                    Ok(()) => continue 'error,
                    Err(k) => kind = k,
                }
            }
            kinds.push(kind.into());
        }
        Some(CollapsedAstError { range, kinds })
    }
    pub fn report_ast_errors(code: &str, code_path: &Path, set: AstErrorSet) {
        let errors = set.errors.into_iter().max_set_by_key(|e| e.range);
        let Some(error) = collapse_errors(errors) else { return };
        {
            ::std::io::_print(format_args!("{0}\n", "Error!".red().bold()));
        };
        for err in &error.kinds {
            let src_info = ::alloc::__export::must_use({
                    let res = ::alloc::fmt::format(
                        format_args!(
                            "{0}:{1}:{2}",
                            code_path.to_string_lossy(),
                            error.range.start.line,
                            error.range.start.col,
                        ),
                    );
                    res
                })
                .normal();
            {
                ::std::io::_print(
                    format_args!(
                        "{0} {1} {2} {3}\n",
                        "@".blue(),
                        src_info,
                        "-->".blue(),
                        err.to_msg(),
                    ),
                );
            };
        }
        let nearby_lines = code
            .lines()
            .enumerate()
            .map(|(i, line)| (i + 1, line))
            .skip(error.range.start.line.saturating_sub(2).saturating_sub(1))
            .take(error.range.start.line.min(3))
            .collect_vec();
        let Some(max_line_number) = nearby_lines.iter().map(|(i, _)| i.to_string()).max()
        else {
            return;
        };
        let line_numbers_width = max_line_number.len() + 1;
        let empty_sidebar = ::alloc::__export::must_use({
                let res = ::alloc::fmt::format(
                    format_args!("{0:>1$} |", "", line_numbers_width),
                );
                res
            })
            .blue()
            .bold();
        {
            ::std::io::_print(format_args!("{0}\n", empty_sidebar));
        };
        for (i, line) in nearby_lines {
            let sidebar = ::alloc::__export::must_use({
                let res = ::alloc::fmt::format(
                    format_args!("{0:>1$} |", i, line_numbers_width),
                );
                res
            });
            {
                ::std::io::_print(
                    format_args!("{0}  {1}\n", sidebar.blue().bold(), line),
                );
            };
        }
        {
            ::std::io::_print(
                format_args!(
                    "{0}  {1}{2}\n",
                    empty_sidebar,
                    " ".repeat(error.range.start.col.saturating_sub(1)),
                    "^"
                        .repeat(
                            1 + error.range.end.col.saturating_sub(error.range.start.col),
                        )
                        .red(),
                ),
            );
        };
    }
}
mod ast_parse {
    use std::sync::Arc;
    use crate::{
        ast_error::{AstErrorKind, AstErrorSet},
        grammar_ast::{
            AddOp, AndOp, ArrayType, Assign, AssignExpr, BaseType, BinaryParenExpr,
            BinaryParenExprOp, Body, BodyItem, CommaList, CommaListLink, Comment, Deref,
            DivOp, ElseBodyItem, ElseIfItem, ElseItem, Empty, EqOp, Expr, Func,
            FunctionCall, GtOp, GteOp, Ident, IdentDeclaration, IfItem, JoinOp, List,
            ListLink, Literal, LtOp, LteOp, Main, Maybe, ModOp, MulOp, Name, NeqOp,
            NotOp, Offset, OrOp, ParenExpr, ParensNest, ParensWrapped, Place, PlaceHead,
            Proc, Program, RefExpr, RefType, SemiList, SemiListLink, Slice, Span,
            Statement, SubOp, TopItem, Type, UnaryParenExpr, UnaryParenExprOp, WhileItem,
        },
        token::{Token, TokenKind},
        token_feed::TokenFeed,
    };
    trait AstParse: Sized {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self>;
    }
    struct AstParseRes<T> {
        item: Result<T, ()>,
        errors: AstErrorSet,
    }
    #[automatically_derived]
    impl<T: ::core::fmt::Debug> ::core::fmt::Debug for AstParseRes<T> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "AstParseRes",
                "item",
                &self.item,
                "errors",
                &&self.errors,
            )
        }
    }
    pub fn parse_ast(tokens: &mut TokenFeed) -> Result<Program, AstErrorSet> {
        let res = Program::parse(tokens);
        match res.item {
            Ok(item) if tokens.is_finished() => Ok(item),
            _ => Err(res.errors),
        }
    }
    impl AstParse for Program {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let items = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((Self { items: Arc::new(items) }, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Comment {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let text = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Comment(text) => Ok(text.as_str().into()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Comment].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Comment].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self { text }, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for TopItem {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Main(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Func(Arc::new(x)), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Main {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Main => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Main].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Main].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let proc = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((Self { proc: Arc::new(proc) }, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Func {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Func => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Func].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Func].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let name = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::LeftParen => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::LeftParen].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::LeftParen].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let params = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::RightParen => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::RightParen].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::RightParen].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let proc = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((
                        Self {
                            name: Arc::new(name),
                            params: Arc::new(params),
                            proc: Arc::new(proc),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Name {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let str = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Name(name) => Ok(name.as_str().into()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Name].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Name].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self { str }, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for IdentDeclaration {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let name = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Colon => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Colon].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Colon].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let ty = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((
                        Self {
                            name: Arc::new(name),
                            ty: Arc::new(ty),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Type {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Base(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Ref(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Array(Arc::new(x)), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for BaseType {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let name = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((Self { name: Arc::new(name) }, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for RefType {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Ampersand => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Ampersand].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Ampersand].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let ty = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((Self { ty: Arc::new(ty) }, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for ArrayType {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::LeftBracket => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::LeftBracket].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::LeftBracket].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let ty = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Semi => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Semi].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Semi].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let len = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::RightBracket => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::RightBracket].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::RightBracket].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((
                        Self {
                            ty: Arc::new(ty),
                            len: Arc::new(len),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Literal {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let str = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Literal(str) => Ok(str.as_str().into()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Literal].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Literal].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self { str }, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Proc {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Pipe => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Pipe].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Pipe].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let idents = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Pipe => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Pipe].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Pipe].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let body = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((
                        Self {
                            idents: Arc::new(idents),
                            body: Arc::new(body),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Body {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::LeftBrace => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::LeftBrace].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::LeftBrace].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let items = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::RightBrace => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::RightBrace].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::RightBrace].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self { items: Arc::new(items) }, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl<T: AstParse> AstParse for CommaList<T> {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Link(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Tail(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Empty(Arc::new(x)), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl<T: AstParse> AstParse for CommaListLink<T> {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let item = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Comma => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Comma].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Comma].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let next = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((
                        Self {
                            item: Arc::new(item),
                            next: Arc::new(next),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Empty {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            #![expect(unused_variables, unused_mut)]
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl<T: AstParse> AstParse for SemiList<T> {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Link(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Tail(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Empty(Arc::new(x)), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl<T: AstParse> AstParse for SemiListLink<T> {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let item = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Semi => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Semi].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Semi].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let next = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((
                        Self {
                            item: Arc::new(item),
                            next: Arc::new(next),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl<T: AstParse> AstParse for List<T> {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Link(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Empty(Arc::new(x)), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl<T: AstParse> AstParse for ListLink<T> {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let item = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    let next = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((
                        Self {
                            item: Arc::new(item),
                            next: Arc::new(next),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for BodyItem {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Statement(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::If(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::While(Arc::new(x)), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for IfItem {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::If => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::If].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::If].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let condition = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    let then_body = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    let else_item = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((
                        Self {
                            condition: Arc::new(condition),
                            then_body: Arc::new(then_body),
                            else_item: Arc::new(else_item),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for ElseItem {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Body(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::If(Arc::new(x)), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for ElseBodyItem {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Else => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Else].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Else].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let body = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((Self { body: Arc::new(body) }, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for ElseIfItem {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Else => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Else].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Else].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let if_item = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((Self { if_item: Arc::new(if_item) }, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for WhileItem {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::While => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::While].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::While].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let condition = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    let body = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((
                        Self {
                            condition: Arc::new(condition),
                            body: Arc::new(body),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Statement {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Assign(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Call(Arc::new(x)), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Assign {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let place = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Eq => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Eq].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Eq].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let expr = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((
                        Self {
                            place: Arc::new(place),
                            expr: Arc::new(expr),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for FunctionCall {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let func_name = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::LeftParen => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::LeftParen].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::LeftParen].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let param_exprs = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::RightParen => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::RightParen].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::RightParen].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((
                        Self {
                            func_name: Arc::new(func_name),
                            param_exprs: Arc::new(param_exprs),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Place {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let head = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    let offset = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((
                        Self {
                            head: Arc::new(head),
                            offset: Arc::new(offset),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl<T: AstParse> AstParse for ParensNest<T> {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Root(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Wrapped(Arc::new(x)), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl<T: AstParse> AstParse for ParensWrapped<T> {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::LeftParen => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::LeftParen].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::LeftParen].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let item = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::RightParen => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::RightParen].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::RightParen].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self { item: Arc::new(item) }, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for PlaceHead {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Ident(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Deref(Arc::new(x)), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Offset {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::LeftBracket => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::LeftBracket].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::LeftBracket].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let expr = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::RightBracket => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::RightBracket].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::RightBracket].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self { expr: Arc::new(expr) }, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl<T: AstParse> AstParse for Maybe<T> {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Item(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Empty(Arc::new(x)), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Ident {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let name = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((Self { name: Arc::new(name) }, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Deref {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Asterisk => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Asterisk].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Asterisk].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let addr = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((Self { addr: Arc::new(addr) }, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for AssignExpr {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Slice(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Span(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Expr(Arc::new(x)), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Span {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::LeftBracket => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::LeftBracket].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::LeftBracket].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let elements = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::RightBracket => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::RightBracket].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::RightBracket].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((
                        Self {
                            elements: Arc::new(elements),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Slice {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let place = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::LeftBracket => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::LeftBracket].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::LeftBracket].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let start_in = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Period => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Period, TokenKind::Period].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Period, TokenKind::Period].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Period => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Period, TokenKind::Period].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Period, TokenKind::Period].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let end_ex = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::RightBracket => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::RightBracket].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::RightBracket].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((
                        Self {
                            place: Arc::new(place),
                            start_in: Arc::new(start_in),
                            end_ex: Arc::new(end_ex),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for Expr {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Paren(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Ref(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Place(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Literal(Arc::new(x)), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for RefExpr {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Ampersand => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Ampersand].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Ampersand].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let place = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    return Ok((Self { place: Arc::new(place) }, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for ParenExpr {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Unary(Arc::new(x)), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Binary(Arc::new(x)), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for UnaryParenExpr {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::LeftParen => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::LeftParen].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::LeftParen].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let op = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    let operand = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::RightParen => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::RightParen].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::RightParen].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((
                        Self {
                            op,
                            operand: Arc::new(operand),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for BinaryParenExpr {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::LeftParen => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::LeftParen].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::LeftParen].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    let left = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    let op = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    let right = {
                        let res = <_ as AstParse>::parse(tokens);
                        errors = errors.merge(res.errors);
                        match res.item {
                            Ok(val) => val,
                            Err(()) => return Err(errors),
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::RightParen => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::RightParen].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::RightParen].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((
                        Self {
                            op,
                            left: Arc::new(left),
                            right: Arc::new(right),
                        },
                        errors,
                    ));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for UnaryParenExprOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Not(x), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for NotOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Bang => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Bang].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Bang].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for BinaryParenExprOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Eq(x), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Neq(x), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Gte(x), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Lte(x), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::And(x), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Or(x), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Add(x), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Sub(x), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Mul(x), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Div(x), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Mod(x), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Gt(x), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Lt(x), errors));
                    }
                    let res = <_ as AstParse>::parse(tokens);
                    errors = errors.merge(res.errors);
                    if let Ok(x) = res.item {
                        return Ok((Self::Join(x), errors));
                    }
                    Err(errors)
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for AddOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Plus => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Plus].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Plus].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for SubOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Dash => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Dash].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Dash].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for MulOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Asterisk => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Asterisk].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Asterisk].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for DivOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Slash => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Slash].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Slash].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for ModOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Percent => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Percent].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Percent].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for EqOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Eq => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Eq, TokenKind::Eq].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Eq, TokenKind::Eq].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Eq => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Eq, TokenKind::Eq].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Eq, TokenKind::Eq].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for NeqOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Bang => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Bang, TokenKind::Eq].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Bang, TokenKind::Eq].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Eq => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Bang, TokenKind::Eq].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Bang, TokenKind::Eq].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for GtOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::RightAngle => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::RightAngle].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::RightAngle].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for LtOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::LeftAngle => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::LeftAngle].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::LeftAngle].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for GteOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::RightAngle => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::RightAngle, TokenKind::Eq].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::RightAngle, TokenKind::Eq].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Eq => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::RightAngle, TokenKind::Eq].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::RightAngle, TokenKind::Eq].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for LteOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::LeftAngle => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::LeftAngle, TokenKind::Eq].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::LeftAngle, TokenKind::Eq].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Eq => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::LeftAngle, TokenKind::Eq].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::LeftAngle, TokenKind::Eq].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for AndOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Ampersand => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Ampersand, TokenKind::Ampersand]
                                                                .into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Ampersand, TokenKind::Ampersand]
                                                        .into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Ampersand => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Ampersand, TokenKind::Ampersand]
                                                                .into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Ampersand, TokenKind::Ampersand]
                                                        .into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for OrOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Pipe => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Pipe, TokenKind::Pipe].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Pipe, TokenKind::Pipe].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Pipe => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Pipe, TokenKind::Pipe].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Pipe, TokenKind::Pipe].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
    impl AstParse for JoinOp {
        fn parse(tokens: &mut TokenFeed) -> AstParseRes<Self> {
            let (item, errors) = match tokens
                .try_match(|tokens| {
                    let mut errors = AstErrorSet::new();
                    #[allow(clippy::let_unit_value)]
                    let _ = {
                        match tokens
                            .try_next({
                                |cell| match cell.res {
                                    Some(t) => {
                                        match &t.token {
                                            Token::Tilde => Ok(()),
                                            token => {
                                                Err(
                                                    AstErrorSet::new_error(
                                                        t.range,
                                                        AstErrorKind::MismatchedTokenString {
                                                            expected: [TokenKind::Tilde].into(),
                                                            found: token.into(),
                                                        },
                                                    ),
                                                )
                                            }
                                        }
                                    }
                                    None => {
                                        Err(
                                            AstErrorSet::new_error(
                                                cell.range,
                                                AstErrorKind::ExpectedTokenString {
                                                    expected: [TokenKind::Tilde].into(),
                                                },
                                            ),
                                        )
                                    }
                                }
                            })
                        {
                            Ok(val) => val,
                            Err(errors) => {
                                errors = errors.merge(errors);
                                return Err(errors);
                            }
                        }
                    };
                    return Ok((Self, errors));
                })
            {
                Ok((item, errors)) => (Ok(item), errors),
                Err(errors) => (Err(()), errors),
            };
            AstParseRes { item, errors }
        }
    }
}
mod compile {
    use std::{collections::HashMap, sync::Arc};
    use crate::link;
    use crate::logic_ast as hast;
    use anyhow::bail;
    use itertools::chain;
    use itertools::Itertools;
    use mem_opt::ast;
    use mem_opt::ast as opt;
    use uuid::Uuid;
    struct StackFrame {
        ident_name_to_info: HashMap<Arc<hast::Name>, Arc<StackVarInfo>>,
    }
    struct StackVarInfo {
        offset: u32,
        size: u32,
    }
    impl StackFrame {
        fn new(idents: &[Arc<hast::IdentDeclaration>]) -> Self {
            let mut ident_name_to_info = HashMap::new();
            let mut total_offset = 0;
            for ident in idents.iter().rev() {
                let size = ident.ty.size();
                total_offset += size;
                let info = StackVarInfo {
                    size,
                    offset: total_offset - 1,
                };
                ident_name_to_info.insert(ident.name.clone(), Arc::new(info));
            }
            Self { ident_name_to_info }
        }
        fn get_info(&self, name: &hast::Name) -> anyhow::Result<&Arc<StackVarInfo>> {
            let Some(info) = self.ident_name_to_info.get(name) else {
                return ::anyhow::__private::Err(
                    ::anyhow::Error::msg(
                        ::alloc::__export::must_use({
                            let res = ::alloc::fmt::format(
                                format_args!(
                                    "ident name {0} is not in current stack frame",
                                    name.str,
                                ),
                            );
                            res
                        }),
                    ),
                );
            };
            Ok(info)
        }
        fn size(&self) -> u32 {
            let return_addr_size = 1;
            let vars_size = self
                .ident_name_to_info
                .values()
                .map(|i| i.size)
                .sum::<u32>();
            return_addr_size + vars_size
        }
    }
    struct FuncManager {
        name_to_head_uuid: HashMap<Arc<hast::Name>, Uuid>,
        name_to_params: HashMap<Arc<hast::Name>, Arc<Vec<Arc<hast::IdentDeclaration>>>>,
    }
    impl FuncManager {
        fn try_new(procs: &[Arc<link::Proc>]) -> anyhow::Result<Self> {
            let mut name_to_head_uuid = HashMap::new();
            let mut name_to_params = HashMap::new();
            for proc in procs {
                let link::ProcKind::Func { name, params } = &proc.kind else { continue };
                let Some(first_sp) = proc.sub_procs.first() else {
                    return ::anyhow::__private::Err(
                        ::anyhow::Error::msg(
                            ::alloc::__export::must_use({
                                let res = ::alloc::fmt::format(
                                    format_args!(
                                        "procedure `{0}` has no sub-procedures",
                                        name.str,
                                    ),
                                );
                                res
                            }),
                        ),
                    );
                };
                name_to_head_uuid.insert(name.clone(), first_sp.uuid);
                name_to_params.insert(name.clone(), params.clone());
            }
            Ok(Self {
                name_to_head_uuid,
                name_to_params,
            })
        }
        fn get_head_uuid(&self, name: &hast::Name) -> anyhow::Result<Uuid> {
            let Some(uuid) = self.name_to_head_uuid.get(name) else {
                return ::anyhow::__private::Err(
                    ::anyhow::Error::msg(
                        ::alloc::__export::must_use({
                            let res = ::alloc::fmt::format(
                                format_args!(
                                    "no head UUID registered for function `{0}`",
                                    name.str,
                                ),
                            );
                            res
                        }),
                    ),
                )
            };
            Ok(*uuid)
        }
        fn get_params(
            &self,
            name: &hast::Name,
        ) -> anyhow::Result<&Arc<Vec<Arc<hast::IdentDeclaration>>>> {
            let Some(params) = self.name_to_params.get(name) else {
                return ::anyhow::__private::Err(
                    ::anyhow::Error::msg(
                        ::alloc::__export::must_use({
                            let res = ::alloc::fmt::format(
                                format_args!(
                                    "no params registered for function `{0}`",
                                    name.str,
                                ),
                            );
                            res
                        }),
                    ),
                )
            };
            Ok(params)
        }
    }
    pub fn compile(
        linked: Vec<Arc<link::Proc>>,
    ) -> anyhow::Result<Vec<Arc<opt::Proc<opt::UMemLoc>>>> {
        let user_main_sp_uuid = find_user_main_sp_uuid(&linked)?;
        let func_manager = FuncManager::try_new(&linked)?;
        let opt_procs = linked
            .into_iter()
            .map(|p| compile_proc(&func_manager, &p))
            .collect::<Result<Vec<_>, _>>()?;
        let opt_procs = {
            let iter = ::itertools::__std_iter::IntoIterator::into_iter([
                create_runner_proc(user_main_sp_uuid),
            ]);
            let iter = ::itertools::__std_iter::Iterator::chain(
                iter,
                ::itertools::__std_iter::IntoIterator::into_iter(opt_procs),
            );
            iter
        }
            .collect();
        Ok(opt_procs)
    }
    fn stack_pointer() -> Arc<opt::UMemLoc> {
        Arc::new(opt::UMemLoc::StackPointer)
    }
    fn literal(literal: impl ScratchLiteral) -> Arc<ast::Expr<opt::UMemLoc>> {
        literal.to_expr()
    }
    trait ScratchLiteral {
        fn to_expr(&self) -> Arc<ast::Expr<opt::UMemLoc>>;
    }
    impl ScratchLiteral for f64 {
        fn to_expr(&self) -> Arc<ast::Expr<ast::UMemLoc>> {
            Arc::new(
                ast::Expr::Value(Arc::new(ast::Value::Literal(self.to_string().into()))),
            )
        }
    }
    impl ScratchLiteral for &str {
        fn to_expr(&self) -> Arc<ast::Expr<ast::UMemLoc>> {
            Arc::new(ast::Expr::Value(Arc::new(ast::Value::Literal((*self).into()))))
        }
    }
    impl ScratchLiteral for hast::Literal {
        fn to_expr(&self) -> Arc<ast::Expr<ast::UMemLoc>> {
            self.str.as_ref().to_expr()
        }
    }
    impl<T: ScratchLiteral> ScratchLiteral for &T {
        fn to_expr(&self) -> Arc<ast::Expr<ast::UMemLoc>> {
            T::to_expr(self)
        }
    }
    fn label(uuid: Uuid) -> Arc<ast::Expr<opt::UMemLoc>> {
        Arc::new(ast::Expr::Value(Arc::new(ast::Value::Label(uuid))))
    }
    fn mem_loc_expr(mem_loc: Arc<opt::UMemLoc>) -> Arc<ast::Expr<opt::UMemLoc>> {
        Arc::new(ast::Expr::MemLoc(mem_loc))
    }
    fn temp_mem_loc() -> Arc<opt::UMemLoc> {
        Arc::new(opt::UMemLoc::Temp(Arc::new(ast::TempVar::new())))
    }
    fn binary_args(
        left: Arc<ast::Expr<opt::UMemLoc>>,
        right: Arc<ast::Expr<opt::UMemLoc>>,
    ) -> Arc<ast::BinaryArgs<opt::UMemLoc>> {
        Arc::new(ast::BinaryArgs { left, right })
    }
    fn find_user_main_sp_uuid(procs: &[Arc<link::Proc>]) -> anyhow::Result<Uuid> {
        let Some(main_proc) = procs
            .iter()
            .find(|proc| match proc.kind {
                link::ProcKind::Main => true,
                _ => false,
            }) else {
            return ::anyhow::__private::Err({
                let error = ::anyhow::__private::format_err(
                    format_args!("could not find user main procedure"),
                );
                error
            });
        };
        let Some(first_sp) = main_proc.sub_procs.first() else {
            return ::anyhow::__private::Err({
                let error = ::anyhow::__private::format_err(
                    format_args!("user main procedure has no sub-procedures"),
                );
                error
            });
        };
        Ok(first_sp.uuid)
    }
    fn create_runner_proc(user_main_sp_uuid: Uuid) -> Arc<opt::Proc<opt::UMemLoc>> {
        let jump_loc = temp();
        Arc::new(opt::Proc {
            kind: Arc::new(opt::ProcKind::Main),
            sub_procs: Arc::new(
                Vec::from([
                    Arc::new(opt::SubProc {
                        uuid: Uuid::new_v4(),
                        commands: Arc::new(
                            Vec::from([
                                Arc::new(opt::Command::SetMemLoc {
                                    mem_loc: stack_pointer(),
                                    val: literal(-1.0),
                                }),
                                Arc::new(opt::Command::SetMemLoc {
                                    mem_loc: jump_loc.clone(),
                                    val: label(user_main_sp_uuid),
                                }),
                            ]),
                        ),
                        call: Arc::new(opt::Call::Jump(mem_loc_expr(jump_loc))),
                    }),
                ]),
            ),
        })
    }
    fn compile_proc(
        func_manager: &FuncManager,
        proc: &link::Proc,
    ) -> anyhow::Result<Arc<opt::Proc<opt::UMemLoc>>> {
        let (kind, stack_params) = match &proc.kind {
            link::ProcKind::Main => {
                (
                    opt::ProcKind::Func {
                        name: "main".into(),
                    },
                    Vec::new(),
                )
            }
            link::ProcKind::Func { name, params } => {
                (
                    opt::ProcKind::Func {
                        name: ::alloc::__export::must_use({
                                let res = ::alloc::fmt::format(
                                    format_args!("func.{0}", name.str),
                                );
                                res
                            })
                            .into(),
                    },
                    params.iter().cloned().collect(),
                )
            }
        };
        let stack_vars = proc.idents.iter().cloned();
        let stack_idents = {
            let iter = ::itertools::__std_iter::IntoIterator::into_iter(stack_params);
            let iter = ::itertools::__std_iter::Iterator::chain(
                iter,
                ::itertools::__std_iter::IntoIterator::into_iter(stack_vars),
            );
            iter
        }
            .collect_vec();
        let stack_frame = StackFrame::new(&stack_idents);
        let sub_procs = proc
            .sub_procs
            .iter()
            .enumerate()
            .map(|(i, sp)| compile_sub_proc(&stack_frame, func_manager, sp, i == 0))
            .collect::<Result<Vec<_>, _>>()?;
        let proc = opt::Proc {
            kind: Arc::new(kind),
            sub_procs: Arc::new(sub_procs),
        };
        Ok(Arc::new(proc))
    }
    fn compile_sub_proc(
        stack_frame: &StackFrame,
        func_manager: &FuncManager,
        sp: &link::SubProc,
        proc_head: bool,
    ) -> anyhow::Result<Arc<opt::SubProc<opt::UMemLoc>>> {
        let stack_frame_commands = if proc_head {
            let stack_frame_size_loc = temp_mem_loc();
            Vec::from([
                Arc::new(opt::Command::SetMemLoc {
                    mem_loc: stack_frame_size_loc.clone(),
                    val: literal(stack_frame.size() as f64),
                }),
                Arc::new(opt::Command::SetMemLoc {
                    mem_loc: stack_pointer(),
                    val: Arc::new(
                        ast::Expr::Add(
                            binary_args(
                                mem_loc_expr(stack_pointer()),
                                mem_loc_expr(stack_frame_size_loc),
                            ),
                        ),
                    ),
                }),
            ])
        } else {
            Vec::new()
        };
        let statement_commands: Vec<_> = sp
            .statements
            .iter()
            .map(|s| compile_statement(stack_frame, s))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect();
        let compiled_call = compile_call(stack_frame, func_manager, &sp.next_call)?;
        let commands = {
            let iter = ::itertools::__std_iter::IntoIterator::into_iter(
                stack_frame_commands,
            );
            let iter = ::itertools::__std_iter::Iterator::chain(
                iter,
                ::itertools::__std_iter::IntoIterator::into_iter(statement_commands),
            );
            let iter = ::itertools::__std_iter::Iterator::chain(
                iter,
                ::itertools::__std_iter::IntoIterator::into_iter(compiled_call.commands),
            );
            iter
        }
            .collect();
        let sp = opt::SubProc {
            uuid: sp.uuid,
            commands: Arc::new(commands),
            call: compiled_call.call,
        };
        Ok(Arc::new(sp))
    }
    struct CompiledCall {
        commands: Vec<Arc<opt::Command<opt::UMemLoc>>>,
        call: Arc<opt::Call<opt::UMemLoc>>,
    }
    fn compile_call(
        stack_frame: &StackFrame,
        func_manager: &FuncManager,
        call: &link::Call,
    ) -> anyhow::Result<CompiledCall> {
        let compiled = match call {
            link::Call::Func { name, param_exprs, return_sub_proc } => {
                let param_idents = func_manager.get_params(name)?;
                let mut param_stack_offset: u32 = 0;
                let mut param_setup_commands = Vec::new();
                for (ident, expr) in param_idents.iter().zip(param_exprs.as_ref()) {
                    let elements = compile_assign_expr_elements(stack_frame, expr)?;
                    for (i, element) in elements.into_iter().enumerate() {
                        param_setup_commands.extend(element.commands);
                        let param_offset_loc = temp();
                        let param_addr_loc = temp();
                        let assign_commands = [
                            Arc::new(opt::Command::SetMemLoc {
                                mem_loc: param_offset_loc.clone(),
                                val: literal((param_stack_offset + i as u32 + 2) as f64),
                            }),
                            Arc::new(opt::Command::SetMemLoc {
                                mem_loc: param_addr_loc.clone(),
                                val: Arc::new(
                                    ast::Expr::Add(
                                        binary_args(
                                            mem_loc_expr(stack_pointer()),
                                            mem_loc_expr(param_offset_loc),
                                        ),
                                    ),
                                ),
                            }),
                            Arc::new(opt::Command::SetStack {
                                addr: mem_loc_expr(param_addr_loc),
                                val: mem_loc_expr(element.mem_loc),
                            }),
                        ];
                        param_setup_commands.extend(assign_commands);
                    }
                    param_stack_offset += ident.ty.size();
                }
                let return_setup_commands = {
                    let return_offset_loc = temp();
                    let return_addr_loc = temp();
                    let return_label_loc = temp();
                    [
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: return_offset_loc.clone(),
                            val: literal(1.0),
                        }),
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: return_addr_loc.clone(),
                            val: Arc::new(
                                ast::Expr::Add(
                                    binary_args(
                                        mem_loc_expr(stack_pointer()),
                                        mem_loc_expr(return_offset_loc.clone()),
                                    ),
                                ),
                            ),
                        }),
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: return_label_loc.clone(),
                            val: label(*return_sub_proc),
                        }),
                        Arc::new(opt::Command::SetStack {
                            addr: mem_loc_expr(return_addr_loc),
                            val: mem_loc_expr(return_label_loc),
                        }),
                    ]
                };
                let (jump_commands, jump_call) = {
                    let func_head_uuid = func_manager.get_head_uuid(name)?;
                    let jump_label_loc = temp();
                    (
                        [
                            Arc::new(opt::Command::SetMemLoc {
                                mem_loc: jump_label_loc.clone(),
                                val: label(func_head_uuid),
                            }),
                        ],
                        Arc::new(opt::Call::Jump(mem_loc_expr(jump_label_loc))),
                    )
                };
                CompiledCall {
                    commands: {
                        let iter = ::itertools::__std_iter::IntoIterator::into_iter(
                            param_setup_commands,
                        );
                        let iter = ::itertools::__std_iter::Iterator::chain(
                            iter,
                            ::itertools::__std_iter::IntoIterator::into_iter(
                                return_setup_commands,
                            ),
                        );
                        let iter = ::itertools::__std_iter::Iterator::chain(
                            iter,
                            ::itertools::__std_iter::IntoIterator::into_iter(
                                jump_commands,
                            ),
                        );
                        iter
                    }
                        .collect(),
                    call: jump_call,
                }
            }
            link::Call::SubProc(uuid) => {
                let return_label_loc = temp();
                CompiledCall {
                    commands: Vec::from([
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: return_label_loc.clone(),
                            val: label(*uuid),
                        }),
                    ]),
                    call: Arc::new(opt::Call::Jump(mem_loc_expr(return_label_loc))),
                }
            }
            link::Call::IfElseBranch { cond_expr, then_sub_proc, else_sub_proc } => {
                let compiled_cond = compile_expr(stack_frame, cond_expr)?;
                let then_label_loc = temp();
                let else_label_loc = temp();
                let (branch_commands, branch_call) = (
                    Vec::from([
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: then_label_loc.clone(),
                            val: label(*then_sub_proc),
                        }),
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: else_label_loc.clone(),
                            val: label(*else_sub_proc),
                        }),
                    ]),
                    Arc::new(opt::Call::Branch {
                        cond: mem_loc_expr(compiled_cond.mem_loc),
                        then_to: mem_loc_expr(then_label_loc),
                        else_to: mem_loc_expr(else_label_loc),
                    }),
                );
                CompiledCall {
                    commands: {
                        let iter = ::itertools::__std_iter::IntoIterator::into_iter(
                            compiled_cond.commands,
                        );
                        let iter = ::itertools::__std_iter::Iterator::chain(
                            iter,
                            ::itertools::__std_iter::IntoIterator::into_iter(
                                branch_commands,
                            ),
                        );
                        iter
                    }
                        .collect(),
                    call: branch_call,
                }
            }
            link::Call::Return => {
                let stack_frame_commands = {
                    let stack_frame_size_loc = temp();
                    [
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: stack_frame_size_loc.clone(),
                            val: literal(stack_frame.size() as f64),
                        }),
                        Arc::new(opt::Command::SetMemLoc {
                            mem_loc: stack_pointer(),
                            val: Arc::new(
                                ast::Expr::Sub(
                                    binary_args(
                                        mem_loc_expr(stack_pointer()),
                                        mem_loc_expr(stack_frame_size_loc),
                                    ),
                                ),
                            ),
                        }),
                    ]
                };
                let (jump_return_commands, jump_return_call) = {
                    let label_offset_loc = temp();
                    let label_addr_loc = temp();
                    let label_loc = temp();
                    (
                        [
                            Arc::new(opt::Command::SetMemLoc {
                                mem_loc: label_offset_loc.clone(),
                                val: literal(1.0),
                            }),
                            Arc::new(opt::Command::SetMemLoc {
                                mem_loc: label_addr_loc.clone(),
                                val: Arc::new(
                                    ast::Expr::Add(
                                        binary_args(
                                            mem_loc_expr(stack_pointer()),
                                            mem_loc_expr(label_offset_loc),
                                        ),
                                    ),
                                ),
                            }),
                            Arc::new(opt::Command::SetMemLoc {
                                mem_loc: label_loc.clone(),
                                val: Arc::new(
                                    ast::Expr::Deref(mem_loc_expr(label_addr_loc)),
                                ),
                            }),
                        ],
                        Arc::new(opt::Call::Jump(mem_loc_expr(label_loc))),
                    )
                };
                CompiledCall {
                    commands: {
                        let iter = ::itertools::__std_iter::IntoIterator::into_iter(
                            stack_frame_commands,
                        );
                        let iter = ::itertools::__std_iter::Iterator::chain(
                            iter,
                            ::itertools::__std_iter::IntoIterator::into_iter(
                                jump_return_commands,
                            ),
                        );
                        iter
                    }
                        .collect(),
                    call: jump_return_call,
                }
            }
            link::Call::Terminate => {
                CompiledCall {
                    commands: Vec::new(),
                    call: Arc::new(opt::Call::Exit),
                }
            }
        };
        Ok(compiled)
    }
    fn compile_statement(
        stack_frame: &StackFrame,
        statement: &link::Statement,
    ) -> anyhow::Result<Vec<Arc<opt::Command<opt::UMemLoc>>>> {
        let assignments = match statement {
            link::Statement::Assign(assign) => {
                let elements = compile_assign_expr_elements(stack_frame, &assign.expr)?;
                let mut element_assignments = Vec::new();
                for (i, element) in elements.into_iter().enumerate() {
                    let compiled_ident_addr = compile_place_to_addr(
                        stack_frame,
                        &assign.place,
                    )?;
                    let compiled_offset = compile_expr(
                        stack_frame,
                        &hast::Expr::Literal(
                            Arc::new(hast::Literal {
                                str: i.to_string().into(),
                            }),
                        ),
                    )?;
                    let compile_dest_addr = compile_addr_offset(
                        compiled_ident_addr.mem_loc,
                        compiled_offset.mem_loc,
                    )?;
                    let mut assignments = {
                        let iter = ::itertools::__std_iter::IntoIterator::into_iter(
                            element.commands,
                        );
                        let iter = ::itertools::__std_iter::Iterator::chain(
                            iter,
                            ::itertools::__std_iter::IntoIterator::into_iter(
                                compiled_ident_addr.commands,
                            ),
                        );
                        let iter = ::itertools::__std_iter::Iterator::chain(
                            iter,
                            ::itertools::__std_iter::IntoIterator::into_iter(
                                compiled_offset.commands,
                            ),
                        );
                        let iter = ::itertools::__std_iter::Iterator::chain(
                            iter,
                            ::itertools::__std_iter::IntoIterator::into_iter(
                                compile_dest_addr.commands,
                            ),
                        );
                        iter
                    }
                        .collect_vec();
                    let dest_addr_loc = compile_dest_addr.mem_loc;
                    assignments
                        .push(
                            Arc::new(opt::Command::SetStack {
                                addr: mem_loc_expr(dest_addr_loc),
                                val: mem_loc_expr(element.mem_loc),
                            }),
                        );
                    element_assignments.extend(assignments);
                }
                element_assignments
            }
            link::Statement::Native(native) => {
                match native.as_ref() {
                    hast::NativeOperation::Out { ident } => {
                        let compiled_ident_addr = compile_place_to_addr(
                            stack_frame,
                            ident,
                        )?;
                        let ident_value_loc = temp();
                        {
                            let iter = ::itertools::__std_iter::IntoIterator::into_iter(
                                compiled_ident_addr.commands,
                            );
                            let iter = ::itertools::__std_iter::Iterator::chain(
                                iter,
                                ::itertools::__std_iter::IntoIterator::into_iter([
                                    Arc::new(opt::Command::SetMemLoc {
                                        mem_loc: ident_value_loc.clone(),
                                        val: Arc::new(
                                            ast::Expr::Deref(mem_loc_expr(compiled_ident_addr.mem_loc)),
                                        ),
                                    }),
                                    Arc::new(opt::Command::Out(mem_loc_expr(ident_value_loc))),
                                ]),
                            );
                            iter
                        }
                            .collect()
                    }
                    hast::NativeOperation::In { dest_ident } => {
                        let compiled_ident_addr = compile_place_to_addr(
                            stack_frame,
                            dest_ident,
                        )?;
                        {
                            let iter = ::itertools::__std_iter::IntoIterator::into_iter(
                                compiled_ident_addr.commands,
                            );
                            let iter = ::itertools::__std_iter::Iterator::chain(
                                iter,
                                ::itertools::__std_iter::IntoIterator::into_iter([
                                    Arc::new(opt::Command::In),
                                    Arc::new(opt::Command::SetStack {
                                        addr: mem_loc_expr(compiled_ident_addr.mem_loc),
                                        val: Arc::new(ast::Expr::InAnswer),
                                    }),
                                ]),
                            );
                            iter
                        }
                            .collect()
                    }
                }
            }
        };
        Ok(assignments)
    }
    fn compile_assign_expr_elements(
        stack_frame: &StackFrame,
        expr: &hast::AssignExpr,
    ) -> anyhow::Result<Vec<CompiledExpr>> {
        let compileds = match expr {
            hast::AssignExpr::Expr(expr) => Vec::from([compile_expr(stack_frame, expr)?]),
            hast::AssignExpr::Span(array) => {
                array
                    .elements
                    .iter()
                    .map(|expr| compile_expr(stack_frame, expr))
                    .collect::<anyhow::Result<Vec<_>>>()?
                    .into_iter()
                    .collect()
            }
            hast::AssignExpr::Slice(slice) => {
                let elements = (slice.start_in..slice.end_ex)
                    .map(|i| {
                        let ident_addr = compile_place_to_addr(
                            stack_frame,
                            &slice.place,
                        )?;
                        let offset = compile_expr(
                            stack_frame,
                            &hast::Expr::Literal(
                                Arc::new(hast::Literal {
                                    str: i.to_string().into(),
                                }),
                            ),
                        )?;
                        let element_addr = compile_addr_offset(
                            ident_addr.mem_loc,
                            offset.mem_loc,
                        )?;
                        let element = compile_addr_deref(&element_addr.mem_loc);
                        let commands = {
                            let iter = ::itertools::__std_iter::IntoIterator::into_iter(
                                ident_addr.commands,
                            );
                            let iter = ::itertools::__std_iter::Iterator::chain(
                                iter,
                                ::itertools::__std_iter::IntoIterator::into_iter(
                                    offset.commands,
                                ),
                            );
                            let iter = ::itertools::__std_iter::Iterator::chain(
                                iter,
                                ::itertools::__std_iter::IntoIterator::into_iter(
                                    element_addr.commands,
                                ),
                            );
                            let iter = ::itertools::__std_iter::Iterator::chain(
                                iter,
                                ::itertools::__std_iter::IntoIterator::into_iter(
                                    element.commands,
                                ),
                            );
                            iter
                        }
                            .collect();
                        Ok(CompiledExpr {
                            mem_loc: element.mem_loc,
                            commands,
                        })
                    });
                elements.collect::<anyhow::Result<Vec<_>>>()?
            }
        };
        Ok(compileds)
    }
    fn compile_expr(
        stack_frame: &StackFrame,
        expr: &hast::Expr,
    ) -> anyhow::Result<CompiledExpr> {
        let compiled = match expr {
            hast::Expr::Literal(lit) => {
                let value_temp = temp();
                let assignments = Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: value_temp.clone(),
                        val: literal(lit.as_ref()),
                    }),
                ]);
                CompiledExpr {
                    mem_loc: value_temp,
                    commands: assignments,
                }
            }
            hast::Expr::Place(ident) => {
                let ident_addr = compile_place_to_addr(stack_frame, ident)?;
                let value = compile_addr_deref(&ident_addr.mem_loc);
                let commands = {
                    let iter = ::itertools::__std_iter::IntoIterator::into_iter(
                        ident_addr.commands,
                    );
                    let iter = ::itertools::__std_iter::Iterator::chain(
                        iter,
                        ::itertools::__std_iter::IntoIterator::into_iter(value.commands),
                    );
                    iter
                }
                    .collect();
                CompiledExpr {
                    mem_loc: value.mem_loc,
                    commands,
                }
            }
            hast::Expr::Ref(expr) => compile_place_to_addr(stack_frame, &expr.place)?,
            hast::Expr::Paren(expr) => compile_paren_expr(stack_frame, expr)?,
        };
        Ok(compiled)
    }
    fn compile_paren_expr(
        stack_frame: &StackFrame,
        expr: &hast::ParenExpr,
    ) -> anyhow::Result<CompiledExpr> {
        match expr {
            hast::ParenExpr::Unary(unary) => compile_unary_paren_expr(stack_frame, unary),
            hast::ParenExpr::Binary(binary) => {
                compile_binary_paren_expr(stack_frame, binary)
            }
        }
    }
    fn compile_unary_paren_expr(
        stack_frame: &StackFrame,
        expr: &hast::UnaryParenExpr,
    ) -> anyhow::Result<CompiledExpr> {
        let res_loc = temp();
        let operand_ops = compile_expr(stack_frame, &expr.operand)?;
        let expr_commands = match expr.op {
            hast::UnaryParenExprOp::Not => {
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: res_loc.clone(),
                        val: Arc::new(opt::Expr::Not(mem_loc_expr(operand_ops.mem_loc))),
                    }),
                ])
            }
        };
        Ok(CompiledExpr {
            mem_loc: res_loc,
            commands: {
                let iter = ::itertools::__std_iter::IntoIterator::into_iter(
                    operand_ops.commands,
                );
                let iter = ::itertools::__std_iter::Iterator::chain(
                    iter,
                    ::itertools::__std_iter::IntoIterator::into_iter(expr_commands),
                );
                iter
            }
                .collect(),
        })
    }
    fn compile_binary_paren_expr(
        stack_frame: &StackFrame,
        expr: &hast::BinaryParenExpr,
    ) -> anyhow::Result<CompiledExpr> {
        let res_loc = temp();
        let left_ops = compile_expr(stack_frame, &expr.left)?;
        let right_ops = compile_expr(stack_frame, &expr.right)?;
        let expr_commands = match expr.op {
            hast::BinaryParenExprOp::Add => {
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: res_loc.clone(),
                        val: Arc::new(
                            opt::Expr::Add(
                                binary_args(
                                    mem_loc_expr(left_ops.mem_loc),
                                    mem_loc_expr(right_ops.mem_loc),
                                ),
                            ),
                        ),
                    }),
                ])
            }
            hast::BinaryParenExprOp::Sub => {
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: res_loc.clone(),
                        val: Arc::new(
                            opt::Expr::Sub(
                                binary_args(
                                    mem_loc_expr(left_ops.mem_loc),
                                    mem_loc_expr(right_ops.mem_loc),
                                ),
                            ),
                        ),
                    }),
                ])
            }
            hast::BinaryParenExprOp::Mul => {
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: res_loc.clone(),
                        val: Arc::new(
                            opt::Expr::Mul(
                                binary_args(
                                    mem_loc_expr(left_ops.mem_loc),
                                    mem_loc_expr(right_ops.mem_loc),
                                ),
                            ),
                        ),
                    }),
                ])
            }
            hast::BinaryParenExprOp::Div => {
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: res_loc.clone(),
                        val: Arc::new(
                            opt::Expr::Div(
                                binary_args(
                                    mem_loc_expr(left_ops.mem_loc),
                                    mem_loc_expr(right_ops.mem_loc),
                                ),
                            ),
                        ),
                    }),
                ])
            }
            hast::BinaryParenExprOp::Mod => {
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: res_loc.clone(),
                        val: Arc::new(
                            opt::Expr::Mod(
                                binary_args(
                                    mem_loc_expr(left_ops.mem_loc),
                                    mem_loc_expr(right_ops.mem_loc),
                                ),
                            ),
                        ),
                    }),
                ])
            }
            hast::BinaryParenExprOp::Eq => {
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: res_loc.clone(),
                        val: Arc::new(
                            opt::Expr::Eq(
                                binary_args(
                                    mem_loc_expr(left_ops.mem_loc),
                                    mem_loc_expr(right_ops.mem_loc),
                                ),
                            ),
                        ),
                    }),
                ])
            }
            hast::BinaryParenExprOp::Neq => {
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: res_loc.clone(),
                        val: Arc::new(
                            opt::Expr::Not(
                                Arc::new(
                                    opt::Expr::Eq(
                                        binary_args(
                                            mem_loc_expr(left_ops.mem_loc),
                                            mem_loc_expr(right_ops.mem_loc),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    }),
                ])
            }
            hast::BinaryParenExprOp::Lt => {
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: res_loc.clone(),
                        val: Arc::new(
                            opt::Expr::Lt(
                                binary_args(
                                    mem_loc_expr(left_ops.mem_loc),
                                    mem_loc_expr(right_ops.mem_loc),
                                ),
                            ),
                        ),
                    }),
                ])
            }
            hast::BinaryParenExprOp::Gt => {
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: res_loc.clone(),
                        val: Arc::new(
                            opt::Expr::Gt(
                                binary_args(
                                    mem_loc_expr(left_ops.mem_loc),
                                    mem_loc_expr(right_ops.mem_loc),
                                ),
                            ),
                        ),
                    }),
                ])
            }
            hast::BinaryParenExprOp::Lte => {
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: res_loc.clone(),
                        val: Arc::new(
                            opt::Expr::Not(
                                Arc::new(
                                    opt::Expr::Gt(
                                        binary_args(
                                            mem_loc_expr(left_ops.mem_loc),
                                            mem_loc_expr(right_ops.mem_loc),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    }),
                ])
            }
            hast::BinaryParenExprOp::Gte => {
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: res_loc.clone(),
                        val: Arc::new(
                            opt::Expr::Not(
                                Arc::new(
                                    opt::Expr::Lt(
                                        binary_args(
                                            mem_loc_expr(left_ops.mem_loc),
                                            mem_loc_expr(right_ops.mem_loc),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    }),
                ])
            }
            hast::BinaryParenExprOp::Join => {
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: res_loc.clone(),
                        val: Arc::new(
                            opt::Expr::Join(
                                binary_args(
                                    mem_loc_expr(left_ops.mem_loc),
                                    mem_loc_expr(right_ops.mem_loc),
                                ),
                            ),
                        ),
                    }),
                ])
            }
            hast::BinaryParenExprOp::Or => {
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: res_loc.clone(),
                        val: Arc::new(
                            opt::Expr::Or(
                                binary_args(
                                    mem_loc_expr(left_ops.mem_loc),
                                    mem_loc_expr(right_ops.mem_loc),
                                ),
                            ),
                        ),
                    }),
                ])
            }
            hast::BinaryParenExprOp::And => {
                Vec::from([
                    Arc::new(opt::Command::SetMemLoc {
                        mem_loc: res_loc.clone(),
                        val: Arc::new(
                            opt::Expr::And(
                                binary_args(
                                    mem_loc_expr(left_ops.mem_loc),
                                    mem_loc_expr(right_ops.mem_loc),
                                ),
                            ),
                        ),
                    }),
                ])
            }
        };
        Ok(CompiledExpr {
            mem_loc: res_loc,
            commands: {
                let iter = ::itertools::__std_iter::IntoIterator::into_iter(
                    left_ops.commands,
                );
                let iter = ::itertools::__std_iter::Iterator::chain(
                    iter,
                    ::itertools::__std_iter::IntoIterator::into_iter(right_ops.commands),
                );
                let iter = ::itertools::__std_iter::Iterator::chain(
                    iter,
                    ::itertools::__std_iter::IntoIterator::into_iter(expr_commands),
                );
                iter
            }
                .collect(),
        })
    }
    fn compile_addr_deref(addr: &Arc<opt::UMemLoc>) -> CompiledExpr {
        let ident_value_loc = temp();
        let commands = Vec::from([
            Arc::new(opt::Command::SetMemLoc {
                mem_loc: ident_value_loc.clone(),
                val: Arc::new(ast::Expr::Deref(mem_loc_expr(addr.clone()))),
            }),
        ]);
        CompiledExpr {
            mem_loc: ident_value_loc,
            commands,
        }
    }
    fn compile_place_to_addr(
        stack_frame: &StackFrame,
        place: &hast::Place,
    ) -> anyhow::Result<CompiledExpr> {
        let compiled_head = compile_place_head_to_add(stack_frame, &place.head)?;
        let compiled_place = match &place.offset {
            None => compiled_head,
            Some(offset) => {
                let compiled_index = compile_expr(stack_frame, &offset.expr)?;
                let offset_addr = compile_addr_offset(
                    compiled_head.mem_loc,
                    compiled_index.mem_loc,
                )?;
                let commands = {
                    let iter = ::itertools::__std_iter::IntoIterator::into_iter(
                        compiled_index.commands,
                    );
                    let iter = ::itertools::__std_iter::Iterator::chain(
                        iter,
                        ::itertools::__std_iter::IntoIterator::into_iter(
                            compiled_head.commands,
                        ),
                    );
                    let iter = ::itertools::__std_iter::Iterator::chain(
                        iter,
                        ::itertools::__std_iter::IntoIterator::into_iter(
                            offset_addr.commands,
                        ),
                    );
                    iter
                }
                    .collect();
                CompiledExpr {
                    mem_loc: offset_addr.mem_loc,
                    commands,
                }
            }
        };
        Ok(compiled_place)
    }
    fn compile_place_head_to_add(
        stack_frame: &StackFrame,
        place_head: &hast::PlaceHead,
    ) -> anyhow::Result<CompiledExpr> {
        match place_head {
            hast::PlaceHead::Ident(ident) => {
                compile_ident_name_to_start_addr(stack_frame, &ident.name)
            }
            hast::PlaceHead::Deref(deref) => compile_expr(stack_frame, &deref.addr),
        }
    }
    fn compile_addr_offset(
        addr_loc: Arc<opt::UMemLoc>,
        offset_loc: Arc<opt::UMemLoc>,
    ) -> anyhow::Result<CompiledExpr> {
        let indexed_addr_loc = temp();
        let index_commands = [
            Arc::new(opt::Command::SetMemLoc {
                mem_loc: indexed_addr_loc.clone(),
                val: Arc::new(
                    ast::Expr::Add(
                        binary_args(mem_loc_expr(addr_loc), mem_loc_expr(offset_loc)),
                    ),
                ),
            }),
        ];
        let commands = {
            let iter = ::itertools::__std_iter::IntoIterator::into_iter(index_commands);
            iter
        }
            .collect();
        Ok(CompiledExpr {
            mem_loc: indexed_addr_loc,
            commands,
        })
    }
    fn compile_ident_name_to_start_addr(
        stack_frame: &StackFrame,
        ident_name: &hast::Name,
    ) -> anyhow::Result<CompiledExpr> {
        let var_info = stack_frame.get_info(ident_name)?;
        let stack_offset_loc = temp();
        let stack_addr_loc = temp();
        let commands = Vec::from([
            Arc::new(opt::Command::SetMemLoc {
                mem_loc: stack_offset_loc.clone(),
                val: literal(var_info.offset as f64),
            }),
            Arc::new(opt::Command::SetMemLoc {
                mem_loc: stack_addr_loc.clone(),
                val: Arc::new(
                    ast::Expr::Sub(
                        binary_args(
                            mem_loc_expr(stack_pointer()),
                            mem_loc_expr(stack_offset_loc),
                        ),
                    ),
                ),
            }),
        ]);
        Ok(CompiledExpr {
            mem_loc: stack_addr_loc,
            commands,
        })
    }
    fn temp() -> Arc<opt::UMemLoc> {
        Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())))
    }
    struct CompiledExpr {
        mem_loc: Arc<opt::UMemLoc>,
        commands: Vec<Arc<opt::Command<opt::UMemLoc>>>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CompiledExpr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CompiledExpr",
                "mem_loc",
                &self.mem_loc,
                "commands",
                &&self.commands,
            )
        }
    }
}
pub mod grammar_ast {
    use std::sync::Arc;
    use crate::{ast_error::AstErrorSet, ast_parse::parse_ast, token_feed::TokenFeed};
    pub enum TopItem {
        Main(Arc<Main>),
        Func(Arc<Func>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for TopItem {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                TopItem::Main(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Main",
                        &__self_0,
                    )
                }
                TopItem::Func(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Func",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct Main {
        pub proc: Arc<Proc>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Main {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Main",
                "proc",
                &&self.proc,
            )
        }
    }
    pub struct Func {
        pub name: Arc<Name>,
        pub params: Arc<CommaList<IdentDeclaration>>,
        pub proc: Arc<Proc>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Func {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "Func",
                "name",
                &self.name,
                "params",
                &self.params,
                "proc",
                &&self.proc,
            )
        }
    }
    pub struct Name {
        pub str: Arc<str>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Name {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Name",
                "str",
                &&self.str,
            )
        }
    }
    #[automatically_derived]
    impl ::core::hash::Hash for Name {
        #[inline]
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            ::core::hash::Hash::hash(&self.str, state)
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Name {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Name {
        #[inline]
        fn eq(&self, other: &Name) -> bool {
            self.str == other.str
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for Name {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<Arc<str>>;
        }
    }
    pub enum Type {
        Ref(Arc<RefType>),
        Array(Arc<ArrayType>),
        Base(Arc<BaseType>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Type {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Type::Ref(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Ref",
                        &__self_0,
                    )
                }
                Type::Array(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Array",
                        &__self_0,
                    )
                }
                Type::Base(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Base",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct RefType {
        pub ty: Arc<Type>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for RefType {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "RefType",
                "ty",
                &&self.ty,
            )
        }
    }
    pub struct ArrayType {
        pub ty: Arc<Type>,
        pub len: Arc<Literal>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ArrayType {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "ArrayType",
                "ty",
                &self.ty,
                "len",
                &&self.len,
            )
        }
    }
    pub struct BaseType {
        pub name: Arc<Name>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for BaseType {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "BaseType",
                "name",
                &&self.name,
            )
        }
    }
    pub struct IdentDeclaration {
        pub name: Arc<Name>,
        pub ty: Arc<Type>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for IdentDeclaration {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "IdentDeclaration",
                "name",
                &self.name,
                "ty",
                &&self.ty,
            )
        }
    }
    pub struct Place {
        pub head: Arc<ParensNest<PlaceHead>>,
        pub offset: Arc<Maybe<Offset>>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Place {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "Place",
                "head",
                &self.head,
                "offset",
                &&self.offset,
            )
        }
    }
    pub enum ParensNest<T> {
        Root(Arc<T>),
        Wrapped(Arc<ParensWrapped<ParensNest<T>>>),
    }
    #[automatically_derived]
    impl<T: ::core::fmt::Debug> ::core::fmt::Debug for ParensNest<T> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                ParensNest::Root(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Root",
                        &__self_0,
                    )
                }
                ParensNest::Wrapped(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Wrapped",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct ParensWrapped<T> {
        pub item: Arc<T>,
    }
    #[automatically_derived]
    impl<T: ::core::fmt::Debug> ::core::fmt::Debug for ParensWrapped<T> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "ParensWrapped",
                "item",
                &&self.item,
            )
        }
    }
    pub enum Maybe<T> {
        Item(Arc<T>),
        Empty(Arc<Empty>),
    }
    #[automatically_derived]
    impl<T: ::core::fmt::Debug> ::core::fmt::Debug for Maybe<T> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Maybe::Item(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Item",
                        &__self_0,
                    )
                }
                Maybe::Empty(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Empty",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub enum PlaceHead {
        Ident(Arc<Ident>),
        Deref(Arc<Deref>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for PlaceHead {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                PlaceHead::Ident(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Ident",
                        &__self_0,
                    )
                }
                PlaceHead::Deref(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Deref",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct Offset {
        pub expr: Arc<Expr>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Offset {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Offset",
                "expr",
                &&self.expr,
            )
        }
    }
    pub struct Ident {
        pub name: Arc<Name>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Ident {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Ident",
                "name",
                &&self.name,
            )
        }
    }
    pub struct Deref {
        pub addr: Arc<Expr>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Deref {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Deref",
                "addr",
                &&self.addr,
            )
        }
    }
    pub struct Proc {
        pub idents: Arc<CommaList<IdentDeclaration>>,
        pub body: Arc<Body>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Proc {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "Proc",
                "idents",
                &self.idents,
                "body",
                &&self.body,
            )
        }
    }
    pub struct Body {
        pub items: Arc<SemiList<BodyItem>>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Body {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Body",
                "items",
                &&self.items,
            )
        }
    }
    pub enum BodyItem {
        Statement(Arc<Statement>),
        If(Arc<IfItem>),
        While(Arc<WhileItem>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for BodyItem {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                BodyItem::Statement(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Statement",
                        &__self_0,
                    )
                }
                BodyItem::If(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "If", &__self_0)
                }
                BodyItem::While(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "While",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct IfItem {
        pub condition: Arc<Expr>,
        pub then_body: Arc<Body>,
        pub else_item: Arc<Maybe<ElseItem>>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for IfItem {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "IfItem",
                "condition",
                &self.condition,
                "then_body",
                &self.then_body,
                "else_item",
                &&self.else_item,
            )
        }
    }
    pub enum ElseItem {
        Body(Arc<ElseBodyItem>),
        If(Arc<ElseIfItem>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ElseItem {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                ElseItem::Body(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Body",
                        &__self_0,
                    )
                }
                ElseItem::If(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "If", &__self_0)
                }
            }
        }
    }
    pub struct ElseBodyItem {
        pub body: Arc<Body>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ElseBodyItem {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "ElseBodyItem",
                "body",
                &&self.body,
            )
        }
    }
    pub struct ElseIfItem {
        pub if_item: Arc<IfItem>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ElseIfItem {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "ElseIfItem",
                "if_item",
                &&self.if_item,
            )
        }
    }
    pub struct WhileItem {
        pub condition: Arc<Expr>,
        pub body: Arc<Body>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for WhileItem {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "WhileItem",
                "condition",
                &self.condition,
                "body",
                &&self.body,
            )
        }
    }
    pub enum Statement {
        Assign(Arc<Assign>),
        Call(Arc<FunctionCall>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Statement {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Statement::Assign(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Assign",
                        &__self_0,
                    )
                }
                Statement::Call(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Call",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct FunctionCall {
        pub func_name: Arc<Name>,
        pub param_exprs: Arc<CommaList<AssignExpr>>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for FunctionCall {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "FunctionCall",
                "func_name",
                &self.func_name,
                "param_exprs",
                &&self.param_exprs,
            )
        }
    }
    pub struct Assign {
        pub place: Arc<Place>,
        pub expr: Arc<AssignExpr>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Assign {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "Assign",
                "place",
                &self.place,
                "expr",
                &&self.expr,
            )
        }
    }
    pub enum AssignExpr {
        Expr(Arc<Expr>),
        Span(Arc<Span>),
        Slice(Arc<Slice>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for AssignExpr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                AssignExpr::Expr(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Expr",
                        &__self_0,
                    )
                }
                AssignExpr::Span(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Span",
                        &__self_0,
                    )
                }
                AssignExpr::Slice(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Slice",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub enum Expr {
        Literal(Arc<Literal>),
        Place(Arc<Place>),
        Ref(Arc<RefExpr>),
        Paren(Arc<ParenExpr>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Expr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Expr::Literal(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Literal",
                        &__self_0,
                    )
                }
                Expr::Place(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Place",
                        &__self_0,
                    )
                }
                Expr::Ref(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Ref",
                        &__self_0,
                    )
                }
                Expr::Paren(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Paren",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct RefExpr {
        pub place: Arc<ParensNest<Place>>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for RefExpr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "RefExpr",
                "place",
                &&self.place,
            )
        }
    }
    pub struct Literal {
        pub str: Arc<str>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Literal {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Literal",
                "str",
                &&self.str,
            )
        }
    }
    pub struct Span {
        pub elements: Arc<CommaList<Expr>>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Span {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Span",
                "elements",
                &&self.elements,
            )
        }
    }
    pub struct Slice {
        pub place: Arc<Place>,
        pub start_in: Arc<Maybe<Literal>>,
        pub end_ex: Arc<Literal>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Slice {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "Slice",
                "place",
                &self.place,
                "start_in",
                &self.start_in,
                "end_ex",
                &&self.end_ex,
            )
        }
    }
    pub enum ParenExpr {
        Unary(Arc<UnaryParenExpr>),
        Binary(Arc<BinaryParenExpr>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ParenExpr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                ParenExpr::Unary(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Unary",
                        &__self_0,
                    )
                }
                ParenExpr::Binary(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Binary",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct UnaryParenExpr {
        pub op: UnaryParenExprOp,
        pub operand: Arc<Expr>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for UnaryParenExpr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "UnaryParenExpr",
                "op",
                &self.op,
                "operand",
                &&self.operand,
            )
        }
    }
    pub enum UnaryParenExprOp {
        Not(NotOp),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for UnaryParenExprOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                UnaryParenExprOp::Not(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Not",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for UnaryParenExprOp {
        #[inline]
        fn clone(&self) -> UnaryParenExprOp {
            let _: ::core::clone::AssertParamIsClone<NotOp>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for UnaryParenExprOp {}
    pub struct NotOp;
    #[automatically_derived]
    impl ::core::fmt::Debug for NotOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "NotOp")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for NotOp {
        #[inline]
        fn clone(&self) -> NotOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for NotOp {}
    pub struct BinaryParenExpr {
        pub op: BinaryParenExprOp,
        pub left: Arc<Expr>,
        pub right: Arc<Expr>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for BinaryParenExpr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "BinaryParenExpr",
                "op",
                &self.op,
                "left",
                &self.left,
                "right",
                &&self.right,
            )
        }
    }
    pub enum BinaryParenExprOp {
        Add(AddOp),
        Sub(SubOp),
        Mul(MulOp),
        Div(DivOp),
        Mod(ModOp),
        Eq(EqOp),
        Neq(NeqOp),
        Gt(GtOp),
        Lt(LtOp),
        Gte(GteOp),
        Lte(LteOp),
        And(AndOp),
        Or(OrOp),
        Join(JoinOp),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for BinaryParenExprOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                BinaryParenExprOp::Add(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Add",
                        &__self_0,
                    )
                }
                BinaryParenExprOp::Sub(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Sub",
                        &__self_0,
                    )
                }
                BinaryParenExprOp::Mul(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Mul",
                        &__self_0,
                    )
                }
                BinaryParenExprOp::Div(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Div",
                        &__self_0,
                    )
                }
                BinaryParenExprOp::Mod(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Mod",
                        &__self_0,
                    )
                }
                BinaryParenExprOp::Eq(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Eq", &__self_0)
                }
                BinaryParenExprOp::Neq(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Neq",
                        &__self_0,
                    )
                }
                BinaryParenExprOp::Gt(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Gt", &__self_0)
                }
                BinaryParenExprOp::Lt(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Lt", &__self_0)
                }
                BinaryParenExprOp::Gte(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Gte",
                        &__self_0,
                    )
                }
                BinaryParenExprOp::Lte(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Lte",
                        &__self_0,
                    )
                }
                BinaryParenExprOp::And(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "And",
                        &__self_0,
                    )
                }
                BinaryParenExprOp::Or(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Or", &__self_0)
                }
                BinaryParenExprOp::Join(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Join",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for BinaryParenExprOp {
        #[inline]
        fn clone(&self) -> BinaryParenExprOp {
            let _: ::core::clone::AssertParamIsClone<AddOp>;
            let _: ::core::clone::AssertParamIsClone<SubOp>;
            let _: ::core::clone::AssertParamIsClone<MulOp>;
            let _: ::core::clone::AssertParamIsClone<DivOp>;
            let _: ::core::clone::AssertParamIsClone<ModOp>;
            let _: ::core::clone::AssertParamIsClone<EqOp>;
            let _: ::core::clone::AssertParamIsClone<NeqOp>;
            let _: ::core::clone::AssertParamIsClone<GtOp>;
            let _: ::core::clone::AssertParamIsClone<LtOp>;
            let _: ::core::clone::AssertParamIsClone<GteOp>;
            let _: ::core::clone::AssertParamIsClone<LteOp>;
            let _: ::core::clone::AssertParamIsClone<AndOp>;
            let _: ::core::clone::AssertParamIsClone<OrOp>;
            let _: ::core::clone::AssertParamIsClone<JoinOp>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for BinaryParenExprOp {}
    pub struct AddOp;
    #[automatically_derived]
    impl ::core::fmt::Debug for AddOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "AddOp")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for AddOp {
        #[inline]
        fn clone(&self) -> AddOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for AddOp {}
    pub struct SubOp;
    #[automatically_derived]
    impl ::core::fmt::Debug for SubOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "SubOp")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for SubOp {
        #[inline]
        fn clone(&self) -> SubOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for SubOp {}
    pub struct MulOp;
    #[automatically_derived]
    impl ::core::fmt::Debug for MulOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "MulOp")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for MulOp {
        #[inline]
        fn clone(&self) -> MulOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for MulOp {}
    pub struct DivOp;
    #[automatically_derived]
    impl ::core::fmt::Debug for DivOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "DivOp")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for DivOp {
        #[inline]
        fn clone(&self) -> DivOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for DivOp {}
    pub struct ModOp;
    #[automatically_derived]
    impl ::core::fmt::Debug for ModOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "ModOp")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ModOp {
        #[inline]
        fn clone(&self) -> ModOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for ModOp {}
    pub struct EqOp;
    #[automatically_derived]
    impl ::core::fmt::Debug for EqOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "EqOp")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for EqOp {
        #[inline]
        fn clone(&self) -> EqOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for EqOp {}
    pub struct NeqOp;
    #[automatically_derived]
    impl ::core::fmt::Debug for NeqOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "NeqOp")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for NeqOp {
        #[inline]
        fn clone(&self) -> NeqOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for NeqOp {}
    pub struct GtOp;
    #[automatically_derived]
    impl ::core::fmt::Debug for GtOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "GtOp")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for GtOp {
        #[inline]
        fn clone(&self) -> GtOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for GtOp {}
    pub struct LtOp;
    #[automatically_derived]
    impl ::core::fmt::Debug for LtOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "LtOp")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for LtOp {
        #[inline]
        fn clone(&self) -> LtOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for LtOp {}
    pub struct GteOp;
    #[automatically_derived]
    impl ::core::fmt::Debug for GteOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "GteOp")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for GteOp {
        #[inline]
        fn clone(&self) -> GteOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for GteOp {}
    pub struct LteOp;
    #[automatically_derived]
    impl ::core::fmt::Debug for LteOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "LteOp")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for LteOp {
        #[inline]
        fn clone(&self) -> LteOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for LteOp {}
    pub struct AndOp;
    #[automatically_derived]
    impl ::core::fmt::Debug for AndOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "AndOp")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for AndOp {
        #[inline]
        fn clone(&self) -> AndOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for AndOp {}
    pub struct OrOp;
    #[automatically_derived]
    impl ::core::fmt::Debug for OrOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "OrOp")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for OrOp {
        #[inline]
        fn clone(&self) -> OrOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for OrOp {}
    pub struct JoinOp;
    #[automatically_derived]
    impl ::core::fmt::Debug for JoinOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "JoinOp")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for JoinOp {
        #[inline]
        fn clone(&self) -> JoinOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for JoinOp {}
    pub enum NativeOperation {
        Out { ident: Arc<Place> },
        In { dest_ident: Arc<Place> },
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for NativeOperation {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                NativeOperation::Out { ident: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "Out",
                        "ident",
                        &__self_0,
                    )
                }
                NativeOperation::In { dest_ident: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "In",
                        "dest_ident",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct Program {
        pub items: Arc<List<TopItem>>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Program {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Program",
                "items",
                &&self.items,
            )
        }
    }
    pub struct Comment {
        pub text: Arc<str>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Comment {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Comment",
                "text",
                &&self.text,
            )
        }
    }
    pub struct Empty;
    #[automatically_derived]
    impl ::core::fmt::Debug for Empty {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "Empty")
        }
    }
    pub enum CommaList<T> {
        Link(Arc<CommaListLink<T>>),
        Tail(Arc<T>),
        Empty(Arc<Empty>),
    }
    #[automatically_derived]
    impl<T: ::core::fmt::Debug> ::core::fmt::Debug for CommaList<T> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CommaList::Link(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Link",
                        &__self_0,
                    )
                }
                CommaList::Tail(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Tail",
                        &__self_0,
                    )
                }
                CommaList::Empty(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Empty",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct CommaListLink<T> {
        pub item: Arc<T>,
        pub next: Arc<CommaList<T>>,
    }
    #[automatically_derived]
    impl<T: ::core::fmt::Debug> ::core::fmt::Debug for CommaListLink<T> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CommaListLink",
                "item",
                &self.item,
                "next",
                &&self.next,
            )
        }
    }
    pub enum SemiList<T> {
        Link(Arc<SemiListLink<T>>),
        Tail(Arc<T>),
        Empty(Arc<Empty>),
    }
    #[automatically_derived]
    impl<T: ::core::fmt::Debug> ::core::fmt::Debug for SemiList<T> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                SemiList::Link(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Link",
                        &__self_0,
                    )
                }
                SemiList::Tail(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Tail",
                        &__self_0,
                    )
                }
                SemiList::Empty(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Empty",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct SemiListLink<T> {
        pub item: Arc<T>,
        pub next: Arc<SemiList<T>>,
    }
    #[automatically_derived]
    impl<T: ::core::fmt::Debug> ::core::fmt::Debug for SemiListLink<T> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "SemiListLink",
                "item",
                &self.item,
                "next",
                &&self.next,
            )
        }
    }
    pub enum List<T> {
        Link(Arc<ListLink<T>>),
        Empty(Arc<Empty>),
    }
    #[automatically_derived]
    impl<T: ::core::fmt::Debug> ::core::fmt::Debug for List<T> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                List::Link(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Link",
                        &__self_0,
                    )
                }
                List::Empty(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Empty",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct ListLink<T> {
        pub item: Arc<T>,
        pub next: Arc<List<T>>,
    }
    #[automatically_derived]
    impl<T: ::core::fmt::Debug> ::core::fmt::Debug for ListLink<T> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "ListLink",
                "item",
                &self.item,
                "next",
                &&self.next,
            )
        }
    }
    pub fn parse(mut tokens: TokenFeed) -> Result<Program, AstErrorSet> {
        parse_ast(&mut tokens)
    }
}
mod grammar_to_logic {
    use std::collections::VecDeque;
    use std::sync::Arc;
    use anyhow::Context;
    use crate::grammar_ast as g;
    use crate::logic_ast as l;
    fn convert<From, To>(from: impl AsRef<From>) -> Arc<To>
    where
        for<'a> &'a From: Into<To>,
    {
        Arc::new(from.as_ref().into())
    }
    fn try_convert<From, To, Error>(from: impl AsRef<From>) -> Result<Arc<To>, Error>
    where
        for<'a> &'a From: TryInto<To, Error = Error>,
    {
        from.as_ref().try_into().map(Arc::new)
    }
    impl TryFrom<&g::Program> for l::Program {
        type Error = anyhow::Error;
        fn try_from(value: &g::Program) -> Result<Self, Self::Error> {
            Ok(Self {
                items: Arc::new(
                    VecDeque::from(value.items.as_ref())
                        .into_iter()
                        .map(try_convert)
                        .collect::<Result<_, _>>()?,
                ),
            })
        }
    }
    impl<T> From<&g::List<T>> for VecDeque<Arc<T>> {
        fn from(value: &g::List<T>) -> Self {
            match value {
                g::List::Empty(_) => Default::default(),
                g::List::Link(link) => {
                    let mut vec = VecDeque::from(link.next.as_ref());
                    vec.push_front(link.item.clone());
                    vec
                }
            }
        }
    }
    impl<T> From<&g::CommaList<T>> for VecDeque<Arc<T>> {
        fn from(value: &g::CommaList<T>) -> Self {
            match value {
                g::CommaList::Empty(_) => Default::default(),
                g::CommaList::Tail(tail) => VecDeque::from([tail.clone()]),
                g::CommaList::Link(link) => {
                    let mut vec = VecDeque::from(link.next.as_ref());
                    vec.push_front(link.item.clone());
                    vec
                }
            }
        }
    }
    impl<T> From<&g::SemiList<T>> for VecDeque<Arc<T>> {
        fn from(value: &g::SemiList<T>) -> Self {
            match value {
                g::SemiList::Empty(_) => Default::default(),
                g::SemiList::Tail(tail) => VecDeque::from([tail.clone()]),
                g::SemiList::Link(link) => {
                    let mut vec = VecDeque::from(link.next.as_ref());
                    vec.push_front(link.item.clone());
                    vec
                }
            }
        }
    }
    impl TryFrom<&g::TopItem> for l::TopItem {
        type Error = anyhow::Error;
        fn try_from(value: &g::TopItem) -> Result<Self, Self::Error> {
            Ok(
                match value {
                    g::TopItem::Main(main) => Self::Main(try_convert(main)?),
                    g::TopItem::Func(func) => Self::Func(try_convert(func)?),
                },
            )
        }
    }
    impl TryFrom<&g::Main> for l::Main {
        type Error = anyhow::Error;
        fn try_from(value: &g::Main) -> Result<Self, Self::Error> {
            Ok(Self {
                proc: try_convert(&value.proc)?,
            })
        }
    }
    impl TryFrom<&g::Func> for l::Func {
        type Error = anyhow::Error;
        fn try_from(value: &g::Func) -> Result<Self, Self::Error> {
            Ok(Self {
                name: convert(&value.name),
                params: Arc::new(
                    VecDeque::from(value.params.as_ref())
                        .into_iter()
                        .map(try_convert)
                        .collect::<Result<_, _>>()?,
                ),
                proc: try_convert(&value.proc)?,
            })
        }
    }
    impl From<&g::Name> for l::Name {
        fn from(value: &g::Name) -> Self {
            Self { str: value.str.clone() }
        }
    }
    impl TryFrom<&g::IdentDeclaration> for l::IdentDeclaration {
        type Error = anyhow::Error;
        fn try_from(value: &g::IdentDeclaration) -> Result<Self, Self::Error> {
            Ok(Self {
                name: convert(&value.name),
                ty: try_convert(&value.ty)?,
            })
        }
    }
    impl TryFrom<&g::Type> for l::Type {
        type Error = anyhow::Error;
        fn try_from(value: &g::Type) -> Result<Self, Self::Error> {
            Ok(
                match value {
                    g::Type::Base(base) => Self::Base(convert(base)),
                    g::Type::Array(array) => Self::Array(try_convert(array)?),
                    g::Type::Ref(ref_ty) => Self::Ref(try_convert(ref_ty)?),
                },
            )
        }
    }
    impl From<&g::BaseType> for l::BaseType {
        fn from(value: &g::BaseType) -> Self {
            Self { name: convert(&value.name) }
        }
    }
    impl TryFrom<&g::ArrayType> for l::ArrayType {
        type Error = anyhow::Error;
        fn try_from(value: &g::ArrayType) -> Result<Self, Self::Error> {
            let len = &value.len.str;
            let len = len
                .parse()
                .context(
                    ::alloc::__export::must_use({
                        let res = ::alloc::fmt::format(
                            format_args!(
                                "Array type length should be a positive integer; Found: {0}",
                                len,
                            ),
                        );
                        res
                    }),
                )?;
            Ok(Self {
                ty: try_convert(&value.ty)?,
                len,
            })
        }
    }
    impl TryFrom<&g::RefType> for l::RefType {
        type Error = anyhow::Error;
        fn try_from(value: &g::RefType) -> Result<Self, Self::Error> {
            Ok(Self {
                ty: try_convert(&value.ty)?,
            })
        }
    }
    impl TryFrom<&g::Proc> for l::Proc {
        type Error = anyhow::Error;
        fn try_from(value: &g::Proc) -> Result<Self, Self::Error> {
            Ok(Self {
                idents: Arc::new(
                    VecDeque::from(value.idents.as_ref())
                        .into_iter()
                        .map(try_convert)
                        .collect::<Result<_, _>>()?,
                ),
                body: try_convert(&value.body)?,
            })
        }
    }
    impl TryFrom<&g::Body> for l::Body {
        type Error = anyhow::Error;
        fn try_from(value: &g::Body) -> Result<Self, Self::Error> {
            Ok(Self {
                items: Arc::new(
                    VecDeque::from(value.items.as_ref())
                        .into_iter()
                        .map(try_convert)
                        .collect::<Result<_, _>>()?,
                ),
            })
        }
    }
    impl TryFrom<&g::BodyItem> for l::BodyItem {
        type Error = anyhow::Error;
        fn try_from(value: &g::BodyItem) -> Result<Self, Self::Error> {
            Ok(
                match value {
                    g::BodyItem::Statement(x) => Self::Statement(try_convert(x)?),
                    g::BodyItem::If(x) => Self::If(try_convert(x)?),
                    g::BodyItem::While(x) => Self::While(try_convert(x)?),
                },
            )
        }
    }
    impl TryFrom<&g::Statement> for l::Statement {
        type Error = anyhow::Error;
        fn try_from(value: &g::Statement) -> Result<Self, Self::Error> {
            Ok(
                match value {
                    g::Statement::Assign(x) => Self::Assign(try_convert(x)?),
                    g::Statement::Call(x) => Self::Call(try_convert(x)?),
                },
            )
        }
    }
    impl TryFrom<&g::IfItem> for l::IfItem {
        type Error = anyhow::Error;
        fn try_from(value: &g::IfItem) -> Result<Self, Self::Error> {
            Ok(Self {
                condition: convert(&value.condition),
                then_body: try_convert(&value.then_body)?,
                else_item: match convert_maybe(&value.else_item) {
                    None => None,
                    Some(x) => Some(try_convert(x)?),
                },
            })
        }
    }
    impl TryFrom<&g::WhileItem> for l::WhileItem {
        type Error = anyhow::Error;
        fn try_from(value: &g::WhileItem) -> Result<Self, Self::Error> {
            Ok(Self {
                condition: convert(&value.condition),
                body: try_convert(&value.body)?,
            })
        }
    }
    impl TryFrom<&g::Assign> for l::Assign {
        type Error = anyhow::Error;
        fn try_from(value: &g::Assign) -> Result<Self, Self::Error> {
            Ok(Self {
                place: convert(&value.place),
                expr: try_convert(&value.expr)?,
            })
        }
    }
    impl TryFrom<&g::FunctionCall> for l::FunctionCall {
        type Error = anyhow::Error;
        fn try_from(value: &g::FunctionCall) -> Result<Self, Self::Error> {
            Ok(Self {
                func_name: convert(&value.func_name),
                param_exprs: Arc::new(
                    VecDeque::from(value.param_exprs.as_ref())
                        .into_iter()
                        .map(try_convert)
                        .collect::<Result<_, _>>()?,
                ),
            })
        }
    }
    impl From<&g::Expr> for l::Expr {
        fn from(value: &g::Expr) -> Self {
            match value {
                g::Expr::Literal(x) => Self::Literal(convert(x)),
                g::Expr::Place(x) => Self::Place(convert(x)),
                g::Expr::Ref(x) => Self::Ref(convert(x)),
                g::Expr::Paren(x) => Self::Paren(convert(x)),
            }
        }
    }
    impl TryFrom<&g::ElseItem> for l::ElseItem {
        type Error = anyhow::Error;
        fn try_from(value: &g::ElseItem) -> Result<Self, Self::Error> {
            Ok(
                match value {
                    g::ElseItem::Body(x) => {
                        l::ElseItem {
                            body: try_convert(&x.body)?,
                        }
                    }
                    g::ElseItem::If(x) => {
                        l::ElseItem {
                            body: Arc::new(l::Body {
                                items: Arc::new(
                                    Vec::from([
                                        Arc::new(l::BodyItem::If(try_convert(&x.if_item)?)),
                                    ]),
                                ),
                            }),
                        }
                    }
                },
            )
        }
    }
    impl TryFrom<&g::AssignExpr> for l::AssignExpr {
        type Error = anyhow::Error;
        fn try_from(value: &g::AssignExpr) -> Result<Self, Self::Error> {
            Ok(
                match value {
                    g::AssignExpr::Expr(expr) => Self::Expr(convert(expr)),
                    g::AssignExpr::Span(span) => Self::Span(convert(span)),
                    g::AssignExpr::Slice(slice) => Self::Slice(try_convert(slice)?),
                },
            )
        }
    }
    impl From<&g::Place> for l::Place {
        fn from(value: &g::Place) -> Self {
            Self {
                head: convert(unnest(&value.head)),
                offset: convert_maybe(&value.offset).map(convert),
            }
        }
    }
    impl From<&g::Literal> for l::Literal {
        fn from(value: &g::Literal) -> Self {
            Self { str: value.str.clone() }
        }
    }
    impl From<&g::RefExpr> for l::RefExpr {
        fn from(value: &g::RefExpr) -> Self {
            Self {
                place: convert(unnest(&value.place)),
            }
        }
    }
    impl From<&g::ParenExpr> for l::ParenExpr {
        fn from(value: &g::ParenExpr) -> Self {
            match value {
                g::ParenExpr::Unary(x) => Self::Unary(convert(x)),
                g::ParenExpr::Binary(x) => Self::Binary(convert(x)),
            }
        }
    }
    impl From<&g::Span> for l::Span {
        fn from(value: &g::Span) -> Self {
            Self {
                elements: Arc::new(
                    VecDeque::from(value.elements.as_ref())
                        .into_iter()
                        .map(convert)
                        .collect(),
                ),
            }
        }
    }
    impl TryFrom<&g::Slice> for l::Slice {
        type Error = anyhow::Error;
        fn try_from(value: &g::Slice) -> Result<Self, Self::Error> {
            let start_in = convert_maybe(value.start_in.as_ref())
                .map(convert::<_, l::Literal>);
            let start_in = match start_in {
                None => 0,
                Some(x) => {
                    x.str
                        .parse()
                        .context(
                            "Inclusive start of slice range should be a positive integer",
                        )?
                }
            };
            let end_ex = value
                .end_ex
                .str
                .parse()
                .context("Exclusive end of slice range should be a positive integer")?;
            Ok(Self {
                place: convert(&value.place),
                start_in,
                end_ex,
            })
        }
    }
    impl From<&g::PlaceHead> for l::PlaceHead {
        fn from(value: &g::PlaceHead) -> Self {
            match value {
                g::PlaceHead::Ident(ident) => Self::Ident(convert(ident)),
                g::PlaceHead::Deref(deref) => Self::Deref(convert(deref)),
            }
        }
    }
    fn unnest<T>(nest: &g::ParensNest<T>) -> &Arc<T> {
        match nest {
            g::ParensNest::Root(x) => x,
            g::ParensNest::Wrapped(wrapped) => unnest(&wrapped.item),
        }
    }
    impl From<&g::Offset> for l::Offset {
        fn from(value: &g::Offset) -> Self {
            Self { expr: convert(&value.expr) }
        }
    }
    fn convert_maybe<T>(maybe: &g::Maybe<T>) -> Option<&Arc<T>> {
        match maybe {
            g::Maybe::Item(x) => Some(x),
            g::Maybe::Empty(_) => None,
        }
    }
    impl From<&g::UnaryParenExpr> for l::UnaryParenExpr {
        fn from(value: &g::UnaryParenExpr) -> Self {
            Self {
                op: value.op.into(),
                operand: convert(&value.operand),
            }
        }
    }
    impl From<&g::BinaryParenExpr> for l::BinaryParenExpr {
        fn from(value: &g::BinaryParenExpr) -> Self {
            Self {
                left: convert(&value.left),
                op: value.op.into(),
                right: convert(&value.right),
            }
        }
    }
    impl From<&g::Ident> for l::Ident {
        fn from(value: &g::Ident) -> Self {
            Self { name: convert(&value.name) }
        }
    }
    impl From<&g::Deref> for l::Deref {
        fn from(value: &g::Deref) -> Self {
            Self { addr: convert(&value.addr) }
        }
    }
    impl From<g::UnaryParenExprOp> for l::UnaryParenExprOp {
        fn from(value: g::UnaryParenExprOp) -> Self {
            match value {
                g::UnaryParenExprOp::Not(_) => Self::Not,
            }
        }
    }
    impl From<g::BinaryParenExprOp> for l::BinaryParenExprOp {
        fn from(value: g::BinaryParenExprOp) -> Self {
            match value {
                g::BinaryParenExprOp::Add(_) => Self::Add,
                g::BinaryParenExprOp::Sub(_) => Self::Sub,
                g::BinaryParenExprOp::Mul(_) => Self::Mul,
                g::BinaryParenExprOp::Div(_) => Self::Div,
                g::BinaryParenExprOp::Mod(_) => Self::Mod,
                g::BinaryParenExprOp::Eq(_) => Self::Eq,
                g::BinaryParenExprOp::Neq(_) => Self::Neq,
                g::BinaryParenExprOp::Gt(_) => Self::Gt,
                g::BinaryParenExprOp::Lt(_) => Self::Lt,
                g::BinaryParenExprOp::Gte(_) => Self::Gte,
                g::BinaryParenExprOp::Lte(_) => Self::Lte,
                g::BinaryParenExprOp::And(_) => Self::And,
                g::BinaryParenExprOp::Or(_) => Self::Or,
                g::BinaryParenExprOp::Join(_) => Self::Join,
            }
        }
    }
}
mod link {
    use std::{iter::Peekable, sync::Arc};
    use itertools::{chain, Itertools};
    use uuid::Uuid;
    use crate::logic_ast;
    pub struct Proc {
        pub kind: ProcKind,
        pub idents: Arc<Vec<Arc<logic_ast::IdentDeclaration>>>,
        pub sub_procs: Arc<Vec<Arc<SubProc>>>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Proc {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "Proc",
                "kind",
                &self.kind,
                "idents",
                &self.idents,
                "sub_procs",
                &&self.sub_procs,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Proc {
        #[inline]
        fn clone(&self) -> Proc {
            Proc {
                kind: ::core::clone::Clone::clone(&self.kind),
                idents: ::core::clone::Clone::clone(&self.idents),
                sub_procs: ::core::clone::Clone::clone(&self.sub_procs),
            }
        }
    }
    pub enum ProcKind {
        Main,
        Func {
            name: Arc<logic_ast::Name>,
            params: Arc<Vec<Arc<logic_ast::IdentDeclaration>>>,
        },
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ProcKind {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                ProcKind::Main => ::core::fmt::Formatter::write_str(f, "Main"),
                ProcKind::Func { name: __self_0, params: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Func",
                        "name",
                        __self_0,
                        "params",
                        &__self_1,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ProcKind {
        #[inline]
        fn clone(&self) -> ProcKind {
            match self {
                ProcKind::Main => ProcKind::Main,
                ProcKind::Func { name: __self_0, params: __self_1 } => {
                    ProcKind::Func {
                        name: ::core::clone::Clone::clone(__self_0),
                        params: ::core::clone::Clone::clone(__self_1),
                    }
                }
            }
        }
    }
    pub struct SubProc {
        pub uuid: Uuid,
        pub statements: Arc<Vec<Arc<Statement>>>,
        pub next_call: Arc<Call>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for SubProc {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "SubProc",
                "uuid",
                &self.uuid,
                "statements",
                &self.statements,
                "next_call",
                &&self.next_call,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for SubProc {
        #[inline]
        fn clone(&self) -> SubProc {
            SubProc {
                uuid: ::core::clone::Clone::clone(&self.uuid),
                statements: ::core::clone::Clone::clone(&self.statements),
                next_call: ::core::clone::Clone::clone(&self.next_call),
            }
        }
    }
    pub enum Call {
        Func {
            name: Arc<logic_ast::Name>,
            param_exprs: Arc<Vec<Arc<logic_ast::AssignExpr>>>,
            return_sub_proc: Uuid,
        },
        SubProc(Uuid),
        IfElseBranch {
            cond_expr: Arc<logic_ast::Expr>,
            then_sub_proc: Uuid,
            else_sub_proc: Uuid,
        },
        Return,
        Terminate,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Call {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Call::Func {
                    name: __self_0,
                    param_exprs: __self_1,
                    return_sub_proc: __self_2,
                } => {
                    ::core::fmt::Formatter::debug_struct_field3_finish(
                        f,
                        "Func",
                        "name",
                        __self_0,
                        "param_exprs",
                        __self_1,
                        "return_sub_proc",
                        &__self_2,
                    )
                }
                Call::SubProc(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "SubProc",
                        &__self_0,
                    )
                }
                Call::IfElseBranch {
                    cond_expr: __self_0,
                    then_sub_proc: __self_1,
                    else_sub_proc: __self_2,
                } => {
                    ::core::fmt::Formatter::debug_struct_field3_finish(
                        f,
                        "IfElseBranch",
                        "cond_expr",
                        __self_0,
                        "then_sub_proc",
                        __self_1,
                        "else_sub_proc",
                        &__self_2,
                    )
                }
                Call::Return => ::core::fmt::Formatter::write_str(f, "Return"),
                Call::Terminate => ::core::fmt::Formatter::write_str(f, "Terminate"),
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Call {
        #[inline]
        fn clone(&self) -> Call {
            match self {
                Call::Func {
                    name: __self_0,
                    param_exprs: __self_1,
                    return_sub_proc: __self_2,
                } => {
                    Call::Func {
                        name: ::core::clone::Clone::clone(__self_0),
                        param_exprs: ::core::clone::Clone::clone(__self_1),
                        return_sub_proc: ::core::clone::Clone::clone(__self_2),
                    }
                }
                Call::SubProc(__self_0) => {
                    Call::SubProc(::core::clone::Clone::clone(__self_0))
                }
                Call::IfElseBranch {
                    cond_expr: __self_0,
                    then_sub_proc: __self_1,
                    else_sub_proc: __self_2,
                } => {
                    Call::IfElseBranch {
                        cond_expr: ::core::clone::Clone::clone(__self_0),
                        then_sub_proc: ::core::clone::Clone::clone(__self_1),
                        else_sub_proc: ::core::clone::Clone::clone(__self_2),
                    }
                }
                Call::Return => Call::Return,
                Call::Terminate => Call::Terminate,
            }
        }
    }
    pub enum Statement {
        Assign(Arc<logic_ast::Assign>),
        Native(Arc<logic_ast::NativeOperation>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Statement {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Statement::Assign(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Assign",
                        &__self_0,
                    )
                }
                Statement::Native(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Native",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Statement {
        #[inline]
        fn clone(&self) -> Statement {
            match self {
                Statement::Assign(__self_0) => {
                    Statement::Assign(::core::clone::Clone::clone(__self_0))
                }
                Statement::Native(__self_0) => {
                    Statement::Native(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub fn link(ast: &logic_ast::Program) -> anyhow::Result<Vec<Arc<Proc>>> {
        fn gen_decl(
            name: &str,
            ty: logic_ast::Type,
        ) -> Arc<logic_ast::IdentDeclaration> {
            Arc::new(logic_ast::IdentDeclaration {
                name: Arc::new(logic_ast::Name {
                    str: name.into(),
                }),
                ty: Arc::new(ty),
            })
        }
        fn gen_ident(name: &str) -> Arc<logic_ast::Place> {
            Arc::new(logic_ast::Place {
                head: Arc::new(
                    logic_ast::PlaceHead::Ident(
                        Arc::new(logic_ast::Ident {
                            name: gen_name(name),
                        }),
                    ),
                ),
                offset: None,
            })
        }
        fn gen_deref(addr: Arc<logic_ast::Expr>) -> Arc<logic_ast::Place> {
            Arc::new(logic_ast::Place {
                head: Arc::new(
                    logic_ast::PlaceHead::Deref(Arc::new(logic_ast::Deref { addr })),
                ),
                offset: None,
            })
        }
        fn gen_name(name: &str) -> Arc<logic_ast::Name> {
            Arc::new(logic_ast::Name {
                str: name.into(),
            })
        }
        fn gen_statement_item(
            statement: logic_ast::Statement,
        ) -> Arc<logic_ast::BodyItem> {
            Arc::new(logic_ast::BodyItem::Statement(Arc::new(statement)))
        }
        fn gen_body(
            items: impl IntoIterator<Item = Arc<logic_ast::BodyItem>>,
        ) -> Arc<logic_ast::Body> {
            Arc::new(logic_ast::Body {
                items: Arc::new(items.into_iter().collect()),
            })
        }
        fn gen_func_item(func: logic_ast::Func) -> Arc<logic_ast::TopItem> {
            Arc::new(logic_ast::TopItem::Func(Arc::new(func)))
        }
        fn gen_base_ty(name: &str) -> Arc<logic_ast::BaseType> {
            Arc::new(logic_ast::BaseType {
                name: gen_name(name),
            })
        }
        fn gen_ref_ty(ty: logic_ast::Type) -> Arc<logic_ast::RefType> {
            Arc::new(logic_ast::RefType {
                ty: Arc::new(ty),
            })
        }
        let out_func = gen_func_item(logic_ast::Func {
            name: gen_name("out"),
            params: Arc::new(
                Vec::from([gen_decl("val", logic_ast::Type::Base(gen_base_ty("any")))]),
            ),
            proc: Arc::new(logic_ast::Proc {
                idents: Arc::new(Vec::from([])),
                body: gen_body([
                    gen_statement_item(
                        logic_ast::Statement::Native(
                            Arc::new(logic_ast::NativeOperation::Out {
                                ident: gen_ident("val"),
                            }),
                        ),
                    ),
                ]),
            }),
        });
        let in_func = gen_func_item(logic_ast::Func {
            name: gen_name("in"),
            params: Arc::new(
                Vec::from([
                    gen_decl(
                        "dest_ref",
                        logic_ast::Type::Ref(
                            gen_ref_ty(logic_ast::Type::Base(gen_base_ty("val"))),
                        ),
                    ),
                ]),
            ),
            proc: Arc::new(logic_ast::Proc {
                idents: Arc::new(
                    Vec::from([
                        gen_decl("answer", logic_ast::Type::Base(gen_base_ty("val"))),
                    ]),
                ),
                body: gen_body([
                    gen_statement_item(
                        logic_ast::Statement::Native(
                            Arc::new(logic_ast::NativeOperation::In {
                                dest_ident: gen_ident("answer"),
                            }),
                        ),
                    ),
                    gen_statement_item(
                        logic_ast::Statement::Assign(
                            Arc::new(logic_ast::Assign {
                                place: gen_deref(
                                    Arc::new(logic_ast::Expr::Place(gen_ident("dest_ref"))),
                                ),
                                expr: Arc::new(
                                    logic_ast::AssignExpr::Expr(
                                        Arc::new(logic_ast::Expr::Place(gen_ident("answer"))),
                                    ),
                                ),
                            }),
                        ),
                    ),
                ]),
            }),
        });
        let top_items = {
            let iter = ::itertools::__std_iter::IntoIterator::into_iter(
                ast.items.iter().cloned(),
            );
            let iter = ::itertools::__std_iter::Iterator::chain(
                iter,
                ::itertools::__std_iter::IntoIterator::into_iter([out_func, in_func]),
            );
            iter
        }
            .collect_vec();
        let mut procs = Vec::<Arc<Proc>>::new();
        for top_item in top_items.iter().map(AsRef::as_ref) {
            let (proc_kind, ast_proc) = match top_item {
                logic_ast::TopItem::Main(main) => {
                    (ProcKind::Main, Arc::clone(&main.proc))
                }
                logic_ast::TopItem::Func(func) => {
                    (
                        ProcKind::Func {
                            name: Arc::clone(&func.name),
                            params: Arc::clone(&func.params),
                        },
                        Arc::clone(&func.proc),
                    )
                }
            };
            let mut body_items = ast_proc
                .body
                .items
                .iter()
                .map(AsRef::as_ref)
                .peekable();
            let create_sub_proc_res = create_sub_proc(
                &mut body_items,
                match proc_kind {
                    ProcKind::Main => true,
                    _ => false,
                },
                None,
            );
            let sub_procs = Vec::from([Arc::new(create_sub_proc_res.root)])
                .into_iter()
                .chain(create_sub_proc_res.rest)
                .collect_vec();
            let proc = Proc {
                kind: proc_kind,
                idents: Arc::clone(&ast_proc.idents),
                sub_procs: Arc::new(sub_procs),
            };
            procs.push(Arc::new(proc));
        }
        Ok(procs)
    }
    struct CreateSubProcRes {
        root: SubProc,
        rest: Vec<Arc<SubProc>>,
    }
    fn create_sub_proc<'a>(
        body_items: &mut Peekable<impl Iterator<Item = &'a logic_ast::BodyItem>>,
        main: bool,
        pop_sub_proc: Option<Uuid>,
    ) -> CreateSubProcRes {
        let mut rest_sps = Vec::<Arc<SubProc>>::new();
        let mut statements = Vec::<Arc<Statement>>::new();
        let mut next_call = None;
        while let Some(body_item) = body_items.next() {
            match body_item {
                logic_ast::BodyItem::If(if_item) => {
                    let pop_sp = {
                        let sp_res = create_sub_proc(body_items, main, pop_sub_proc);
                        let sp = Arc::new(sp_res.root);
                        rest_sps.push(Arc::clone(&sp));
                        rest_sps.extend(sp_res.rest);
                        sp
                    };
                    let then_sp = {
                        let sp_res = create_sub_proc(
                            &mut if_item
                                .then_body
                                .items
                                .iter()
                                .map(AsRef::as_ref)
                                .peekable(),
                            main,
                            Some(pop_sp.uuid),
                        );
                        let sp = Arc::new(sp_res.root);
                        rest_sps.push(Arc::clone(&sp));
                        rest_sps.extend(sp_res.rest);
                        sp
                    };
                    let call = match &if_item.else_item {
                        None => {
                            Call::IfElseBranch {
                                cond_expr: Arc::clone(&if_item.condition),
                                then_sub_proc: then_sp.uuid,
                                else_sub_proc: pop_sp.uuid,
                            }
                        }
                        Some(else_item) => {
                            let else_sp = {
                                let sp_res = create_sub_proc(
                                    &mut else_item
                                        .body
                                        .items
                                        .iter()
                                        .map(AsRef::as_ref)
                                        .peekable(),
                                    main,
                                    Some(pop_sp.uuid),
                                );
                                let sp = Arc::new(sp_res.root);
                                rest_sps.push(Arc::clone(&sp));
                                rest_sps.extend(sp_res.rest);
                                sp
                            };
                            Call::IfElseBranch {
                                cond_expr: Arc::clone(&if_item.condition),
                                then_sub_proc: then_sp.uuid,
                                else_sub_proc: else_sp.uuid,
                            }
                        }
                    };
                    next_call = Some(call);
                }
                logic_ast::BodyItem::While(while_item) => {
                    let pop_sp = {
                        let sp_res = create_sub_proc(body_items, main, pop_sub_proc);
                        let sp = Arc::new(sp_res.root);
                        rest_sps.push(Arc::clone(&sp));
                        rest_sps.extend(sp_res.rest);
                        sp
                    };
                    let check_uuid = Uuid::new_v4();
                    let then_sp = {
                        let sp_res = create_sub_proc(
                            &mut while_item
                                .body
                                .items
                                .iter()
                                .map(AsRef::as_ref)
                                .peekable(),
                            main,
                            Some(check_uuid),
                        );
                        let sp = Arc::new(sp_res.root);
                        rest_sps.push(Arc::clone(&sp));
                        rest_sps.extend(sp_res.rest);
                        sp
                    };
                    let check_sp = SubProc {
                        uuid: check_uuid,
                        statements: Arc::new(Vec::new()),
                        next_call: Arc::new(Call::IfElseBranch {
                            cond_expr: Arc::clone(&while_item.condition),
                            then_sub_proc: then_sp.uuid,
                            else_sub_proc: pop_sp.uuid,
                        }),
                    };
                    let call = Call::SubProc(check_sp.uuid);
                    rest_sps.push(Arc::new(check_sp));
                    next_call = Some(call);
                }
                logic_ast::BodyItem::Statement(statement) => {
                    match statement.as_ref() {
                        logic_ast::Statement::Assign(assign) => {
                            statements
                                .push(Arc::new(Statement::Assign(Arc::clone(assign))));
                        }
                        logic_ast::Statement::Native(native) => {
                            statements
                                .push(Arc::new(Statement::Native(Arc::clone(native))));
                        }
                        logic_ast::Statement::Call(call) => {
                            let return_sp = {
                                let sp_res = create_sub_proc(
                                    body_items,
                                    main,
                                    pop_sub_proc,
                                );
                                let sp = Arc::new(sp_res.root);
                                rest_sps.push(Arc::clone(&sp));
                                rest_sps.extend(sp_res.rest);
                                sp
                            };
                            next_call = Some(Call::Func {
                                name: Arc::clone(&call.func_name),
                                param_exprs: Arc::clone(&call.param_exprs),
                                return_sub_proc: return_sp.uuid,
                            });
                        }
                    }
                }
            }
        }
        let next_call = match next_call {
            Some(next_call) => next_call,
            None => {
                if body_items.peek().is_none() {
                    match pop_sub_proc {
                        Some(pop_sub_proc) => Call::SubProc(pop_sub_proc),
                        None => {
                            match main {
                                true => Call::Terminate,
                                false => Call::Return,
                            }
                        }
                    }
                } else {
                    let return_sp = {
                        let sp_res = create_sub_proc(body_items, main, None);
                        let sp = Arc::new(sp_res.root);
                        rest_sps.push(Arc::clone(&sp));
                        rest_sps.extend(sp_res.rest);
                        sp
                    };
                    Call::SubProc(return_sp.uuid)
                }
            }
        };
        let sp = SubProc {
            uuid: Uuid::new_v4(),
            statements: Arc::new(statements),
            next_call: Arc::new(next_call),
        };
        CreateSubProcRes {
            root: sp,
            rest: rest_sps,
        }
    }
}
mod logic_ast {
    use std::sync::Arc;
    use crate::grammar_ast as g;
    use crate::{ast_error::AstErrorSet, token_feed::TokenFeed};
    pub enum TopItem {
        Main(Arc<Main>),
        Func(Arc<Func>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for TopItem {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                TopItem::Main(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Main",
                        &__self_0,
                    )
                }
                TopItem::Func(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Func",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct Main {
        pub proc: Arc<Proc>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Main {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Main",
                "proc",
                &&self.proc,
            )
        }
    }
    pub struct Func {
        pub name: Arc<Name>,
        pub params: Arc<Vec<Arc<IdentDeclaration>>>,
        pub proc: Arc<Proc>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Func {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "Func",
                "name",
                &self.name,
                "params",
                &self.params,
                "proc",
                &&self.proc,
            )
        }
    }
    pub struct Name {
        pub str: Arc<str>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Name {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Name",
                "str",
                &&self.str,
            )
        }
    }
    #[automatically_derived]
    impl ::core::hash::Hash for Name {
        #[inline]
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            ::core::hash::Hash::hash(&self.str, state)
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Name {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Name {
        #[inline]
        fn eq(&self, other: &Name) -> bool {
            self.str == other.str
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for Name {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<Arc<str>>;
        }
    }
    pub enum Type {
        Ref(Arc<RefType>),
        Array(Arc<ArrayType>),
        Base(Arc<BaseType>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Type {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Type::Ref(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Ref",
                        &__self_0,
                    )
                }
                Type::Array(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Array",
                        &__self_0,
                    )
                }
                Type::Base(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Base",
                        &__self_0,
                    )
                }
            }
        }
    }
    impl Type {
        pub fn size(&self) -> u32 {
            match self {
                Self::Ref(_) => 1,
                Self::Base(_) => 1,
                Self::Array(arr) => &arr.ty.size() * arr.len,
            }
        }
    }
    pub struct RefType {
        pub ty: Arc<Type>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for RefType {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "RefType",
                "ty",
                &&self.ty,
            )
        }
    }
    pub struct ArrayType {
        pub ty: Arc<Type>,
        pub len: u32,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ArrayType {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "ArrayType",
                "ty",
                &self.ty,
                "len",
                &&self.len,
            )
        }
    }
    pub struct BaseType {
        pub name: Arc<Name>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for BaseType {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "BaseType",
                "name",
                &&self.name,
            )
        }
    }
    pub struct IdentDeclaration {
        pub name: Arc<Name>,
        pub ty: Arc<Type>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for IdentDeclaration {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "IdentDeclaration",
                "name",
                &self.name,
                "ty",
                &&self.ty,
            )
        }
    }
    pub struct Place {
        pub head: Arc<PlaceHead>,
        pub offset: Option<Arc<Offset>>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Place {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "Place",
                "head",
                &self.head,
                "offset",
                &&self.offset,
            )
        }
    }
    pub enum PlaceHead {
        Ident(Arc<Ident>),
        Deref(Arc<Deref>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for PlaceHead {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                PlaceHead::Ident(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Ident",
                        &__self_0,
                    )
                }
                PlaceHead::Deref(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Deref",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct Offset {
        pub expr: Arc<Expr>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Offset {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Offset",
                "expr",
                &&self.expr,
            )
        }
    }
    pub struct Ident {
        pub name: Arc<Name>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Ident {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Ident",
                "name",
                &&self.name,
            )
        }
    }
    pub struct Deref {
        pub addr: Arc<Expr>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Deref {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Deref",
                "addr",
                &&self.addr,
            )
        }
    }
    pub struct Proc {
        pub idents: Arc<Vec<Arc<IdentDeclaration>>>,
        pub body: Arc<Body>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Proc {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "Proc",
                "idents",
                &self.idents,
                "body",
                &&self.body,
            )
        }
    }
    pub struct Body {
        pub items: Arc<Vec<Arc<BodyItem>>>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Body {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Body",
                "items",
                &&self.items,
            )
        }
    }
    pub enum BodyItem {
        Statement(Arc<Statement>),
        If(Arc<IfItem>),
        While(Arc<WhileItem>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for BodyItem {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                BodyItem::Statement(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Statement",
                        &__self_0,
                    )
                }
                BodyItem::If(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "If", &__self_0)
                }
                BodyItem::While(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "While",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct IfItem {
        pub condition: Arc<Expr>,
        pub then_body: Arc<Body>,
        pub else_item: Option<Arc<ElseItem>>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for IfItem {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "IfItem",
                "condition",
                &self.condition,
                "then_body",
                &self.then_body,
                "else_item",
                &&self.else_item,
            )
        }
    }
    pub struct ElseItem {
        pub body: Arc<Body>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ElseItem {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "ElseItem",
                "body",
                &&self.body,
            )
        }
    }
    pub struct WhileItem {
        pub condition: Arc<Expr>,
        pub body: Arc<Body>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for WhileItem {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "WhileItem",
                "condition",
                &self.condition,
                "body",
                &&self.body,
            )
        }
    }
    pub enum Statement {
        Assign(Arc<Assign>),
        Call(Arc<FunctionCall>),
        Native(Arc<NativeOperation>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Statement {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Statement::Assign(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Assign",
                        &__self_0,
                    )
                }
                Statement::Call(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Call",
                        &__self_0,
                    )
                }
                Statement::Native(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Native",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct FunctionCall {
        pub func_name: Arc<Name>,
        pub param_exprs: Arc<Vec<Arc<AssignExpr>>>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for FunctionCall {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "FunctionCall",
                "func_name",
                &self.func_name,
                "param_exprs",
                &&self.param_exprs,
            )
        }
    }
    pub struct Assign {
        pub place: Arc<Place>,
        pub expr: Arc<AssignExpr>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Assign {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "Assign",
                "place",
                &self.place,
                "expr",
                &&self.expr,
            )
        }
    }
    pub enum AssignExpr {
        Expr(Arc<Expr>),
        Span(Arc<Span>),
        Slice(Arc<Slice>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for AssignExpr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                AssignExpr::Expr(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Expr",
                        &__self_0,
                    )
                }
                AssignExpr::Span(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Span",
                        &__self_0,
                    )
                }
                AssignExpr::Slice(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Slice",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub enum Expr {
        Literal(Arc<Literal>),
        Place(Arc<Place>),
        Ref(Arc<RefExpr>),
        Paren(Arc<ParenExpr>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Expr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Expr::Literal(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Literal",
                        &__self_0,
                    )
                }
                Expr::Place(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Place",
                        &__self_0,
                    )
                }
                Expr::Ref(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Ref",
                        &__self_0,
                    )
                }
                Expr::Paren(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Paren",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct RefExpr {
        pub place: Arc<Place>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for RefExpr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "RefExpr",
                "place",
                &&self.place,
            )
        }
    }
    pub struct Literal {
        pub str: Arc<str>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Literal {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Literal",
                "str",
                &&self.str,
            )
        }
    }
    pub struct Span {
        pub elements: Arc<Vec<Arc<Expr>>>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Span {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Span",
                "elements",
                &&self.elements,
            )
        }
    }
    pub struct Slice {
        pub place: Arc<Place>,
        pub start_in: u32,
        pub end_ex: u32,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Slice {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "Slice",
                "place",
                &self.place,
                "start_in",
                &self.start_in,
                "end_ex",
                &&self.end_ex,
            )
        }
    }
    pub enum ParenExpr {
        Unary(Arc<UnaryParenExpr>),
        Binary(Arc<BinaryParenExpr>),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ParenExpr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                ParenExpr::Unary(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Unary",
                        &__self_0,
                    )
                }
                ParenExpr::Binary(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Binary",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct UnaryParenExpr {
        pub op: UnaryParenExprOp,
        pub operand: Arc<Expr>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for UnaryParenExpr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "UnaryParenExpr",
                "op",
                &self.op,
                "operand",
                &&self.operand,
            )
        }
    }
    pub enum UnaryParenExprOp {
        Not,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for UnaryParenExprOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "Not")
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for UnaryParenExprOp {
        #[inline]
        fn clone(&self) -> UnaryParenExprOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for UnaryParenExprOp {}
    pub struct BinaryParenExpr {
        pub op: BinaryParenExprOp,
        pub left: Arc<Expr>,
        pub right: Arc<Expr>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for BinaryParenExpr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "BinaryParenExpr",
                "op",
                &self.op,
                "left",
                &self.left,
                "right",
                &&self.right,
            )
        }
    }
    pub enum BinaryParenExprOp {
        Add,
        Sub,
        Mul,
        Div,
        Mod,
        Eq,
        Neq,
        Gt,
        Lt,
        Gte,
        Lte,
        And,
        Or,
        Join,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for BinaryParenExprOp {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    BinaryParenExprOp::Add => "Add",
                    BinaryParenExprOp::Sub => "Sub",
                    BinaryParenExprOp::Mul => "Mul",
                    BinaryParenExprOp::Div => "Div",
                    BinaryParenExprOp::Mod => "Mod",
                    BinaryParenExprOp::Eq => "Eq",
                    BinaryParenExprOp::Neq => "Neq",
                    BinaryParenExprOp::Gt => "Gt",
                    BinaryParenExprOp::Lt => "Lt",
                    BinaryParenExprOp::Gte => "Gte",
                    BinaryParenExprOp::Lte => "Lte",
                    BinaryParenExprOp::And => "And",
                    BinaryParenExprOp::Or => "Or",
                    BinaryParenExprOp::Join => "Join",
                },
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for BinaryParenExprOp {
        #[inline]
        fn clone(&self) -> BinaryParenExprOp {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for BinaryParenExprOp {}
    pub enum NativeOperation {
        Out { ident: Arc<Place> },
        In { dest_ident: Arc<Place> },
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for NativeOperation {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                NativeOperation::Out { ident: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "Out",
                        "ident",
                        &__self_0,
                    )
                }
                NativeOperation::In { dest_ident: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "In",
                        "dest_ident",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct Program {
        pub items: Arc<Vec<Arc<TopItem>>>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Program {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Program",
                "items",
                &&self.items,
            )
        }
    }
    pub struct Comment {
        #[expect(unused)]
        pub text: Arc<str>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Comment {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "Comment",
                "text",
                &&self.text,
            )
        }
    }
}
mod src_pos {
    use std::fmt::Display;
    pub struct SrcPos {
        pub line: usize,
        pub col: usize,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for SrcPos {
        #[inline]
        fn clone(&self) -> SrcPos {
            let _: ::core::clone::AssertParamIsClone<usize>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for SrcPos {}
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for SrcPos {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for SrcPos {
        #[inline]
        fn eq(&self, other: &SrcPos) -> bool {
            self.line == other.line && self.col == other.col
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for SrcPos {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<usize>;
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for SrcPos {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "SrcPos",
                "line",
                &self.line,
                "col",
                &&self.col,
            )
        }
    }
    impl Default for SrcPos {
        fn default() -> Self {
            Self { line: 1, col: 1 }
        }
    }
    impl Ord for SrcPos {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.line.cmp(&other.line).then(self.col.cmp(&other.col))
        }
    }
    impl PartialOrd for SrcPos {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }
    pub struct SrcRange {
        pub start: SrcPos,
        pub end: SrcPos,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for SrcRange {
        #[inline]
        fn clone(&self) -> SrcRange {
            let _: ::core::clone::AssertParamIsClone<SrcPos>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for SrcRange {}
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for SrcRange {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for SrcRange {
        #[inline]
        fn eq(&self, other: &SrcRange) -> bool {
            self.start == other.start && self.end == other.end
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for SrcRange {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<SrcPos>;
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for SrcRange {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "SrcRange",
                "start",
                &self.start,
                "end",
                &&self.end,
            )
        }
    }
    #[automatically_derived]
    impl ::core::default::Default for SrcRange {
        #[inline]
        fn default() -> SrcRange {
            SrcRange {
                start: ::core::default::Default::default(),
                end: ::core::default::Default::default(),
            }
        }
    }
    impl SrcRange {
        pub fn new_zero_len(pos: SrcPos) -> Self {
            Self { start: pos, end: pos }
        }
        pub fn extend_to(self, pos: SrcPos) -> Self {
            Self {
                start: self.start.min(pos),
                end: self.end.max(pos),
            }
        }
    }
    impl Ord for SrcRange {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.start.cmp(&other.start).then(self.end.cmp(&other.end))
        }
    }
    impl PartialOrd for SrcRange {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }
}
mod token {
    use anyhow::bail;
    use itertools::{Itertools, MultiPeek};
    use crate::src_pos::{SrcPos, SrcRange};
    #[strum_discriminants(name(TokenKind), derive(Hash))]
    pub enum Token {
        Name(String),
        Eq,
        Semi,
        LeftParen,
        RightParen,
        LeftBrace,
        RightBrace,
        Pipe,
        Comma,
        Literal(String),
        Main,
        Func,
        If,
        Else,
        While,
        LeftBracket,
        RightBracket,
        Period,
        Comment(String),
        Asterisk,
        Slash,
        Bang,
        Plus,
        Dash,
        Tilde,
        LeftAngle,
        RightAngle,
        Percent,
        Ampersand,
        Colon,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Token {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Token::Name(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Name",
                        &__self_0,
                    )
                }
                Token::Eq => ::core::fmt::Formatter::write_str(f, "Eq"),
                Token::Semi => ::core::fmt::Formatter::write_str(f, "Semi"),
                Token::LeftParen => ::core::fmt::Formatter::write_str(f, "LeftParen"),
                Token::RightParen => ::core::fmt::Formatter::write_str(f, "RightParen"),
                Token::LeftBrace => ::core::fmt::Formatter::write_str(f, "LeftBrace"),
                Token::RightBrace => ::core::fmt::Formatter::write_str(f, "RightBrace"),
                Token::Pipe => ::core::fmt::Formatter::write_str(f, "Pipe"),
                Token::Comma => ::core::fmt::Formatter::write_str(f, "Comma"),
                Token::Literal(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Literal",
                        &__self_0,
                    )
                }
                Token::Main => ::core::fmt::Formatter::write_str(f, "Main"),
                Token::Func => ::core::fmt::Formatter::write_str(f, "Func"),
                Token::If => ::core::fmt::Formatter::write_str(f, "If"),
                Token::Else => ::core::fmt::Formatter::write_str(f, "Else"),
                Token::While => ::core::fmt::Formatter::write_str(f, "While"),
                Token::LeftBracket => ::core::fmt::Formatter::write_str(f, "LeftBracket"),
                Token::RightBracket => {
                    ::core::fmt::Formatter::write_str(f, "RightBracket")
                }
                Token::Period => ::core::fmt::Formatter::write_str(f, "Period"),
                Token::Comment(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Comment",
                        &__self_0,
                    )
                }
                Token::Asterisk => ::core::fmt::Formatter::write_str(f, "Asterisk"),
                Token::Slash => ::core::fmt::Formatter::write_str(f, "Slash"),
                Token::Bang => ::core::fmt::Formatter::write_str(f, "Bang"),
                Token::Plus => ::core::fmt::Formatter::write_str(f, "Plus"),
                Token::Dash => ::core::fmt::Formatter::write_str(f, "Dash"),
                Token::Tilde => ::core::fmt::Formatter::write_str(f, "Tilde"),
                Token::LeftAngle => ::core::fmt::Formatter::write_str(f, "LeftAngle"),
                Token::RightAngle => ::core::fmt::Formatter::write_str(f, "RightAngle"),
                Token::Percent => ::core::fmt::Formatter::write_str(f, "Percent"),
                Token::Ampersand => ::core::fmt::Formatter::write_str(f, "Ampersand"),
                Token::Colon => ::core::fmt::Formatter::write_str(f, "Colon"),
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Token {
        #[inline]
        fn clone(&self) -> Token {
            match self {
                Token::Name(__self_0) => {
                    Token::Name(::core::clone::Clone::clone(__self_0))
                }
                Token::Eq => Token::Eq,
                Token::Semi => Token::Semi,
                Token::LeftParen => Token::LeftParen,
                Token::RightParen => Token::RightParen,
                Token::LeftBrace => Token::LeftBrace,
                Token::RightBrace => Token::RightBrace,
                Token::Pipe => Token::Pipe,
                Token::Comma => Token::Comma,
                Token::Literal(__self_0) => {
                    Token::Literal(::core::clone::Clone::clone(__self_0))
                }
                Token::Main => Token::Main,
                Token::Func => Token::Func,
                Token::If => Token::If,
                Token::Else => Token::Else,
                Token::While => Token::While,
                Token::LeftBracket => Token::LeftBracket,
                Token::RightBracket => Token::RightBracket,
                Token::Period => Token::Period,
                Token::Comment(__self_0) => {
                    Token::Comment(::core::clone::Clone::clone(__self_0))
                }
                Token::Asterisk => Token::Asterisk,
                Token::Slash => Token::Slash,
                Token::Bang => Token::Bang,
                Token::Plus => Token::Plus,
                Token::Dash => Token::Dash,
                Token::Tilde => Token::Tilde,
                Token::LeftAngle => Token::LeftAngle,
                Token::RightAngle => Token::RightAngle,
                Token::Percent => Token::Percent,
                Token::Ampersand => Token::Ampersand,
                Token::Colon => Token::Colon,
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Token {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Token {
        #[inline]
        fn eq(&self, other: &Token) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
                && match (self, other) {
                    (Token::Name(__self_0), Token::Name(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (Token::Literal(__self_0), Token::Literal(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (Token::Comment(__self_0), Token::Comment(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    _ => true,
                }
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for Token {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<String>;
        }
    }
    /// Auto-generated discriminant enum variants
    pub enum TokenKind {
        Name,
        Eq,
        Semi,
        LeftParen,
        RightParen,
        LeftBrace,
        RightBrace,
        Pipe,
        Comma,
        Literal,
        Main,
        Func,
        If,
        Else,
        While,
        LeftBracket,
        RightBracket,
        Period,
        Comment,
        Asterisk,
        Slash,
        Bang,
        Plus,
        Dash,
        Tilde,
        LeftAngle,
        RightAngle,
        Percent,
        Ampersand,
        Colon,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for TokenKind {
        #[inline]
        fn clone(&self) -> TokenKind {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for TokenKind {}
    #[automatically_derived]
    impl ::core::fmt::Debug for TokenKind {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    TokenKind::Name => "Name",
                    TokenKind::Eq => "Eq",
                    TokenKind::Semi => "Semi",
                    TokenKind::LeftParen => "LeftParen",
                    TokenKind::RightParen => "RightParen",
                    TokenKind::LeftBrace => "LeftBrace",
                    TokenKind::RightBrace => "RightBrace",
                    TokenKind::Pipe => "Pipe",
                    TokenKind::Comma => "Comma",
                    TokenKind::Literal => "Literal",
                    TokenKind::Main => "Main",
                    TokenKind::Func => "Func",
                    TokenKind::If => "If",
                    TokenKind::Else => "Else",
                    TokenKind::While => "While",
                    TokenKind::LeftBracket => "LeftBracket",
                    TokenKind::RightBracket => "RightBracket",
                    TokenKind::Period => "Period",
                    TokenKind::Comment => "Comment",
                    TokenKind::Asterisk => "Asterisk",
                    TokenKind::Slash => "Slash",
                    TokenKind::Bang => "Bang",
                    TokenKind::Plus => "Plus",
                    TokenKind::Dash => "Dash",
                    TokenKind::Tilde => "Tilde",
                    TokenKind::LeftAngle => "LeftAngle",
                    TokenKind::RightAngle => "RightAngle",
                    TokenKind::Percent => "Percent",
                    TokenKind::Ampersand => "Ampersand",
                    TokenKind::Colon => "Colon",
                },
            )
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for TokenKind {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for TokenKind {
        #[inline]
        fn eq(&self, other: &TokenKind) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for TokenKind {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {}
    }
    #[automatically_derived]
    impl ::core::hash::Hash for TokenKind {
        #[inline]
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            ::core::hash::Hash::hash(&__self_discr, state)
        }
    }
    impl ::strum::IntoDiscriminant for Token {
        type Discriminant = TokenKind;
        #[inline]
        fn discriminant(&self) -> Self::Discriminant {
            <Self::Discriminant as ::core::convert::From<&Self>>::from(self)
        }
    }
    impl ::core::convert::From<Token> for TokenKind {
        #[inline]
        fn from(val: Token) -> TokenKind {
            match val {
                Token::Name(..) => TokenKind::Name,
                Token::Eq => TokenKind::Eq,
                Token::Semi => TokenKind::Semi,
                Token::LeftParen => TokenKind::LeftParen,
                Token::RightParen => TokenKind::RightParen,
                Token::LeftBrace => TokenKind::LeftBrace,
                Token::RightBrace => TokenKind::RightBrace,
                Token::Pipe => TokenKind::Pipe,
                Token::Comma => TokenKind::Comma,
                Token::Literal(..) => TokenKind::Literal,
                Token::Main => TokenKind::Main,
                Token::Func => TokenKind::Func,
                Token::If => TokenKind::If,
                Token::Else => TokenKind::Else,
                Token::While => TokenKind::While,
                Token::LeftBracket => TokenKind::LeftBracket,
                Token::RightBracket => TokenKind::RightBracket,
                Token::Period => TokenKind::Period,
                Token::Comment(..) => TokenKind::Comment,
                Token::Asterisk => TokenKind::Asterisk,
                Token::Slash => TokenKind::Slash,
                Token::Bang => TokenKind::Bang,
                Token::Plus => TokenKind::Plus,
                Token::Dash => TokenKind::Dash,
                Token::Tilde => TokenKind::Tilde,
                Token::LeftAngle => TokenKind::LeftAngle,
                Token::RightAngle => TokenKind::RightAngle,
                Token::Percent => TokenKind::Percent,
                Token::Ampersand => TokenKind::Ampersand,
                Token::Colon => TokenKind::Colon,
            }
        }
    }
    impl<'_enum> ::core::convert::From<&'_enum Token> for TokenKind {
        #[inline]
        fn from(val: &'_enum Token) -> TokenKind {
            match val {
                Token::Name(..) => TokenKind::Name,
                Token::Eq => TokenKind::Eq,
                Token::Semi => TokenKind::Semi,
                Token::LeftParen => TokenKind::LeftParen,
                Token::RightParen => TokenKind::RightParen,
                Token::LeftBrace => TokenKind::LeftBrace,
                Token::RightBrace => TokenKind::RightBrace,
                Token::Pipe => TokenKind::Pipe,
                Token::Comma => TokenKind::Comma,
                Token::Literal(..) => TokenKind::Literal,
                Token::Main => TokenKind::Main,
                Token::Func => TokenKind::Func,
                Token::If => TokenKind::If,
                Token::Else => TokenKind::Else,
                Token::While => TokenKind::While,
                Token::LeftBracket => TokenKind::LeftBracket,
                Token::RightBracket => TokenKind::RightBracket,
                Token::Period => TokenKind::Period,
                Token::Comment(..) => TokenKind::Comment,
                Token::Asterisk => TokenKind::Asterisk,
                Token::Slash => TokenKind::Slash,
                Token::Bang => TokenKind::Bang,
                Token::Plus => TokenKind::Plus,
                Token::Dash => TokenKind::Dash,
                Token::Tilde => TokenKind::Tilde,
                Token::LeftAngle => TokenKind::LeftAngle,
                Token::RightAngle => TokenKind::RightAngle,
                Token::Percent => TokenKind::Percent,
                Token::Ampersand => TokenKind::Ampersand,
                Token::Colon => TokenKind::Colon,
            }
        }
    }
    pub struct SrcToken {
        pub token: Token,
        pub range: SrcRange,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for SrcToken {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "SrcToken",
                "token",
                &self.token,
                "range",
                &&self.range,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for SrcToken {
        #[inline]
        fn clone(&self) -> SrcToken {
            SrcToken {
                token: ::core::clone::Clone::clone(&self.token),
                range: ::core::clone::Clone::clone(&self.range),
            }
        }
    }
    fn consume_char(
        chars: &mut impl Iterator<Item = SrcChar>,
        token: Option<Token>,
    ) -> anyhow::Result<Option<SrcToken>> {
        let Some(char) = chars.next() else {
            return ::anyhow::__private::Err({
                let error = ::anyhow::__private::format_err(
                    format_args!("Expected char"),
                );
                error
            })
        };
        Ok(
            token
                .map(|token| SrcToken {
                    token,
                    range: SrcRange::new_zero_len(char.pos),
                }),
        )
    }
    fn consume_literal(
        chars: &mut impl Iterator<Item = SrcChar>,
    ) -> anyhow::Result<SrcToken> {
        let Some(opening_quote) = chars.next() else {
            return ::anyhow::__private::Err({
                let error = ::anyhow::__private::format_err(
                    format_args!("Expected literal opening quote"),
                );
                error
            })
        };
        if opening_quote.char != '"' {
            return ::anyhow::__private::Err({
                let error = ::anyhow::__private::format_err(
                    format_args!("Expected literal opening quote"),
                );
                error
            });
        }
        let mut value = String::new();
        let mut range = SrcRange::new_zero_len(opening_quote.pos);
        let mut closed = false;
        for c in chars {
            range = range.extend_to(c.pos);
            if c.char == '"' {
                closed = true;
                break;
            }
            value.push(c.char);
        }
        if !closed {
            return ::anyhow::__private::Err({
                let error = ::anyhow::__private::format_err(
                    format_args!("Expected literal closing quote"),
                );
                error
            });
        }
        Ok(SrcToken {
            token: Token::Literal(value),
            range,
        })
    }
    fn consume_word(
        chars: &mut MultiPeek<impl Iterator<Item = SrcChar>>,
    ) -> anyhow::Result<SrcToken> {
        let mut word = String::new();
        let mut range: Option<SrcRange> = None;
        while let Some(&c) = {
            chars.reset_peek();
            chars.peek()
        } {
            if c.char.is_ascii_alphanumeric() || c.char == '_' {
                word.push(c.char);
                chars.next();
                range = Some(
                    match range {
                        None => SrcRange::new_zero_len(c.pos),
                        Some(range) => range.extend_to(c.pos),
                    },
                );
            } else {
                break;
            }
        }
        let Some(range) = range else {
            return ::anyhow::__private::Err({
                let error = ::anyhow::__private::format_err(
                    format_args!("Word is empty"),
                );
                error
            })
        };
        let token = match word.as_str() {
            "main" => Token::Main,
            "func" => Token::Func,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            _ => {
                if word.is_empty() {
                    return ::anyhow::__private::Err({
                        let error = ::anyhow::__private::format_err(
                            format_args!("Word is empty"),
                        );
                        error
                    });
                }
                if word.chars().next().map(|c| c.is_numeric()).unwrap_or(false) {
                    Token::Literal(word)
                } else {
                    Token::Name(word)
                }
            }
        };
        Ok(SrcToken { token, range })
    }
    fn consume_comment(
        chars: &mut impl Iterator<Item = SrcChar>,
    ) -> anyhow::Result<SrcToken> {
        let Some(slash) = chars.next() else {
            return ::anyhow::__private::Err({
                let error = ::anyhow::__private::format_err(format_args!("Expected //"));
                error
            })
        };
        if slash.char != '/' {
            return ::anyhow::__private::Err({
                let error = ::anyhow::__private::format_err(format_args!("Expected //"));
                error
            });
        }
        let mut range = SrcRange::new_zero_len(slash.pos);
        let Some(slash) = chars.next() else {
            return ::anyhow::__private::Err({
                let error = ::anyhow::__private::format_err(format_args!("Expected //"));
                error
            })
        };
        if slash.char != '/' {
            return ::anyhow::__private::Err({
                let error = ::anyhow::__private::format_err(format_args!("Expected //"));
                error
            });
        }
        range = range.extend_to(slash.pos);
        let mut comment = String::new();
        for c in chars {
            if c.char == '\n' {
                break;
            }
            comment.push(c.char);
            range = range.extend_to(c.pos);
        }
        Ok(SrcToken {
            token: Token::Comment(comment),
            range,
        })
    }
    struct SrcChar {
        char: char,
        pos: SrcPos,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for SrcChar {
        #[inline]
        fn clone(&self) -> SrcChar {
            let _: ::core::clone::AssertParamIsClone<char>;
            let _: ::core::clone::AssertParamIsClone<SrcPos>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for SrcChar {}
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for SrcChar {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for SrcChar {
        #[inline]
        fn eq(&self, other: &SrcChar) -> bool {
            self.char == other.char && self.pos == other.pos
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for SrcChar {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<char>;
            let _: ::core::cmp::AssertParamIsEq<SrcPos>;
        }
    }
    fn position_src_chars(code: &str) -> Vec<SrcChar> {
        let mut pos = SrcPos { line: 1, col: 1 };
        let mut src_chars = Vec::new();
        for c in code.chars() {
            src_chars.push(SrcChar { char: c, pos });
            if c == '\n' {
                pos.line += 1;
                pos.col = 1;
            } else {
                pos.col += 1;
            }
        }
        src_chars
    }
    pub fn tokenize(code: &str) -> anyhow::Result<Vec<SrcToken>> {
        let mut chars = position_src_chars(code).into_iter().multipeek();
        let mut tokens = Vec::new();
        while let Some(peek) = {
            chars.reset_peek();
            chars.peek()
        } {
            let token = match peek.char {
                '=' => consume_char(&mut chars, Some(Token::Eq))?,
                ';' => consume_char(&mut chars, Some(Token::Semi))?,
                '(' => consume_char(&mut chars, Some(Token::LeftParen))?,
                ')' => consume_char(&mut chars, Some(Token::RightParen))?,
                '{' => consume_char(&mut chars, Some(Token::LeftBrace))?,
                '}' => consume_char(&mut chars, Some(Token::RightBrace))?,
                '[' => consume_char(&mut chars, Some(Token::LeftBracket))?,
                ']' => consume_char(&mut chars, Some(Token::RightBracket))?,
                '|' => consume_char(&mut chars, Some(Token::Pipe))?,
                ',' => consume_char(&mut chars, Some(Token::Comma))?,
                '.' => consume_char(&mut chars, Some(Token::Period))?,
                '*' => consume_char(&mut chars, Some(Token::Asterisk))?,
                '!' => consume_char(&mut chars, Some(Token::Bang))?,
                '+' => consume_char(&mut chars, Some(Token::Plus))?,
                '-' => consume_char(&mut chars, Some(Token::Dash))?,
                '~' => consume_char(&mut chars, Some(Token::Tilde))?,
                '<' => consume_char(&mut chars, Some(Token::LeftAngle))?,
                '>' => consume_char(&mut chars, Some(Token::RightAngle))?,
                '%' => consume_char(&mut chars, Some(Token::Percent))?,
                '&' => consume_char(&mut chars, Some(Token::Ampersand))?,
                ':' => consume_char(&mut chars, Some(Token::Colon))?,
                '"' => Some(consume_literal(&mut chars)?),
                '/' => {
                    match chars.peek().map(|c| c.char) {
                        Some('/') => Some(consume_comment(&mut chars)?),
                        _ => consume_char(&mut chars, Some(Token::Slash))?,
                    }
                }
                c if c.is_whitespace() => consume_char(&mut chars, None)?,
                _ => Some(consume_word(&mut chars)?),
            };
            if let Some(token) = token {
                tokens.push(token);
            }
        }
        Ok(tokens)
    }
}
mod token_feed {
    use std::ops::Not;
    use crate::{
        src_pos::{SrcPos, SrcRange},
        token::{SrcToken, Token},
        token_feed::cursor::Cursor,
    };
    pub struct TokenFeed {
        tokens: Vec<SrcToken>,
        cursor: Cursor,
    }
    mod cursor {
        use crate::{
            src_pos::{SrcPos, SrcRange},
            token::SrcToken,
        };
        pub struct Cursor {
            index: usize,
            range: SrcRange,
        }
        #[automatically_derived]
        impl ::core::clone::Clone for Cursor {
            #[inline]
            fn clone(&self) -> Cursor {
                let _: ::core::clone::AssertParamIsClone<usize>;
                let _: ::core::clone::AssertParamIsClone<SrcRange>;
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for Cursor {}
        #[automatically_derived]
        impl ::core::default::Default for Cursor {
            #[inline]
            fn default() -> Cursor {
                Cursor {
                    index: ::core::default::Default::default(),
                    range: ::core::default::Default::default(),
                }
            }
        }
        impl Cursor {
            pub fn index(&self) -> usize {
                self.index
            }
            pub fn range(&self) -> SrcRange {
                self.range
            }
            pub fn increment(self, last_token: Option<&SrcToken>) -> Self {
                let range = match last_token {
                    Some(t) => t.range,
                    None => {
                        SrcRange::new_zero_len(SrcPos {
                            col: self.range.end.col + 1,
                            ..self.range.end
                        })
                    }
                };
                Self {
                    index: self.index + 1,
                    range,
                }
            }
        }
    }
    impl<T: IntoIterator<Item = SrcToken>> From<T> for TokenFeed {
        fn from(value: T) -> Self {
            TokenFeed {
                tokens: value
                    .into_iter()
                    .filter(|t| {
                        match t.token {
                            Token::Comment(_) => true,
                            _ => false,
                        }
                            .not()
                    })
                    .collect(),
                cursor: Cursor::default(),
            }
        }
    }
    impl TokenFeed {
        fn next(&mut self) -> FeedCell<Option<&SrcToken>> {
            let token = self.tokens.get(self.cursor.index());
            self.cursor = self.cursor.increment(token);
            FeedCell {
                res: token,
                range: self.cursor.range(),
            }
        }
        pub fn try_match<T, E>(
            &mut self,
            parser: impl FnOnce(&mut Self) -> Result<T, E>,
        ) -> Result<T, E> {
            let prev_cursor = self.cursor;
            let res = parser(self);
            if res.is_err() {
                self.cursor = prev_cursor;
            }
            res
        }
        pub fn is_finished(&self) -> bool {
            self.tokens.len() == self.cursor.index()
        }
        pub fn try_next<T, E>(
            &mut self,
            consumer: impl FnOnce(FeedCell<Option<&SrcToken>>) -> Result<T, E>,
        ) -> Result<T, E> {
            self.try_match(|tokens| consumer(tokens.next()))
        }
        pub fn parse<T: Parse>(&mut self) -> FeedCell<Result<T, T::Error>> {
            let res = self.try_match(T::parse);
            FeedCell {
                res,
                range: self.cursor.range(),
            }
        }
    }
    pub trait Parse: Sized {
        type Error;
        fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error>;
    }
    pub struct FeedCell<T> {
        pub res: T,
        pub range: SrcRange,
    }
    #[automatically_derived]
    impl<T: ::core::clone::Clone> ::core::clone::Clone for FeedCell<T> {
        #[inline]
        fn clone(&self) -> FeedCell<T> {
            FeedCell {
                res: ::core::clone::Clone::clone(&self.res),
                range: ::core::clone::Clone::clone(&self.range),
            }
        }
    }
    #[automatically_derived]
    impl<T: ::core::marker::Copy> ::core::marker::Copy for FeedCell<T> {}
    #[automatically_derived]
    impl<T: ::core::fmt::Debug> ::core::fmt::Debug for FeedCell<T> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "FeedCell",
                "res",
                &self.res,
                "range",
                &&self.range,
            )
        }
    }
}
#[command(version, about, long_about = None)]
struct Args {
    /// Enables stack monitoring
    #[arg(long)]
    dev: bool,
    /// The Steplo source code file (.lo) to compile
    src_path: String,
    /// The folder to save the compiled program (and other build artifacts) within
    out_path: String,
    /// The folder containing Scratch resources
    res_path: String,
}
#[automatically_derived]
#[allow(unused_qualifications, clippy::redundant_locals)]
impl clap::Parser for Args {}
#[allow(
    dead_code,
    unreachable_code,
    unused_variables,
    unused_braces,
    unused_qualifications,
)]
#[allow(
    clippy::style,
    clippy::complexity,
    clippy::pedantic,
    clippy::restriction,
    clippy::perf,
    clippy::deprecated,
    clippy::nursery,
    clippy::cargo,
    clippy::suspicious_else_formatting,
    clippy::almost_swapped,
    clippy::redundant_locals,
)]
#[automatically_derived]
impl clap::CommandFactory for Args {
    fn command<'b>() -> clap::Command {
        let __clap_app = clap::Command::new("high_compiler");
        <Self as clap::Args>::augment_args(__clap_app)
    }
    fn command_for_update<'b>() -> clap::Command {
        let __clap_app = clap::Command::new("high_compiler");
        <Self as clap::Args>::augment_args_for_update(__clap_app)
    }
}
#[allow(
    dead_code,
    unreachable_code,
    unused_variables,
    unused_braces,
    unused_qualifications,
)]
#[allow(
    clippy::style,
    clippy::complexity,
    clippy::pedantic,
    clippy::restriction,
    clippy::perf,
    clippy::deprecated,
    clippy::nursery,
    clippy::cargo,
    clippy::suspicious_else_formatting,
    clippy::almost_swapped,
    clippy::redundant_locals,
)]
#[automatically_derived]
impl clap::FromArgMatches for Args {
    fn from_arg_matches(
        __clap_arg_matches: &clap::ArgMatches,
    ) -> ::std::result::Result<Self, clap::Error> {
        Self::from_arg_matches_mut(&mut __clap_arg_matches.clone())
    }
    fn from_arg_matches_mut(
        __clap_arg_matches: &mut clap::ArgMatches,
    ) -> ::std::result::Result<Self, clap::Error> {
        #![allow(deprecated)]
        let v = Args {
            dev: __clap_arg_matches
                .remove_one::<bool>("dev")
                .ok_or_else(|| clap::Error::raw(
                    clap::error::ErrorKind::MissingRequiredArgument,
                    "The following required argument was not provided: dev",
                ))?,
            src_path: __clap_arg_matches
                .remove_one::<String>("src_path")
                .ok_or_else(|| clap::Error::raw(
                    clap::error::ErrorKind::MissingRequiredArgument,
                    "The following required argument was not provided: src_path",
                ))?,
            out_path: __clap_arg_matches
                .remove_one::<String>("out_path")
                .ok_or_else(|| clap::Error::raw(
                    clap::error::ErrorKind::MissingRequiredArgument,
                    "The following required argument was not provided: out_path",
                ))?,
            res_path: __clap_arg_matches
                .remove_one::<String>("res_path")
                .ok_or_else(|| clap::Error::raw(
                    clap::error::ErrorKind::MissingRequiredArgument,
                    "The following required argument was not provided: res_path",
                ))?,
        };
        ::std::result::Result::Ok(v)
    }
    fn update_from_arg_matches(
        &mut self,
        __clap_arg_matches: &clap::ArgMatches,
    ) -> ::std::result::Result<(), clap::Error> {
        self.update_from_arg_matches_mut(&mut __clap_arg_matches.clone())
    }
    fn update_from_arg_matches_mut(
        &mut self,
        __clap_arg_matches: &mut clap::ArgMatches,
    ) -> ::std::result::Result<(), clap::Error> {
        #![allow(deprecated)]
        if __clap_arg_matches.contains_id("dev") {
            #[allow(non_snake_case)]
            let dev = &mut self.dev;
            *dev = __clap_arg_matches
                .remove_one::<bool>("dev")
                .ok_or_else(|| clap::Error::raw(
                    clap::error::ErrorKind::MissingRequiredArgument,
                    "The following required argument was not provided: dev",
                ))?;
        }
        if __clap_arg_matches.contains_id("src_path") {
            #[allow(non_snake_case)]
            let src_path = &mut self.src_path;
            *src_path = __clap_arg_matches
                .remove_one::<String>("src_path")
                .ok_or_else(|| clap::Error::raw(
                    clap::error::ErrorKind::MissingRequiredArgument,
                    "The following required argument was not provided: src_path",
                ))?;
        }
        if __clap_arg_matches.contains_id("out_path") {
            #[allow(non_snake_case)]
            let out_path = &mut self.out_path;
            *out_path = __clap_arg_matches
                .remove_one::<String>("out_path")
                .ok_or_else(|| clap::Error::raw(
                    clap::error::ErrorKind::MissingRequiredArgument,
                    "The following required argument was not provided: out_path",
                ))?;
        }
        if __clap_arg_matches.contains_id("res_path") {
            #[allow(non_snake_case)]
            let res_path = &mut self.res_path;
            *res_path = __clap_arg_matches
                .remove_one::<String>("res_path")
                .ok_or_else(|| clap::Error::raw(
                    clap::error::ErrorKind::MissingRequiredArgument,
                    "The following required argument was not provided: res_path",
                ))?;
        }
        ::std::result::Result::Ok(())
    }
}
#[allow(
    dead_code,
    unreachable_code,
    unused_variables,
    unused_braces,
    unused_qualifications,
)]
#[allow(
    clippy::style,
    clippy::complexity,
    clippy::pedantic,
    clippy::restriction,
    clippy::perf,
    clippy::deprecated,
    clippy::nursery,
    clippy::cargo,
    clippy::suspicious_else_formatting,
    clippy::almost_swapped,
    clippy::redundant_locals,
)]
#[automatically_derived]
impl clap::Args for Args {
    fn group_id() -> Option<clap::Id> {
        Some(clap::Id::from("Args"))
    }
    fn augment_args<'b>(__clap_app: clap::Command) -> clap::Command {
        {
            let __clap_app = __clap_app
                .group(
                    clap::ArgGroup::new("Args")
                        .multiple(true)
                        .args({
                            let members: [clap::Id; 4usize] = [
                                clap::Id::from("dev"),
                                clap::Id::from("src_path"),
                                clap::Id::from("out_path"),
                                clap::Id::from("res_path"),
                            ];
                            members
                        }),
                );
            let __clap_app = __clap_app
                .arg({
                    #[allow(deprecated)]
                    let arg = clap::Arg::new("dev")
                        .value_name("DEV")
                        .required(true && clap::ArgAction::SetTrue.takes_values())
                        .value_parser({
                            use ::clap_builder::builder::impl_prelude::*;
                            let auto = ::clap_builder::builder::_infer_ValueParser_for::<
                                bool,
                            >::new();
                            (&&&&&&auto).value_parser()
                        })
                        .action(clap::ArgAction::SetTrue);
                    let arg = arg
                        .help("Enables stack monitoring")
                        .long_help(None)
                        .long("dev");
                    let arg = arg;
                    arg
                });
            let __clap_app = __clap_app
                .arg({
                    #[allow(deprecated)]
                    let arg = clap::Arg::new("src_path")
                        .value_name("SRC_PATH")
                        .required(true && clap::ArgAction::Set.takes_values())
                        .value_parser({
                            use ::clap_builder::builder::impl_prelude::*;
                            let auto = ::clap_builder::builder::_infer_ValueParser_for::<
                                String,
                            >::new();
                            (&&&&&&auto).value_parser()
                        })
                        .action(clap::ArgAction::Set);
                    let arg = arg
                        .help("The Steplo source code file (.lo) to compile")
                        .long_help(None);
                    let arg = arg;
                    arg
                });
            let __clap_app = __clap_app
                .arg({
                    #[allow(deprecated)]
                    let arg = clap::Arg::new("out_path")
                        .value_name("OUT_PATH")
                        .required(true && clap::ArgAction::Set.takes_values())
                        .value_parser({
                            use ::clap_builder::builder::impl_prelude::*;
                            let auto = ::clap_builder::builder::_infer_ValueParser_for::<
                                String,
                            >::new();
                            (&&&&&&auto).value_parser()
                        })
                        .action(clap::ArgAction::Set);
                    let arg = arg
                        .help(
                            "The folder to save the compiled program (and other build artifacts) within",
                        )
                        .long_help(None);
                    let arg = arg;
                    arg
                });
            let __clap_app = __clap_app
                .arg({
                    #[allow(deprecated)]
                    let arg = clap::Arg::new("res_path")
                        .value_name("RES_PATH")
                        .required(true && clap::ArgAction::Set.takes_values())
                        .value_parser({
                            use ::clap_builder::builder::impl_prelude::*;
                            let auto = ::clap_builder::builder::_infer_ValueParser_for::<
                                String,
                            >::new();
                            (&&&&&&auto).value_parser()
                        })
                        .action(clap::ArgAction::Set);
                    let arg = arg
                        .help("The folder containing Scratch resources")
                        .long_help(None);
                    let arg = arg;
                    arg
                });
            __clap_app.version("0.1.0").long_about(None)
        }
    }
    fn augment_args_for_update<'b>(__clap_app: clap::Command) -> clap::Command {
        {
            let __clap_app = __clap_app
                .group(
                    clap::ArgGroup::new("Args")
                        .multiple(true)
                        .args({
                            let members: [clap::Id; 4usize] = [
                                clap::Id::from("dev"),
                                clap::Id::from("src_path"),
                                clap::Id::from("out_path"),
                                clap::Id::from("res_path"),
                            ];
                            members
                        }),
                );
            let __clap_app = __clap_app
                .arg({
                    #[allow(deprecated)]
                    let arg = clap::Arg::new("dev")
                        .value_name("DEV")
                        .required(true && clap::ArgAction::SetTrue.takes_values())
                        .value_parser({
                            use ::clap_builder::builder::impl_prelude::*;
                            let auto = ::clap_builder::builder::_infer_ValueParser_for::<
                                bool,
                            >::new();
                            (&&&&&&auto).value_parser()
                        })
                        .action(clap::ArgAction::SetTrue);
                    let arg = arg
                        .help("Enables stack monitoring")
                        .long_help(None)
                        .long("dev");
                    let arg = arg.required(false);
                    arg
                });
            let __clap_app = __clap_app
                .arg({
                    #[allow(deprecated)]
                    let arg = clap::Arg::new("src_path")
                        .value_name("SRC_PATH")
                        .required(true && clap::ArgAction::Set.takes_values())
                        .value_parser({
                            use ::clap_builder::builder::impl_prelude::*;
                            let auto = ::clap_builder::builder::_infer_ValueParser_for::<
                                String,
                            >::new();
                            (&&&&&&auto).value_parser()
                        })
                        .action(clap::ArgAction::Set);
                    let arg = arg
                        .help("The Steplo source code file (.lo) to compile")
                        .long_help(None);
                    let arg = arg.required(false);
                    arg
                });
            let __clap_app = __clap_app
                .arg({
                    #[allow(deprecated)]
                    let arg = clap::Arg::new("out_path")
                        .value_name("OUT_PATH")
                        .required(true && clap::ArgAction::Set.takes_values())
                        .value_parser({
                            use ::clap_builder::builder::impl_prelude::*;
                            let auto = ::clap_builder::builder::_infer_ValueParser_for::<
                                String,
                            >::new();
                            (&&&&&&auto).value_parser()
                        })
                        .action(clap::ArgAction::Set);
                    let arg = arg
                        .help(
                            "The folder to save the compiled program (and other build artifacts) within",
                        )
                        .long_help(None);
                    let arg = arg.required(false);
                    arg
                });
            let __clap_app = __clap_app
                .arg({
                    #[allow(deprecated)]
                    let arg = clap::Arg::new("res_path")
                        .value_name("RES_PATH")
                        .required(true && clap::ArgAction::Set.takes_values())
                        .value_parser({
                            use ::clap_builder::builder::impl_prelude::*;
                            let auto = ::clap_builder::builder::_infer_ValueParser_for::<
                                String,
                            >::new();
                            (&&&&&&auto).value_parser()
                        })
                        .action(clap::ArgAction::Set);
                    let arg = arg
                        .help("The folder containing Scratch resources")
                        .long_help(None);
                    let arg = arg.required(false);
                    arg
                });
            __clap_app.version("0.1.0").long_about(None)
        }
    }
}
#[automatically_derived]
impl ::core::fmt::Debug for Args {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field4_finish(
            f,
            "Args",
            "dev",
            &self.dev,
            "src_path",
            &self.src_path,
            "out_path",
            &self.out_path,
            "res_path",
            &&self.res_path,
        )
    }
}
fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    {
        ::std::io::_print(format_args!("STEPLO COMPILER\n"));
    };
    time_total("Total compile time:", || compile_all(args))?;
    Ok(())
}
fn compile_all(args: Args) -> anyhow::Result<()> {
    let src_path = Path::new(&args.src_path);
    if !(src_path.extension().and_then(|x| x.to_str()) == Some("lo")) {
        {
            ::core::panicking::panic_fmt(
                format_args!("input file must be a steplo source code (.lo) file"),
            );
        }
    }
    let mut in_file = File::open(src_path).expect("input file should open");
    let input = {
        let mut buf = String::new();
        in_file.read_to_string(&mut buf).expect("should be able to read input file");
        buf
    };
    let tokens = time("Tokenizing...", || tokenize(&input))?;
    let ast = match time("Parsing grammar...", || parse(tokens.into())) {
        Ok(ast) => ast,
        Err(err) => {
            report_ast_errors(&input, src_path, err);
            return Ok(());
        }
    };
    let ast = time("Converting grammar...", || logic_ast::Program::try_from(&ast))?;
    let linked = time("Linking...", || link(&ast))?;
    let mem_opt_ast = time(
        "Compiling high-level to designation IR...",
        || compile(linked),
    )?;
    time(
        "Writing intermediate opt 0 file...",
        || {
            let asm_export = mem_opt::export::export(
                mem_opt_ast.iter().map(AsRef::as_ref),
            );
            let name = src_path
                .file_stem()
                .and_then(|stem| stem.to_str())
                .expect("input file should have stem");
            let path = Path::new(&args.out_path)
                .join(
                    ::alloc::__export::must_use({
                        let res = ::alloc::fmt::format(format_args!("{0}.opt0", name));
                        res
                    }),
                );
            fs::write(path, asm_export).expect("opt export should succeed");
        },
    );
    let mem_opt1_ast = time(
        "Optimizing code...",
        || mem_opt::opt::optimize(mem_opt_ast.iter().cloned()),
    );
    time(
        "Writing intermediate opt 1 file...",
        || {
            let asm_export = mem_opt::export::export(
                mem_opt1_ast.iter().map(AsRef::as_ref),
            );
            let name = src_path
                .file_stem()
                .and_then(|stem| stem.to_str())
                .expect("input file should have stem");
            let path = Path::new(&args.out_path)
                .join(
                    ::alloc::__export::must_use({
                        let res = ::alloc::fmt::format(format_args!("{0}.opt1", name));
                        res
                    }),
                );
            fs::write(path, asm_export).expect("opt export should succeed");
        },
    );
    let mem_opt_designated = time(
        "Designating registers...",
        || mem_opt::designate::designate_registers(&mem_opt1_ast),
    );
    let ez = time(
        "Compiling to EZ...",
        || {
            mem_opt::compile::compile(
                mem_opt_designated.iter().map(AsRef::as_ref),
                mem_opt::compile::CompileOptions {
                    stack_monitoring: args.dev,
                },
            )
        },
    );
    let ir = time("Transpiling to IR...", || ez.compile());
    let js_val = time("Compiling to JSON...", || ir.compile());
    let json = time(
        "Serializing...",
        || ::alloc::__export::must_use({
            let res = ::alloc::fmt::format(format_args!("{0:#}", js_val));
            res
        }),
    );
    time("Exporting...", || write_json(&json, src_path, &args.out_path, &args.res_path));
    Ok(())
}
