use std::{
    collections::{BTreeSet, HashSet},
    path::Path,
    sync::Arc,
};

use colored::Colorize;
use itertools::Itertools;

use crate::{src_pos::SrcRange, token::TokenKind};

#[derive(Debug, Default)]
pub struct AstErrorSet {
    errors: Vec<AstError>,
}

#[derive(Debug)]
pub struct AstError {
    kind: AstErrorKind,
    range: SrcRange,
}

#[derive(Debug)]
pub enum AstErrorKind {
    MismatchedTokenString { expected: Arc<[TokenKind]>, found: TokenKind },
    ExpectedTokenString { expected: Arc<[TokenKind]> },
    ExpectedTopItem,
    InvalidArrayLength,
    InvalidInclRangeStart,
    InvalidExclRangeEnd,
}

impl AstErrorSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_error(range: SrcRange, kind: AstErrorKind) -> Self {
        let err = AstError { range, kind };
        AstErrorSet { errors: Vec::from([err]) }
    }

    pub fn merge(self, other: AstErrorSet) -> Self {
        Self { errors: self.errors.into_iter().chain(other.errors).collect() }
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
            },
            (
                Self::ExpectedTokenString { expected },
                AstErrorKind::ExpectedTokenString { expected: other_expected },
            ) => {
                expected.insert(other_expected.clone());
                return Ok(());
            },
            (Self::ExpectedTopItem, AstErrorKind::ExpectedTopItem) => return Ok(()),
            (Self::InvalidArrayLength, AstErrorKind::InvalidArrayLength) => return Ok(()),
            (Self::InvalidInclRangeStart, AstErrorKind::InvalidInclRangeStart) => return Ok(()),
            (Self::InvalidExclRangeEnd, AstErrorKind::InvalidExclRangeEnd) => return Ok(()),
            _ => {},
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
                    cur_token_string = Some(match cur_token_string {
                        None => match ts {
                            TokenString::Generic(s) => FmtTokenString::Generic(s.into()),
                            TokenString::Keyword(s) => FmtTokenString::Verbatim(s.into()),
                            TokenString::Punctuation(s) => FmtTokenString::Verbatim(s.into()),
                        },
                        Some(generic @ FmtTokenString::Generic(_)) => {
                            fmt_token_strings.push(generic);
                            match ts {
                                TokenString::Generic(s) => FmtTokenString::Generic(s.into()),
                                TokenString::Keyword(s) => FmtTokenString::Verbatim(s.into()),
                                TokenString::Punctuation(s) => FmtTokenString::Verbatim(s.into()),
                            }
                        },
                        Some(FmtTokenString::Verbatim(verbatim)) => match ts {
                            TokenString::Generic(s) => {
                                fmt_token_strings.push(FmtTokenString::Verbatim(verbatim));
                                FmtTokenString::Generic(s.into())
                            },
                            TokenString::Keyword(s) => {
                                FmtTokenString::Verbatim(format!("{} {}", verbatim, s))
                            },
                            TokenString::Punctuation(s) => {
                                FmtTokenString::Verbatim(format!("{}{}", verbatim, s))
                            },
                        },
                    });
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
                    FmtTokenString::Verbatim(s) => format!("`{}`", s),
                })
                .join("");

            token_string
        }

        fn token_strings_to_string<'a>(strings: impl Iterator<Item = &'a [TokenKind]>) -> String {
            strings.map(tokens_to_string).sorted().join(", ")
        }

        match self {
            Self::MismatchedTokenString { expected, found } => {
                format!(
                    "Expected {}; Found: {}",
                    token_strings_to_string(expected.iter().map(AsRef::as_ref)),
                    tokens_to_string(&[*found])
                )
            },
            Self::ExpectedTokenString { expected } => {
                format!(
                    "Expected {}; Found end of document",
                    token_strings_to_string(expected.iter().map(AsRef::as_ref)),
                )
            },
            Self::ExpectedTopItem => "Expected top item".into(),
            Self::InvalidArrayLength => "Array length should be a positive integer".into(),
            Self::InvalidInclRangeStart => {
                "Inclusive start of range must be a positive integer".into()
            },
            Self::InvalidExclRangeEnd => "Exclusive end of range must be a positive integer".into(),
        }
    }
}

impl From<AstErrorKind> for CollapsedAstErrorKind {
    fn from(value: AstErrorKind) -> Self {
        match value {
            AstErrorKind::MismatchedTokenString { expected, found } => {
                Self::MismatchedTokenString { expected: HashSet::from([expected]), found }
            },
            AstErrorKind::ExpectedTokenString { expected } => {
                Self::ExpectedTokenString { expected: HashSet::from([expected]) }
            },
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

    println!("{}", "Error!".red().bold());

    for err in &error.kinds {
        let src_info = format!(
            "{}:{}:{}",
            code_path.to_string_lossy(),
            error.range.start.line,
            error.range.start.col
        )
        .normal();

        println!("{} {} {} {}", "@".blue(), src_info, "-->".blue(), err.to_msg());
    }

    let nearby_lines = code
        .lines()
        .enumerate()
        .map(|(i, line)| (i + 1, line))
        .skip(error.range.start.line.saturating_sub(2).saturating_sub(1))
        .take(error.range.start.line.min(3))
        .collect_vec();

    let Some(max_line_number) = nearby_lines.iter().map(|(i, _)| i.to_string()).max() else {
        return;
    };

    let line_numbers_width = max_line_number.len() + 1;

    let empty_sidebar = format!("{:>width$} |", "", width = line_numbers_width).blue().bold();
    println!("{}", empty_sidebar);

    for (i, line) in nearby_lines {
        let sidebar = format!("{:>width$} |", i, width = line_numbers_width);
        println!("{}  {}", sidebar.blue().bold(), line);
    }

    println!(
        "{}  {}{}",
        empty_sidebar,
        " ".repeat(error.range.start.col.saturating_sub(1)),
        "^".repeat(1 + error.range.end.col.saturating_sub(error.range.start.col)).red(),
    );
}
