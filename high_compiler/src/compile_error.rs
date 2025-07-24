use std::{cmp::Ordering, collections::HashSet, path::Path, sync::Arc};

use colored::Colorize;
use itertools::Itertools;

use crate::{srced::SrcRange, token::TokenKind};

#[derive(Debug, Default)]
pub struct CompileErrorSet {
    errors: Vec<CompileError>,
    range: Option<SrcRange>,
}

#[derive(Debug)]
pub(crate) enum CompileError {
    Grammar(GrammarError),
    Convert(LogicError),
}

#[derive(Debug)]
pub(crate) enum GrammarError {
    MismatchedTokenString { expected: Arc<[TokenKind]>, found: TokenKind },
    ExpectedTokenString { expected: Arc<[TokenKind]> },
}

#[derive(Debug)]
#[allow(clippy::enum_variant_names)]
pub(crate) enum LogicError {
    InvalidArrayTypeLen,
    InvalidType,
    InvalidRangeStartIncl,
    InvalidRangeEndExcl,
    InvalidNumLiteral,
}

impl CompileErrorSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub(crate) fn new_error(range: SrcRange, error: CompileError) -> Self {
        CompileErrorSet { errors: Vec::from([error]), range: Some(range) }
    }

    pub fn merge(mut self, other: CompileErrorSet) -> Self {
        match self.range.cmp(&other.range) {
            Ordering::Less => other,
            Ordering::Greater => self,
            Ordering::Equal => {
                self.errors.extend(other.errors);
                self
            },
        }
    }
}

enum CollapsedCompileError {
    Grammar(CollapsedGrammarError),
    Logic(CollapsedLogicError),
}

enum CollapsedGrammarError {
    MismatchedTokenString { expected: HashSet<Arc<[TokenKind]>>, found: TokenKind },
    ExpectedTokenString { expected: HashSet<Arc<[TokenKind]>> },
}

#[allow(clippy::enum_variant_names)]
enum CollapsedLogicError {
    InvalidArrayTypeLen,
    InvalidType,
    InvalidRangeStartIncl,
    InvalidRangeEndExcl,
    InvalidNumLiteral,
}

impl CollapsedGrammarError {
    pub fn try_collapse(&mut self, other: GrammarError) -> Result<(), GrammarError> {
        match (self, &other) {
            (
                Self::MismatchedTokenString { expected, found },
                GrammarError::MismatchedTokenString {
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
                GrammarError::ExpectedTokenString { expected: other_expected },
            ) => {
                expected.insert(other_expected.clone());
                return Ok(());
            },
            _ => {},
        }

        Err(other)
    }
}

impl CollapsedLogicError {
    pub fn try_collapse(&mut self, other: LogicError) -> Result<(), LogicError> {
        match (self, &other) {
            (Self::InvalidArrayTypeLen, LogicError::InvalidArrayTypeLen) => {
                return Ok(());
            },
            (Self::InvalidType, LogicError::InvalidType) => return Ok(()),
            (Self::InvalidRangeStartIncl, LogicError::InvalidRangeStartIncl) => return Ok(()),
            (Self::InvalidRangeEndExcl, LogicError::InvalidRangeEndExcl) => return Ok(()),
            (Self::InvalidNumLiteral, LogicError::InvalidNumLiteral) => return Ok(()),
            _ => {},
        }

        Err(other)
    }
}

impl CollapsedCompileError {
    pub fn try_collapse(&mut self, other: CompileError) -> Result<(), CompileError> {
        match (self, other) {
            (CollapsedCompileError::Grammar(ast), CompileError::Grammar(other_ast)) => {
                ast.try_collapse(other_ast).map_err(CompileError::Grammar)
            },
            (CollapsedCompileError::Logic(convert), CompileError::Convert(other_convert)) => {
                convert.try_collapse(other_convert).map_err(CompileError::Convert)
            },
            (_, other) => Err(other),
        }
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
                    TokenKind::String => TokenString::Generic("string"),
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
                    TokenKind::Digits => TokenString::Punctuation("number"),
                    TokenKind::True => TokenString::Keyword("true"),
                    TokenKind::False => TokenString::Keyword("false"),
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
                                FmtTokenString::Verbatim(format!("{verbatim} {s}"))
                            },
                            TokenString::Punctuation(s) => {
                                FmtTokenString::Verbatim(format!("{verbatim}{s}"))
                            },
                        },
                    });
                }

                if let Some(ts) = cur_token_string {
                    fmt_token_strings.push(ts);
                }

                fmt_token_strings
            }

            tokens_to_fts(tokens)
                .into_iter()
                .map(|ts| match ts {
                    FmtTokenString::Generic(s) => s,
                    FmtTokenString::Verbatim(s) => format!("`{s}`"),
                })
                .join("")
        }

        fn token_strings_to_string<'a>(strings: impl Iterator<Item = &'a [TokenKind]>) -> String {
            strings.map(tokens_to_string).sorted().join(", ")
        }

        match self {
            Self::Grammar(ast) => match ast {
                CollapsedGrammarError::MismatchedTokenString { expected, found } => {
                    format!(
                        "Expected {}; Found: {}",
                        token_strings_to_string(expected.iter().map(AsRef::as_ref)),
                        tokens_to_string(&[*found])
                    )
                },
                CollapsedGrammarError::ExpectedTokenString { expected } => {
                    format!(
                        "Expected {}; Found end of document",
                        token_strings_to_string(expected.iter().map(AsRef::as_ref)),
                    )
                },
            },
            Self::Logic(convert) => match convert {
                CollapsedLogicError::InvalidArrayTypeLen => {
                    "Array length should be a positive integer".into()
                },
                CollapsedLogicError::InvalidType => "This type does not exist".into(),
                CollapsedLogicError::InvalidRangeStartIncl => {
                    "Inclusive start of range must be a positive integer".into()
                },
                CollapsedLogicError::InvalidRangeEndExcl => {
                    "Exclusive end of range must be a positive integer".into()
                },
                CollapsedLogicError::InvalidNumLiteral => "Invalid number literal".into(),
            },
        }
    }

    pub fn phase(&self) -> &'static str {
        match self {
            Self::Grammar(_) => "grammar",
            Self::Logic(_) => "logic",
        }
    }
}

impl From<CompileError> for CollapsedCompileError {
    fn from(value: CompileError) -> Self {
        match value {
            CompileError::Grammar(ast) => Self::Grammar(match ast {
                GrammarError::MismatchedTokenString { expected, found } => {
                    CollapsedGrammarError::MismatchedTokenString {
                        expected: HashSet::from([expected]),
                        found,
                    }
                },
                GrammarError::ExpectedTokenString { expected } => {
                    CollapsedGrammarError::ExpectedTokenString {
                        expected: HashSet::from([expected]),
                    }
                },
            }),
            CompileError::Convert(convert) => Self::Logic(match convert {
                LogicError::InvalidArrayTypeLen => CollapsedLogicError::InvalidArrayTypeLen,
                LogicError::InvalidType => CollapsedLogicError::InvalidType,
                LogicError::InvalidRangeStartIncl => CollapsedLogicError::InvalidRangeStartIncl,
                LogicError::InvalidRangeEndExcl => CollapsedLogicError::InvalidRangeEndExcl,
                LogicError::InvalidNumLiteral => CollapsedLogicError::InvalidNumLiteral,
            }),
        }
    }
}

fn collapse_errors(errors: Vec<CompileError>) -> Vec<CollapsedCompileError> {
    let mut kinds = Vec::<CollapsedCompileError>::new();
    'error: for mut error in errors {
        for collapsed in &mut kinds {
            match collapsed.try_collapse(error) {
                Ok(()) => continue 'error,
                Err(k) => error = k,
            }
        }

        kinds.push(error.into());
    }

    kinds
}

pub fn report_compile_errors(code: &str, code_path: &Path, set: CompileErrorSet) {
    let Some(range) = set.range else { return };
    let errors = collapse_errors(set.errors);

    println!("{}", "Error!".red().bold());

    for err in &errors {
        let src_info =
            format!("{}:{}:{}", code_path.to_string_lossy(), range.start.line, range.start.col)
                .normal();

        println!("{} {} {} {} {}", err.phase(), "@".blue(), src_info, "-->".blue(), err.to_msg());
    }

    let nearby_lines = code
        .lines()
        .enumerate()
        .map(|(i, line)| (i + 1, line))
        .skip(range.start.line.saturating_sub(2).saturating_sub(1))
        .take(range.start.line.min(3))
        .collect_vec();

    let Some(max_line_number) = nearby_lines.iter().map(|(i, _)| i.to_string()).max() else {
        return;
    };

    let line_numbers_width = max_line_number.len() + 1;

    let empty_sidebar = format!("{:>width$} |", "", width = line_numbers_width).blue().bold();
    println!("{empty_sidebar}");

    for (i, line) in nearby_lines {
        let sidebar = format!("{i:>line_numbers_width$} |");
        println!("{}  {}", sidebar.blue().bold(), line);
    }

    println!(
        "{}  {}{}",
        empty_sidebar,
        " ".repeat(range.start.col.saturating_sub(1)),
        "^".repeat(1 + range.end.col.saturating_sub(range.start.col)).red(),
    );
}
