use std::{cmp::Ordering, collections::HashSet, fmt::Display, path::Path, sync::Arc};

use colored::Colorize;
use itertools::Itertools;

use crate::{
    logic_ast::{PrimitiveType, Type},
    srced::SrcRange,
    token::TokenKind,
};

#[derive(Debug, Default)]
pub struct CompileErrorSet {
    errors: Vec<CompileError>,
    range: Option<SrcRange>,
}

#[derive(Debug)]
pub(crate) enum CompileError {
    Grammar(GrammarError),
    Convert(LogicError),
    Type(TypeError),
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
    InvalidNumLiteral,
}

#[derive(Debug)]
pub(crate) enum TypeError {
    Mismatch {
        expected: Arc<VagueType>,
        found: Arc<VagueType>,
    },
    InvalidCast {
        from: Arc<VagueType>,
        to: Arc<VagueType>,
    },
    InvalidTransmute {
        from: Arc<VagueType>,
        to: Arc<VagueType>,
    },
    IndexNonArray {
        found: Arc<VagueType>,
    },
    IdentNotFound {
        name: Arc<str>,
    },
    DerefNonRef {
        found: Arc<VagueType>,
    },
    FuncNotFound {
        name: Arc<str>,
    },
    ArgsLenMismatch {
        func_name: Arc<str>,
        expected: usize,
        found: usize,
    },
    BinaryOpOperandsMismatch {
        op: Arc<str>,
        expected: Arc<[(Arc<VagueType>, Arc<VagueType>)]>,
        found: (Arc<VagueType>, Arc<VagueType>),
    },
}

#[derive(Debug)]
pub enum VagueType {
    Unknown,
    Any,
    Primitive(PrimitiveType),
    Ref(Arc<VagueType>),
    Array { ty: Arc<VagueType>, len: Option<u32> },
    Struct(Arc<Vec<Arc<VagueTypeField>>>),
}

#[derive(Debug)]
pub struct VagueTypeField {
    name: Arc<str>,
    ty: Arc<VagueType>,
}

impl From<&Type> for VagueType {
    fn from(value: &Type) -> Self {
        match value {
            Type::Any => Self::Any,
            Type::Primitive(base) => Self::Primitive(*base),
            Type::Ref(ty) => Self::Ref(Arc::new(ty.as_ref().into())),
            Type::Array { ty, len } => {
                Self::Array { ty: Arc::new(ty.as_ref().into()), len: Some(*len) }
            },
            Type::Struct(fields) => Self::Struct(Arc::new(
                fields
                    .iter()
                    .map(|field| {
                        Arc::new(VagueTypeField {
                            name: field.name.clone(),
                            ty: Arc::new(field.ty.as_ref().into()),
                        })
                    })
                    .collect(),
            )),
        }
    }
}

impl Display for VagueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Any => write!(f, "any"),
            Self::Unknown => write!(f, "_"),
            Self::Primitive(base) => {
                let base = match base {
                    PrimitiveType::Val => "val",
                    PrimitiveType::Num => "num",
                    PrimitiveType::Int => "int",
                    PrimitiveType::Uint => "uint",
                    PrimitiveType::Bool => "bool",
                };

                write!(f, "{base}")
            },
            Self::Ref(ty) => write!(f, "&{}", ty.as_ref()),
            Self::Array { ty, len } => {
                let len = match len {
                    Some(len) => &len.to_string(),
                    None => "_",
                };

                write!(f, "[{ty}; {len}]")
            },
            Self::Struct(fields) => {
                let fields =
                    fields.iter().map(|field| format!("{}: {}", field.name, field.ty)).join(", ");
                write!(f, "{{ {fields} }}")
            },
        }
    }
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
    Type(CollapsedTypeError),
}

enum CollapsedGrammarError {
    MismatchedTokenString { expected: HashSet<Arc<[TokenKind]>>, found: TokenKind },
    ExpectedTokenString { expected: HashSet<Arc<[TokenKind]>> },
}

#[allow(clippy::enum_variant_names)]
enum CollapsedLogicError {
    InvalidArrayTypeLen,
    InvalidType,
    InvalidNumLiteral,
}

enum CollapsedTypeError {
    Mismatch {
        expected: Arc<VagueType>,
        found: Arc<VagueType>,
    },
    InvalidCast {
        from: Arc<VagueType>,
        to: Arc<VagueType>,
    },
    InvalidTransmute {
        from: Arc<VagueType>,
        to: Arc<VagueType>,
    },
    IndexNonArray {
        found: Arc<VagueType>,
    },
    IdentNotFound {
        name: Arc<str>,
    },
    DerefNonRef {
        found: Arc<VagueType>,
    },
    FuncNotFound {
        name: Arc<str>,
    },
    ArgsLenMismatch {
        func_name: Arc<str>,
        expected: usize,
        found: usize,
    },
    BinaryOpOperandsMismatch {
        op: Arc<str>,
        expected: Arc<[(Arc<VagueType>, Arc<VagueType>)]>,
        found: (Arc<VagueType>, Arc<VagueType>),
    },
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
            (Self::InvalidNumLiteral, LogicError::InvalidNumLiteral) => return Ok(()),
            _ => {},
        }

        Err(other)
    }
}

impl CollapsedTypeError {
    pub fn try_collapse(&mut self, other: TypeError) -> Result<(), TypeError> {
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
            (CollapsedCompileError::Type(ty), CompileError::Type(other_ty)) => {
                ty.try_collapse(other_ty).map_err(CompileError::Type)
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
                    TokenKind::Struct => TokenString::Keyword("struct"),
                    TokenKind::Enum => TokenString::Keyword("enum"),
                    TokenKind::Type => TokenString::Keyword("type"),
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
                        "Expected one of: {}; Found: {}",
                        token_strings_to_string(expected.iter().map(AsRef::as_ref)),
                        tokens_to_string(&[*found])
                    )
                },
                CollapsedGrammarError::ExpectedTokenString { expected } => {
                    format!(
                        "Expected one of: {}; Found end of document",
                        token_strings_to_string(expected.iter().map(AsRef::as_ref)),
                    )
                },
            },
            Self::Logic(convert) => match convert {
                CollapsedLogicError::InvalidArrayTypeLen => {
                    "Array length should be a positive integer".into()
                },
                CollapsedLogicError::InvalidType => "This type does not exist".into(),
                CollapsedLogicError::InvalidNumLiteral => "Invalid number literal".into(),
            },
            Self::Type(ty) => match ty {
                CollapsedTypeError::Mismatch { expected, found } => {
                    format!("Expected `{expected}`; Found `{found}`")
                },
                CollapsedTypeError::InvalidCast { from, to } => {
                    format!("Unable to cast `{from}` to `{to}`")
                },
                CollapsedTypeError::InvalidTransmute { from, to } => {
                    format!("Unable to transmute `{from}` to `{to}`")
                },
                CollapsedTypeError::IndexNonArray { found } => {
                    format!("Unable to index a type that isn't an array; Found: `{found}`")
                },
                CollapsedTypeError::IdentNotFound { name } => {
                    format!("Identifier `{name}` was not found in scope")
                },
                CollapsedTypeError::DerefNonRef { found } => {
                    format!("Unable to dereference a type that isn't a reference; Found: `{found}`")
                },
                CollapsedTypeError::FuncNotFound { name } => {
                    format!("Function `{name}` was not found")
                },
                CollapsedTypeError::ArgsLenMismatch { func_name, expected, found } => {
                    format!(
                        "Function `{func_name}` expected {expected} arguments; Found {found} arguments"
                    )
                },
                CollapsedTypeError::BinaryOpOperandsMismatch { op, expected, found } => {
                    let fmt_binary_op_expr =
                        |(left, right): &(_, _)| format!("`{left}` {op} `{right}`");

                    let expected = expected.iter().map(fmt_binary_op_expr).join(", ");
                    let found = fmt_binary_op_expr(found);

                    format!("Expected one of: {expected}; Found {found}")
                },
            },
        }
    }

    pub fn phase(&self) -> &'static str {
        match self {
            Self::Grammar(_) => "grammar",
            Self::Logic(_) => "logic",
            Self::Type(_) => "type",
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
                LogicError::InvalidNumLiteral => CollapsedLogicError::InvalidNumLiteral,
            }),
            CompileError::Type(ty) => Self::Type(match ty {
                TypeError::Mismatch { expected, found } => {
                    CollapsedTypeError::Mismatch { expected, found }
                },
                TypeError::InvalidCast { from, to } => CollapsedTypeError::InvalidCast { from, to },
                TypeError::InvalidTransmute { from, to } => {
                    CollapsedTypeError::InvalidTransmute { from, to }
                },
                TypeError::IndexNonArray { found } => CollapsedTypeError::IndexNonArray { found },
                TypeError::IdentNotFound { name } => CollapsedTypeError::IdentNotFound { name },
                TypeError::DerefNonRef { found } => CollapsedTypeError::DerefNonRef { found },
                TypeError::FuncNotFound { name } => CollapsedTypeError::FuncNotFound { name },
                TypeError::ArgsLenMismatch { func_name, expected, found } => {
                    CollapsedTypeError::ArgsLenMismatch { func_name, expected, found }
                },
                TypeError::BinaryOpOperandsMismatch { op, expected, found } => {
                    CollapsedTypeError::BinaryOpOperandsMismatch { op, expected, found }
                },
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
            format!("{}:{}:{}", code_path.to_string_lossy(), range.start().line, range.start().col)
                .normal();

        println!("{} {} {} {} {}", err.phase(), "@".blue(), src_info, "-->".blue(), err.to_msg());
    }

    let nearby_lines = code
        .lines()
        .enumerate()
        .map(|(i, line)| (i + 1, line))
        .skip(range.start().line.saturating_sub(2).saturating_sub(1))
        .take(range.start().line.min(3))
        .collect_vec();

    let Some(max_line_number) = nearby_lines.iter().map(|(i, _)| i.to_string()).max() else {
        return;
    };

    let line_numbers_width = max_line_number.len() + 1;

    let empty_sidebar = format!("{:>line_numbers_width$} |", "").blue().bold();
    println!("{empty_sidebar}");

    let range_len = 1 + range.end().col.saturating_sub(range.start().col);

    for (i, line) in nearby_lines {
        let sidebar = format!("{i:>line_numbers_width$} |");

        let line = if i == range.start().line && range.is_guess() {
            let mut first_half = String::new();
            let mut second_half = String::new();
            for (i, char) in line.chars().enumerate() {
                if i < range.start().col {
                    first_half.push(char);
                } else {
                    second_half.push(char);
                }
            }

            &format!("{}{}{}", first_half, "_".repeat(range_len).red(), second_half)
        } else {
            line
        };

        println!("{}  {}", sidebar.blue().bold(), line);
    }

    println!(
        "{}  {}{}",
        empty_sidebar,
        " ".repeat(range.start().col.saturating_sub(if range.is_guess() { 0 } else { 1 })),
        "^".repeat(range_len).red(),
    );
}
