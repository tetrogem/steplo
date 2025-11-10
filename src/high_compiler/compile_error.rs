use std::{cmp::Ordering, collections::HashSet, fmt::Display, ops::Not, path::Path, sync::Arc};

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
    Logic(LogicError),
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
    InvalidNumLiteral,
    ElementAfterSpread,
    FoundInternalIdent,
}

#[derive(Debug)]
pub(crate) enum TypeError {
    Mismatch {
        expected_any_of: Arc<Vec<Arc<VagueType>>>,
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
    OffsetIndexNonArray {
        found: Arc<VagueType>,
    },
    FieldIndexNonStruct {
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
    UnknownAlias {
        name: Arc<str>,
    },
    UnknownEnum {
        name: Arc<str>,
    },
    UnknownVariant {
        enum_name: Arc<str>,
        variant_name: Arc<str>,
    },
    IndexInvalidField {
        struct_type: Arc<VagueType>,
        field_name: Arc<str>,
    },
    AssignCellExprToCompoundPlace {
        place_size: u32,
    },
    StructLiteralMissingField {
        field_type: Arc<VagueType>,
        field_name: Arc<str>,
    },
    UnmatchableCase,
    NonExhaustiveMatch {
        unmatched_cases: Arc<Vec<Arc<str>>>,
    },
    CannotInfer,
    Uncomparable {
        left: Arc<VagueType>,
        right: Arc<VagueType>,
    },
    ExpectedConstExpr,
}

#[derive(Debug)]
pub enum VagueType {
    Unknown,
    Any,
    Primitive(PrimitiveType),
    Ref(Arc<VagueType>),
    Array { ty: Arc<VagueType>, len: Option<u32> },
    Struct(Option<Arc<Vec<Arc<VagueTypeField>>>>),
    Enum { name: Option<Arc<str>> },
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
            Type::Struct(fields) => Self::Struct(Some(Arc::new(
                fields
                    .iter()
                    .map(|field| {
                        Arc::new(VagueTypeField {
                            name: field.name.clone(),
                            ty: Arc::new(field.ty.as_ref().into()),
                        })
                    })
                    .collect(),
            ))),
            Type::Enum { name } => Self::Enum { name: Some(name.clone()) },
        }
    }
}

enum VagueTypeDisplay {
    Ticked(Arc<str>),
    Unticked(Arc<str>),
}

impl VagueTypeDisplay {
    pub fn str(&self) -> &Arc<str> {
        match self {
            Self::Ticked(str) => str,
            Self::Unticked(str) => str,
        }
    }

    pub fn display(&self) -> String {
        match self {
            Self::Ticked(str) => format!("`{str}`"),
            Self::Unticked(str) => str.to_string(),
        }
    }
}

impl VagueType {
    fn to_display(&self) -> VagueTypeDisplay {
        match self {
            Self::Any => VagueTypeDisplay::Ticked("any".into()),
            Self::Unknown => VagueTypeDisplay::Ticked("_".into()),
            Self::Primitive(base) => match base {
                PrimitiveType::Val => VagueTypeDisplay::Ticked("val".into()),
                PrimitiveType::Str => VagueTypeDisplay::Ticked("str".into()),
                PrimitiveType::Num => VagueTypeDisplay::Ticked("num".into()),
                PrimitiveType::Int => VagueTypeDisplay::Ticked("int".into()),
                PrimitiveType::Uint => VagueTypeDisplay::Ticked("uint".into()),
                PrimitiveType::Bool => VagueTypeDisplay::Ticked("bool".into()),
            },
            Self::Ref(ty) => VagueTypeDisplay::Ticked(format!("&{}", ty.to_display().str()).into()),
            Self::Array { ty, len } => {
                let len = match len {
                    Some(len) => &len.to_string(),
                    None => "_",
                };

                VagueTypeDisplay::Ticked(format!("[{}; {len}]", ty.to_display().str()).into())
            },
            Self::Struct(fields) => {
                let fields = match fields {
                    None => "...",
                    Some(fields) => &fields
                        .iter()
                        .map(|field| format!("{}: {}", field.name, field.ty.to_display().str()))
                        .join(", "),
                };

                VagueTypeDisplay::Ticked(
                    format!(
                        "{{{padding}{fields}{padding}}}",
                        padding = if fields.is_empty().not() { " " } else { "" }
                    )
                    .into(),
                )
            },
            Self::Enum { name } => match name {
                Some(name) => VagueTypeDisplay::Ticked(name.clone()),
                None => VagueTypeDisplay::Unticked("enum".into()),
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

enum CollapsedLogicError {
    InvalidArrayTypeLen,
    InvalidNumLiteral,
    ElementAfterSpread,
    FoundInternalIdent,
}

enum CollapsedTypeError {
    Mismatch {
        expected_any_of: Arc<Vec<Arc<VagueType>>>,
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
    OffsetIndexNonArray {
        found: Arc<VagueType>,
    },
    FieldIndexNonStruct {
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
    UnknownType {
        name: Arc<str>,
    },
    UnknownEnum {
        name: Arc<str>,
    },
    UnknownVariant {
        enum_name: Arc<str>,
        variant_name: Arc<str>,
    },
    IndexInvalidField {
        struct_type: Arc<VagueType>,
        field_name: Arc<str>,
    },
    AssignCellExprToCompoundPlace {
        place_size: u32,
    },
    StructLiteralMissingField {
        field_name: Arc<str>,
        field_type: Arc<VagueType>,
    },
    UnmatchableCase,
    NonExhaustiveMatch {
        unmatched_cases: Arc<Vec<Arc<str>>>,
    },
    CannotInfer,
    Uncomparable {
        left: Arc<VagueType>,
        right: Arc<VagueType>,
    },
    ExpectedConstExpr,
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
            (CollapsedCompileError::Logic(convert), CompileError::Logic(other_convert)) => {
                convert.try_collapse(other_convert).map_err(CompileError::Logic)
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
                    TokenKind::Hashtag => TokenString::Punctuation("#"),
                    TokenKind::Match => TokenString::Keyword("match"),
                    TokenKind::Let => TokenString::Keyword("let"),
                    TokenKind::Undefined => TokenString::Keyword("undefined"),
                    TokenKind::Static => TokenString::Keyword("static"),
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
                CollapsedLogicError::InvalidNumLiteral => "Invalid number literal".into(),
                CollapsedLogicError::ElementAfterSpread => {
                    "Additional array elements cannot come after spread element".into()
                },
                CollapsedLogicError::FoundInternalIdent => {
                    "Internal idents should not be defineable in user code".into()
                },
            },
            Self::Type(ty) => match ty {
                CollapsedTypeError::Mismatch { expected_any_of, found } => {
                    format!(
                        "Expected {}; Found {}",
                        expected_any_of
                            .iter()
                            .map(|expected| expected.to_display().display())
                            .join(", "),
                        found.to_display().display()
                    )
                },
                CollapsedTypeError::InvalidCast { from, to } => {
                    format!(
                        "Unable to cast {} to {}",
                        from.to_display().display(),
                        to.to_display().display()
                    )
                },
                CollapsedTypeError::InvalidTransmute { from, to } => {
                    format!(
                        "Unable to transmute {} to {}",
                        from.to_display().display(),
                        to.to_display().display()
                    )
                },
                CollapsedTypeError::OffsetIndexNonArray { found } => {
                    format!(
                        "Unable to index a type that isn't an array; Found: {}",
                        found.to_display().display()
                    )
                },
                CollapsedTypeError::FieldIndexNonStruct { found } => {
                    format!(
                        "Unable to get field of type that isn't a struct; Found: {}",
                        found.to_display().display()
                    )
                },
                CollapsedTypeError::IdentNotFound { name } => {
                    format!("Identifier `{name}` was not found in scope")
                },
                CollapsedTypeError::DerefNonRef { found } => {
                    format!(
                        "Unable to dereference a type that isn't a reference; Found: {}",
                        found.to_display().display()
                    )
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
                    let fmt_binary_op_expr = |(left, right): &(Arc<VagueType>, Arc<VagueType>)| {
                        format!(
                            "{} {} {}",
                            left.to_display().display(),
                            op,
                            right.to_display().display()
                        )
                    };

                    let expected = expected.iter().map(fmt_binary_op_expr).join(", ");
                    let found = fmt_binary_op_expr(found);

                    format!("Expected one of: {expected}; Found {found}")
                },
                CollapsedTypeError::UnknownType { name } => {
                    format!("Unable to find type `{name}`")
                },
                CollapsedTypeError::UnknownEnum { name } => {
                    format!("Unable to find enum `{name}`")
                },
                CollapsedTypeError::UnknownVariant { enum_name, variant_name } => {
                    format!("Enum `{enum_name}` does not have variant `{variant_name}`")
                },
                CollapsedTypeError::IndexInvalidField { struct_type, field_name } => {
                    format!(
                        "Field `{field_name}` is not in type {}",
                        struct_type.to_display().display()
                    )
                },
                CollapsedTypeError::AssignCellExprToCompoundPlace { place_size } => {
                    format!("Cannot assign cell expression to place with size {place_size}")
                },
                CollapsedTypeError::StructLiteralMissingField { field_name, field_type } => {
                    format!(
                        "Struct literal is missing field `{field_name}` with type {}",
                        field_type.to_display().display()
                    )
                },
                CollapsedTypeError::UnmatchableCase => "This case will never be matched".into(),
                CollapsedTypeError::NonExhaustiveMatch { unmatched_cases } => format!(
                    "Match statement does not cover the following cases: `{}`",
                    unmatched_cases.iter().map(|x| x.to_string()).join(" | ")
                ),
                CollapsedTypeError::CannotInfer => {
                    "Cannot infer type; Explicit type is needed".into()
                },
                CollapsedTypeError::Uncomparable { left, right } => {
                    format!(
                        "Cannot compare types {} and {}",
                        left.to_display().display(),
                        right.to_display().display()
                    )
                },
                CollapsedTypeError::ExpectedConstExpr => {
                    "Expected const expr in this context".to_string()
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
            CompileError::Logic(convert) => Self::Logic(match convert {
                LogicError::InvalidArrayTypeLen => CollapsedLogicError::InvalidArrayTypeLen,
                LogicError::InvalidNumLiteral => CollapsedLogicError::InvalidNumLiteral,
                LogicError::ElementAfterSpread => CollapsedLogicError::ElementAfterSpread,
                LogicError::FoundInternalIdent => CollapsedLogicError::FoundInternalIdent,
            }),
            CompileError::Type(ty) => Self::Type(match ty {
                TypeError::Mismatch { expected_any_of, found } => {
                    CollapsedTypeError::Mismatch { expected_any_of, found }
                },
                TypeError::InvalidCast { from, to } => CollapsedTypeError::InvalidCast { from, to },
                TypeError::InvalidTransmute { from, to } => {
                    CollapsedTypeError::InvalidTransmute { from, to }
                },
                TypeError::OffsetIndexNonArray { found } => {
                    CollapsedTypeError::OffsetIndexNonArray { found }
                },
                TypeError::FieldIndexNonStruct { found } => {
                    CollapsedTypeError::FieldIndexNonStruct { found }
                },
                TypeError::IdentNotFound { name } => CollapsedTypeError::IdentNotFound { name },
                TypeError::DerefNonRef { found } => CollapsedTypeError::DerefNonRef { found },
                TypeError::FuncNotFound { name } => CollapsedTypeError::FuncNotFound { name },
                TypeError::ArgsLenMismatch { func_name, expected, found } => {
                    CollapsedTypeError::ArgsLenMismatch { func_name, expected, found }
                },
                TypeError::BinaryOpOperandsMismatch { op, expected, found } => {
                    CollapsedTypeError::BinaryOpOperandsMismatch { op, expected, found }
                },
                TypeError::UnknownAlias { name } => CollapsedTypeError::UnknownType { name },
                TypeError::IndexInvalidField { struct_type, field_name } => {
                    CollapsedTypeError::IndexInvalidField { struct_type, field_name }
                },
                TypeError::AssignCellExprToCompoundPlace { place_size } => {
                    CollapsedTypeError::AssignCellExprToCompoundPlace { place_size }
                },
                TypeError::StructLiteralMissingField { field_name, field_type } => {
                    CollapsedTypeError::StructLiteralMissingField { field_name, field_type }
                },
                TypeError::UnknownEnum { name } => CollapsedTypeError::UnknownEnum { name },
                TypeError::UnknownVariant { enum_name, variant_name } => {
                    CollapsedTypeError::UnknownVariant { enum_name, variant_name }
                },
                TypeError::UnmatchableCase => CollapsedTypeError::UnmatchableCase,
                TypeError::NonExhaustiveMatch { unmatched_cases } => {
                    CollapsedTypeError::NonExhaustiveMatch { unmatched_cases }
                },
                TypeError::CannotInfer => CollapsedTypeError::CannotInfer,
                TypeError::Uncomparable { left, right } => {
                    CollapsedTypeError::Uncomparable { left, right }
                },
                TypeError::ExpectedConstExpr => CollapsedTypeError::ExpectedConstExpr,
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
