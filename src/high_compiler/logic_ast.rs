use std::{
    ops::Not,
    sync::{Arc, LazyLock},
};

use itertools::{EitherOrBoth, Itertools};
use uuid::Uuid;

use crate::{high_compiler::srced::SrcRange, srced::Srced};

pub type Ref<T> = Arc<Srced<T>>;

pub fn nominal_type_hint(range: SrcRange, name: &str) -> TypeHint {
    TypeHint::Nominal(Arc::new(Srced { range, val: Name { str: name.into() } }))
}

pub static ANY_TYPE: LazyLock<Arc<Type>> = LazyLock::new(|| Arc::new(Type::Any));

pub fn any_type_hint(range: SrcRange) -> TypeHint {
    nominal_type_hint(range, Type::ANY_NAME)
}

pub static VAL_TYPE: LazyLock<Arc<Type>> =
    LazyLock::new(|| Arc::new(Type::Primitive(PrimitiveType::Val)));

pub fn val_type_hint(range: SrcRange) -> TypeHint {
    nominal_type_hint(range, Type::VAL_NAME)
}

pub static STR_TYPE: LazyLock<Arc<Type>> =
    LazyLock::new(|| Arc::new(Type::Primitive(PrimitiveType::Str)));

pub fn str_type_hint(range: SrcRange) -> TypeHint {
    nominal_type_hint(range, Type::STR_NAME)
}

pub static NUM_TYPE: LazyLock<Arc<Type>> =
    LazyLock::new(|| Arc::new(Type::Primitive(PrimitiveType::Num)));

pub fn num_type_hint(range: SrcRange) -> TypeHint {
    nominal_type_hint(range, Type::NUM_NAME)
}

pub static INTEGER_TYPE: LazyLock<Arc<Type>> =
    LazyLock::new(|| Arc::new(Type::Primitive(PrimitiveType::Int)));

pub fn integer_type_hint(range: SrcRange) -> TypeHint {
    nominal_type_hint(range, Type::INTEGER_NAME)
}

pub static UINTEGER_TYPE: LazyLock<Arc<Type>> =
    LazyLock::new(|| Arc::new(Type::Primitive(PrimitiveType::Uint)));

pub fn uinteger_type_hint(range: SrcRange) -> TypeHint {
    nominal_type_hint(range, Type::UINTEGER_NAME)
}

pub static BOOL_TYPE: LazyLock<Arc<Type>> =
    LazyLock::new(|| Arc::new(Type::Primitive(PrimitiveType::Bool)));

pub fn bool_type_hint(range: SrcRange) -> TypeHint {
    nominal_type_hint(range, Type::BOOL_NAME)
}

pub static UNIT_TYPE: LazyLock<Arc<Type>> =
    LazyLock::new(|| Arc::new(Type::Struct(Arc::new(Vec::new()))));

pub fn unit_type_hint(range: SrcRange) -> TypeHint {
    TypeHint::Struct(Arc::new(Srced { range, val: Vec::new() }))
}

#[derive(Debug)]
pub enum TopItem {
    Exe(Ref<ExeItem>),
    Type(Ref<TypeItem>),
}

#[derive(Debug)]
pub enum ExeItem {
    Main(Ref<Main>),
    Func(Ref<Func>),
}

#[derive(Debug)]
pub enum TypeItem {
    Alias(Ref<TypeAlias>),
    Enum(Ref<EnumItem>),
}

#[derive(Debug)]
pub struct TypeAlias {
    pub name: Ref<Name>,
    pub ty: Ref<TypeHint>,
}

#[derive(Debug)]
pub struct EnumItem {
    pub name: Ref<Name>,
    pub variants: Ref<Vec<Ref<Name>>>,
}

#[derive(Debug)]
pub struct Main {
    pub proc: Ref<Proc>,
}

#[derive(Debug)]
pub struct Func {
    pub name: Ref<Name>,
    pub params: Ref<Vec<Ref<IdentDef>>>,
    pub proc: Ref<Proc>,
}

#[derive(Debug)]
pub struct Name {
    pub str: Arc<str>,
}

#[derive(Debug)]
pub enum TypeHint {
    Nominal(Ref<Name>),
    Ref(Ref<TypeHint>),
    Array { ty: Ref<TypeHint>, len: u32 },
    Struct(Ref<Vec<Ref<FieldTypeHint>>>),
}

impl TypeHint {
    pub fn from_type(ty: &Type, range: SrcRange) -> Self {
        match ty {
            Type::Any => any_type_hint(range),
            Type::Primitive(p) => match p {
                PrimitiveType::Val => val_type_hint(range),
                PrimitiveType::Str => str_type_hint(range),
                PrimitiveType::Num => num_type_hint(range),
                PrimitiveType::Int => integer_type_hint(range),
                PrimitiveType::Uint => uinteger_type_hint(range),
                PrimitiveType::Bool => bool_type_hint(range),
            },
            Type::Enum { name } => nominal_type_hint(range, name),
            Type::Ref(ty) => {
                TypeHint::Ref(Arc::new(Srced { range, val: TypeHint::from_type(ty, range) }))
            },
            Type::Array { ty, len } => TypeHint::Array {
                ty: Arc::new(Srced { range, val: TypeHint::from_type(ty, range) }),
                len: *len,
            },
            Type::Struct(fields) => TypeHint::Struct(Arc::new(Srced {
                range,
                val: fields
                    .iter()
                    .map(|field| {
                        let field = FieldTypeHint {
                            name: Arc::new(Srced { range, val: Name { str: field.name.clone() } }),
                            ty: Arc::new(Srced {
                                range,
                                val: TypeHint::from_type(&field.ty, range),
                            }),
                        };

                        Arc::new(Srced { range, val: field })
                    })
                    .collect(),
            })),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Any,
    Primitive(PrimitiveType),
    Ref(Arc<Type>),
    Array { ty: Arc<Type>, len: u32 },
    Struct(Arc<Vec<Arc<FieldType>>>),
    Enum { name: Arc<str> },
}

impl Type {
    pub const ANY_NAME: &'static str = "any";
    pub const VAL_NAME: &'static str = "val";
    pub const STR_NAME: &'static str = "str";
    pub const NUM_NAME: &'static str = "num";
    pub const INTEGER_NAME: &'static str = "int";
    pub const UINTEGER_NAME: &'static str = "uint";
    pub const BOOL_NAME: &'static str = "bool";

    pub fn size(&self) -> u32 {
        match self {
            Self::Any => 1,
            Self::Primitive(_) => 1,
            Self::Ref(_) => 1,
            Self::Array { ty, len } => ty.size() * len,
            Self::Struct(fields) => fields.iter().map(|field| field.ty.size()).sum(),
            Self::Enum { .. } => 1,
        }
    }

    pub fn is_isomorphic_with(&self, other: &Self) -> bool {
        self.is_subtype_of(other) && other.is_subtype_of(self)
    }

    pub fn is_subtype_of(&self, other: &Self) -> bool {
        if self == other {
            return true;
        }

        // special `any` type
        if self.size() == 1 && matches!(other, Self::Any) {
            return true;
        }

        match (self, other) {
            (Self::Any, Self::Any) => true,
            (Self::Ref(a), Self::Ref(b)) => a.is_isomorphic_with(b),
            (Self::Array { ty: a, len: a_len }, Self::Array { ty: b, len: b_len }) => {
                a_len == b_len && a.is_subtype_of(b)
            },
            (Self::Primitive(a), Self::Primitive(b)) => a.is_subtype_of(*b),
            (Self::Struct(a), Self::Struct(b)) => {
                for eob in a.iter().zip_longest(b.iter()) {
                    if let EitherOrBoth::Both(a, b) = eob
                        && a.name == b.name
                        && a.ty.is_subtype_of(&b.ty)
                    {
                        continue;
                    }

                    return false;
                }

                true
            },
            _ => false,
        }
    }

    pub fn cells_repr(&self) -> Vec<CellType> {
        match self {
            Self::Any => Vec::from([CellType::Any]),
            Self::Primitive(x) => Vec::from([CellType::Primitive(*x)]),
            Self::Ref(_) => Self::Primitive(PrimitiveType::Uint).cells_repr(),
            Self::Array { ty, len } => (0..*len).flat_map(|_| ty.cells_repr()).collect(),
            Self::Struct(fields) => fields.iter().flat_map(|field| field.ty.cells_repr()).collect(),
            Self::Enum { .. } => Self::Primitive(PrimitiveType::Uint).cells_repr(),
        }
    }

    pub fn contains_ref(&self) -> bool {
        match self {
            Self::Any => false,
            Self::Primitive(_) => false,
            Self::Ref(_) => true,
            Self::Array { ty, .. } => ty.contains_ref(),
            Self::Struct(fields) => fields.iter().any(|field| field.ty.contains_ref()),
            Self::Enum { .. } => false,
        }
    }

    pub fn contains_enum(&self) -> bool {
        match self {
            Self::Any => false,
            Self::Primitive(_) => false,
            Self::Ref(ty) => ty.contains_enum(),
            Self::Array { ty, .. } => ty.contains_enum(),
            Self::Struct(fields) => fields.iter().any(|field| field.ty.contains_enum()),
            Self::Enum { .. } => true,
        }
    }

    // cast: may break invariant of type casting to
    pub fn can_cast_to(&self, other: &Self) -> bool {
        // can always cast to current type or subtype
        if self.is_subtype_of(other) {
            return true;
        }

        // cannot cast safely to refs, as they can write to memory with any original type
        // breaking the original invariant of that memory location

        // cannot cast safely to enums, because the source value could be outside of the range
        // of the variants of the target type

        // but, we can cast to types that *don't* contain these types, but have the same
        // in-memory representation
        other.contains_ref().not()
            && other.contains_enum().not()
            && self.cells_repr().into_iter().zip_longest(other.cells_repr()).all(|cells| {
                let EitherOrBoth::Both(self_cell, other_cell) = cells else { return false };
                Type::from(self_cell).is_subtype_of(&Type::from(other_cell))
            })
    }

    // transmute: may break invariant of type casting to AND the value casting from
    pub fn can_transmute_to(&self, other: &Self) -> bool {
        self.size() == other.size()
    }

    pub fn closest_common_supertype<'a>(a: &'a Arc<Type>, b: &'a Arc<Type>) -> Option<Arc<Type>> {
        if a.is_subtype_of(b) {
            Some(b.clone())
        } else if b.is_subtype_of(a) {
            Some(a.clone())
        } else if let Type::Primitive(pa) = a.as_ref()
            && let Type::Primitive(pb) = b.as_ref()
        {
            Some(Arc::new(Type::Primitive(PrimitiveType::closest_common_supertype(*pa, *pb))))
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CellType {
    Any,
    Primitive(PrimitiveType),
}

impl From<CellType> for Type {
    fn from(value: CellType) -> Self {
        match value {
            CellType::Any => Self::Any,
            CellType::Primitive(x) => Self::Primitive(x),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PrimitiveType {
    Val,
    Str,
    Num,
    Int,
    Uint,
    Bool,
}

impl PrimitiveType {
    pub fn supertype(&self) -> Option<PrimitiveType> {
        Some(match self {
            Self::Val => return None,
            Self::Str => Self::Val,
            Self::Num => Self::Val,
            Self::Int => Self::Num,
            Self::Uint => Self::Int,
            Self::Bool => Self::Val,
        })
    }

    pub fn is_subtype_of(&self, other: PrimitiveType) -> bool {
        if *self == other {
            return true;
        }

        self.supertype().is_some_and(|supertype| supertype.is_subtype_of(other))
    }

    pub fn closest_common_supertype(a: PrimitiveType, b: PrimitiveType) -> PrimitiveType {
        if a.is_subtype_of(b) {
            b
        } else if b.is_subtype_of(a) {
            a
        } else {
            PrimitiveType::Val
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FieldType {
    pub name: Arc<str>,
    pub ty: Arc<Type>,
}

#[derive(Debug)]
pub struct FieldTypeHint {
    pub name: Ref<Name>,
    pub ty: Ref<TypeHint>,
}

#[derive(Debug)]
pub struct IdentDef {
    pub ident: Ref<Ident>,
    pub ty: Ref<TypeHint>,
}

#[derive(Debug)]
pub struct IdentInit {
    pub def: Ref<IdentDef>,
    pub expr: Ref<Expr>,
}

#[derive(Debug)]
pub struct Place {
    pub head: Ref<PlaceHead>,
    pub index_chain: Ref<Vec<Ref<PlaceIndex>>>,
}

#[derive(Debug)]
pub enum PlaceIndex {
    Offset(Ref<Expr>),
    Field(Ref<Name>),
}

#[derive(Debug)]
pub enum PlaceHead {
    Ident(Ref<Ident>),
    Deref(Ref<Deref>),
}

#[derive(Debug)]
pub enum Ident {
    User { name: Ref<Name> },
    Internal { name: Ref<Name>, uuid: Uuid },
}

impl Ident {
    pub fn name(&self) -> &Ref<Name> {
        match self {
            Self::User { name } => name,
            Self::Internal { name, .. } => name,
        }
    }
}

#[derive(Debug)]
pub struct Deref {
    pub addr: Ref<Expr>,
}

#[derive(Debug)]
pub struct Proc {
    pub body: Ref<Expr>,
}

#[derive(Debug)]
pub struct Trail<T> {
    pub items: Ref<Vec<T>>,
    pub trailing: bool,
}

#[derive(Debug)]
pub struct Block {
    pub items: Ref<Trail<Ref<Expr>>>,
}

#[derive(Debug)]
pub struct IfItem {
    pub condition: Ref<Expr>,
    pub then_body: Ref<Expr>,
    pub else_item: Option<Ref<ElseItem>>,
}

#[derive(Debug)]
pub struct ElseItem {
    pub body: Ref<Expr>,
}

#[derive(Debug)]
pub struct WhileItem {
    pub condition: Ref<Expr>,
    pub body: Ref<Expr>,
}

#[derive(Debug)]
pub struct MatchItem {
    pub expr: Ref<Expr>,
    pub cases: Ref<Vec<Ref<MatchCase>>>,
}

#[derive(Debug)]
pub struct MatchCase {
    pub variant: Ref<VariantLiteral>,
    pub body: Ref<Expr>,
}

#[derive(Debug)]
pub enum Statement {
    IdentInit(Ref<IdentInit>),
    Assign(Ref<Assign>),
    Call(Ref<FunctionCall>),
    Native(Ref<NativeOperation>), // not compiled to by source code, internal/built-ins only
}

#[derive(Debug)]
pub struct FunctionCall {
    pub func_name: Ref<Name>,
    pub param_exprs: Ref<Vec<Ref<Expr>>>,
}

#[derive(Debug)]
pub struct Assign {
    pub place: Ref<Place>,
    pub expr: Ref<Expr>,
}

#[derive(Debug)]
pub struct StructAssignField {
    pub name: Ref<Name>,
    pub assign: Ref<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Literal(Ref<Literal>),
    Place(Ref<Place>),
    Ref(Ref<Place>),
    Paren(Ref<ParenExpr>),
    Cast { ty: Ref<TypeHint>, expr: Ref<Expr> },
    Transmute { ty: Ref<TypeHint>, expr: Ref<Expr> },
    Statement(Ref<Statement>),
    If(Ref<IfItem>),
    While(Ref<WhileItem>),
    Match(Ref<MatchItem>),
    Block(Ref<Block>),
    Array { single_exprs: Ref<Vec<Ref<Expr>>>, spread_expr: Option<Ref<Expr>> },
    Struct(Ref<Vec<Ref<StructAssignField>>>),
    Undefined,
}

#[derive(Debug)]
pub enum Literal {
    Str(Arc<str>),
    Num(f64),
    Int(f64),
    Uint(f64),
    Bool(bool),
    Variant(Ref<VariantLiteral>),
}

#[derive(Debug)]
pub struct VariantLiteral {
    pub enum_name: Option<Ref<Name>>,
    pub variant_name: Ref<Name>,
}

#[derive(Debug)]
pub enum ParenExpr {
    Unary(Ref<UnaryParenExpr>),
    Binary(Ref<BinaryParenExpr>),
}

#[derive(Debug)]
pub struct UnaryParenExpr {
    pub op: Ref<UnaryParenExprOp>,
    pub operand: Ref<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryParenExprOp {
    // boolean
    Not,
}

#[derive(Debug)]
pub struct BinaryParenExpr {
    pub op: Ref<BinaryParenExprOp>,
    pub left: Ref<Expr>,
    pub right: Ref<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryParenExprOp {
    // math
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // inequality
    Eq,
    Neq,
    Gt,
    Lt,
    Gte,
    Lte,
    // boolean
    And,
    Or,
    // string
    Join,
}

#[derive(Debug)]
pub enum NativeOperation {
    Out { val: Ref<Expr> },
    In { dest_place: Ref<Place> },
    Random { dest_place: Ref<Place>, min: Ref<Expr>, max: Ref<Expr> },
    StdoutClear,
    StdoutRead { dest_place: Ref<Place>, index: Ref<Expr> },
    StdoutWrite { val: Ref<Expr>, index: Ref<Expr> },
    StdoutLen { dest_place: Ref<Place> },
    Wait { duration_s: Ref<Expr> },
    TimerGet { dest_place: Ref<Place> },
}

#[derive(Debug)]
pub struct Program {
    pub items: Ref<Vec<Ref<TopItem>>>,
}
