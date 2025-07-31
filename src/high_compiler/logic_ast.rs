use std::{ops::Not, sync::Arc};

use itertools::{EitherOrBoth, Itertools};

use crate::srced::Srced;

pub type Ref<T> = Arc<Srced<T>>;

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
}

#[derive(Debug)]
pub struct TypeAlias {
    pub name: Ref<Name>,
    pub ty: Ref<TypeHint>,
}

#[derive(Debug)]
pub struct Main {
    pub proc: Ref<Proc>,
}

#[derive(Debug)]
pub struct Func {
    pub name: Ref<Name>,
    pub params: Ref<Vec<Ref<IdentDeclaration>>>,
    pub proc: Ref<Proc>,
}

#[derive(Debug)]
pub struct Name {
    pub str: Arc<str>,
}

#[derive(Debug)]
pub enum TypeHint {
    Any,
    Primitive(PrimitiveType),
    Ref(Ref<TypeHint>),
    Array { ty: Ref<TypeHint>, len: u32 },
    Struct(Ref<Vec<Ref<FieldTypeHint>>>),
    Alias(Ref<Name>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Any,
    Primitive(PrimitiveType),
    Ref(Arc<Type>),
    Array { ty: Arc<Type>, len: u32 },
    Struct(Arc<Vec<Arc<FieldType>>>),
}

impl Type {
    pub fn size(&self) -> u32 {
        match self {
            Self::Any => 1,
            Self::Primitive(_) => 1,
            Self::Ref(_) => 1,
            Self::Array { ty, len } => ty.size() * len,
            Self::Struct(fields) => fields.iter().map(|field| field.ty.size()).sum(),
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
            Self::Ref(_) => Vec::from([CellType::Primitive(PrimitiveType::Uint)]),
            Self::Array { ty, len } => (0..*len).flat_map(|_| ty.cells_repr()).collect(),
            Self::Struct(fields) => fields.iter().flat_map(|field| field.ty.cells_repr()).collect(),
        }
    }

    pub fn contains_ref(&self) -> bool {
        match self {
            Self::Any => false,
            Self::Primitive(_) => false,
            Self::Ref(_) => true,
            Self::Array { ty, .. } => ty.contains_ref(),
            Self::Struct(fields) => fields.iter().any(|field| field.ty.contains_ref()),
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
        // but, we can cast to types that *don't* contain refs, but have the same
        // in-memory representation
        if other.contains_ref().not()
            && self.cells_repr().into_iter().zip_longest(other.cells_repr()).all(|cells| {
                let EitherOrBoth::Both(self_cell, other_cell) = cells else { return false };
                Type::from(self_cell).is_subtype_of(&Type::from(other_cell))
            })
        {
            return true;
        }

        // otherwise, casts can be used to perform inverses of type coercion
        other.is_subtype_of(self)
    }

    // transmute: may break invariant of type casting to AND the value casting from
    pub fn can_transmute_to(&self, other: &Self) -> bool {
        self.size() == other.size()
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
    Num,
    Int,
    Uint,
    Bool,
}

impl PrimitiveType {
    pub fn supertype(&self) -> Option<PrimitiveType> {
        Some(match self {
            Self::Val => return None,
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
pub struct IdentDeclaration {
    pub name: Ref<Name>,
    pub ty: Ref<TypeHint>,
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
pub struct Ident {
    pub name: Ref<Name>,
}

#[derive(Debug)]
pub struct Deref {
    pub addr: Ref<Expr>,
}

#[derive(Debug)]
pub struct Proc {
    pub idents: Ref<Vec<Ref<IdentDeclaration>>>,
    pub body: Ref<Body>,
}

#[derive(Debug)]
pub struct Body {
    pub items: Ref<Vec<Ref<BodyItem>>>,
}

#[derive(Debug)]
pub enum BodyItem {
    Statement(Ref<Statement>),
    If(Ref<IfItem>),
    While(Ref<WhileItem>),
}

#[derive(Debug)]
pub struct IfItem {
    pub condition: Ref<Expr>,
    pub then_body: Ref<Body>,
    pub else_item: Option<Ref<ElseItem>>,
}

#[derive(Debug)]
pub struct ElseItem {
    pub body: Ref<Body>,
}

#[derive(Debug)]
pub struct WhileItem {
    pub condition: Ref<Expr>,
    pub body: Ref<Body>,
}

#[derive(Debug)]
pub enum Statement {
    Assign(Ref<Assign>),
    Call(Ref<FunctionCall>),
    Native(Ref<NativeOperation>), // not compiled to by source code, internal/built-ins only
}

#[derive(Debug)]
pub struct FunctionCall {
    pub func_name: Ref<Name>,
    pub param_exprs: Ref<Vec<Ref<AssignExpr>>>,
}

#[derive(Debug)]
pub struct Assign {
    pub place: Ref<Place>,
    pub expr: Ref<AssignExpr>,
}

#[derive(Debug)]
pub enum AssignExpr {
    Expr(Ref<Expr>),
    Array { single_exprs: Ref<Vec<Ref<AssignExpr>>>, spread_expr: Option<Ref<AssignExpr>> },
    Struct(Ref<Vec<Ref<StructAssignField>>>),
}

#[derive(Debug)]
pub struct StructAssignField {
    pub name: Ref<Name>,
    pub assign: Ref<AssignExpr>,
}

#[derive(Debug)]
pub enum Expr {
    Literal(Ref<Literal>),
    Place(Ref<Place>),
    Ref(Ref<Place>),
    Paren(Ref<ParenExpr>),
    Cast { ty: Ref<TypeHint>, expr: Ref<Expr> },
    Transmute { ty: Ref<TypeHint>, expr: Ref<Expr> },
}

#[derive(Debug)]
pub enum Literal {
    Val(Arc<str>),
    Num(f64),
    Int(f64),
    Uint(f64),
    Bool(bool),
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
    Out { place: Ref<Place> },
    In { dest_place: Ref<Place> },
    Random { dest_place: Ref<Place>, min: Ref<Expr>, max: Ref<Expr> },
    StdoutClear,
    StdoutRead { dest_place: Ref<Place>, index: Ref<Expr> },
    StdoutWrite { val: Ref<Expr>, index: Ref<Expr> },
    StdoutLen { dest_place: Ref<Place> },
}

#[derive(Debug)]
pub struct Program {
    pub items: Ref<Vec<Ref<TopItem>>>,
}
