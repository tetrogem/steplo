use std::sync::Arc;

use crate::srced::Srced;

pub type Ref<T> = Arc<Srced<T>>;

#[derive(Debug)]
pub enum TopItem {
    Main(Ref<Main>),
    Func(Ref<Func>),
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

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Ref(Arc<Type>),
    Array { ty: Arc<Type>, len: u32 },
    Base(Arc<BaseType>),
}

impl Type {
    pub fn size(&self) -> u32 {
        match self {
            Self::Ref(_) => 1,
            Self::Base(_) => 1,
            Self::Array { ty, len } => ty.size() * len,
        }
    }

    pub fn is_assignable_to(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ref(a), Self::Ref(b)) => a.is_assignable_to(b) && b.is_assignable_to(a),
            (Self::Ref(_), Self::Base(b)) => matches!(b.as_ref(), BaseType::Any),
            (Self::Array { ty: a, len: a_len }, Self::Array { ty: b, len: b_len }) => {
                a_len == b_len && a.is_assignable_to(b) && b.is_assignable_to(a)
            },
            (Self::Base(a), Self::Base(b)) => {
                use BaseType::*;
                matches!(
                    (a.as_ref(), b.as_ref()),
                    (Any, Any)
                        | (Val, Val | Any)
                        | (Num, Num | Val | Any)
                        | (Int, Int | Num | Val | Any)
                        | (Uint, Uint | Int | Num | Val | Any)
                        | (Bool, Bool | Val | Any)
                )
            },
            _ => false,
        }
    }

    // cast: may break invariant of type casting to
    pub fn can_cast_to(&self, other: &Self) -> bool {
        // can always cast to current type or subtype
        if self.is_assignable_to(other) {
            return true;
        }

        // cannot cast safely to refs, as they can write to memory with any original type
        // breaking the original invariant of that memory location
        if matches!(other, Self::Ref(_)) {
            return false;
        }

        // refs can be cast to uints safely (and by proxy, every supertype of a uint)
        if matches!(self, Self::Ref(_))
            && Self::Base(Arc::new(BaseType::Uint)).is_assignable_to(other)
        {
            return true;
        }

        // otherwise, casts can be used to perform inverses of type coercion
        other.is_assignable_to(self)
    }

    // transmute: may break invariant of type casting to AND the value casting from
    pub fn can_transmute_to(&self, other: &Self) -> bool {
        self.size() == other.size()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BaseType {
    Any,
    Val,
    Num,
    Int,
    Uint,
    Bool,
}

#[derive(Debug)]
pub struct IdentDeclaration {
    pub name: Ref<Name>,
    pub ty: Arc<Type>,
}

#[derive(Debug)]
pub struct Place {
    pub head: Ref<PlaceHead>,
    pub offset: Option<Ref<Offset>>,
}

#[derive(Debug)]
pub enum PlaceHead {
    Ident(Ref<Ident>),
    Deref(Ref<Deref>),
}

#[derive(Debug)]
pub struct Offset {
    pub expr: Ref<Expr>,
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
    Span(Ref<Vec<Ref<Expr>>>),
    Slice { place: Ref<Place>, start_in: u32, end_ex: u32 },
}

#[derive(Debug)]
pub enum Expr {
    Literal(Ref<Literal>),
    Place(Ref<Place>),
    Ref(Ref<Place>),
    Paren(Ref<ParenExpr>),
    Cast { ty: Arc<Type>, expr: Ref<Expr> },
    Transmute { ty: Arc<Type>, expr: Ref<Expr> },
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
    Out { ident: Ref<Place> },
    In { dest_ident: Ref<Place> },
    Random { dest_ident: Ref<Place>, min: Ref<Expr>, max: Ref<Expr> },
}

#[derive(Debug)]
pub struct Program {
    pub items: Ref<Vec<Ref<TopItem>>>,
}
