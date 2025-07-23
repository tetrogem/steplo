use std::{ops::Not, sync::Arc};

#[derive(Debug)]
pub enum TopItem {
    Main(Arc<Main>),
    Func(Arc<Func>),
}

#[derive(Debug)]
pub struct Main {
    pub proc: Arc<Proc>,
}

#[derive(Debug)]
pub struct Func {
    pub name: Arc<Name>,
    pub params: Arc<Vec<Arc<IdentDeclaration>>>,
    pub proc: Arc<Proc>,
}

#[derive(Debug, Hash, PartialEq, Eq)]
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
    pub name: Arc<Name>,
    pub ty: Arc<Type>,
}

#[derive(Debug)]
pub struct Place {
    pub head: Arc<PlaceHead>,
    pub offset: Option<Arc<Offset>>,
}

#[derive(Debug)]
pub enum PlaceHead {
    Ident(Arc<Ident>),
    Deref(Arc<Deref>),
}

#[derive(Debug)]
pub struct Offset {
    pub expr: Arc<Expr>,
}

#[derive(Debug)]
pub struct Ident {
    pub name: Arc<Name>,
}

#[derive(Debug)]
pub struct Deref {
    pub addr: Arc<Expr>,
}

#[derive(Debug)]
pub struct Proc {
    pub idents: Arc<Vec<Arc<IdentDeclaration>>>,
    pub body: Arc<Body>,
}

#[derive(Debug)]
pub struct Body {
    pub items: Arc<Vec<Arc<BodyItem>>>,
}

#[derive(Debug)]
pub enum BodyItem {
    Statement(Arc<Statement>),
    If(Arc<IfItem>),
    While(Arc<WhileItem>),
}

#[derive(Debug)]
pub struct IfItem {
    pub condition: Arc<Expr>,
    pub then_body: Arc<Body>,
    pub else_item: Option<Arc<ElseItem>>,
}

#[derive(Debug)]
pub struct ElseItem {
    pub body: Arc<Body>,
}

#[derive(Debug)]
pub struct WhileItem {
    pub condition: Arc<Expr>,
    pub body: Arc<Body>,
}

#[derive(Debug)]
pub enum Statement {
    Assign(Arc<Assign>),
    Call(Arc<FunctionCall>),
    Native(Arc<NativeOperation>), // not compiled to by source code, internal/built-ins only
}

#[derive(Debug)]
pub struct FunctionCall {
    pub func_name: Arc<Name>,
    pub param_exprs: Arc<Vec<Arc<AssignExpr>>>,
}

#[derive(Debug)]
pub struct Assign {
    pub place: Arc<Place>,
    pub expr: Arc<AssignExpr>,
}

#[derive(Debug)]
pub enum AssignExpr {
    Expr(Arc<Expr>),
    Span(Arc<Vec<Arc<Expr>>>),
    Slice { place: Arc<Place>, start_in: u32, end_ex: u32 },
}

#[derive(Debug)]
pub enum Expr {
    Literal(Arc<Literal>),
    Place(Arc<Place>),
    Ref(Arc<Place>),
    Paren(Arc<ParenExpr>),
    Cast { ty: Arc<Type>, expr: Arc<Expr> },
    Transmute { ty: Arc<Type>, expr: Arc<Expr> },
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
    Unary(Arc<UnaryParenExpr>),
    Binary(Arc<BinaryParenExpr>),
}

#[derive(Debug)]
pub struct UnaryParenExpr {
    pub op: UnaryParenExprOp,
    pub operand: Arc<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryParenExprOp {
    // boolean
    Not,
}

#[derive(Debug)]
pub struct BinaryParenExpr {
    pub op: BinaryParenExprOp,
    pub left: Arc<Expr>,
    pub right: Arc<Expr>,
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
    Out { ident: Arc<Place> },
    In { dest_ident: Arc<Place> },
    Random { dest_ident: Arc<Place>, min: Arc<Expr>, max: Arc<Expr> },
}

#[derive(Debug)]
pub struct Program {
    pub items: Arc<Vec<Arc<TopItem>>>,
}
