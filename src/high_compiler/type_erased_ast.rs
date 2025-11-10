use std::sync::Arc;

use crate::srced::Srced;

pub type Ref<T> = Arc<Srced<T>>;

#[derive(Debug)]
pub enum ExeItem {
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
    pub params: Ref<Vec<Ref<IdentDef>>>,
    pub proc: Ref<Proc>,
}

#[derive(Debug)]
pub struct UserName {
    pub str: Arc<str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InternalName {
    Return,
}

#[derive(Debug)]
pub enum Name {
    Internal(Ref<InternalName>),
    User(Ref<UserName>),
}

impl Name {
    pub fn to_str(&self) -> Arc<str> {
        match self {
            Self::User(user) => user.val.str.clone(),
            Self::Internal(internal) => {
                let str = match internal.val {
                    InternalName::Return => "return",
                };

                format!("~{str}").into()
            },
        }
    }
}

#[derive(Debug)]
pub struct IdentDef {
    pub name: Ref<Name>,
    pub size: u32,
}

#[derive(Debug)]
pub struct Place {
    pub head: Ref<PlaceHead>,
    pub offset: Option<Ref<Offset>>,
}

#[derive(Debug)]
pub enum Offset {
    Static(Ref<u32>),
    Expr(Ref<Expr>),
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
    pub idents: Arc<Vec<Ref<IdentDef>>>,
    pub body: Ref<Body>,
}

#[derive(Debug)]
pub struct Body {
    pub items: Ref<Vec<Ref<Statement>>>,
}

#[derive(Debug)]
pub enum Statement {
    Assign(Ref<Assign>),
    Call(Ref<FunctionCall>),
    If(Ref<IfItem>),
    While(Ref<WhileItem>),
    Native(Ref<NativeOperation>), // not compiled to by source code, internal/built-ins only
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
pub struct FunctionCall {
    pub func_name: Ref<Name>,
    pub param_exprs: Ref<Vec<Ref<Vec<Ref<AssignExpr>>>>>,
}

#[derive(Debug)]
pub struct Assign {
    pub place: Ref<Place>,
    pub exprs: Ref<Vec<Ref<AssignExpr>>>,
}

#[derive(Debug)]
pub struct AssignExpr {
    pub offset: u32,
    pub expr: Ref<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Literal(Ref<Literal>),
    Place(Ref<Place>),
    Ref(Ref<Place>),
    Paren(Ref<ParenExpr>),
}

#[derive(Debug)]
pub enum Literal {
    Str(Arc<str>),
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
    pub items: Arc<Vec<Ref<ExeItem>>>,
    pub statics: Arc<Vec<Ref<IdentDef>>>,
}
