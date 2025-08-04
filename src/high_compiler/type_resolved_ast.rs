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
    pub params: Ref<Vec<Ref<IdentDeclaration>>>,
    pub proc: Ref<Proc>,
}

#[derive(Debug)]
pub struct Name {
    pub str: Arc<str>,
}

#[derive(Debug)]
pub struct IdentDeclaration {
    pub name: Ref<Name>,
    pub size: u32,
}

#[derive(Debug)]
pub struct Place {
    pub head: Ref<PlaceHead>,
    pub offset: Option<Ref<Expr>>,
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
    pub param_exprs: Ref<Vec<Ref<Vec<Ref<AssignExpr>>>>>,
}

#[derive(Debug)]
pub struct Assign {
    pub place: Ref<Place>,
    pub expr: Ref<Vec<Ref<AssignExpr>>>,
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
    Out { place: Ref<Place> },
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
    pub items: Ref<Vec<Ref<ExeItem>>>,
}
