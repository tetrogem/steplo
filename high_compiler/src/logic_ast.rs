use std::sync::Arc;

use crate::grammar_ast as g;
use crate::{ast_error::AstErrorSet, token_feed::TokenFeed};

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

#[derive(Debug)]
pub enum Type {
    Ref(Arc<RefType>),
    Array(Arc<ArrayType>),
    Base(Arc<BaseType>),
}

impl Type {
    pub fn size(&self) -> u32 {
        match self {
            Self::Ref(_) => 1,
            Self::Base(_) => 1,
            Self::Array(arr) => &arr.ty.size() * arr.len,
        }
    }
}

#[derive(Debug)]
pub struct RefType {
    pub ty: Arc<Type>,
}

#[derive(Debug)]
pub struct ArrayType {
    pub ty: Arc<Type>,
    pub len: u32,
}

#[derive(Debug)]
pub struct BaseType {
    pub name: Arc<Name>,
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
    Span(Arc<Span>),
    Slice(Arc<Slice>),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Arc<Literal>),
    Place(Arc<Place>),
    Ref(Arc<RefExpr>),
    Paren(Arc<ParenExpr>),
}

#[derive(Debug)]
pub struct RefExpr {
    pub place: Arc<Place>,
}

#[derive(Debug)]
pub struct Literal {
    pub str: Arc<str>,
}

#[derive(Debug)]
pub struct Span {
    pub elements: Arc<Vec<Arc<Expr>>>,
}

#[derive(Debug)]
pub struct Slice {
    pub place: Arc<Place>,
    pub start_in: u32,
    pub end_ex: u32,
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

#[derive(Debug)]
pub struct Comment {
    #[expect(unused)]
    pub text: Arc<str>,
}
