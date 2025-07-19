use std::sync::Arc;

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
    pub params: Arc<CommaSeparated<IdentDeclaration>>,
    pub proc: Arc<Proc>,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Name {
    pub str: Arc<str>,
}

#[derive(Debug)]
pub enum IdentDeclaration {
    Single(Arc<SingleIdentDeclaration>),
    Span(Arc<SpanIdentDeclaration>),
}

#[derive(Debug)]
pub struct SingleIdentDeclaration {
    pub name: Arc<Name>,
}

#[derive(Debug)]
pub struct SpanIdentDeclaration {
    pub name: Arc<Name>,
    pub size: usize,
}

impl IdentDeclaration {
    pub fn name(&self) -> &Arc<Name> {
        match self {
            Self::Single(single) => &single.name,
            Self::Span(span) => &span.name,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Single(_) => 1,
            Self::Span(span) => span.size,
        }
    }
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
    pub idents: Arc<CommaSeparated<IdentDeclaration>>,
    pub body: Arc<Body>,
}

#[derive(Debug)]
pub struct Body {
    pub items: Arc<SemiSeparated<BodyItem>>,
}

#[derive(Debug)]
pub struct SemiSeparated<T> {
    pub elements: Arc<Vec<Arc<T>>>,
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
    pub param_exprs: Arc<CommaSeparated<AssignExpr>>,
}

#[derive(Debug)]
pub struct Assign {
    pub loc: Arc<Place>,
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
    pub elements: Arc<CommaSeparated<Expr>>,
}

#[derive(Debug)]
pub struct Slice {
    pub place: Arc<Place>,
    pub start_in: usize,
    pub end_ex: usize,
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
    Not(NotOp),
}

#[derive(Debug, Clone, Copy)]
pub struct NotOp;

#[derive(Debug)]
pub struct BinaryParenExpr {
    pub op: BinaryParenExprOp,
    pub left: Arc<Expr>,
    pub right: Arc<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryParenExprOp {
    // math
    Add(AddOp),
    Sub(SubOp),
    Mul(MulOp),
    Div(DivOp),
    Mod(ModOp),
    // inequality
    Eq(EqOp),
    Neq(NeqOp),
    Gt(GtOp),
    Lt(LtOp),
    Gte(GteOp),
    Lte(LteOp),
    // boolean
    And(AndOp),
    Or(OrOp),
    // string
    Join(JoinOp),
}

#[derive(Debug, Clone, Copy)]
pub struct AddOp;

#[derive(Debug, Clone, Copy)]
pub struct SubOp;

#[derive(Debug, Clone, Copy)]
pub struct MulOp;

#[derive(Debug, Clone, Copy)]
pub struct DivOp;

#[derive(Debug, Clone, Copy)]
pub struct ModOp;

#[derive(Debug, Clone, Copy)]
pub struct EqOp;

#[derive(Debug, Clone, Copy)]
pub struct NeqOp;

#[derive(Debug, Clone, Copy)]
pub struct GtOp;

#[derive(Debug, Clone, Copy)]
pub struct LtOp;

#[derive(Debug, Clone, Copy)]
pub struct GteOp;

#[derive(Debug, Clone, Copy)]
pub struct LteOp;

#[derive(Debug, Clone, Copy)]
pub struct AndOp;

#[derive(Debug, Clone, Copy)]
pub struct OrOp;

#[derive(Debug, Clone, Copy)]
pub struct JoinOp;

#[derive(Debug)]
pub enum NativeOperation {
    Out { ident: Arc<Place> },
    In { dest_ident: Arc<Place> },
}

#[derive(Debug)]
pub struct Program {
    pub items: Arc<Vec<Arc<TopItem>>>,
}

#[derive(Debug)]
pub struct Comment {
    pub text: Arc<str>,
}

#[derive(Debug)]
pub struct CommaSeparated<T> {
    pub elements: Arc<Vec<Arc<T>>>,
}

pub fn parse(mut tokens: TokenFeed) -> Result<Program, AstErrorSet> {
    tokens.parse().res
}
