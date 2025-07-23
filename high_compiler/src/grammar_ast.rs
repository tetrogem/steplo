use std::sync::Arc;

use crate::{ast_error::AstErrorSet, ast_parse::parse_ast, token_feed::TokenFeed};

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
    pub params: Arc<CommaList<IdentDeclaration>>,
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

#[derive(Debug)]
pub struct RefType {
    pub ty: Arc<Type>,
}

#[derive(Debug)]
pub struct ArrayType {
    pub ty: Arc<Type>,
    pub len: Arc<NumLiteral>,
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
    pub head: Arc<ParensNest<PlaceHead>>,
    pub offset: Arc<Maybe<Offset>>,
}

#[derive(Debug)]
pub enum ParensNest<T> {
    Root(Arc<T>),
    Wrapped(Arc<ParensWrapped<ParensNest<T>>>),
}

#[derive(Debug)]
pub struct ParensWrapped<T> {
    pub item: Arc<T>,
}

#[derive(Debug)]
pub enum Maybe<T: ?Sized> {
    Item(Arc<T>),
    Empty(Arc<Empty>),
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
    pub idents: Arc<CommaList<IdentDeclaration>>,
    pub body: Arc<Body>,
}

#[derive(Debug)]
pub struct Body {
    pub items: Arc<SemiList<BodyItem>>,
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
    pub else_item: Arc<Maybe<ElseItem>>,
}

#[derive(Debug)]
pub enum ElseItem {
    Body(Arc<ElseBodyItem>),
    If(Arc<ElseIfItem>),
}

#[derive(Debug)]
pub struct ElseBodyItem {
    pub body: Arc<Body>,
}

#[derive(Debug)]
pub struct ElseIfItem {
    pub if_item: Arc<IfItem>,
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
}

#[derive(Debug)]
pub struct FunctionCall {
    pub func_name: Arc<Name>,
    pub param_exprs: Arc<CommaList<AssignExpr>>,
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
    Cast(Arc<CastExpr<Expr>>),
    Transmute(Arc<TransmuteExpr<Expr>>),
}

#[derive(Debug)]
pub struct CastExpr<T> {
    pub ty: Arc<Type>,
    pub item: Arc<ParensNest<T>>,
}

#[derive(Debug)]
pub struct TransmuteExpr<T> {
    pub ty: Arc<Type>,
    pub item: Arc<ParensNest<T>>,
}

#[derive(Debug)]
pub struct RefExpr {
    pub place: Arc<ParensNest<Place>>,
}

#[derive(Debug)]
pub struct StrLiteral {
    pub str: Arc<str>,
}

#[derive(Debug)]
pub struct NumLiteral {
    pub negative: Arc<Maybe<Negative>>,
    pub int: Arc<Digits>,
    pub dec: Arc<Maybe<Decimal>>,
}

#[derive(Debug)]
pub struct Decimal {
    pub digits: Arc<Digits>,
}

#[derive(Debug)]
pub struct Digits {
    pub digits: Arc<str>,
}

#[derive(Debug)]
pub struct Negative;

#[derive(Debug)]
pub enum BoolLiteral {
    True(Arc<TrueLiteral>),
    False(Arc<FalseLiteral>),
}

#[derive(Debug)]
pub struct TrueLiteral;

#[derive(Debug)]
pub struct FalseLiteral;

#[derive(Debug)]
pub enum Literal {
    Str(Arc<StrLiteral>),
    Num(Arc<NumLiteral>),
    Bool(Arc<BoolLiteral>),
}

#[derive(Debug)]
pub struct Span {
    pub elements: Arc<CommaList<Expr>>,
}

#[derive(Debug)]
pub struct Slice {
    pub place: Arc<Place>,
    pub start_in: Arc<Maybe<NumLiteral>>,
    pub end_ex: Arc<NumLiteral>,
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
    pub items: Arc<List<TopItem>>,
}

#[derive(Debug)]
pub struct Comment {
    pub text: Arc<str>,
}

#[derive(Debug)]
pub struct Empty;

#[derive(Debug)]
pub enum CommaList<T> {
    // <item>,
    Link(Arc<CommaListLink<T>>),
    // <item>
    Tail(Arc<T>),
    //
    Empty(Arc<Empty>),
}

#[derive(Debug)]
pub struct CommaListLink<T> {
    pub item: Arc<T>,
    pub next: Arc<CommaList<T>>,
}

#[derive(Debug)]
pub enum SemiList<T> {
    // <item>,
    Link(Arc<SemiListLink<T>>),
    // <item>
    Tail(Arc<T>),
    //
    Empty(Arc<Empty>),
}

#[derive(Debug)]
pub struct SemiListLink<T> {
    pub item: Arc<T>,
    pub next: Arc<SemiList<T>>,
}

#[derive(Debug)]
pub enum List<T> {
    // <item>
    Link(Arc<ListLink<T>>),
    //
    Empty(Arc<Empty>),
}

#[derive(Debug)]
pub struct ListLink<T> {
    pub item: Arc<T>,
    pub next: Arc<List<T>>,
}

pub fn parse(mut tokens: TokenFeed) -> Result<Program, AstErrorSet> {
    parse_ast(&mut tokens)
}
