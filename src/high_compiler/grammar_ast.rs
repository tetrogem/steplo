use std::sync::Arc;

use super::{
    compile_error::CompileErrorSet, grammar_parse::parse_ast, srced::Srced, token_feed::TokenFeed,
};

pub type Ref<T> = Arc<Srced<T>>;

#[derive(Debug)]
pub enum TopItem {
    Main(Ref<Main>),
    Func(Ref<Func>),
    TypeAlias(Ref<TypeAlias>),
    Enum(Ref<EnumItem>),
}

#[derive(Debug)]
pub struct Main {
    pub proc: Ref<Proc>,
}

#[derive(Debug)]
pub struct Func {
    pub name: Ref<Name>,
    pub params: Ref<CommaList<IdentDef>>,
    pub proc: Ref<Proc>,
}

#[derive(Debug)]
pub struct TypeAlias {
    pub name: Ref<Name>,
    pub ty: Ref<Type>,
}

#[derive(Debug)]
pub struct EnumItem {
    pub name: Ref<Name>,
    pub variants: Ref<PipeList<Name>>,
}

#[derive(Debug)]
pub struct Name {
    pub str: Arc<str>,
}

#[derive(Debug)]
pub enum Type {
    Nominal(Ref<NominalType>),
    Ref(Ref<RefType>),
    Array(Ref<ArrayType>),
    Struct(Ref<StructType>),
}

#[derive(Debug)]
pub struct NominalType {
    pub name: Ref<Name>,
}

#[derive(Debug)]
pub struct RefType {
    pub ty: Ref<Type>,
}

#[derive(Debug)]
pub struct ArrayType {
    pub ty: Ref<Type>,
    pub len: Ref<NumLiteral>,
}

#[derive(Debug)]
pub struct StructType {
    pub fields: Ref<CommaList<IdentDef>>,
}

#[derive(Debug)]
pub struct IdentDef {
    pub name: Ref<Name>,
    pub ty: Ref<Type>,
}

#[derive(Debug)]
pub struct IdentInit {
    pub def: Ref<IdentDef>,
    pub expr: Ref<Expr>,
}

#[derive(Debug)]
pub struct PlaceIndexLink {
    pub index: Ref<PlaceIndex>,
    pub next_link: Ref<Maybe<PlaceIndexLink>>,
}

#[derive(Debug)]
pub struct Place {
    pub head: Ref<ParensNest<PlaceHead>>,
    pub index_link: Ref<Maybe<PlaceIndexLink>>,
}

#[derive(Debug)]
pub enum PlaceIndex {
    Offset(Ref<Offset>),
    Field(Ref<Field>),
}

#[derive(Debug)]
pub enum ParensNest<T> {
    Root(Ref<T>),
    Wrapped(Ref<ParensWrapped<ParensNest<T>>>),
}

#[derive(Debug)]
pub struct ParensWrapped<T> {
    pub item: Ref<T>,
}

#[derive(Debug)]
pub enum Maybe<T> {
    Item(Ref<T>),
    #[expect(unused)]
    Empty(Ref<Empty>),
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
pub struct Field {
    pub name: Ref<Name>,
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
    pub body: Ref<Expr>,
}

#[derive(Debug)]
pub struct Block {
    pub items: Ref<List<ExprOrSemi>>,
}

#[derive(Debug)]
pub enum ExprOrSemi {
    Expr(Ref<ShapedExpr>),
    Semi(Ref<Semi>),
}

#[derive(Debug)]
pub struct Semi;

#[derive(Debug)]
pub struct IfItem {
    pub condition: Ref<Expr>,
    pub then_body: Ref<Expr>,
    pub else_item: Ref<Maybe<ElseItem>>,
}

#[derive(Debug)]
pub enum ElseItem {
    Body(Ref<ElseBodyItem>),
    If(Ref<ElseIfItem>),
}

#[derive(Debug)]
pub struct ElseBodyItem {
    pub body: Ref<Expr>,
}

#[derive(Debug)]
pub struct ElseIfItem {
    pub if_item: Ref<IfItem>,
}

#[derive(Debug)]
pub struct WhileItem {
    pub condition: Ref<Expr>,
    pub body: Ref<Expr>,
}

#[derive(Debug)]
pub struct MatchItem {
    pub expr: Ref<Expr>,
    pub cases: Ref<List<MatchCase>>,
}

#[derive(Debug)]
pub struct MatchCase {
    pub variant: Ref<VariantLiteral>,
    pub body: Ref<Expr>,
}

#[derive(Debug)]
pub enum ShapedExpr {
    IdentInit(Ref<IdentInit>),
    Assign(Ref<Assign>),
    Call(Ref<FunctionCall>),
    NonShaped(Ref<Expr>),
}

#[derive(Debug)]
pub struct FunctionCall {
    pub func_name: Ref<Name>,
    pub param_exprs: Ref<CommaList<Expr>>,
}

#[derive(Debug)]
pub struct Assign {
    pub place: Ref<Place>,
    pub expr: Ref<ParensNest<Expr>>,
}

#[derive(Debug)]
pub enum Expr {
    Literal(Ref<Literal>),
    Place(Ref<Place>),
    Ref(Ref<RefExpr>),
    Paren(Ref<ParenExpr>),
    Cast(Ref<CastExpr<Expr>>),
    Transmute(Ref<TransmuteExpr<Expr>>),
    If(Ref<IfItem>),
    While(Ref<WhileItem>),
    Match(Ref<MatchItem>),
    Block(Ref<Block>),
    Array(Ref<ArrayAssign>),
    Struct(Ref<StructAssign>),
    Undefined(Ref<Undefined>),
    Shaped(Ref<ShapedExpr>),
}

#[derive(Debug)]
pub struct Undefined;

#[derive(Debug)]
pub struct CastExpr<T> {
    pub ty: Ref<Type>,
    pub item: Ref<ParensNest<T>>,
}

#[derive(Debug)]
pub struct TransmuteExpr<T> {
    pub ty: Ref<Type>,
    pub item: Ref<ParensNest<T>>,
}

#[derive(Debug)]
pub struct RefExpr {
    pub place: Ref<ParensNest<Place>>,
}

#[derive(Debug)]
pub struct StrLiteral {
    pub str: Arc<str>,
}

#[derive(Debug)]
pub struct NumLiteral {
    pub negative: Ref<Maybe<Negative>>,
    pub int: Ref<Digits>,
    pub dec: Ref<Maybe<Decimal>>,
}

#[derive(Debug)]
pub struct Decimal {
    pub digits: Ref<Digits>,
}

#[derive(Debug)]
pub struct Digits {
    pub digits: Arc<str>,
}

#[derive(Debug)]
pub struct Negative;

#[derive(Debug)]
#[expect(unused)]
pub enum BoolLiteral {
    True(Ref<TrueLiteral>),
    False(Ref<FalseLiteral>),
}

#[derive(Debug)]
pub struct TrueLiteral;

#[derive(Debug)]
pub struct FalseLiteral;

#[derive(Debug)]
pub struct VariantLiteral {
    pub enum_name: Ref<Maybe<Name>>,
    pub variant_name: Ref<Name>,
}

#[derive(Debug)]
pub enum Literal {
    Str(Ref<StrLiteral>),
    Num(Ref<NumLiteral>),
    Bool(Ref<BoolLiteral>),
    Variant(Ref<VariantLiteral>),
}

#[derive(Debug)]
pub struct ArrayAssign {
    pub elements: Ref<CommaList<ArrayAssignExpr>>,
}

#[derive(Debug)]
pub enum ArrayAssignExpr {
    Single(Ref<Expr>),
    Spread(Ref<SpreadAssignExpr>),
}

#[derive(Debug)]
pub struct SpreadAssignExpr {
    pub expr: Ref<Expr>,
}

#[derive(Debug)]
pub struct StructAssign {
    pub fields: Ref<CommaList<StructAssignField>>,
}

#[derive(Debug)]
pub struct StructAssignField {
    pub name: Ref<Name>,
    pub assign: Ref<Expr>,
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

#[derive(Debug)]
#[expect(unused)]
pub enum UnaryParenExprOp {
    // boolean
    Not(Ref<NotOp>),
}

#[derive(Debug, Clone, Copy)]
pub struct NotOp;

#[derive(Debug)]
pub struct BinaryParenExpr {
    pub op: Ref<BinaryParenExprOp>,
    pub left: Ref<Expr>,
    pub right: Ref<Expr>,
}

#[derive(Debug)]
#[expect(unused)]
pub enum BinaryParenExprOp {
    // math
    Add(Ref<AddOp>),
    Sub(Ref<SubOp>),
    Mul(Ref<MulOp>),
    Div(Ref<DivOp>),
    Mod(Ref<ModOp>),
    // inequality
    Eq(Ref<EqOp>),
    Neq(Ref<NeqOp>),
    Gt(Ref<GtOp>),
    Lt(Ref<LtOp>),
    Gte(Ref<GteOp>),
    Lte(Ref<LteOp>),
    // boolean
    And(Ref<AndOp>),
    Or(Ref<OrOp>),
    // string
    Join(Ref<JoinOp>),
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
pub struct Program {
    pub items: Ref<List<TopItem>>,
}

#[derive(Debug)]
pub struct Comment {
    #[expect(unused)]
    pub text: Arc<str>,
}

#[derive(Debug)]
pub struct Empty;

#[derive(Debug)]
pub enum CommaList<T> {
    // <item>,
    Link(Ref<CommaListLink<T>>),
    // <item>
    Tail(Ref<T>),
    //
    #[expect(unused)]
    Empty(Ref<Empty>),
}

#[derive(Debug)]
pub struct CommaListLink<T> {
    pub item: Ref<T>,
    pub next: Ref<CommaList<T>>,
}

#[derive(Debug)]
pub enum SemiList<T> {
    // <item>;
    Link(Ref<SemiListLink<T>>),
    // <item>
    Tail(Ref<T>),
    //
    #[expect(unused)]
    Empty(Ref<Empty>),
}

#[derive(Debug)]
pub struct SemiListLink<T> {
    pub item: Ref<T>,
    pub next: Ref<SemiList<T>>,
}

#[derive(Debug)]
pub enum List<T> {
    // <item>
    Link(Ref<ListLink<T>>),
    //
    #[expect(unused)]
    Empty(Ref<Empty>),
}

#[derive(Debug)]
pub struct ListLink<T> {
    pub item: Ref<T>,
    pub next: Ref<List<T>>,
}

#[derive(Debug)]
pub enum PipeList<T> {
    // <item> |
    Link(Ref<PipeListLink<T>>),
    // <item>
    Tail(Ref<T>),
    //
    #[expect(unused)]
    Empty(Ref<Empty>),
}

#[derive(Debug)]
pub struct PipeListLink<T> {
    pub item: Ref<T>,
    pub next: Ref<PipeList<T>>,
}

pub fn parse(mut tokens: TokenFeed) -> Result<Srced<Program>, CompileErrorSet> {
    parse_ast(&mut tokens)
}
