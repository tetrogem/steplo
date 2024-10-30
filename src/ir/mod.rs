pub mod compile;

use std::sync::Arc;

use uuid::Uuid;

pub struct Program {
    pub stages: Arc<Vec<Arc<Stage>>>,
    pub monitors: Arc<Vec<Arc<Monitor>>>,
}

pub struct Monitor {
    pub uuid: Uuid,
    pub name: Arc<str>,
}

pub struct Stage {
    pub lists: Arc<Vec<Arc<List>>>,
    pub broadcasts: Arc<Vec<Arc<Broadcast>>>,
    pub blocks: Arc<Vec<Arc<Block>>>,
}

pub struct List {
    pub uuid: Uuid,
    pub name: Arc<str>,
}

pub struct Broadcast {
    pub uuid: Uuid,
    pub name: Arc<str>,
}

pub struct Block {
    pub uuid: Uuid,
    pub op: Op,
    pub next: Option<Arc<Block>>,
    pub parent: Option<Arc<Block>>,
}

pub enum Op {
    Event(EventOp),
    Data(DataOp),
    Control(ControlOp),
    Sensing(SensingOp),
}

pub enum EventOp {
    WhenFlagClicked,
    WhenBroadcastReceived { broadcast: Arc<Broadcast> },
    BroadcastAndWait { input: Arc<Expr> },
    Broadcast { input: Arc<Expr> },
}

#[expect(clippy::enum_variant_names, reason = "this may not be a problem forever")]
pub enum DataOp {
    AddToList { list: Arc<List>, item: Arc<Expr> },
    DeleteAllOfList { list: Arc<List> },
    DeleteOfList { list: Arc<List>, index: Arc<Expr> },
    ReplaceItemOfList { list: Arc<List>, index: Arc<Expr>, item: Arc<Expr> },
}

pub enum ControlOp {
    If { condition: Arc<Expr>, then_substack: Arc<Expr> },
    IfElse { condition: Arc<Expr>, then_substack: Arc<Expr>, else_substack: Arc<Expr> },
    Wait { duration: Arc<Expr> },
}

pub enum SensingOp {
    AskAndWait { question: Arc<Expr> },
}

pub enum Literal {
    Num(f64),
    PosNum(f64),
    PosInt(u32),
    Int(i32),
    Angle(f64),
    Color { r: u8, g: u8, b: u8 },
    String(Arc<str>),
}

pub struct Stack {
    root: Arc<Block>,
    chain: Arc<Vec<Arc<Block>>>,
}

pub enum Expr {
    Literal(Arc<Literal>),
    Derived(Arc<Derived>),
    Broadcast(Arc<Broadcast>),
    Stack(Arc<Stack>),
}

pub enum Derived {
    Data(Arc<DataDerived>),
    Op(Arc<OpDerived>),
    Sensing(Arc<SensingDerived>),
}

pub enum DataDerived {
    LengthOfList { list: Arc<List> },
    ItemOfList { list: Arc<List>, index: Arc<Expr> },
}

pub enum OpDerived {
    Subtract { num_a: Arc<Expr>, num_b: Arc<Expr> },
    Mod { num_a: Arc<Expr>, num_b: Arc<Expr> },
    Add { num_a: Arc<Expr>, num_b: Arc<Expr> },
    Multiply { num_a: Arc<Expr>, num_b: Arc<Expr> },
    Divide { num_a: Arc<Expr>, num_b: Arc<Expr> },
    Join { string_a: Arc<Expr>, string_b: Arc<Expr> },
    Random { from: Arc<Expr>, to: Arc<Expr> },
}

pub enum SensingDerived {
    Answer,
    Timer,
}
