use std::sync::Arc;

use uuid::Uuid;

#[derive(Debug)]
pub struct Program {
    pub stages: Arc<Vec<Arc<Stage>>>,
    pub monitors: Arc<Vec<Arc<Monitor>>>,
}

#[derive(Debug)]
pub struct Monitor {
    pub uuid: Uuid,
    pub name: Arc<str>,
}

#[derive(Debug)]
pub struct Stage {
    pub lists: Arc<Vec<Arc<List>>>,
    pub broadcasts: Arc<Vec<Arc<Broadcast>>>,
    pub stacks: Arc<Vec<Arc<Stack>>>,
}

#[derive(Debug)]
pub struct List {
    pub uuid: Uuid,
    pub name: Arc<str>,
}

#[derive(Debug)]
pub struct Broadcast {
    pub uuid: Uuid,
    pub name: Arc<str>,
}

#[derive(Debug)]
pub struct Stack {
    pub root: Arc<Op>,
    pub rest: Arc<Vec<Arc<Op>>>,
}

#[derive(Debug)]
pub enum Op {
    Event(EventOp),
    Data(DataOp),
    Control(ControlOp),
    Sensing(SensingOp),
    Operator(OperatorOp),
}

#[derive(Debug)]
pub enum EventOp {
    WhenFlagClicked,
    WhenBroadcastReceived { broadcast: Arc<Broadcast> },
    BroadcastAndWait { input: Arc<Expr> },
    Broadcast { input: Arc<Expr> },
}

#[derive(Debug)]
pub enum DataOp {
    AddToList { list: Arc<List>, item: Arc<Expr> },
    DeleteAllOfList { list: Arc<List> },
    DeleteOfList { list: Arc<List>, index: Arc<Expr> },
    ReplaceItemOfList { list: Arc<List>, index: Arc<Expr>, item: Arc<Expr> },
    LengthOfList { list: Arc<List> },
    ItemOfList { list: Arc<List>, index: Arc<Expr> },
}

#[derive(Debug)]
pub enum ControlOp {
    If { condition: Arc<Expr>, then_substack: Arc<Expr> },
    IfElse { condition: Arc<Expr>, then_substack: Arc<Expr>, else_substack: Arc<Expr> },
    Wait { duration: Arc<Expr> },
    Repeat { times: Arc<Expr>, looped_substack: Arc<Expr> },
}

#[derive(Debug)]
pub enum SensingOp {
    AskAndWait { question: Arc<Expr> },
    Answer,
    Timer,
}

#[derive(Debug)]
pub enum OperatorOp {
    Subtract { num_a: Arc<Expr>, num_b: Arc<Expr> },
    Mod { num_a: Arc<Expr>, num_b: Arc<Expr> },
    Add { num_a: Arc<Expr>, num_b: Arc<Expr> },
    Multiply { num_a: Arc<Expr>, num_b: Arc<Expr> },
    Divide { num_a: Arc<Expr>, num_b: Arc<Expr> },
    Join { string_a: Arc<Expr>, string_b: Arc<Expr> },
    Random { from: Arc<Expr>, to: Arc<Expr> },
    Equals { operand_a: Arc<Expr>, operand_b: Arc<Expr> },
    Not { operand: Arc<Expr> },
}

#[derive(Debug)]
pub enum Expr {
    Literal(Arc<Literal>),
    Derived(Arc<Op>),
    Broadcast(Arc<Broadcast>),
    Stack(Arc<Stack>),
}

#[derive(Debug)]
pub enum Literal {
    Num(f64),
    PosNum(f64),
    PosInt(u32),
    Int(i32),
    Angle(f64),
    Color { r: u8, g: u8, b: u8 },
    String(Arc<str>),
}
