use std::sync::Arc;

use uuid::Uuid;

#[derive(Debug)]
pub struct Program {
    pub stages: Arc<Vec<Arc<Stage>>>,
    pub monitors: Arc<Vec<Arc<Monitor>>>,
}

#[derive(Debug)]
pub struct Monitor {
    pub list: Arc<List>,
    pub width: f64,
    pub height: f64,
    pub x: f64,
    pub y: f64,
    pub visible: bool,
}

#[derive(Debug)]
pub struct Stage {
    pub variables: Arc<Vec<Arc<Variable>>>,
    pub lists: Arc<Vec<Arc<List>>>,
    pub broadcasts: Arc<Vec<Arc<Broadcast>>>,
    pub stacks: Arc<Vec<Arc<Stack>>>,
}

#[derive(Debug)]
pub struct Variable {
    pub uuid: Uuid,
    pub name: Arc<str>,
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
    Procedure(ProcedureOp),
    Argument(ArgumentOp),
}

#[derive(Debug)]
pub enum EventOp {
    WhenFlagClicked,
    WhenBroadcastReceived { broadcast: Arc<Broadcast> },
    BroadcastAndWait { input: Arc<Expr> },
    Broadcast { input: Arc<Expr> },
    WhenKeyPressed { key_option: KeyOption },
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum KeyOption {
    Space,
    UpArrow,
    DownArrow,
    RightArrow,
    LeftArrow,
    Any,
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
    P,
    Q,
    R,
    S,
    T,
    U,
    V,
    W,
    X,
    Y,
    Z,
    Num0,
    Num1,
    Num2,
    Num3,
    Num4,
    Num5,
    Num6,
    Num7,
    Num8,
    Num9,
}

#[derive(Debug)]
pub enum DataOp {
    AddToList { list: Arc<List>, item: Arc<Expr> },
    DeleteAllOfList { list: Arc<List> },
    DeleteOfList { list: Arc<List>, index: Arc<Expr> },
    ReplaceItemOfList { list: Arc<List>, index: Arc<Expr>, item: Arc<Expr> },
    LengthOfList { list: Arc<List> },
    ItemOfList { list: Arc<List>, index: Arc<Expr> },
    SetVariableTo { variable: Arc<Variable>, value: Arc<Expr> },
}

#[derive(Debug)]
pub enum ControlOp {
    If {
        condition: Arc<Expr>,
        then_substack: Option<Arc<Expr>>,
    },
    IfElse {
        condition: Arc<Expr>,
        then_substack: Option<Arc<Expr>>,
        else_substack: Option<Arc<Expr>>,
    },
    Wait {
        duration_s: Arc<Expr>,
    },
    Repeat {
        times: Arc<Expr>,
        looped_substack: Option<Arc<Expr>>,
    },
    RepeatUntil {
        condition: Arc<Expr>,
        then_substack: Option<Arc<Expr>>,
    },
}

#[derive(Debug)]
pub enum SensingOp {
    AskAndWait { question: Arc<Expr> },
    Answer,
    Timer,
    DaysSince2000,
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
    GreaterThan { operand_a: Arc<Expr>, operand_b: Arc<Expr> },
    LessThan { operand_a: Arc<Expr>, operand_b: Arc<Expr> },
    And { operand_a: Arc<Expr>, operand_b: Arc<Expr> },
    Or { operand_a: Arc<Expr>, operand_b: Arc<Expr> },
}

#[derive(Debug)]
pub enum ProcedureOp {
    Definition {
        prototype_stack: Arc<Expr>,
    },
    Prototype {
        custom_block: Arc<CustomBlock>,
        arguments_with_stacks: Arc<Vec<(Arc<Argument>, Arc<Expr>)>>,
    },
    Call {
        custom_block: Arc<CustomBlock>,
        argument_inputs: Arc<Vec<(Arc<Argument>, Arc<Expr>)>>,
    },
}

#[derive(Debug)]
pub enum ArgumentOp {
    ReporterStringNumber { arg: Arc<Argument> },
}

#[derive(Debug)]
pub enum Expr {
    Literal(Arc<Literal>),
    Derived(Arc<Op>),
    Variable(Arc<Variable>),
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

#[derive(Debug)]
pub struct CustomBlock {
    pub name: Arc<str>,
}

#[derive(Debug)]
pub struct Argument {
    pub uuid: Uuid,
    pub name: Arc<str>,
    pub default: Arc<str>,
}
