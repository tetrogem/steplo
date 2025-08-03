use std::sync::Arc;

use uuid::Uuid;

#[derive(Debug)]
pub struct Proc<MemLoc> {
    pub kind: Arc<ProcKind>,
    pub sub_procs: Arc<Vec<Arc<SubProc<MemLoc>>>>,
}

#[derive(Debug)]
pub enum ProcKind {
    Main,
    Func { name: Arc<str> },
}

#[derive(Debug)]
pub struct SubProc<MemLoc> {
    pub uuid: Uuid,
    pub commands: Arc<Vec<Arc<Command<MemLoc>>>>,
    pub call: Arc<Call<MemLoc>>,
}

#[derive(Debug)]
pub enum Value {
    Literal(Arc<str>),
    Label(Uuid),
}

#[derive(Debug)]
pub struct BinaryArgs<MemLoc> {
    pub left: Arc<Expr<MemLoc>>,
    pub right: Arc<Expr<MemLoc>>,
}

#[derive(Debug)]
pub enum Expr<MemLoc> {
    MemLoc(Arc<MemLoc>),
    Value(Arc<Value>),
    StackDeref(Arc<Expr<MemLoc>>),
    StdoutDeref(Arc<Expr<MemLoc>>),
    StdoutLen,
    Timer,
    Add(Arc<BinaryArgs<MemLoc>>),
    Sub(Arc<BinaryArgs<MemLoc>>),
    Mul(Arc<BinaryArgs<MemLoc>>),
    Div(Arc<BinaryArgs<MemLoc>>),
    Mod(Arc<BinaryArgs<MemLoc>>),
    Eq(Arc<BinaryArgs<MemLoc>>),
    Lt(Arc<BinaryArgs<MemLoc>>),
    Gt(Arc<BinaryArgs<MemLoc>>),
    Not(Arc<Expr<MemLoc>>),
    Or(Arc<BinaryArgs<MemLoc>>),
    And(Arc<BinaryArgs<MemLoc>>),
    InAnswer,
    Join(Arc<BinaryArgs<MemLoc>>),
    Random(Arc<BinaryArgs<MemLoc>>),
}

#[derive(Debug)]
pub enum Command<MemLoc> {
    SetMemLoc { mem_loc: Arc<MemLoc>, val: Arc<Expr<MemLoc>> },
    SetStack { addr: Arc<Expr<MemLoc>>, val: Arc<Expr<MemLoc>> },
    In,
    Out(Arc<Expr<MemLoc>>),
    ClearStdout,
    WriteStdout { index: Arc<Expr<MemLoc>>, val: Arc<Expr<MemLoc>> },
    Wait { duration_s: Arc<Expr<MemLoc>> },
}

#[derive(Debug)]
pub enum Call<MemLoc> {
    Exit,
    Jump(Arc<Expr<MemLoc>>),
    Branch { cond: Arc<Expr<MemLoc>>, then_to: Arc<Expr<MemLoc>>, else_to: Arc<Expr<MemLoc>> },
}

#[derive(Debug)]
pub enum UMemLoc {
    StackPointer,
    Temp(Arc<TempVar>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TempVar {
    pub uuid: Uuid,
}

impl TempVar {
    pub fn new() -> Self {
        Self { uuid: Uuid::new_v4() }
    }
}
