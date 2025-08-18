use std::sync::Arc;

use uuid::Uuid;

#[derive(Debug)]
pub struct Proc {
    pub kind: Arc<ProcKind>,
    pub sub_procs: Arc<Vec<Arc<SubProc>>>,
    pub ordered_arg_infos: Arc<Vec<VarInfo>>,
    pub ordered_local_infos: Arc<Vec<VarInfo>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarInfo {
    pub uuid: Uuid,
    pub size: u32,
}

impl VarInfo {
    pub fn new(size: u32) -> Self {
        Self { uuid: Uuid::new_v4(), size }
    }
}

#[derive(Debug)]
pub enum ProcKind {
    Main,
    Func { name: Arc<str> },
}

#[derive(Debug)]
pub struct SubProc {
    pub uuid: Uuid,
    pub commands: Arc<Vec<Arc<Command>>>,
    pub call: Arc<Call>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Literal(Arc<str>),
    Label(Uuid),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BinaryArgs {
    pub left: Arc<Expr>,
    pub right: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    Loc(Arc<Loc>),
    StackAddr(Arc<StackAddr>),
    Value(Arc<Value>),
    StdoutDeref(Arc<Expr>),
    StdoutLen,
    Timer,
    Add(Arc<BinaryArgs>),
    Sub(Arc<BinaryArgs>),
    Mul(Arc<BinaryArgs>),
    Div(Arc<BinaryArgs>),
    Mod(Arc<BinaryArgs>),
    Eq(Arc<BinaryArgs>),
    Lt(Arc<BinaryArgs>),
    Gt(Arc<BinaryArgs>),
    Not(Arc<Expr>),
    Or(Arc<BinaryArgs>),
    And(Arc<BinaryArgs>),
    InAnswer,
    Join(Arc<BinaryArgs>),
    Random(Arc<BinaryArgs>),
}

#[derive(Debug)]
pub enum Command {
    SetLoc { loc: Arc<Loc>, val: Arc<Expr> },
    In,
    Out(Arc<Expr>),
    ClearStdout,
    WriteStdout { index: Arc<Expr>, val: Arc<Expr> },
}

#[derive(Debug)]
pub enum Call {
    Jump { to: Arc<Expr> },
    Branch { cond: Arc<Expr>, then_to: Arc<Expr>, else_to: Arc<Expr> },
    Sleep { duration_s: Arc<Expr>, to: Arc<Expr> },
    Func { to_func_name: Arc<str>, arg_assignments: Arc<Vec<ArgAssignment>> },
    Return { to: Arc<Expr> },
    Exit,
}

#[derive(Debug)]
pub struct ArgAssignment {
    pub arg_uuid: Uuid,
    pub arg_offset: u32,
    pub expr: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Loc {
    Temp(Arc<TempVar>),
    Deref(Arc<Expr>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum StackAddr {
    Arg { uuid: Uuid },
    Local { uuid: Uuid },
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
