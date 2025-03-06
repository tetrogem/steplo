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
}

#[derive(Debug)]
pub enum Value {
    Literal(Arc<str>),
    Label(Uuid),
}

#[derive(Debug)]
pub struct SetArgs<MemLoc> {
    pub dest: Arc<MemLoc>,
    pub value: Arc<Value>,
}

#[derive(Debug)]
pub struct UnaryArgs<MemLoc> {
    pub dest: Arc<MemLoc>,
    pub src: Arc<MemLoc>,
}

#[derive(Debug)]
pub struct BinaryArgs<MemLoc> {
    pub dest: Arc<MemLoc>,
    pub left: Arc<MemLoc>,
    pub right: Arc<MemLoc>,
}

#[derive(Debug)]
pub struct VoidArgs<MemLoc> {
    pub src: Arc<MemLoc>,
}

#[derive(Debug)]
pub struct InputArgs<MemLoc> {
    pub dest: Arc<MemLoc>,
}

#[derive(Debug)]
pub struct CondArgs<MemLoc> {
    pub cond: Arc<MemLoc>,
    pub src: Arc<MemLoc>,
}

#[derive(Debug)]
pub enum Command<MemLoc> {
    Set(Arc<SetArgs<MemLoc>>),
    Copy(Arc<UnaryArgs<MemLoc>>),
    CopyDerefDest(Arc<UnaryArgs<MemLoc>>),
    Deref(Arc<UnaryArgs<MemLoc>>),
    Add(Arc<BinaryArgs<MemLoc>>),
    Sub(Arc<BinaryArgs<MemLoc>>),
    Mul(Arc<BinaryArgs<MemLoc>>),
    Div(Arc<BinaryArgs<MemLoc>>),
    Mod(Arc<BinaryArgs<MemLoc>>),
    Jump(Arc<VoidArgs<MemLoc>>),
    Out(Arc<VoidArgs<MemLoc>>),
    In(Arc<InputArgs<MemLoc>>),
    Exit,
    Branch(Arc<CondArgs<MemLoc>>),
    Eq(Arc<BinaryArgs<MemLoc>>),
    Lte(Arc<BinaryArgs<MemLoc>>),
    Neq(Arc<BinaryArgs<MemLoc>>),
    Not(Arc<UnaryArgs<MemLoc>>),
}

#[derive(Debug)]
pub enum UMemLoc {
    StackPointer,
    Temp(Arc<TempVar>),
    // Stack { var: Arc<StackVar>, offset: Option<Arc<UMemLoc>> },
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TempVar {
    uuid: Uuid,
}

impl TempVar {
    pub fn new() -> Self {
        Self { uuid: Uuid::new_v4() }
    }
}
