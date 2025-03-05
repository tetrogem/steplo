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
pub enum Command<MemLoc> {
    Set { dest: Arc<MemLoc>, value: Arc<Value> },
    Copy { dest: Arc<MemLoc>, src: Arc<MemLoc> },
    CopyDerefDest { dest: Arc<MemLoc>, src: Arc<MemLoc> },
    Deref { dest: Arc<MemLoc>, src: Arc<MemLoc> },
    Add { dest: Arc<MemLoc>, left: Arc<MemLoc>, right: Arc<MemLoc> },
    Sub { dest: Arc<MemLoc>, left: Arc<MemLoc>, right: Arc<MemLoc> },
    Jump { src: Arc<MemLoc> },
    Out { src: Arc<MemLoc> },
    In { dest: Arc<MemLoc> },
    Exit,
    Branch { cond: Arc<MemLoc>, label: Arc<MemLoc> },
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
