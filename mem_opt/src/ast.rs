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
    pub assignments: Arc<Vec<Arc<Assignment<MemLoc>>>>,
    pub next_call: Arc<Call<MemLoc>>,
}

#[derive(Debug)]
pub struct StackVar {
    uuid: Uuid,
}

impl StackVar {
    pub fn new() -> Self {
        Self { uuid: Uuid::new_v4() }
    }
}

#[derive(Debug)]
pub enum Call<MemLoc> {
    Func { name: Arc<str>, params: Arc<Vec<Arc<MemLoc>>>, return_sub_proc: Uuid },
    SubProc(Uuid),
    IfBranch { cond: Arc<MemLoc>, then_sub_proc: Uuid, pop_sub_proc: Uuid },
    IfElseBranch { cond: Arc<MemLoc>, then_sub_proc: Uuid, else_sub_proc: Uuid },
    Return,
    Terminate,
}

#[derive(Debug)]
pub struct Literal {
    pub value: Arc<str>,
}

#[derive(Debug)]
pub struct Assignment<MemLoc> {
    pub dest: Arc<MemLoc>,
    pub expr: Arc<Expr<MemLoc>>,
}

#[derive(Debug)]
pub enum Expr<MemLoc> {
    Set { literal: Arc<Literal> },
    Copy { src: Arc<MemLoc> },
    CopyDerefDest { src: Arc<MemLoc> },
    Deref { src: Arc<MemLoc> },
    Add { left: Arc<MemLoc>, right: Arc<MemLoc> },
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
