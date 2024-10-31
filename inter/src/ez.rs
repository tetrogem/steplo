use std::sync::Arc;

use itertools::Itertools;
use uuid::Uuid;

use crate::ir;

pub struct Program {
    pub stages: Arc<Vec<Arc<Stage>>>,
    pub monitors: Arc<Vec<Arc<Monitor>>>,
}

impl Program {
    pub fn compile(&self) -> ir::Program {
        ir::Program {
            stages: Arc::new(self.stages.iter().map(|x| Arc::new(x.compile())).collect_vec()),
            monitors: Arc::new(self.monitors.iter().map(|x| Arc::new(x.compile())).collect_vec()),
        }
    }
}

pub struct Monitor {
    pub uuid: Uuid,
    pub name: Arc<str>,
}

impl Monitor {
    pub fn compile(&self) -> ir::Monitor {
        ir::Monitor { uuid: self.uuid, name: Arc::clone(&self.name) }
    }
}

pub struct Stage {
    pub lists: Arc<Vec<Arc<List>>>,
    pub broadcasts: Arc<Vec<Arc<Broadcast>>>,
    pub stack: Arc<Stack>,
}

impl Stage {
    pub fn compile(&self) -> ir::Stage {
        ir::Stage {
            lists: Arc::new(self.lists.iter().map(|x| Arc::new(x.compile())).collect_vec()),
            broadcasts: Arc::new(
                self.broadcasts.iter().map(|x| Arc::new(x.compile())).collect_vec(),
            ),
            blocks: Arc::new(self.stack.compile()),
        }
    }
}

pub struct List {
    pub uuid: Uuid,
    pub name: Arc<str>,
}

impl List {
    pub fn compile(&self) -> ir::List {
        ir::List { uuid: self.uuid, name: Arc::clone(&self.name) }
    }
}

pub struct Broadcast {
    pub uuid: Uuid,
    pub name: Arc<str>,
}

impl Broadcast {
    pub fn compile(&self) -> ir::Broadcast {
        ir::Broadcast { uuid: self.uuid, name: Arc::clone(&self.name) }
    }
}

pub struct Stack {
    pub ops: Arc<Vec<Arc<Op>>>,
}

impl Stack {
    pub fn compile(&self) -> Vec<Arc<ir::Block>> {
        let mut uuid_prev = None;
        let mut uuid_curr = Uuid::new_v4();
        let mut uuid_next = Uuid::new_v4();

        let mut ir_blocks = Vec::<Arc<ir::Block>>::new();

        let mut ops = self.ops.iter().peekable();
        while let Some(op) = ops.next() {
            let dependent_ir_op = op.compile(uuid_curr);

            let ir_block = ir::Block {
                uuid: uuid_curr,
                parent: uuid_prev,
                next: ops.peek().map(|_| uuid_next),
                op: dependent_ir_op.ir,
            };

            uuid_prev = Some(uuid_curr);
            uuid_curr = uuid_next;
            uuid_next = Uuid::new_v4();

            ir_blocks.push(Arc::new(ir_block));
            ir_blocks.extend(dependent_ir_op.deps);
        }

        ir_blocks
    }
}

pub enum Op {
    Event(EventOp),
    Data(DataOp),
    Operator(OperatorOp),
}

impl Op {
    pub fn compile(&self, parent: Uuid) -> Dependent<ir::Op> {
        let mut deps = Vec::new();
        let mut compile = |expr: &Expr| compile_expr(expr, parent, &mut deps);

        let ir_op = match self {
            Self::Event(op) => {
                let ir_op = match op {
                    EventOp::WhenFlagClicked => ir::EventOp::WhenFlagClicked,
                };

                ir::Op::Event(ir_op)
            },
            Self::Data(op) => {
                let ir_op = match op {
                    DataOp::AddToList { list, item } => ir::DataOp::AddToList {
                        list: Arc::new(list.compile()),
                        item: compile(item),
                    },
                    DataOp::ItemOfList { list, index } => ir::DataOp::ItemOfList {
                        list: Arc::new(list.compile()),
                        index: compile(index),
                    },
                };

                ir::Op::Data(ir_op)
            },
            Self::Operator(op) => {
                let ir_op = match op {
                    OperatorOp::Add { num_a, num_b } => {
                        ir::OperatorOp::Add { num_a: compile(num_a), num_b: compile(num_b) }
                    },
                };

                ir::Op::Operator(ir_op)
            },
        };

        Dependent { ir: ir_op, deps }
    }
}

pub enum EventOp {
    WhenFlagClicked,
}

pub enum DataOp {
    AddToList { list: Arc<List>, item: Arc<Expr> },
    ItemOfList { list: Arc<List>, index: Arc<Expr> },
}

pub enum OperatorOp {
    Add { num_a: Arc<Expr>, num_b: Arc<Expr> },
}

pub enum Expr {
    Literal(Arc<Literal>),
    Derived(Arc<Op>),
}

pub struct Dependent<Ir> {
    ir: Ir,
    deps: Vec<Arc<ir::Block>>,
}

impl Expr {
    pub fn num(n: f64) -> Arc<Expr> {
        Arc::new(Expr::Literal(Arc::new(Literal::Num(n))))
    }

    pub fn derived(d: &Arc<Op>) -> Arc<Expr> {
        Arc::new(Expr::Derived(Arc::clone(d)))
    }

    pub fn compile(&self, parent: Uuid) -> Dependent<ir::Expr> {
        let mut deps = Vec::new();

        let ir_expr = match self {
            Expr::Literal(l) => ir::Expr::Literal(Arc::new(l.compile())),
            Expr::Derived(d) => {
                let uuid = Uuid::new_v4();
                let dependent_ir_op = d.compile(uuid);
                let ir_block = Arc::new(ir::Block {
                    op: dependent_ir_op.ir,
                    uuid,
                    parent: Some(parent),
                    next: None,
                });

                deps.extend(dependent_ir_op.deps);
                deps.push(Arc::clone(&ir_block));

                ir::Expr::Derived(ir_block)
            },
        };

        Dependent { ir: ir_expr, deps }
    }
}

pub enum Literal {
    Num(f64),
    PosInt(u32),
    Int(i32),
}

impl Literal {
    pub fn compile(&self) -> ir::Literal {
        match self {
            Self::Num(n) => ir::Literal::Num(*n),
            Self::PosInt(i) => ir::Literal::PosInt(*i),
            Self::Int(i) => ir::Literal::Int(*i),
        }
    }
}

fn compile_expr(expr: &Expr, parent: Uuid, deps: &mut Vec<Arc<ir::Block>>) -> Arc<ir::Expr> {
    let dependent = expr.compile(parent);
    deps.extend(dependent.deps);
    Arc::new(dependent.ir)
}
