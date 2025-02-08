use std::sync::Arc;

use itertools::Itertools;
use uuid::Uuid;

use crate::ir;

use super::{
    Broadcast, ControlOp, DataOp, EventOp, Expr, List, Literal, Monitor, Op, OperatorOp, Program,
    SensingOp, Stack, Stage,
};

impl Program {
    pub fn compile(&self) -> ir::Program {
        ir::Program {
            stages: Arc::new(self.stages.iter().map(|x| Arc::new(x.compile())).collect_vec()),
            monitors: Arc::new(self.monitors.iter().map(|x| Arc::new(x.compile())).collect_vec()),
        }
    }
}

impl Monitor {
    pub fn compile(&self) -> ir::Monitor {
        ir::Monitor { uuid: self.uuid, name: Arc::clone(&self.name) }
    }
}

impl Stage {
    pub fn compile(&self) -> ir::Stage {
        let ir_blocks =
            self.stacks.iter().flat_map(|stack| stack.compile(None).ir_all_blocks).collect_vec();

        ir::Stage {
            lists: Arc::new(self.lists.iter().map(|x| Arc::new(x.compile())).collect_vec()),
            broadcasts: Arc::new(
                self.broadcasts.iter().map(|x| Arc::new(x.compile())).collect_vec(),
            ),
            blocks: Arc::new(ir_blocks),
        }
    }
}

impl List {
    pub fn compile(&self) -> ir::List {
        ir::List { uuid: self.uuid, name: Arc::clone(&self.name) }
    }
}

impl Broadcast {
    pub fn compile(&self) -> ir::Broadcast {
        ir::Broadcast { uuid: self.uuid, name: Arc::clone(&self.name) }
    }
}

pub struct StackCompiled {
    ir_root_block: Arc<ir::Block>,
    ir_all_blocks: Vec<Arc<ir::Block>>,
}

impl Stack {
    pub fn compile(&self, parent: Option<Uuid>) -> StackCompiled {
        let mut uuid_prev = parent;
        let mut uuid_curr = Uuid::new_v4();
        let mut uuid_next = Uuid::new_v4();

        let mut ir_all_blocks = Vec::<Arc<ir::Block>>::new();

        let mut compile_op = |op_curr: &Op, op_next: Option<&Op>| {
            let dependent_ir_op = op_curr.compile(uuid_curr);

            let ir_block = Arc::new(ir::Block {
                uuid: uuid_curr,
                parent: uuid_prev,
                next: op_next.map(|_| uuid_next),
                op: dependent_ir_op.ir,
            });

            uuid_prev = Some(uuid_curr);
            uuid_curr = uuid_next;
            uuid_next = Uuid::new_v4();

            ir_all_blocks.push(Arc::clone(&ir_block));
            ir_all_blocks.extend(dependent_ir_op.deps.iter().cloned());

            ir_block
        };

        let mut rest = self.rest.iter().peekable();
        let ir_root_block = compile_op(&self.root, rest.peek().map(AsRef::as_ref));

        while let Some(op) = rest.next() {
            compile_op(op, rest.peek().map(AsRef::as_ref));
        }

        StackCompiled { ir_root_block, ir_all_blocks }
    }
}

impl Op {
    pub fn compile(&self, parent: Uuid) -> Dependent<ir::Op> {
        let mut deps = Vec::new();
        let mut compile = |expr: &Expr| compile_expr(expr, parent, &mut deps);

        let ir_op = match self {
            Self::Event(op) => {
                let ir_op = match op {
                    EventOp::WhenFlagClicked => ir::EventOp::WhenFlagClicked,
                    EventOp::WhenBroadcastReceived { broadcast } => {
                        ir::EventOp::WhenBroadcastReceived {
                            broadcast: Arc::new(broadcast.compile()),
                        }
                    },
                    EventOp::BroadcastAndWait { input } => {
                        ir::EventOp::BroadcastAndWait { input: compile(input) }
                    },
                    EventOp::Broadcast { input } => {
                        ir::EventOp::Broadcast { input: compile(input) }
                    },
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
                    DataOp::DeleteAllOfList { list } => {
                        ir::DataOp::DeleteAllOfList { list: Arc::new(list.compile()) }
                    },
                    DataOp::DeleteOfList { list, index } => ir::DataOp::DeleteOfList {
                        list: Arc::new(list.compile()),
                        index: compile(index),
                    },
                    DataOp::LengthOfList { list } => {
                        ir::DataOp::LengthOfList { list: Arc::new(list.compile()) }
                    },
                    DataOp::ReplaceItemOfList { list, index, item } => {
                        ir::DataOp::ReplaceItemOfList {
                            list: Arc::new(list.compile()),
                            index: compile(index),
                            item: compile(item),
                        }
                    },
                };

                ir::Op::Data(ir_op)
            },
            Self::Operator(op) => {
                let ir_op = match op {
                    OperatorOp::Add { num_a, num_b } => {
                        ir::OperatorOp::Add { num_a: compile(num_a), num_b: compile(num_b) }
                    },
                    OperatorOp::Divide { num_a, num_b } => {
                        ir::OperatorOp::Divide { num_a: compile(num_a), num_b: compile(num_b) }
                    },
                    OperatorOp::Join { string_a, string_b } => ir::OperatorOp::Join {
                        string_a: compile(string_a),
                        string_b: compile(string_b),
                    },
                    OperatorOp::Mod { num_a, num_b } => {
                        ir::OperatorOp::Mod { num_a: compile(num_a), num_b: compile(num_b) }
                    },
                    OperatorOp::Multiply { num_a, num_b } => {
                        ir::OperatorOp::Multiply { num_a: compile(num_a), num_b: compile(num_b) }
                    },
                    OperatorOp::Random { from, to } => {
                        ir::OperatorOp::Random { from: compile(from), to: compile(to) }
                    },
                    OperatorOp::Subtract { num_a, num_b } => {
                        ir::OperatorOp::Subtract { num_a: compile(num_a), num_b: compile(num_b) }
                    },
                    OperatorOp::Equals { operand_a, operand_b } => ir::OperatorOp::Equals {
                        operand_a: compile(operand_a),
                        operand_b: compile(operand_b),
                    },
                    OperatorOp::Not { operand } => {
                        ir::OperatorOp::Not { operand: compile(operand) }
                    },
                };

                ir::Op::Operator(ir_op)
            },
            Op::Control(op) => {
                let ir_op = match op {
                    ControlOp::If { condition, then_substack } => ir::ControlOp::If {
                        condition: compile(condition),
                        then_substack: compile(then_substack),
                    },
                    ControlOp::IfElse { condition, then_substack, else_substack } => {
                        ir::ControlOp::IfElse {
                            condition: compile(condition),
                            then_substack: compile(then_substack),
                            else_substack: compile(else_substack),
                        }
                    },
                    ControlOp::Wait { duration } => {
                        ir::ControlOp::Wait { duration: compile(duration) }
                    },
                    ControlOp::Repeat { times, looped_substack } => ir::ControlOp::Repeat {
                        times: compile(times),
                        looped_substack: compile(looped_substack),
                    },
                };

                ir::Op::Control(ir_op)
            },
            Op::Sensing(op) => {
                let ir_op = match op {
                    SensingOp::Answer => ir::SensingOp::Answer,
                    SensingOp::AskAndWait { question } => {
                        ir::SensingOp::AskAndWait { question: compile(question) }
                    },
                    SensingOp::Timer => ir::SensingOp::Timer,
                };

                ir::Op::Sensing(ir_op)
            },
        };

        Dependent { ir: ir_op, deps }
    }
}

impl Expr {
    pub fn num(n: f64) -> Arc<Expr> {
        Arc::new(Expr::Literal(Arc::new(Literal::Num(n))))
    }

    pub fn compile(&self, parent: Uuid) -> Dependent<ir::Expr> {
        let mut deps = Vec::new();

        let ir_expr = match self {
            Expr::Literal(l) => ir::Expr::Literal(Arc::new(l.compile())),
            Expr::Broadcast(b) => ir::Expr::Broadcast(Arc::new(b.compile())),
            Expr::Stack(s) => {
                let stack_compiled = s.compile(Some(parent));
                deps.extend(stack_compiled.ir_all_blocks);
                ir::Expr::Stack(stack_compiled.ir_root_block)
            },
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

impl Literal {
    pub fn compile(&self) -> ir::Literal {
        match self {
            Self::Num(n) => ir::Literal::Num(*n),
            Self::PosNum(n) => ir::Literal::PosNum(*n),
            Self::PosInt(i) => ir::Literal::PosInt(*i),
            Self::Int(i) => ir::Literal::Int(*i),
            Self::Angle(a) => ir::Literal::Angle(*a),
            Self::Color { r, g, b } => ir::Literal::Color { r: *r, g: *g, b: *b },
            Self::String(s) => ir::Literal::String(Arc::clone(s)),
        }
    }
}

fn compile_expr(expr: &Expr, parent: Uuid, deps: &mut Vec<Arc<ir::Block>>) -> Arc<ir::Expr> {
    let dependent = expr.compile(parent);
    deps.extend(dependent.deps);
    Arc::new(dependent.ir)
}

pub struct Dependent<Ir> {
    ir: Ir,
    deps: Vec<Arc<ir::Block>>,
}
