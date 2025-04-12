use itertools::{chain, Itertools};
use std::{collections::BTreeMap, sync::Arc};

use inter::ez;
use uuid::Uuid;

use crate::{
    ast::{Call, Command, Expr, Proc, ProcKind, Value},
    designate::{RMemLoc, Register},
};

pub fn compile<'a>(procs: impl Iterator<Item = &'a Proc<RMemLoc>>) -> ez::Program {
    let procs = procs.collect_vec();

    let mut sp_uuid_to_broadcast = BTreeMap::new();
    for proc in procs.iter() {
        let proc_name = match proc.kind.as_ref() {
            ProcKind::Main => "main".into(),
            ProcKind::Func { name } => format!("func_{}", name),
        };

        for (i, sp) in proc.sub_procs.iter().enumerate() {
            sp_uuid_to_broadcast.insert(
                sp.uuid,
                Arc::new(ez::Broadcast {
                    uuid: Uuid::new_v4(),
                    name: format!("{}.{}", proc_name, i).into(),
                }),
            );
        }
    }

    let mut compile_m = CompileManager {
        sp_uuid_to_broadcast,
        stack_pointer_variable: Arc::new(ez::Variable { uuid: Uuid::new_v4(), name: "sp".into() }),
        register_to_variable: Default::default(),
        stack_list: Arc::new(ez::List { uuid: Uuid::new_v4(), name: "stack".into() }),
        stdout_list: Arc::new(ez::List { uuid: Uuid::new_v4(), name: "stdout".into() }),
    };

    let stacks =
        procs.iter().flat_map(|proc| compile_proc(&mut compile_m, proc)).map(Arc::new).collect();

    let stage = ez::Stage {
        variables: Arc::new(compile_m.variables()),
        broadcasts: Arc::new(compile_m.broadcasts()),
        lists: Arc::new(compile_m.lists()),
        stacks: Arc::new(stacks),
    };

    let stages = Vec::from([Arc::new(stage)]);
    ez::Program { monitors: Arc::new(Vec::new()), stages: Arc::new(stages) }
}

struct CompileManager {
    sp_uuid_to_broadcast: BTreeMap<Uuid, Arc<ez::Broadcast>>,
    stack_pointer_variable: Arc<ez::Variable>,
    register_to_variable: BTreeMap<Arc<Register>, Arc<ez::Variable>>,
    stack_list: Arc<ez::List>,
    stdout_list: Arc<ez::List>,
}

impl CompileManager {
    pub fn get_broadcast(&self, sp_uuid: &Uuid) -> &Arc<ez::Broadcast> {
        self.sp_uuid_to_broadcast
            .get(sp_uuid)
            .expect("sub proc UUID should have corresponding Broadcast registered")
    }

    pub fn get_variable(&mut self, rmem: &RMemLoc) -> &Arc<ez::Variable> {
        match rmem {
            RMemLoc::StackPointer => &self.stack_pointer_variable,
            RMemLoc::Register(register) => {
                self.register_to_variable.entry(register.clone()).or_insert_with(|| {
                    Arc::new(ez::Variable {
                        uuid: Uuid::new_v4(),
                        name: format!("t{}", register.id).into(),
                    })
                })
            },
        }
    }

    pub fn stack_list(&self) -> &Arc<ez::List> {
        &self.stack_list
    }

    pub fn stdout_list(&self) -> &Arc<ez::List> {
        &self.stdout_list
    }

    pub fn variables(&self) -> Vec<Arc<ez::Variable>> {
        chain!([self.stack_pointer_variable.clone()], self.register_to_variable.values().cloned())
            .collect()
    }

    pub fn broadcasts(&self) -> Vec<Arc<ez::Broadcast>> {
        self.sp_uuid_to_broadcast.values().cloned().collect()
    }

    pub fn lists(&self) -> Vec<Arc<ez::List>> {
        Vec::from([self.stack_list.clone(), self.stdout_list.clone()])
    }
}

fn compile_proc(compile_m: &mut CompileManager, proc: &Proc<RMemLoc>) -> Vec<ez::Stack> {
    let mut stacks = Vec::new();

    for (i, sp) in proc.sub_procs.iter().enumerate() {
        let is_program_start = 'root: {
            if let ProcKind::Main = proc.kind.as_ref() {
                if i == 0 {
                    break 'root true;
                }
            }

            false
        };

        let root = match is_program_start {
            true => ez::EventOp::WhenFlagClicked,
            false => ez::EventOp::WhenBroadcastReceived {
                broadcast: compile_m.get_broadcast(&sp.uuid).clone(),
            },
        };

        let command_ops = sp
            .commands
            .iter()
            .flat_map(|command| compile_command(compile_m, command))
            .map(Arc::new)
            .collect_vec();

        let command_ops = match is_program_start {
            false => command_ops,
            true => {
                let init_ops = Vec::from([
                    // delete all of stack
                    Arc::new(ez::Op::Data(ez::DataOp::DeleteAllOfList {
                        list: Arc::clone(&compile_m.stack_list().clone()),
                    })),
                    // delete all of stdout
                    Arc::new(ez::Op::Data(ez::DataOp::DeleteAllOfList {
                        list: Arc::clone(&compile_m.stdout_list().clone()),
                    })),
                    // init stack memory
                    Arc::new(ez::Op::Control(ez::ControlOp::Repeat {
                        times: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::Int(200_000)))),
                        looped_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                            root: Arc::new(ez::Op::Data(ez::DataOp::AddToList {
                                list: Arc::clone(&compile_m.stack_list().clone()),
                                item: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                    "".into(),
                                )))),
                            })),
                            rest: Arc::new(Vec::new()),
                        }))),
                    })),
                ]);
                chain!(init_ops, command_ops).collect()
            },
        };

        let call_ops = compile_call(compile_m, &sp.call).into_iter().map(Arc::new);

        let stack = ez::Stack {
            root: Arc::new(ez::Op::Event(root)),
            rest: Arc::new(chain!(command_ops, call_ops).collect()),
        };
        stacks.push(stack);
    }

    stacks
}

fn compile_command(compile_m: &mut CompileManager, command: &Command<RMemLoc>) -> Vec<ez::Op> {
    match command {
        Command::SetMemLoc { mem_loc, val } => {
            Vec::from([ez::Op::Data(ez::DataOp::SetVariableTo {
                variable: compile_m.get_variable(mem_loc).clone(),
                value: compile_expr(compile_m, val),
            })])
        },
        Command::SetStack { addr, val } => {
            Vec::from([ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: compile_m.stack_list().clone(),
                index: compile_expr(compile_m, addr),
                item: compile_expr(compile_m, val),
            })])
        },
        Command::In => Vec::from([ez::Op::Sensing(ez::SensingOp::AskAndWait {
            question: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String("".into())))),
        })]),
        Command::Out(expr) => Vec::from([ez::Op::Data(ez::DataOp::AddToList {
            list: compile_m.stdout_list().clone(),
            item: compile_expr(compile_m, expr),
        })]),
    }
}

fn compile_call(compile_m: &mut CompileManager, call: &Call<RMemLoc>) -> Vec<ez::Op> {
    match call {
        Call::Exit => Vec::new(),
        Call::Jump(to) => Vec::from([ez::Op::Event(ez::EventOp::BroadcastAndWait {
            input: compile_expr(compile_m, to),
        })]),
        Call::Branch { cond, then_to, else_to } => {
            Vec::from([ez::Op::Control(ez::ControlOp::IfElse {
                condition: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(
                    ez::OperatorOp::Equals {
                        operand_a: compile_expr(compile_m, cond),
                        operand_b: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            "true".into(),
                        )))),
                    },
                )))),
                then_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                    root: Arc::new(ez::Op::Event(ez::EventOp::BroadcastAndWait {
                        input: compile_expr(compile_m, then_to),
                    })),
                    rest: Arc::new(Vec::new()),
                }))),
                else_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                    root: Arc::new(ez::Op::Event(ez::EventOp::BroadcastAndWait {
                        input: compile_expr(compile_m, else_to),
                    })),
                    rest: Arc::new(Vec::new()),
                }))),
            })])
        },
    }
}

fn compile_expr(compile_m: &mut CompileManager, expr: &Expr<RMemLoc>) -> Arc<ez::Expr> {
    let compiled_expr = match expr {
        Expr::MemLoc(mem_loc) => ez::Expr::Variable(compile_m.get_variable(mem_loc).clone()),
        Expr::Value(value) => match value.as_ref() {
            Value::Literal(literal) => {
                ez::Expr::Literal(Arc::new(ez::Literal::String(literal.clone())))
            },
            Value::Label(label) => ez::Expr::Broadcast(compile_m.get_broadcast(label).clone()),
        },
        Expr::Deref(expr) => ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
            list: compile_m.stack_list().clone(),
            index: compile_expr(compile_m, expr),
        }))),
        Expr::Add(args) => ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Add {
            num_a: compile_expr(compile_m, &args.left),
            num_b: compile_expr(compile_m, &args.right),
        }))),
        Expr::Sub(args) => {
            ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Subtract {
                num_a: compile_expr(compile_m, &args.left),
                num_b: compile_expr(compile_m, &args.right),
            })))
        },
        Expr::Mul(args) => {
            ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Multiply {
                num_a: compile_expr(compile_m, &args.left),
                num_b: compile_expr(compile_m, &args.right),
            })))
        },
        Expr::Div(args) => ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Divide {
            num_a: compile_expr(compile_m, &args.left),
            num_b: compile_expr(compile_m, &args.right),
        }))),
        Expr::Mod(args) => ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Mod {
            num_a: compile_expr(compile_m, &args.left),
            num_b: compile_expr(compile_m, &args.right),
        }))),
        Expr::Eq(args) => ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Equals {
            operand_a: compile_expr(compile_m, &args.left),
            operand_b: compile_expr(compile_m, &args.right),
        }))),
        Expr::Lte(args) => ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Not {
            operand: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(
                ez::OperatorOp::GreaterThan {
                    operand_a: compile_expr(compile_m, &args.left),
                    operand_b: compile_expr(compile_m, &args.right),
                },
            )))),
        }))),
        Expr::Neq(args) => ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Not {
            operand: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(
                ez::OperatorOp::Equals {
                    operand_a: compile_expr(compile_m, &args.left),
                    operand_b: compile_expr(compile_m, &args.right),
                },
            )))),
        }))),
        Expr::Not(expr) => ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Not {
            operand: compile_expr(compile_m, expr),
        }))),
        Expr::InAnswer => ez::Expr::Derived(Arc::new(ez::Op::Sensing(ez::SensingOp::Answer))),
    };

    Arc::new(compiled_expr)
}
