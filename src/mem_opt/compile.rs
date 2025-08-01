use itertools::{Itertools, chain};
use std::{collections::BTreeMap, ops::Not, sync::Arc};

use crate::{Target, ez};
use uuid::Uuid;

use super::{
    ast::{Call, Command, Expr, Proc, ProcKind, Value},
    designate::{RMemLoc, Register},
};

#[derive(Clone, Copy)]
pub struct CompileOptions {
    pub stack_monitoring: bool,
    pub target: Target,
}

pub fn compile<'a>(
    procs: impl Iterator<Item = &'a Proc<RMemLoc>>,
    opt: CompileOptions,
) -> ez::Program {
    let procs = procs.collect_vec();

    match opt.target {
        Target::Scratch => {
            compile_to_target(ScratchTargetManager::new(procs.iter().copied()), procs, opt)
        },
        Target::TurboWarp => {
            compile_to_target(TurboWarpTargetManager::new(procs.iter().copied()), procs, opt)
        },
    }
}

fn compile_to_target(
    target_manager: impl TargetManager,
    procs: Vec<&Proc<RMemLoc>>,
    opt: CompileOptions,
) -> ez::Program {
    let mut compile_m = CompileManager {
        target_manager,
        stack_pointer_variable: Arc::new(ez::Variable { uuid: Uuid::new_v4(), name: "sp".into() }),
        register_to_variable: Default::default(),
        stack_list: Arc::new(ez::List { uuid: Uuid::new_v4(), name: "stack".into() }),
        stdout_list: Arc::new(ez::List { uuid: Uuid::new_v4(), name: "stdout".into() }),
    };

    let proc_stacks =
        procs.iter().flat_map(|proc| compile_proc(&mut compile_m, proc)).collect_vec();

    let stacks = compile_m.target_manager.attach_stacks(proc_stacks);

    let stage = ez::Stage {
        variables: Arc::new(compile_m.variables()),
        broadcasts: Arc::new(compile_m.target_manager.broadcasts()),
        lists: Arc::new(compile_m.lists()),
        stacks: Arc::new(stacks),
    };

    let stages = Vec::from([Arc::new(stage)]);

    let monitors = if opt.stack_monitoring {
        Vec::from([
            Arc::new(ez::Monitor {
                list: compile_m.stdout_list.clone(),
                width: 360.,
                height: 360.,
                x: 0.,
                y: 0.,
                visible: true,
            }),
            Arc::new(ez::Monitor {
                list: compile_m.stack_list.clone(),
                width: 120.,
                height: 360.,
                x: 360.,
                y: 0.,
                visible: true,
            }),
        ])
    } else {
        Vec::from([Arc::new(ez::Monitor {
            list: compile_m.stdout_list.clone(),
            width: 480.,
            height: 360.,
            x: 0.,
            y: 0.,
            visible: true,
        })])
    };

    ez::Program { monitors: Arc::new(monitors), stages: Arc::new(stages) }
}

struct CompileManager<TM: TargetManager> {
    target_manager: TM,
    stack_pointer_variable: Arc<ez::Variable>,
    register_to_variable: BTreeMap<Arc<Register>, Arc<ez::Variable>>,
    stack_list: Arc<ez::List>,
    stdout_list: Arc<ez::List>,
}

trait TargetManager {
    fn attach_stacks(&self, proc_stacks: Vec<ez::Stack>) -> Vec<Arc<ez::Stack>>;
    fn broadcasts(&self) -> Vec<Arc<ez::Broadcast>>;
    fn create_proc_root(&self, sp_uuid: &Uuid) -> ez::Op;
    fn create_jump(&self, proc_addr_expr: Arc<ez::Expr>) -> ez::Op;
    fn compile_label(&self, label: &Uuid) -> ez::Expr;
    fn extra_program_start_ops(&self) -> Vec<Arc<ez::Op>>;
    fn program_exit_call(&self) -> Vec<Arc<ez::Op>>;
}

struct ScratchTargetManager {
    sp_uuid_to_broadcast: BTreeMap<Uuid, Arc<ez::Broadcast>>,
}

impl ScratchTargetManager {
    pub fn new<'a>(procs: impl Iterator<Item = &'a Proc<RMemLoc>>) -> Self {
        let mut sp_uuid_to_broadcast = BTreeMap::new();
        for proc in procs {
            let proc_name = match proc.kind.as_ref() {
                ProcKind::Main => "main".into(),
                ProcKind::Func { name } => format!("func_{name}"),
            };

            for (i, sp) in proc.sub_procs.iter().enumerate() {
                sp_uuid_to_broadcast.insert(
                    sp.uuid,
                    Arc::new(ez::Broadcast {
                        uuid: Uuid::new_v4(),
                        name: format!("{proc_name}.{i}").into(),
                    }),
                );
            }
        }

        Self { sp_uuid_to_broadcast }
    }

    pub fn get_broadcast(&self, sp_uuid: &Uuid) -> &Arc<ez::Broadcast> {
        self.sp_uuid_to_broadcast
            .get(sp_uuid)
            .expect("sub proc UUID should have corresponding Broadcast registered")
    }
}

impl TargetManager for ScratchTargetManager {
    fn attach_stacks(&self, proc_stacks: Vec<ez::Stack>) -> Vec<Arc<ez::Stack>> {
        proc_stacks.into_iter().map(Arc::new).collect()
    }

    fn broadcasts(&self) -> Vec<Arc<ez::Broadcast>> {
        self.sp_uuid_to_broadcast.values().cloned().collect()
    }

    fn create_proc_root(&self, sp_uuid: &Uuid) -> ez::Op {
        ez::Op::Event(ez::EventOp::WhenBroadcastReceived {
            broadcast: self.get_broadcast(sp_uuid).clone(),
        })
    }

    fn create_jump(&self, proc_addr_expr: Arc<ez::Expr>) -> ez::Op {
        ez::Op::Event(ez::EventOp::BroadcastAndWait { input: proc_addr_expr })
    }

    fn compile_label(&self, label: &Uuid) -> ez::Expr {
        ez::Expr::Broadcast(self.get_broadcast(label).clone())
    }

    fn extra_program_start_ops(&self) -> Vec<Arc<ez::Op>> {
        Vec::new()
    }

    fn program_exit_call(&self) -> Vec<Arc<ez::Op>> {
        Vec::new()
    }
}

struct TurboWarpTargetManager {
    sp_uuid_to_custom_block: BTreeMap<Uuid, Arc<ez::CustomBlock>>,
    dyn_custom_block: Arc<ez::CustomBlock>,
    dyn_proc_argument: Arc<ez::Argument>,
    dyn_proc_variable: Arc<ez::Variable>,
}

impl TurboWarpTargetManager {
    pub fn new<'a>(procs: impl Iterator<Item = &'a Proc<RMemLoc>>) -> Self {
        let mut sp_uuid_to_custom_block = BTreeMap::new();
        for proc in procs {
            let proc_name = match proc.kind.as_ref() {
                ProcKind::Main => "main".into(),
                ProcKind::Func { name } => format!("func_{name}"),
            };

            for (i, sp) in proc.sub_procs.iter().enumerate() {
                sp_uuid_to_custom_block.insert(
                    sp.uuid,
                    Arc::new(ez::CustomBlock { name: format!("{proc_name}.{i}").into() }),
                );
            }
        }

        Self {
            sp_uuid_to_custom_block,
            dyn_custom_block: Arc::new(ez::CustomBlock { name: "dyn %s".into() }),
            dyn_proc_argument: Arc::new(ez::Argument {
                uuid: Uuid::new_v4(),
                name: "proc".into(),
                default: "".into(),
            }),
            dyn_proc_variable: Arc::new(ez::Variable { uuid: Uuid::new_v4(), name: "proc".into() }),
        }
    }

    pub fn get_custom_block(&self, sp_uuid: &Uuid) -> &Arc<ez::CustomBlock> {
        self.sp_uuid_to_custom_block
            .get(sp_uuid)
            .expect("sub proc UUID should have corresponding Custom Block registered")
    }
}

impl TargetManager for TurboWarpTargetManager {
    fn attach_stacks(&self, proc_stacks: Vec<ez::Stack>) -> Vec<Arc<ez::Stack>> {
        let dyn_stacks = {
            let proc_arg_op = Arc::new(ez::Op::Argument(ez::ArgumentOp::ReporterStringNumber {
                arg: self.dyn_proc_argument.clone(),
            }));

            let proc_arg_stack = Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                root: proc_arg_op.clone(),
                rest: Arc::new(Vec::new()),
            })));

            let prototype = Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                root: Arc::new(ez::Op::Procedure(ez::ProcedureOp::Prototype {
                    custom_block: self.dyn_custom_block.clone(),
                    arguments_with_stacks: Arc::new(Vec::from([(
                        self.dyn_proc_argument.clone(),
                        proc_arg_stack,
                    )])),
                })),
                rest: Arc::new(Vec::new()),
            })));

            let def = Arc::new(ez::Op::Procedure(ez::ProcedureOp::Definition {
                prototype_stack: prototype.clone(),
            }));

            Vec::from([ez::Stack {
                root: def,
                rest: Arc::new(Vec::from([Arc::new(ez::Op::Data(ez::DataOp::SetVariableTo {
                    variable: self.dyn_proc_variable.clone(),
                    value: Arc::new(ez::Expr::Derived(proc_arg_op.clone())),
                }))])),
            }])
        };

        proc_stacks.into_iter().chain(dyn_stacks).map(Arc::new).collect()
    }

    fn broadcasts(&self) -> Vec<Arc<ez::Broadcast>> {
        Vec::new()
    }

    fn create_proc_root(&self, sp_uuid: &Uuid) -> ez::Op {
        ez::Op::Procedure(ez::ProcedureOp::Definition {
            prototype_stack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                root: Arc::new(ez::Op::Procedure(ez::ProcedureOp::Prototype {
                    custom_block: self.get_custom_block(sp_uuid).clone(),
                    arguments_with_stacks: Arc::new(Vec::new()),
                })),
                rest: Arc::new(Vec::new()),
            }))),
        })
    }

    fn create_jump(&self, proc_addr_expr: Arc<ez::Expr>) -> ez::Op {
        ez::Op::Procedure(ez::ProcedureOp::Call {
            custom_block: self.dyn_custom_block.clone(),
            argument_inputs: Arc::new(Vec::from([(
                self.dyn_proc_argument.clone(),
                proc_addr_expr,
            )])),
        })
    }

    fn compile_label(&self, label: &Uuid) -> ez::Expr {
        ez::Expr::Literal(Arc::new(ez::Literal::String(self.get_custom_block(label).name.clone())))
    }

    fn extra_program_start_ops(&self) -> Vec<Arc<ez::Op>> {
        let mut branch_stack = None;

        for custom_block in self.sp_uuid_to_custom_block.values() {
            branch_stack = Some(match branch_stack {
                None => Arc::new(ez::Op::Control(ez::ControlOp::If {
                    condition: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(
                        ez::OperatorOp::Equals {
                            operand_a: Arc::new(ez::Expr::Variable(self.dyn_proc_variable.clone())),
                            operand_b: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                custom_block.name.clone(),
                            )))),
                        },
                    )))),
                    then_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                        root: Arc::new(ez::Op::Procedure(ez::ProcedureOp::Call {
                            custom_block: custom_block.clone(),
                            argument_inputs: Arc::new(Vec::new()),
                        })),
                        rest: Arc::new(Vec::new()),
                    }))),
                })),
                Some(prev) => Arc::new(ez::Op::Control(ez::ControlOp::IfElse {
                    condition: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(
                        ez::OperatorOp::Equals {
                            operand_a: Arc::new(ez::Expr::Variable(self.dyn_proc_variable.clone())),
                            operand_b: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                                custom_block.name.clone(),
                            )))),
                        },
                    )))),
                    then_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                        root: Arc::new(ez::Op::Procedure(ez::ProcedureOp::Call {
                            custom_block: custom_block.clone(),
                            argument_inputs: Arc::new(Vec::new()),
                        })),
                        rest: Arc::new(Vec::new()),
                    }))),
                    else_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                        root: prev,
                        rest: Arc::new(Vec::new()),
                    }))),
                })),
            });
        }

        if let Some(branch_stack) = branch_stack {
            Vec::from([Arc::new(ez::Op::Control(ez::ControlOp::RepeatUntil {
                condition: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(
                    ez::OperatorOp::Equals {
                        operand_a: Arc::new(ez::Expr::Variable(self.dyn_proc_variable.clone())),
                        operand_b: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            "".into(),
                        )))),
                    },
                )))),
                then_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                    root: branch_stack,
                    rest: Arc::new(Vec::new()),
                }))),
            }))])
        } else {
            Vec::new()
        }
    }

    fn program_exit_call(&self) -> Vec<Arc<ez::Op>> {
        Vec::from([Arc::new(ez::Op::Data(ez::DataOp::SetVariableTo {
            variable: self.dyn_proc_variable.clone(),
            value: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String("".into())))),
        }))])
    }
}

impl<TM: TargetManager> CompileManager<TM> {
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

    pub fn lists(&self) -> Vec<Arc<ez::List>> {
        Vec::from([self.stack_list.clone(), self.stdout_list.clone()])
    }
}

fn compile_proc(
    compile_m: &mut CompileManager<impl TargetManager>,
    proc: &Proc<RMemLoc>,
) -> Vec<ez::Stack> {
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
            true => ez::Op::Event(ez::EventOp::WhenFlagClicked),
            false => compile_m.target_manager.create_proc_root(&sp.uuid),
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

        let call_ops = compile_call(compile_m, &sp.call).into_iter().map(Arc::new).collect_vec();

        let call_ops = match call_ops.is_empty() {
            false => call_ops,
            true => compile_m.target_manager.program_exit_call(),
        };

        let extra_program_start_ops = match is_program_start {
            true => compile_m.target_manager.extra_program_start_ops(),
            false => Vec::new(),
        };

        let stack = ez::Stack {
            root: Arc::new(root),
            rest: Arc::new(chain!(command_ops, call_ops, extra_program_start_ops).collect()),
        };
        stacks.push(stack);
    }

    stacks
}

fn compile_command(
    compile_m: &mut CompileManager<impl TargetManager>,
    command: &Command<RMemLoc>,
) -> Vec<ez::Op> {
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
        Command::ClearStdout => Vec::from([ez::Op::Data(ez::DataOp::DeleteAllOfList {
            list: compile_m.stdout_list().clone(),
        })]),
        Command::WriteStdout { index, val } => {
            Vec::from([ez::Op::Data(ez::DataOp::ReplaceItemOfList {
                list: compile_m.stdout_list().clone(),
                index: compile_expr(compile_m, index),
                item: compile_expr(compile_m, val),
            })])
        },
        Command::Wait { duration_s } => Vec::from([ez::Op::Control(ez::ControlOp::Wait {
            duration_s: compile_expr(compile_m, duration_s),
        })]),
    }
}

fn compile_call(
    compile_m: &mut CompileManager<impl TargetManager>,
    call: &Call<RMemLoc>,
) -> Vec<ez::Op> {
    match call {
        Call::Exit => Vec::new(),
        Call::Jump(to) => Vec::from([{
            let proc_addr_expr = compile_expr(compile_m, to);
            compile_m.target_manager.create_jump(proc_addr_expr)
        }]),
        Call::Branch { cond, then_to, else_to } => {
            let compiled_cond = compile_expr(compile_m, cond);
            let wrap_cond = 'wrap: {
                let ez::Expr::Derived(op) = compiled_cond.as_ref() else { break 'wrap true };
                let ez::Op::Operator(op) = op.as_ref() else { break 'wrap true };

                matches!(
                    op,
                    ez::OperatorOp::And { .. }
                        | ez::OperatorOp::Equals { .. }
                        | ez::OperatorOp::GreaterThan { .. }
                        | ez::OperatorOp::LessThan { .. }
                        | ez::OperatorOp::Not { .. }
                        | ez::OperatorOp::Or { .. }
                )
                .not()
            };

            let compiled_cond = match wrap_cond {
                false => compiled_cond,
                true => Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(
                    ez::OperatorOp::Equals {
                        operand_a: compiled_cond,
                        operand_b: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::String(
                            "true".into(),
                        )))),
                    },
                )))),
            };

            Vec::from([ez::Op::Control(ez::ControlOp::IfElse {
                condition: compiled_cond,
                then_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                    root: {
                        let proc_addr_expr = compile_expr(compile_m, then_to);
                        Arc::new(compile_m.target_manager.create_jump(proc_addr_expr))
                    },
                    rest: Arc::new(Vec::new()),
                }))),
                else_substack: Arc::new(ez::Expr::Stack(Arc::new(ez::Stack {
                    root: {
                        let proc_addr_expr = compile_expr(compile_m, else_to);
                        Arc::new(compile_m.target_manager.create_jump(proc_addr_expr))
                    },
                    rest: Arc::new(Vec::new()),
                }))),
            })])
        },
    }
}

fn compile_expr(
    compile_m: &mut CompileManager<impl TargetManager>,
    expr: &Expr<RMemLoc>,
) -> Arc<ez::Expr> {
    let compiled_expr = match expr {
        Expr::MemLoc(mem_loc) => ez::Expr::Variable(compile_m.get_variable(mem_loc).clone()),
        Expr::Value(value) => match value.as_ref() {
            Value::Literal(literal) => {
                ez::Expr::Literal(Arc::new(ez::Literal::String(literal.clone())))
            },
            Value::Label(label) => compile_m.target_manager.compile_label(label),
        },
        Expr::StackDeref(expr) => {
            ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                list: compile_m.stack_list().clone(),
                index: compile_expr(compile_m, expr),
            })))
        },
        Expr::StdoutDeref(expr) => {
            ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                list: compile_m.stdout_list().clone(),
                index: compile_expr(compile_m, expr),
            })))
        },
        Expr::StdoutLen => ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::LengthOfList {
            list: compile_m.stdout_list().clone(),
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
        Expr::Gt(args) => {
            ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::GreaterThan {
                operand_a: compile_expr(compile_m, &args.left),
                operand_b: compile_expr(compile_m, &args.right),
            })))
        },
        Expr::Lt(args) => ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::LessThan {
            operand_a: compile_expr(compile_m, &args.left),
            operand_b: compile_expr(compile_m, &args.right),
        }))),
        Expr::Not(expr) => ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Not {
            operand: compile_expr(compile_m, expr),
        }))),
        Expr::Or(args) => ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Or {
            operand_a: compile_expr(compile_m, &args.left),
            operand_b: compile_expr(compile_m, &args.right),
        }))),
        Expr::And(args) => ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::And {
            operand_a: compile_expr(compile_m, &args.left),
            operand_b: compile_expr(compile_m, &args.right),
        }))),
        Expr::InAnswer => ez::Expr::Derived(Arc::new(ez::Op::Sensing(ez::SensingOp::Answer))),
        Expr::Join(args) => ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Join {
            string_a: compile_expr(compile_m, &args.left),
            string_b: compile_expr(compile_m, &args.right),
        }))),
        Expr::Random(args) => {
            ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Random {
                from: compile_expr(compile_m, &args.left),
                to: compile_expr(compile_m, &args.right),
            })))
        },
    };

    Arc::new(compiled_expr)
}
