use anyhow::bail;
use asm_compiler::ast::{self as asm_ast, BinaryArgs, UnaryArgs, Value};
use itertools::Itertools;
use std::{collections::HashMap, sync::Arc};
use uuid::Uuid;

use crate::{
    ast::{Assign, Command, NativeCommand},
    link::{Call, Proc, ProcKind, Statement},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PrivRegister(&'static str);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PubRegister(&'static str);

trait Register {
    fn addr(&self) -> &'static str;

    fn addr_value(&self) -> Arc<asm_ast::Value> {
        Arc::new(asm_ast::Value::Literal(Arc::new(asm_ast::Literal { val: self.addr().into() })))
    }
}

impl Register for PrivRegister {
    fn addr(&self) -> &'static str {
        self.0
    }
}

impl Register for PubRegister {
    fn addr(&self) -> &'static str {
        self.0
    }
}

const STACK_POINTER: PrivRegister = PrivRegister("1");
const RESULT: PrivRegister = PrivRegister("2");
const OPERAND: PrivRegister = PrivRegister("3");
const LEFT: PubRegister = PubRegister("4");
const RIGHT: PubRegister = PubRegister("5");

macro_rules! chain {
    ($($iter:expr),* $(,)?) => {
        [].into_iter()
            $(
                .chain($iter)
            )*
            .collect()
    };
}

fn data(command: asm_ast::DataCommand) -> Arc<asm_ast::Command> {
    Arc::new(asm_ast::Command::Data(Arc::new(command)))
}

fn control(command: asm_ast::ControlCommand) -> Arc<asm_ast::Command> {
    Arc::new(asm_ast::Command::Control(Arc::new(command)))
}

fn literal(value: impl ToString) -> Arc<Value> {
    Arc::new(asm_ast::Value::Literal(Arc::new(asm_ast::Literal { val: value.to_string().into() })))
}

fn label(value: &str) -> Arc<Value> {
    Arc::new(asm_ast::Value::Label(Arc::new(asm_ast::Label { name: value.into() })))
}

fn compute_stack_addr(dest: PubRegister, offset: usize) -> Vec<Arc<asm_ast::Command>> {
    Vec::from([
        data(asm_ast::DataCommand::Move(asm_ast::BinaryArgs {
            dest: RESULT.addr_value(),
            val: STACK_POINTER.addr_value(),
        })),
        data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
            dest: OPERAND.addr_value(),
            val: literal(offset),
        })),
        data(asm_ast::DataCommand::Sub(asm_ast::TernaryArgs {
            dest: dest.addr_value(),
            left: RESULT.addr_value(),
            right: OPERAND.addr_value(),
        })),
    ])
}

fn compute_param_addr(dest: PubRegister, offset: usize) -> Vec<Arc<asm_ast::Command>> {
    Vec::from([
        data(asm_ast::DataCommand::Move(asm_ast::BinaryArgs {
            dest: RESULT.addr_value(),
            val: STACK_POINTER.addr_value(),
        })),
        data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
            dest: OPERAND.addr_value(),
            val: literal(offset),
        })),
        data(asm_ast::DataCommand::Add(asm_ast::TernaryArgs {
            dest: dest.addr_value(),
            left: RESULT.addr_value(),
            right: OPERAND.addr_value(),
        })),
    ])
}

fn get_stack_value(dest: PubRegister, offset: usize) -> Vec<Arc<asm_ast::Command>> {
    [].into_iter()
        .chain(compute_stack_addr(dest, offset))
        .chain([data(asm_ast::DataCommand::MoveDerefSrc(asm_ast::BinaryArgs {
            dest: dest.addr_value(),
            val: dest.addr_value(),
        }))])
        .collect_vec()
}

pub fn compile(linked: Vec<Arc<Proc>>) -> anyhow::Result<Vec<Arc<asm_ast::Procedure>>> {
    // compile items to asm
    let mut asm_procs = Vec::<Arc<asm_ast::Procedure>>::new();

    let asm_main_setup = asm_ast::Procedure {
        kind: asm_ast::ProcedureKind::Main,
        commands: Vec::from([
            // init stack pointer
            data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                dest: STACK_POINTER.addr_value(), // stored in mem addr 1
                val: RIGHT.addr_value(),          // points to last addr in stack (2)
            })),
            // run high-level main method
            data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                dest: RESULT.addr_value(),
                val: label("main.0"),
            })),
            control(asm_ast::ControlCommand::Jump(asm_ast::UnaryArgs { val: RESULT.addr_value() })),
        ]),
    };

    asm_procs.push(Arc::new(asm_main_setup));

    for proc in linked {
        let compiled_asm_procs = compile_proc(proc.as_ref())?;
        asm_procs.extend(compiled_asm_procs);
    }

    Ok(asm_procs)
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Var {
    ReturnAddr,
    User(Arc<str>),
}

fn compile_proc(proc: &Proc) -> anyhow::Result<Vec<Arc<asm_ast::Procedure>>> {
    let stack_vars: Vec<Var> = match &proc.kind {
        ProcKind::Main => proc.vars.iter().map(|var| Var::User(Arc::clone(var))).collect(),
        ProcKind::Func { params, .. } => chain!(
            [Var::ReturnAddr],
            params.iter().chain(proc.vars.iter()).map(|var| Var::User(Arc::clone(var))),
        ),
    };

    // init information about stack vars / their offsets
    let mut var_to_offset = HashMap::<Var, usize>::new();
    for (offset, var) in stack_vars.iter().rev().enumerate() {
        var_to_offset.insert(var.clone(), offset);
    }

    // init information about sub proc indexes
    let mut sub_proc_uuid_to_index = HashMap::<Uuid, usize>::new();
    for (index, sub_proc) in proc.sub_procs.iter().enumerate() {
        sub_proc_uuid_to_index.insert(sub_proc.uuid, index);
    }

    let mut asm_procs = Vec::new();
    for (sub_proc_index, sub_proc) in proc.sub_procs.iter().enumerate() {
        let is_head = sub_proc_index == 0;
        let is_tail = matches!(sub_proc.next_call.as_ref(), Call::Return | Call::Terminate);

        let mut asm_commands = Vec::<Arc<asm_ast::Command>>::new();

        // allocate stack vars
        if is_head {
            asm_commands.extend([
                data(asm_ast::DataCommand::Move(asm_ast::BinaryArgs {
                    dest: RESULT.addr_value(),
                    val: STACK_POINTER.addr_value(),
                })),
                data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                    dest: OPERAND.addr_value(),
                    val: literal(stack_vars.len()),
                })),
                data(asm_ast::DataCommand::Add(asm_ast::TernaryArgs {
                    dest: STACK_POINTER.addr_value(),
                    left: RESULT.addr_value(),
                    right: OPERAND.addr_value(),
                })),
            ]);
        }

        // run statements
        for statement in sub_proc.statements.iter().map(AsRef::as_ref) {
            let asm_statement_commands = match statement {
                Statement::Assign(assign) => {
                    let Assign { deref_var, var, command } = assign.as_ref();

                    let Some(&var_offset) = var_to_offset.get(&Var::User(Arc::clone(var))) else {
                        bail!("Failed to find var offset")
                    };

                    // value to assign to var should end up in temp_right
                    let mut assign_asm_commands = compile_command(command, &var_to_offset)?;

                    // set var addr to assign in temp_left
                    assign_asm_commands.extend(compute_stack_addr(LEFT, var_offset));

                    // deref var addr if its a ref assign
                    if *deref_var {
                        assign_asm_commands.push(data(asm_ast::DataCommand::MoveDerefSrc(
                            asm_ast::BinaryArgs { dest: LEFT.addr_value(), val: LEFT.addr_value() },
                        )));
                    }

                    // move value at addr in temp_right to addr in temp_left
                    assign_asm_commands.push(data(asm_ast::DataCommand::MoveDerefDest(
                        asm_ast::BinaryArgs { dest: LEFT.addr_value(), val: RIGHT.addr_value() },
                    )));

                    assign_asm_commands
                },
                Statement::Native(command) => match command.as_ref() {
                    NativeCommand::Out { var } => {
                        let Some(&var_offset) = var_to_offset.get(&Var::User(Arc::clone(var)))
                        else {
                            bail!("Failed to find var offset")
                        };

                        chain!(
                            get_stack_value(RIGHT, var_offset),
                            [data(asm_ast::DataCommand::Out(asm_ast::UnaryArgs {
                                val: RIGHT.addr_value()
                            }))],
                        )
                    },
                },
            };

            asm_commands.extend(asm_statement_commands);
        }

        if is_tail {
            // cleanup stack vars
            asm_commands.extend([
                data(asm_ast::DataCommand::Move(asm_ast::BinaryArgs {
                    dest: RESULT.addr_value(),
                    val: STACK_POINTER.addr_value(),
                })),
                data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                    dest: OPERAND.addr_value(),
                    val: literal(stack_vars.len()),
                })),
                data(asm_ast::DataCommand::Sub(asm_ast::TernaryArgs {
                    dest: STACK_POINTER.addr_value(),
                    left: RESULT.addr_value(),
                    right: OPERAND.addr_value(),
                })),
            ]);
        }

        let asm_proc_name = match &proc.kind {
            ProcKind::Main => "main".into(),
            ProcKind::Func { name, .. } => format!("func_{}", name),
        };

        let asm_call_commands = match sub_proc.next_call.as_ref() {
            Call::Func { name, param_coms, return_sub_proc } => {
                // set param values on func's stack
                let mut param_asm_commands = Vec::new();

                for (param_offset, command) in param_coms.iter().enumerate() {
                    let command_asm_commands = compile_command(command, &var_to_offset)?;

                    let asm_commands: Vec<Arc<asm_ast::Command>> = chain!(
                        // chain commands to put param value in RIGHT
                        command_asm_commands,
                        // store addr for func param in LEFT
                        compute_param_addr(LEFT, param_offset + 2),
                        // move value in call var to func param
                        [data(asm_ast::DataCommand::MoveDerefDest(asm_ast::BinaryArgs {
                            dest: LEFT.addr_value(),
                            val: RIGHT.addr_value(),
                        })),]
                    );

                    param_asm_commands.extend(asm_commands);
                }

                // set return addr
                let Some(return_sub_proc_index) = sub_proc_uuid_to_index.get(return_sub_proc)
                else {
                    bail!("Failed to find return sub proc index");
                };

                let return_asm_commands: Vec<Arc<asm_ast::Command>> = chain!(
                    // store addr for call var in LEFT
                    compute_param_addr(LEFT, 1),
                    // store return addr in RIGHT
                    [data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                        dest: RIGHT.addr_value(),
                        val: label(&format!("{}.{}", asm_proc_name, return_sub_proc_index)),
                    }))],
                    // move value in RIGHT (return addr) to addr in LEFT (return addr var)
                    [data(asm_ast::DataCommand::MoveDerefDest(asm_ast::BinaryArgs {
                        dest: LEFT.addr_value(),
                        val: RIGHT.addr_value()
                    }))],
                );

                let broadcast_asm_commands = Vec::from([
                    // broadcast function message
                    data(asm_ast::DataCommand::Set(BinaryArgs {
                        dest: RESULT.addr_value(),
                        val: label(&format!("func_{}.0", name)),
                    })),
                    control(asm_ast::ControlCommand::Jump(UnaryArgs { val: RESULT.addr_value() })),
                ]);

                chain!(param_asm_commands, return_asm_commands, broadcast_asm_commands)
            },
            Call::SubProc(sub_proc) => {
                let Some(sub_proc_index) = sub_proc_uuid_to_index.get(sub_proc) else {
                    bail!("Failed to find sub proc index");
                };

                Vec::from([
                    // broadcast function message
                    data(asm_ast::DataCommand::Set(BinaryArgs {
                        dest: RESULT.addr_value(),
                        val: label(&format!("{}.{}", asm_proc_name, sub_proc_index)),
                    })),
                    control(asm_ast::ControlCommand::Jump(UnaryArgs { val: RESULT.addr_value() })),
                ])
            },
            Call::Return => {
                chain!(
                    // store return addr in RIGHT
                    compute_param_addr(RIGHT, 1),
                    // broadcast function message
                    [
                        data(asm_ast::DataCommand::MoveDerefSrc(BinaryArgs {
                            dest: RESULT.addr_value(),
                            val: RIGHT.addr_value()
                        })),
                        control(asm_ast::ControlCommand::Jump(UnaryArgs {
                            val: RESULT.addr_value()
                        })),
                    ],
                )
            },
            Call::Terminate => Vec::from([control(asm_ast::ControlCommand::Exit)]),
            Call::IfBranch { cond_com, then_sub_proc, pop_sub_proc } => {
                let Some(then_proc_index) = sub_proc_uuid_to_index.get(then_sub_proc) else {
                    bail!("Failed to find then sub proc index");
                };

                let Some(pop_proc_index) = sub_proc_uuid_to_index.get(pop_sub_proc) else {
                    bail!("Failed to find pop sub proc index");
                };

                chain!(
                    compile_command(cond_com, &var_to_offset)?,
                    // store `then` sub proc name in OPERAND and `pop` sub proc name in RESULT
                    [
                        data(asm_ast::DataCommand::Set(BinaryArgs {
                            dest: OPERAND.addr_value(),
                            val: label(&format!("{}.{}", asm_proc_name, then_proc_index)),
                        })),
                        data(asm_ast::DataCommand::Set(BinaryArgs {
                            dest: RESULT.addr_value(),
                            val: label(&format!("{}.{}", asm_proc_name, pop_proc_index)),
                        })),
                    ],
                    [
                        control(asm_ast::ControlCommand::Branch(asm_ast::BinaryArgs {
                            dest: OPERAND.addr_value(),
                            val: RIGHT.addr_value(),
                        })),
                        control(asm_ast::ControlCommand::Jump(asm_ast::UnaryArgs {
                            val: RESULT.addr_value(),
                        }))
                    ]
                )
            },
            Call::IfElseBranch { cond_com, then_sub_proc, else_sub_proc } => {
                let Some(then_proc_index) = sub_proc_uuid_to_index.get(then_sub_proc) else {
                    bail!("Failed to find then sub proc index");
                };

                let Some(else_proc_index) = sub_proc_uuid_to_index.get(else_sub_proc) else {
                    bail!("Failed to find else sub proc index");
                };

                chain!(
                    compile_command(cond_com, &var_to_offset)?,
                    // store `then` sub proc name in OPERAND and `else` sub proc name in RESULT
                    [
                        data(asm_ast::DataCommand::Set(BinaryArgs {
                            dest: OPERAND.addr_value(),
                            val: label(&format!("{}.{}", asm_proc_name, then_proc_index)),
                        })),
                        data(asm_ast::DataCommand::Set(BinaryArgs {
                            dest: RESULT.addr_value(),
                            val: label(&format!("{}.{}", asm_proc_name, else_proc_index)),
                        })),
                    ],
                    [
                        control(asm_ast::ControlCommand::Branch(asm_ast::BinaryArgs {
                            dest: OPERAND.addr_value(),
                            val: RIGHT.addr_value(),
                        })),
                        control(asm_ast::ControlCommand::Jump(asm_ast::UnaryArgs {
                            val: RESULT.addr_value(),
                        }))
                    ]
                )
            },
        };

        asm_commands.extend(asm_call_commands);

        // finalize sub proc
        let asm_sub_proc_name = format!("{}.{}", asm_proc_name, sub_proc_index);
        let asm_proc_kind = asm_ast::ProcedureKind::Sub { name: asm_sub_proc_name.into() };
        let asm_proc = asm_ast::Procedure { kind: asm_proc_kind, commands: asm_commands };
        asm_procs.push(Arc::new(asm_proc));
    }

    Ok(asm_procs)
}

fn compile_command(
    command: &Command,
    var_to_offset: &HashMap<Var, usize>,
) -> anyhow::Result<Vec<Arc<asm_ast::Command>>> {
    let asm_commands = match command {
        Command::Literal(val) => Vec::from([
            // set var to literal stored in temp_operand
            data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                dest: RIGHT.addr_value(),
                val: literal(val),
            })),
        ]),
        Command::Add { left, right } => {
            let Some(left_offset) = var_to_offset.get(&Var::User(Arc::clone(left))) else {
                bail!("Failed to find left offset")
            };

            let Some(right_offset) = var_to_offset.get(&Var::User(Arc::clone(right))) else {
                bail!("Failed to find right offset")
            };

            chain!(
                // store value for left in LEFT
                get_stack_value(LEFT, *left_offset),
                // store value for right in RIGHT
                get_stack_value(RIGHT, *right_offset),
                // set var to sum of temp_left and temp_right
                [data(asm_ast::DataCommand::Add(asm_ast::TernaryArgs {
                    dest: RIGHT.addr_value(),
                    left: LEFT.addr_value(),
                    right: RIGHT.addr_value(),
                }))],
            )
        },
        Command::Ref { var } => {
            let Some(ref_offset) = var_to_offset.get(&Var::User(Arc::clone(var))) else {
                bail!("Failed to find ref offset")
            };

            chain!(
                // store addr for ref in RIGHT
                compute_stack_addr(RIGHT, *ref_offset),
            )
        },
        Command::CopyDeref { var } => {
            let Some(var_offset) = var_to_offset.get(&Var::User(Arc::clone(var))) else {
                bail!("Failed to find ref offset")
            };

            chain!(
                // store value of var in RIGHT
                get_stack_value(RIGHT, *var_offset),
                // deref RIGHT
                [data(asm_ast::DataCommand::MoveDerefSrc(asm_ast::BinaryArgs {
                    dest: RIGHT.addr_value(),
                    val: RIGHT.addr_value(),
                }))]
            )
        },
        Command::Copy { var } => {
            let Some(copy_offset) = var_to_offset.get(&Var::User(Arc::clone(var))) else {
                bail!("Failed to find copy offset")
            };

            chain!(
                // get value of var and store in RIGHT
                get_stack_value(RIGHT, *copy_offset),
            )
        },
        Command::Eq { left, right } => {
            let Some(left_offset) = var_to_offset.get(&Var::User(Arc::clone(left))) else {
                bail!("Failed to find left offset")
            };

            let Some(right_offset) = var_to_offset.get(&Var::User(Arc::clone(right))) else {
                bail!("Failed to find right offset")
            };

            chain!(
                get_stack_value(LEFT, *left_offset),
                get_stack_value(RIGHT, *right_offset),
                [data(asm_ast::DataCommand::Eq(asm_ast::TernaryArgs {
                    dest: RIGHT.addr_value(),
                    left: LEFT.addr_value(),
                    right: RIGHT.addr_value(),
                }))],
            )
        },
        Command::Not { var } => {
            let Some(var_offset) = var_to_offset.get(&Var::User(Arc::clone(var))) else {
                bail!("Failed to find var offset")
            };

            chain!(
                get_stack_value(RIGHT, *var_offset),
                [data(asm_ast::DataCommand::Not(asm_ast::BinaryArgs {
                    dest: RIGHT.addr_value(),
                    val: RIGHT.addr_value(),
                }))],
            )
        },
        Command::Sub { left, right } => {
            let Some(left_offset) = var_to_offset.get(&Var::User(Arc::clone(left))) else {
                bail!("Failed to find left offset")
            };

            let Some(right_offset) = var_to_offset.get(&Var::User(Arc::clone(right))) else {
                bail!("Failed to find right offset")
            };

            chain!(
                // store value for left in LEFT
                get_stack_value(LEFT, *left_offset),
                // store value for right in RIGHT
                get_stack_value(RIGHT, *right_offset),
                // set var to sum of temp_left and temp_right
                [data(asm_ast::DataCommand::Sub(asm_ast::TernaryArgs {
                    dest: RIGHT.addr_value(),
                    left: LEFT.addr_value(),
                    right: RIGHT.addr_value(),
                }))],
            )
        },
    };

    Ok(asm_commands)
}
