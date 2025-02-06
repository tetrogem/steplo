use anyhow::bail;
use asm_compiler::ast::{self as asm_ast, BinaryArgs, UnaryArgs};
use itertools::Itertools;
use std::{collections::HashMap, sync::Arc};

use crate::ast::{Command, Func, Item, NativeCommand, Proc, Statement};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PrivRegister(&'static str);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PubRegister(&'static str);

trait Register {
    fn addr(&self) -> &'static str;

    fn addr_value(&self) -> asm_ast::Value {
        asm_ast::Value { str: self.addr().into() }
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

fn compute_stack_addr(dest: PubRegister, offset: usize) -> Vec<Arc<asm_ast::Command>> {
    Vec::from([
        Arc::new(asm_ast::Command::Move(asm_ast::BinaryArgs {
            dest: RESULT.addr_value(),
            val: STACK_POINTER.addr_value(),
        })),
        Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
            dest: OPERAND.addr_value(),
            val: asm_ast::Value { str: offset.to_string().into() },
        })),
        Arc::new(asm_ast::Command::Sub(asm_ast::TernaryArgs {
            dest: dest.addr_value(),
            left: RESULT.addr_value(),
            right: OPERAND.addr_value(),
        })),
    ])
}

fn compute_param_addr(dest: PubRegister, offset: usize) -> Vec<Arc<asm_ast::Command>> {
    Vec::from([
        Arc::new(asm_ast::Command::Move(asm_ast::BinaryArgs {
            dest: RESULT.addr_value(),
            val: STACK_POINTER.addr_value(),
        })),
        Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
            dest: OPERAND.addr_value(),
            val: asm_ast::Value { str: offset.to_string().into() },
        })),
        Arc::new(asm_ast::Command::Add(asm_ast::TernaryArgs {
            dest: dest.addr_value(),
            left: RESULT.addr_value(),
            right: OPERAND.addr_value(),
        })),
    ])
}

fn get_stack_value(dest: PubRegister, offset: usize) -> Vec<Arc<asm_ast::Command>> {
    [].into_iter()
        .chain(compute_stack_addr(dest, offset))
        .chain([Arc::new(asm_ast::Command::MoveDerefSrc(asm_ast::BinaryArgs {
            dest: dest.addr_value(),
            val: dest.addr_value(),
        }))])
        .collect_vec()
}

pub fn compile(mut ast: Vec<Arc<Item>>) -> anyhow::Result<Vec<Arc<asm_ast::Procedure>>> {
    // add built-in native functions
    let out_func = Func {
        name: "out".into(),
        params: Arc::new(Vec::from(["val".into()])),
        proc: Arc::new(Proc {
            vars: Arc::new(Vec::new()),
            statements: Arc::new(Vec::from([Arc::new(Statement::Native {
                command: NativeCommand::Out { var: "val".into() },
            })])),
        }),
    };

    ast.push(Arc::new(Item::Func(Arc::new(out_func))));

    // compile items to asm
    let mut asm_procs = Vec::<Arc<asm_ast::Procedure>>::new();

    let asm_main_setup = asm_ast::Procedure {
        kind: asm_ast::ProcedureKind::Main,
        commands: Vec::from([
            // init stack pointer
            Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                dest: STACK_POINTER.addr_value(), // stored in mem addr 1
                val: RIGHT.addr_value(),          // points to last addr in stack (2)
            })),
            // run high-level main method
            Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                dest: RESULT.addr_value(),
                val: asm_ast::Value { str: "main".into() },
            })),
            Arc::new(asm_ast::Command::Jump(asm_ast::UnaryArgs { val: RESULT.addr_value() })),
        ]),
    };

    asm_procs.push(Arc::new(asm_main_setup));

    for item in ast {
        let asm_proc = compile_item(item.as_ref())?;
        asm_procs.push(Arc::new(asm_proc));
    }

    Ok(asm_procs)
}

fn compile_item(item: &Item) -> anyhow::Result<asm_ast::Procedure> {
    let (proc, stack_vars) = match item {
        Item::Main(main) => (&main.proc, main.proc.vars.iter().collect_vec()),
        Item::Func(func) => {
            (&func.proc, func.params.as_ref().iter().chain(func.proc.vars.iter()).collect_vec())
        },
    };

    let mut asm_commands = Vec::<Arc<asm_ast::Command>>::new();
    let mut var_to_offset = HashMap::<Arc<str>, usize>::new();

    // init stack vars
    for (offset, var) in stack_vars.iter().rev().enumerate() {
        var_to_offset.insert(Arc::clone(*var), offset);
    }

    asm_commands.extend([
        Arc::new(asm_ast::Command::Move(asm_ast::BinaryArgs {
            dest: RESULT.addr_value(),
            val: STACK_POINTER.addr_value(),
        })),
        Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
            dest: OPERAND.addr_value(),
            val: asm_ast::Value { str: stack_vars.len().to_string().into() },
        })),
        Arc::new(asm_ast::Command::Add(asm_ast::TernaryArgs {
            dest: STACK_POINTER.addr_value(),
            left: RESULT.addr_value(),
            right: OPERAND.addr_value(),
        })),
    ]);

    // run statements
    for statement in proc.statements.iter().map(AsRef::as_ref) {
        let asm_statement_commands = match statement {
            Statement::Assign { deref_var, var, command } => {
                let Some(&var_offset) = var_to_offset.get(var) else {
                    bail!("Failed to find var offset")
                };

                // value to assign to var should end up in temp_right
                let mut assign_asm_commands = match command.as_ref() {
                    Command::Literal(literal) => Vec::from([
                        // set var to literal stored in temp_operand
                        Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                            dest: RIGHT.addr_value(),
                            val: asm_ast::Value { str: literal.clone() },
                        })),
                    ]),
                    Command::Add { left, right } => {
                        let Some(left_offset) = var_to_offset.get(left) else {
                            bail!("Failed to find left offset")
                        };

                        let Some(right_offset) = var_to_offset.get(right) else {
                            bail!("Failed to find right offset")
                        };

                        [].into_iter()
                            .chain(
                                // store value for left in LEFT
                                get_stack_value(LEFT, *left_offset),
                            )
                            .chain(
                                // store value for right in RIGHT
                                get_stack_value(RIGHT, *right_offset),
                            )
                            .chain([
                                // set var to sum of temp_left and temp_right
                                Arc::new(asm_ast::Command::Add(asm_ast::TernaryArgs {
                                    dest: RIGHT.addr_value(),
                                    left: LEFT.addr_value(),
                                    right: RIGHT.addr_value(),
                                })),
                            ])
                            .collect_vec()
                    },
                    Command::Ref { var } => {
                        let Some(ref_offset) = var_to_offset.get(var) else {
                            bail!("Failed to find ref offset")
                        };

                        [].into_iter()
                            .chain(
                                // store addr for ref in RIGHT
                                compute_stack_addr(RIGHT, *ref_offset),
                            )
                            .collect_vec()
                    },
                    Command::CopyDeref { var } => {
                        let Some(var_offset) = var_to_offset.get(var) else {
                            bail!("Failed to find ref offset")
                        };

                        [].into_iter()
                            .chain(
                                // store value of var in RIGHT
                                get_stack_value(RIGHT, *var_offset),
                            )
                            .chain([
                                // deref RIGHT
                                Arc::new(asm_ast::Command::MoveDerefSrc(asm_ast::BinaryArgs {
                                    dest: RIGHT.addr_value(),
                                    val: RIGHT.addr_value(),
                                })),
                            ])
                            .collect_vec()
                    },
                    Command::Copy { var } => {
                        let Some(copy_offset) = var_to_offset.get(var) else {
                            bail!("Failed to find copy offset")
                        };

                        [].into_iter()
                            .chain(
                                // get value of var and store in RIGHT
                                get_stack_value(RIGHT, *copy_offset),
                            )
                            .collect_vec()
                    },
                };

                // set var addr to assign in temp_left
                assign_asm_commands.extend(compute_stack_addr(LEFT, var_offset));

                // deref var addr if its a ref assign
                if *deref_var {
                    assign_asm_commands.push(Arc::new(asm_ast::Command::MoveDerefSrc(
                        asm_ast::BinaryArgs { dest: LEFT.addr_value(), val: LEFT.addr_value() },
                    )));
                }

                // move value at addr in temp_right to addr in temp_left
                assign_asm_commands.push(Arc::new(asm_ast::Command::MoveDerefDest(
                    asm_ast::BinaryArgs { dest: LEFT.addr_value(), val: RIGHT.addr_value() },
                )));

                assign_asm_commands
            },
            Statement::Call { func_item, param_vars, cond_var } => {
                // set param values on func's stack
                let mut param_asm_commands = Vec::new();

                for (param_offset, call_var) in param_vars.iter().enumerate() {
                    let Some(call_var_offset) = var_to_offset.get(call_var) else {
                        bail!("Failed to find left offset")
                    };

                    let asm_commands = []
                        .into_iter()
                        .chain(
                            // store addr for call var in LEFT
                            compute_stack_addr(LEFT, *call_var_offset),
                        )
                        .chain(
                            // store addr for func param in RIGHT
                            compute_param_addr(RIGHT, param_offset + 1),
                        )
                        .chain([
                            // move value in call var to func param
                            Arc::new(asm_ast::Command::MoveDerefSrc(asm_ast::BinaryArgs {
                                dest: OPERAND.addr_value(),
                                val: LEFT.addr_value(),
                            })),
                            Arc::new(asm_ast::Command::MoveDerefDest(asm_ast::BinaryArgs {
                                dest: RIGHT.addr_value(),
                                val: OPERAND.addr_value(),
                            })),
                        ])
                        .collect_vec();

                    param_asm_commands.extend(asm_commands);
                }

                let broadcast_asm_commands = Vec::from([
                    // broadcast function message
                    Arc::new(asm_ast::Command::Set(BinaryArgs {
                        dest: RESULT.addr_value(),
                        val: asm_ast::Value { str: format!("func_{}", func_item).into() },
                    })),
                    Arc::new(asm_ast::Command::Jump(UnaryArgs { val: RESULT.addr_value() })),
                ]);

                param_asm_commands.into_iter().chain(broadcast_asm_commands).collect_vec()
            },
            Statement::Native { command } => match command {
                NativeCommand::Out { var } => {
                    let Some(&var_offset) = var_to_offset.get(var) else {
                        bail!("Failed to find var offset")
                    };

                    [].into_iter()
                        .chain(get_stack_value(RIGHT, var_offset))
                        .chain([Arc::new(asm_ast::Command::Out(asm_ast::UnaryArgs {
                            val: RIGHT.addr_value(),
                        }))])
                        .collect_vec()
                },
            },
        };

        asm_commands.extend(asm_statement_commands);
    }

    // cleanup stack vars
    asm_commands.extend([
        Arc::new(asm_ast::Command::Move(asm_ast::BinaryArgs {
            dest: RESULT.addr_value(),
            val: STACK_POINTER.addr_value(),
        })),
        Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
            dest: OPERAND.addr_value(),
            val: asm_ast::Value { str: stack_vars.len().to_string().into() },
        })),
        Arc::new(asm_ast::Command::Sub(asm_ast::TernaryArgs {
            dest: STACK_POINTER.addr_value(),
            left: RESULT.addr_value(),
            right: OPERAND.addr_value(),
        })),
    ]);

    // finalize proc
    let asm_proc_kind = match item {
        Item::Main(_) => asm_ast::ProcedureKind::Sub { name: "main".into() },
        Item::Func(func) => {
            asm_ast::ProcedureKind::Sub { name: format!("func_{}", func.name).into() }
        },
    };

    Ok(asm_ast::Procedure { kind: asm_proc_kind, commands: asm_commands })
}
