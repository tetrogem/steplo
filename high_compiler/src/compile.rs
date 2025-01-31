use anyhow::bail;
use asm_compiler::ast::{self as asm_ast, BinaryArgs, JumpArgs};
use itertools::Itertools;
use std::{collections::HashMap, sync::Arc};

use crate::ast::{Command, Item, Statement};

pub fn compile(ast: Vec<Arc<Item>>) -> anyhow::Result<Vec<Arc<asm_ast::Procedure>>> {
    let mut asm_procs = Vec::<Arc<asm_ast::Procedure>>::new();

    const STACK_POINTER_ADDR: &str = "1";
    const TEMP_RESULT_ADDR: &str = "2";
    const TEMP_OPERAND_ADDR: &str = "3";
    const TEMP_LEFT_ADDR: &str = "4";
    const TEMP_RIGHT_ADDR: &str = "5";

    let asm_main_setup = asm_ast::Procedure {
        kind: asm_ast::ProcedureKind::Main,
        commands: Vec::from([
            // init stack pointer
            Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                dest: asm_ast::Value { str: STACK_POINTER_ADDR.into() }, // stored in mem addr 1
                val: asm_ast::Value { str: TEMP_RIGHT_ADDR.into() }, // points to last addr in stack (2)
            })),
            // run high-level main method
            Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                val: asm_ast::Value { str: "main".into() },
            })),
            Arc::new(asm_ast::Command::Jump(asm_ast::JumpArgs {
                src: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
            })),
        ]),
    };

    asm_procs.push(Arc::new(asm_main_setup));

    for item in ast {
        let (proc, stack_vars) = match item.as_ref() {
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
                dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                val: asm_ast::Value { str: STACK_POINTER_ADDR.into() },
            })),
            Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                dest: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                val: asm_ast::Value { str: stack_vars.len().to_string().into() },
            })),
            Arc::new(asm_ast::Command::Add(asm_ast::TernaryArgs {
                dest: asm_ast::Value { str: STACK_POINTER_ADDR.into() },
                left: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                right: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
            })),
        ]);

        // run statements
        for statement in proc.statements.iter().map(AsRef::as_ref) {
            let asm_statement_commands = match statement {
                Statement::Assign { ref_var, var, command } => {
                    let Some(&var_offset) = var_to_offset.get(var) else {
                        bail!("Failed to find var offset")
                    };

                    // value to assign to var should end up in temp_right
                    let mut assign_asm_commands = match command.as_ref() {
                        Command::Literal(literal) => Vec::from([
                            // set var to literal stored in temp_operand
                            Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                                dest: asm_ast::Value { str: TEMP_RIGHT_ADDR.into() },
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

                            Vec::from([
                                // store addr for left in temp_result
                                Arc::new(asm_ast::Command::Move(asm_ast::BinaryArgs {
                                    dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                    val: asm_ast::Value { str: STACK_POINTER_ADDR.into() },
                                })),
                                Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                                    dest: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                                    val: asm_ast::Value { str: left_offset.to_string().into() },
                                })),
                                Arc::new(asm_ast::Command::Sub(asm_ast::TernaryArgs {
                                    dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                    left: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                    right: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                                })),
                                // store left value in temp_left
                                Arc::new(asm_ast::Command::MoveDerefSrc(asm_ast::BinaryArgs {
                                    dest: asm_ast::Value { str: TEMP_LEFT_ADDR.into() },
                                    val: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                })),
                                // store addr for right in temp_result
                                Arc::new(asm_ast::Command::Move(asm_ast::BinaryArgs {
                                    dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                    val: asm_ast::Value { str: STACK_POINTER_ADDR.into() },
                                })),
                                Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                                    dest: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                                    val: asm_ast::Value { str: right_offset.to_string().into() },
                                })),
                                Arc::new(asm_ast::Command::Sub(asm_ast::TernaryArgs {
                                    dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                    left: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                    right: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                                })),
                                // store right value in temp_left
                                Arc::new(asm_ast::Command::MoveDerefSrc(asm_ast::BinaryArgs {
                                    dest: asm_ast::Value { str: TEMP_RIGHT_ADDR.into() },
                                    val: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                })),
                                // set var to sum of temp_left and temp_right
                                Arc::new(asm_ast::Command::Add(asm_ast::TernaryArgs {
                                    dest: asm_ast::Value { str: TEMP_RIGHT_ADDR.into() },
                                    left: asm_ast::Value { str: TEMP_LEFT_ADDR.into() },
                                    right: asm_ast::Value { str: TEMP_RIGHT_ADDR.into() },
                                })),
                            ])
                        },
                        Command::Ref { var } => {
                            let Some(ref_offset) = var_to_offset.get(var) else {
                                bail!("Failed to find ref offset")
                            };

                            Vec::from([
                                // store addr for ref in temp_right
                                Arc::new(asm_ast::Command::Move(asm_ast::BinaryArgs {
                                    dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                    val: asm_ast::Value { str: STACK_POINTER_ADDR.into() },
                                })),
                                Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                                    dest: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                                    val: asm_ast::Value { str: ref_offset.to_string().into() },
                                })),
                                Arc::new(asm_ast::Command::Sub(asm_ast::TernaryArgs {
                                    dest: asm_ast::Value { str: TEMP_RIGHT_ADDR.into() },
                                    left: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                    right: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                                })),
                            ])
                        },
                    };

                    // set var addr to assign in temp_left
                    assign_asm_commands.extend(Vec::from([
                        Arc::new(asm_ast::Command::Move(asm_ast::BinaryArgs {
                            dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                            val: asm_ast::Value { str: STACK_POINTER_ADDR.into() },
                        })),
                        Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                            dest: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                            val: asm_ast::Value { str: var_offset.to_string().into() },
                        })),
                        Arc::new(asm_ast::Command::Sub(asm_ast::TernaryArgs {
                            dest: asm_ast::Value { str: TEMP_LEFT_ADDR.into() },
                            left: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                            right: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                        })),
                    ]));

                    // deref var addr if its a ref assign
                    if *ref_var {
                        assign_asm_commands.push(Arc::new(asm_ast::Command::MoveDerefSrc(
                            asm_ast::BinaryArgs {
                                dest: asm_ast::Value { str: TEMP_LEFT_ADDR.into() },
                                val: asm_ast::Value { str: TEMP_LEFT_ADDR.into() },
                            },
                        )));
                    }

                    // move value at addr in temp_right to addr in temp_left
                    assign_asm_commands.push(Arc::new(asm_ast::Command::MoveDerefDest(
                        asm_ast::BinaryArgs {
                            dest: asm_ast::Value { str: TEMP_LEFT_ADDR.into() },
                            val: asm_ast::Value { str: TEMP_RIGHT_ADDR.into() },
                        },
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

                        let asm_commands = Vec::from([
                            // store addr for call var in temp_left
                            Arc::new(asm_ast::Command::Move(asm_ast::BinaryArgs {
                                dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                val: asm_ast::Value { str: STACK_POINTER_ADDR.into() },
                            })),
                            Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                                dest: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                                val: asm_ast::Value { str: call_var_offset.to_string().into() },
                            })),
                            Arc::new(asm_ast::Command::Sub(asm_ast::TernaryArgs {
                                dest: asm_ast::Value { str: TEMP_LEFT_ADDR.into() },
                                left: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                right: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                            })),
                            // store addr for func param in temp_right
                            Arc::new(asm_ast::Command::Move(asm_ast::BinaryArgs {
                                dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                val: asm_ast::Value { str: STACK_POINTER_ADDR.into() },
                            })),
                            Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                                dest: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                                val: asm_ast::Value { str: (param_offset + 1).to_string().into() },
                            })),
                            Arc::new(asm_ast::Command::Add(asm_ast::TernaryArgs {
                                dest: asm_ast::Value { str: TEMP_RIGHT_ADDR.into() },
                                left: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                right: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                            })),
                            // move value in call var to func param
                            Arc::new(asm_ast::Command::MoveDerefSrc(asm_ast::BinaryArgs {
                                dest: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                                val: asm_ast::Value { str: TEMP_LEFT_ADDR.into() },
                            })),
                            Arc::new(asm_ast::Command::MoveDerefDest(asm_ast::BinaryArgs {
                                dest: asm_ast::Value { str: TEMP_RIGHT_ADDR.into() },
                                val: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                            })),
                        ]);

                        param_asm_commands.extend(asm_commands);
                    }

                    let broadcast_asm_commands = Vec::from([
                        // broadcast function message
                        Arc::new(asm_ast::Command::Set(BinaryArgs {
                            dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                            val: asm_ast::Value { str: format!("func_{}", func_item).into() },
                        })),
                        Arc::new(asm_ast::Command::Jump(JumpArgs {
                            src: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                        })),
                    ]);

                    param_asm_commands.into_iter().chain(broadcast_asm_commands).collect_vec()
                },
            };

            asm_commands.extend(asm_statement_commands);
        }

        // cleanup stack vars
        asm_commands.extend([
            Arc::new(asm_ast::Command::Move(asm_ast::BinaryArgs {
                dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                val: asm_ast::Value { str: STACK_POINTER_ADDR.into() },
            })),
            Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                dest: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                val: asm_ast::Value { str: stack_vars.len().to_string().into() },
            })),
            Arc::new(asm_ast::Command::Sub(asm_ast::TernaryArgs {
                dest: asm_ast::Value { str: STACK_POINTER_ADDR.into() },
                left: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                right: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
            })),
        ]);

        // finalize proc
        let asm_proc_kind = match item.as_ref() {
            Item::Main(_) => asm_ast::ProcedureKind::Sub { name: "main".into() },
            Item::Func(func) => {
                asm_ast::ProcedureKind::Sub { name: format!("func_{}", func.name).into() }
            },
        };

        let asm_proc = asm_ast::Procedure { kind: asm_proc_kind, commands: asm_commands };

        asm_procs.push(Arc::new(asm_proc));
    }

    Ok(asm_procs)
}
