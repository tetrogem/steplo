use anyhow::bail;
use asm_compiler::ast::{self as asm_ast, BinaryArgs, JumpArgs};
use std::{collections::HashMap, sync::Arc};

use crate::ast::{Command, Item, Statement};

pub fn compile(ast: Vec<Arc<Item>>) -> anyhow::Result<Vec<Arc<asm_ast::Procedure>>> {
    let mut asm_procs = Vec::<Arc<asm_ast::Procedure>>::new();

    const STACK_POINTER_ADDR: &str = "1";
    const TEMP_RESULT_ADDR: &str = "2";
    const TEMP_OPERAND_ADDR: &str = "3";

    let asm_main_setup = asm_ast::Procedure {
        kind: asm_ast::ProcedureKind::Main,
        commands: Vec::from([
            // init stack pointer
            Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                dest: asm_ast::Value { str: STACK_POINTER_ADDR.into() }, // stored in mem addr 1
                val: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() }, // points to last addr in stack (2)
            })),
            // init temp result
            Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() }, // stored in mem addr 2
                val: asm_ast::Value { str: "".into() },                // empty by default
            })),
            // init temp operand
            Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                dest: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() }, // stored in mem addr 3
                val: asm_ast::Value { str: "".into() },                 // empty by default
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
        let proc = match item.as_ref() {
            Item::Main(main) => &main.proc,
            Item::Func(func) => &func.proc,
        };

        let mut asm_commands = Vec::<Arc<asm_ast::Command>>::new();
        let mut var_to_offset = HashMap::<Arc<str>, usize>::new();

        // init stack vars
        for (offset, var) in proc.vars.iter().enumerate() {
            var_to_offset.insert(var.clone(), offset);
        }

        asm_commands.extend([
            Arc::new(asm_ast::Command::Move(asm_ast::BinaryArgs {
                dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                val: asm_ast::Value { str: STACK_POINTER_ADDR.into() },
            })),
            Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                dest: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                val: asm_ast::Value { str: proc.vars.len().to_string().into() },
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

                    match command.as_ref() {
                        Command::Literal(literal) => Vec::from([
                            Arc::new(asm_ast::Command::Move(asm_ast::BinaryArgs {
                                dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                val: asm_ast::Value { str: STACK_POINTER_ADDR.into() },
                            })),
                            Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                                dest: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                                val: asm_ast::Value { str: var_offset.to_string().into() },
                            })),
                            Arc::new(asm_ast::Command::Sub(asm_ast::TernaryArgs {
                                dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                left: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                right: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                            })),
                            Arc::new(asm_ast::Command::Set(asm_ast::BinaryArgs {
                                dest: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                                val: asm_ast::Value { str: literal.clone() },
                            })),
                            Arc::new(asm_ast::Command::MoveDerefDest(asm_ast::BinaryArgs {
                                dest: asm_ast::Value { str: TEMP_RESULT_ADDR.into() },
                                val: asm_ast::Value { str: TEMP_OPERAND_ADDR.into() },
                            })),
                        ]),
                    }
                },
                Statement::Call { func_item, cond_var } => todo!(),
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
                val: asm_ast::Value { str: proc.vars.len().to_string().into() },
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
