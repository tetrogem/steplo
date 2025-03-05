use std::{
    collections::{BTreeMap, HashMap},
    sync::Arc,
};

use anyhow::bail;
use asm_compiler::ast as asm;
use itertools::{chain, Itertools};
use uuid::Uuid;

use crate::{
    ast,
    designate::{RMemLoc, Register},
};

struct AsmProcManager {
    sp_uuid_to_asm_name: HashMap<Uuid, Arc<str>>,
}

impl AsmProcManager {
    fn new(procs: &Vec<Arc<ast::Proc<RMemLoc>>>) -> Self {
        let mut sp_uuid_to_asm_name = HashMap::new();

        for proc in procs {
            for (i, sp) in proc.sub_procs.iter().enumerate() {
                let asm_name = match proc.kind.as_ref() {
                    ast::ProcKind::Main => {
                        if i == 0 {
                            continue;
                        }

                        format!("main.{}", i)
                    },
                    ast::ProcKind::Func { name, .. } => format!("func.{}.{}", name, i),
                };

                let prev = sp_uuid_to_asm_name.insert(sp.uuid, asm_name.into());
                if prev.is_some() {
                    panic!("ASM name generated twice for the same sub-procedure UUID");
                }
            }
        }

        Self { sp_uuid_to_asm_name }
    }

    fn get_asm_kind(&self, sp_uuid: &Uuid) -> asm::ProcedureKind {
        self.sp_uuid_to_asm_name
            .get(sp_uuid)
            .map(|name| asm::ProcedureKind::Sub { name: name.clone() })
            .unwrap_or(asm::ProcedureKind::Main)
    }
}

struct RegisterManager {
    stack_pointer_register: Arc<asm::Register>,
    opt_to_asm: BTreeMap<Arc<Register>, Arc<asm::Register>>,
}

impl RegisterManager {
    fn new() -> Self {
        Self {
            stack_pointer_register: Arc::new(asm::Register { name: "sp".into() }),
            opt_to_asm: Default::default(),
        }
    }

    fn stack_pointer(&self) -> Arc<asm::Register> {
        self.stack_pointer_register.clone()
    }

    fn get_asm(&mut self, register: Arc<Register>) -> Arc<asm::Register> {
        let id = register.id;
        self.opt_to_asm
            .entry(register)
            .or_insert_with(|| Arc::new(asm::Register { name: format!("t{}", id).into() }))
            .clone()
    }

    fn into_asm_registers(self) -> Vec<Arc<asm::Register>> {
        chain!([self.stack_pointer_register], self.opt_to_asm.into_values()).collect()
    }
}

pub fn compile(procs: Vec<Arc<ast::Proc<RMemLoc>>>) -> anyhow::Result<Arc<asm::Program>> {
    let asm_proc_manager = AsmProcManager::new(&procs);
    let mut register_manager = RegisterManager::new();

    let procedures = procs
        .iter()
        .map(|p| compile_proc(&asm_proc_manager, &mut register_manager, p))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .flatten()
        .collect_vec();

    let program = asm::Program {
        procedures: Arc::new(procedures),
        registers: Arc::new(
            register_manager.into_asm_registers().iter().map(|r| r.name.clone()).collect(),
        ),
    };

    Ok(Arc::new(program))
}

fn compile_proc(
    asm_proc_manager: &AsmProcManager,
    register_manager: &mut RegisterManager,
    proc: &ast::Proc<RMemLoc>,
) -> anyhow::Result<Vec<Arc<asm::Procedure>>> {
    proc.sub_procs
        .iter()
        .map(|sp| compile_sub_proc(asm_proc_manager, register_manager, sp))
        .collect()
}

fn compile_sub_proc(
    asm_proc_manager: &AsmProcManager,
    register_manager: &mut RegisterManager,
    sp: &ast::SubProc<RMemLoc>,
) -> anyhow::Result<Arc<asm::Procedure>> {
    let kind = asm_proc_manager.get_asm_kind(&sp.uuid);

    let commands = sp
        .commands
        .iter()
        .map(|a| compile_command(asm_proc_manager, register_manager, a))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .flatten()
        .collect();

    let procedure = asm::Procedure { kind, commands };

    Ok(Arc::new(procedure))
}

fn compile_command(
    asm_proc_manager: &AsmProcManager,
    register_manager: &mut RegisterManager,
    command: &ast::Command<RMemLoc>,
) -> anyhow::Result<Vec<Arc<asm::Command>>> {
    let commands = match command {
        ast::Command::Set { dest, value } => {
            Vec::from([data(asm::DataCommand::Set(asm::BinaryArgs {
                dest: compile_mem_loc(register_manager, dest),
                val: compile_value(asm_proc_manager, value)?,
            }))])
        },
        ast::Command::Copy { dest, src } => {
            Vec::from([data(asm::DataCommand::Move(asm::BinaryArgs {
                dest: compile_mem_loc(register_manager, dest),
                val: compile_mem_loc(register_manager, src),
            }))])
        },
        ast::Command::CopyDerefDest { dest, src } => {
            Vec::from([data(asm::DataCommand::MoveDerefDest(asm::BinaryArgs {
                dest: compile_mem_loc(register_manager, dest),
                val: compile_mem_loc(register_manager, src),
            }))])
        },
        ast::Command::Deref { dest, src } => {
            Vec::from([data(asm::DataCommand::MoveDerefSrc(asm::BinaryArgs {
                dest: compile_mem_loc(register_manager, dest),
                val: compile_mem_loc(register_manager, src),
            }))])
        },
        ast::Command::Add { dest, left, right } => {
            Vec::from([data(asm::DataCommand::Add(asm::TernaryArgs {
                dest: compile_mem_loc(register_manager, dest),
                left: compile_mem_loc(register_manager, left),
                right: compile_mem_loc(register_manager, right),
            }))])
        },
        ast::Command::Sub { dest, left, right } => {
            Vec::from([data(asm::DataCommand::Sub(asm::TernaryArgs {
                dest: compile_mem_loc(register_manager, dest),
                left: compile_mem_loc(register_manager, left),
                right: compile_mem_loc(register_manager, right),
            }))])
        },
        ast::Command::Jump { src } => {
            Vec::from([control(asm::ControlCommand::Jump(asm::UnaryArgs {
                val: compile_mem_loc(register_manager, src),
            }))])
        },
        ast::Command::Out { src } => Vec::from([data(asm::DataCommand::Out(asm::UnaryArgs {
            val: compile_mem_loc(register_manager, src),
        }))]),
        ast::Command::In { dest } => Vec::from([data(asm::DataCommand::In(asm::UnaryArgs {
            val: compile_mem_loc(register_manager, dest),
        }))]),
        ast::Command::Exit => Vec::from([control(asm::ControlCommand::Exit)]),
    };

    Ok(commands)
}

fn compile_mem_loc(register_manager: &mut RegisterManager, mem_loc: &RMemLoc) -> Arc<asm::Value> {
    let value = match mem_loc {
        RMemLoc::StackPointer => asm::Value::Register(register_manager.stack_pointer()),
        RMemLoc::Register(register) => {
            asm::Value::Register(register_manager.get_asm(register.clone()))
        },
    };

    Arc::new(value)
}

fn compile_value(
    asm_proc_manager: &AsmProcManager,
    value: &ast::Value,
) -> anyhow::Result<Arc<asm::Value>> {
    let value = match value {
        ast::Value::Literal(literal) => {
            asm::Value::Literal(Arc::new(asm::Literal { val: literal.clone() }))
        },
        ast::Value::Label(sp_uuid) => {
            let asm_proc_kind = asm_proc_manager.get_asm_kind(sp_uuid);

            let name = match asm_proc_kind {
                asm::ProcedureKind::Main => bail!("attempted to create label value for main"),
                asm::ProcedureKind::Sub { name } => name.clone(),
            };

            asm::Value::Label(Arc::new(asm::Label { name }))
        },
    };

    Ok(Arc::new(value))
}

fn data(command: asm::DataCommand) -> Arc<asm::Command> {
    Arc::new(asm::Command::Data(Arc::new(command)))
}

fn control(command: asm::ControlCommand) -> Arc<asm::Command> {
    Arc::new(asm::Command::Control(Arc::new(command)))
}
