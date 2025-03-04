use std::{
    collections::{BTreeMap, HashMap},
    sync::Arc,
};

use asm_compiler::ast as asm;
use itertools::chain;
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

pub fn compile(procs: Vec<Arc<ast::Proc<RMemLoc>>>) -> Arc<asm::Program> {
    let asm_proc_manager = AsmProcManager::new(&procs);
    let mut register_manager = RegisterManager::new();

    let procedures = procs
        .iter()
        .flat_map(|p| compile_proc(&asm_proc_manager, &mut register_manager, p))
        .collect();

    let program = asm::Program {
        procedures: Arc::new(procedures),
        registers: Arc::new(
            register_manager.into_asm_registers().iter().map(|r| r.name.clone()).collect(),
        ),
    };

    Arc::new(program)
}

fn compile_proc(
    asm_proc_manager: &AsmProcManager,
    register_manager: &mut RegisterManager,
    proc: &ast::Proc<RMemLoc>,
) -> Vec<Arc<asm::Procedure>> {
    proc.sub_procs
        .iter()
        .map(|sp| compile_sub_proc(asm_proc_manager, register_manager, sp))
        .collect()
}

fn compile_sub_proc(
    asm_proc_manager: &AsmProcManager,
    register_manager: &mut RegisterManager,
    sp: &ast::SubProc<RMemLoc>,
) -> Arc<asm::Procedure> {
    let kind = asm_proc_manager.get_asm_kind(&sp.uuid);

    let commands =
        sp.assignments.iter().flat_map(|a| compile_assignment(register_manager, a)).collect();

    let procedure = asm::Procedure { kind, commands };

    Arc::new(procedure)
}

fn compile_assignment(
    register_manager: &mut RegisterManager,
    assignment: &ast::Assignment<RMemLoc>,
) -> Vec<Arc<asm::Command>> {
    let dest_asm = compile_mem_loc(register_manager, &assignment.dest);

    let expr_commands = match assignment.expr.as_ref() {
        ast::Expr::Set { literal } => Vec::from([asm::DataCommand::Set(asm::BinaryArgs {
            dest: dest_asm,
            val: Arc::new(asm::Value::Literal(Arc::new(asm::Literal {
                val: literal.value.clone(),
            }))),
        })]),
        ast::Expr::Copy { src } => Vec::from([asm::DataCommand::Move(asm::BinaryArgs {
            dest: dest_asm,
            val: compile_mem_loc(register_manager, src),
        })]),
        ast::Expr::CopyDerefDest { src } => {
            Vec::from([asm::DataCommand::MoveDerefDest(asm::BinaryArgs {
                dest: dest_asm,
                val: compile_mem_loc(register_manager, src),
            })])
        },
        ast::Expr::Deref { src } => Vec::from([asm::DataCommand::MoveDerefSrc(asm::BinaryArgs {
            dest: dest_asm,
            val: compile_mem_loc(register_manager, src),
        })]),
        ast::Expr::Add { left, right } => Vec::from([asm::DataCommand::Add(asm::TernaryArgs {
            dest: dest_asm,
            left: compile_mem_loc(register_manager, left),
            right: compile_mem_loc(register_manager, right),
        })]),
    };

    chain!(expr_commands).map(|c| Arc::new(asm::Command::Data(Arc::new(c)))).collect()
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
