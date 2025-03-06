use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use itertools::{chain, Itertools};

use crate::ast;

#[derive(Default)]
struct RegisterManager {
    free: BTreeSet<Arc<Register>>,
    used: BTreeSet<Arc<Register>>,
}

impl RegisterManager {
    pub fn alloc(&mut self) -> Arc<Register> {
        let next = self.free.pop_first().unwrap_or_else(|| Arc::new(Register::new(self.len())));

        let inserted = self.used.insert(next.clone());
        if !inserted {
            panic!("Attempted to use already used register");
        }

        next
    }

    pub fn free(&mut self, register: &Arc<Register>) {
        match self.used.take(register) {
            None => panic!("Attempted to free unused register"),
            Some(register) => {
                let inserted = self.free.insert(register);
                if !inserted {
                    panic!("Attempted to free already freed register");
                }
            },
        }
    }

    pub fn free_all(&mut self) {
        while let Some(register) = self.used.pop_first() {
            let inserted = self.free.insert(register);
            if !inserted {
                panic!("Attempted to free already freed register");
            }
        }
    }

    pub fn len(&self) -> usize {
        self.used.len() + self.free.len()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Register {
    pub id: usize,
}

impl Register {
    fn new(id: usize) -> Self {
        Self { id }
    }
}

#[derive(Debug)]
pub enum RMemLoc {
    StackPointer,
    Register(Arc<Register>),
    // Stack { var: Arc<ast::StackVar>, offset: Option<Arc<RMemLoc>> },
}

struct TempManager<'a> {
    temp_to_register: BTreeMap<Arc<ast::TempVar>, Arc<Register>>,
    register_manager: &'a mut RegisterManager,
}

impl TempManager<'_> {
    fn rmem(&mut self, umem: &ast::UMemLoc) -> Arc<RMemLoc> {
        let rmem = match umem {
            ast::UMemLoc::StackPointer => RMemLoc::StackPointer,
            ast::UMemLoc::Temp(temp) => match self.temp_to_register.get(temp) {
                Some(register) => RMemLoc::Register(register.clone()),
                None => {
                    let register = self.register_manager.alloc();
                    let previous = self.temp_to_register.insert(temp.clone(), register.clone());
                    if previous.is_some() {
                        panic!("Tried to assign register to temp var twice");
                    }

                    RMemLoc::Register(register)
                },
            },
        };

        Arc::new(rmem)
    }

    fn free(&mut self, temp: &ast::TempVar) {
        let Some(register) = self.temp_to_register.remove(temp) else {
            panic!("Attempted to free a temp var that was never allocated");
        };

        self.register_manager.free(&register);
    }
}

pub fn designate_registers(
    ast: &Vec<Arc<ast::Proc<ast::UMemLoc>>>,
) -> Vec<Arc<ast::Proc<RMemLoc>>> {
    let mut reg_m = RegisterManager::default();

    let mut procs = Vec::new();
    for proc in ast {
        let mut sps = Vec::new();
        for sp in proc.sub_procs.as_ref() {
            let mut temp_m =
                TempManager { temp_to_register: Default::default(), register_manager: &mut reg_m };

            let mut command_with_temp_frees = Vec::new();
            let mut freed_temps = BTreeSet::<Arc<ast::TempVar>>::new();

            // find when temps are last used (iterate over statements in reverse)
            for command in sp.commands.as_ref().iter().rev() {
                let add = |umems: &[Arc<ast::UMemLoc>]| {
                    let mut temps = Vec::new();
                    for umem in umems {
                        if let ast::UMemLoc::Temp(temp) = umem.as_ref() {
                            temps.push(temp.clone());
                        }
                    }

                    temps
                };

                let mem_locs = match command.as_ref() {
                    ast::Command::Set(args) => args.to_mem_locs(),
                    ast::Command::Copy(args) => args.to_mem_locs(),
                    ast::Command::CopyDerefDest(args) => args.to_mem_locs(),
                    ast::Command::Deref(args) => args.to_mem_locs(),
                    ast::Command::Add(args) => args.to_mem_locs(),
                    ast::Command::Sub(args) => args.to_mem_locs(),
                    ast::Command::Mul(args) => args.to_mem_locs(),
                    ast::Command::Div(args) => args.to_mem_locs(),
                    ast::Command::Mod(args) => args.to_mem_locs(),
                    ast::Command::Jump(args) => args.to_mem_locs(),
                    ast::Command::Out(args) => args.to_mem_locs(),
                    ast::Command::In(args) => args.to_mem_locs(),
                    ast::Command::Exit => Vec::new(),
                    ast::Command::Branch(args) => args.to_mem_locs(),
                    ast::Command::Eq(args) => args.to_mem_locs(),
                    ast::Command::Lte(args) => args.to_mem_locs(),
                    ast::Command::Neq(args) => args.to_mem_locs(),
                    ast::Command::Not(args) => args.to_mem_locs(),
                };

                let temps = add(&mem_locs);

                // since we're iterating in reverse, we will encounter each temp when its used last
                let mut temps_freed = Vec::new();
                for temp in temps {
                    // if its inserted, this is the first time it was seen (in reverse)
                    // meaning this is the last time it will be used
                    if freed_temps.insert(temp.clone()) {
                        temps_freed.push(temp);
                    }
                }

                command_with_temp_frees.push((command, temps_freed))
            }

            // convert statements umem -> rmem
            let mut commands = Vec::new();
            for (command, temp_frees) in command_with_temp_frees.into_iter().rev() {
                let temp_m = &mut temp_m;
                let command = match command.as_ref() {
                    ast::Command::Set(args) => ast::Command::Set(args.rmem(temp_m)),
                    ast::Command::Copy(args) => ast::Command::Copy(args.rmem(temp_m)),
                    ast::Command::CopyDerefDest(args) => {
                        ast::Command::CopyDerefDest(args.rmem(temp_m))
                    },
                    ast::Command::Deref(args) => ast::Command::Deref(args.rmem(temp_m)),
                    ast::Command::Add(args) => ast::Command::Add(args.rmem(temp_m)),
                    ast::Command::Sub(args) => ast::Command::Sub(args.rmem(temp_m)),
                    ast::Command::Mul(args) => ast::Command::Mul(args.rmem(temp_m)),
                    ast::Command::Div(args) => ast::Command::Div(args.rmem(temp_m)),
                    ast::Command::Mod(args) => ast::Command::Mod(args.rmem(temp_m)),
                    ast::Command::Jump(args) => ast::Command::Jump(args.rmem(temp_m)),
                    ast::Command::Out(args) => ast::Command::Out(args.rmem(temp_m)),
                    ast::Command::In(args) => ast::Command::In(args.rmem(temp_m)),
                    ast::Command::Exit => ast::Command::Exit,
                    ast::Command::Branch(args) => ast::Command::Branch(args.rmem(temp_m)),
                    ast::Command::Eq(args) => ast::Command::Eq(args.rmem(temp_m)),
                    ast::Command::Lte(args) => ast::Command::Lte(args.rmem(temp_m)),
                    ast::Command::Neq(args) => ast::Command::Neq(args.rmem(temp_m)),
                    ast::Command::Not(args) => ast::Command::Not(args.rmem(temp_m)),
                };

                commands.push(Arc::new(command));

                // free the registers of temps which will never be used again
                for temp in temp_frees {
                    temp_m.free(&temp);
                }
            }

            let sp = ast::SubProc { uuid: sp.uuid, commands: Arc::new(commands) };

            sps.push(Arc::new(sp));
            reg_m.free_all();
        }

        let proc = ast::Proc { kind: proc.kind.clone(), sub_procs: Arc::new(sps) };

        procs.push(Arc::new(proc));
    }

    procs
}

trait ArgsExt {
    type RMem;

    fn to_mem_locs(&self) -> Vec<Arc<ast::UMemLoc>>;
    fn rmem(&self, temp_m: &mut TempManager) -> Arc<Self::RMem>;
}

impl ArgsExt for ast::SetArgs<ast::UMemLoc> {
    type RMem = ast::SetArgs<RMemLoc>;

    fn to_mem_locs(&self) -> Vec<Arc<ast::UMemLoc>> {
        Vec::from([self.dest.clone()])
    }

    fn rmem(&self, temp_m: &mut TempManager) -> Arc<Self::RMem> {
        Arc::new(ast::SetArgs { dest: temp_m.rmem(&self.dest), value: self.value.clone() })
    }
}

impl ArgsExt for ast::UnaryArgs<ast::UMemLoc> {
    type RMem = ast::UnaryArgs<RMemLoc>;

    fn to_mem_locs(&self) -> Vec<Arc<ast::UMemLoc>> {
        Vec::from([self.dest.clone(), self.src.clone()])
    }

    fn rmem(&self, temp_m: &mut TempManager) -> Arc<Self::RMem> {
        Arc::new(ast::UnaryArgs { dest: temp_m.rmem(&self.dest), src: temp_m.rmem(&self.src) })
    }
}

impl ArgsExt for ast::BinaryArgs<ast::UMemLoc> {
    type RMem = ast::BinaryArgs<RMemLoc>;

    fn to_mem_locs(&self) -> Vec<Arc<ast::UMemLoc>> {
        Vec::from([self.dest.clone(), self.left.clone(), self.right.clone()])
    }

    fn rmem(&self, temp_m: &mut TempManager) -> Arc<Self::RMem> {
        Arc::new(ast::BinaryArgs {
            dest: temp_m.rmem(&self.dest),
            left: temp_m.rmem(&self.left),
            right: temp_m.rmem(&self.right),
        })
    }
}

impl ArgsExt for ast::VoidArgs<ast::UMemLoc> {
    type RMem = ast::VoidArgs<RMemLoc>;

    fn to_mem_locs(&self) -> Vec<Arc<ast::UMemLoc>> {
        Vec::from([self.src.clone()])
    }

    fn rmem(&self, temp_m: &mut TempManager) -> Arc<Self::RMem> {
        Arc::new(ast::VoidArgs { src: temp_m.rmem(&self.src) })
    }
}

impl ArgsExt for ast::InputArgs<ast::UMemLoc> {
    type RMem = ast::InputArgs<RMemLoc>;

    fn to_mem_locs(&self) -> Vec<Arc<ast::UMemLoc>> {
        Vec::from([self.dest.clone()])
    }

    fn rmem(&self, temp_m: &mut TempManager) -> Arc<Self::RMem> {
        Arc::new(ast::InputArgs { dest: temp_m.rmem(&self.dest) })
    }
}

impl ArgsExt for ast::CondArgs<ast::UMemLoc> {
    type RMem = ast::CondArgs<RMemLoc>;

    fn to_mem_locs(&self) -> Vec<Arc<ast::UMemLoc>> {
        Vec::from([self.cond.clone(), self.src.clone()])
    }

    fn rmem(&self, temp_m: &mut TempManager) -> Arc<Self::RMem> {
        Arc::new(ast::CondArgs { cond: temp_m.rmem(&self.cond), src: temp_m.rmem(&self.src) })
    }
}
