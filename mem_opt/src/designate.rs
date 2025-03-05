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
                let add = |umems: &[&Arc<ast::UMemLoc>]| {
                    let mut temps = Vec::new();
                    for umem in umems {
                        if let ast::UMemLoc::Temp(temp) = umem.as_ref() {
                            temps.push(temp.clone());
                        }
                    }

                    temps
                };

                let mem_locs: &[&Arc<ast::UMemLoc>] = match command.as_ref() {
                    ast::Command::Set { dest, value: _ } => &[dest],
                    ast::Command::Copy { dest, src } => &[dest, src],
                    ast::Command::CopyDerefDest { dest, src } => &[dest, src],
                    ast::Command::Deref { dest, src } => &[dest, src],
                    ast::Command::Add { dest, left, right } => &[dest, left, right],
                    ast::Command::Sub { dest, left, right } => &[dest, left, right],
                    ast::Command::Jump { src } => &[src],
                    ast::Command::Out { src } => &[src],
                    ast::Command::In { dest } => &[dest],
                    ast::Command::Exit => &[],
                };

                let mem_locs = mem_locs.iter().copied().collect_vec();
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
                let command = match command.as_ref() {
                    ast::Command::Set { dest, value } => {
                        ast::Command::Set { dest: temp_m.rmem(dest), value: value.clone() }
                    },
                    ast::Command::Copy { dest, src } => {
                        ast::Command::Copy { dest: temp_m.rmem(dest), src: temp_m.rmem(src) }
                    },
                    ast::Command::CopyDerefDest { dest, src } => ast::Command::CopyDerefDest {
                        dest: temp_m.rmem(dest),
                        src: temp_m.rmem(src),
                    },
                    ast::Command::Deref { dest, src } => {
                        ast::Command::Deref { dest: temp_m.rmem(dest), src: temp_m.rmem(src) }
                    },
                    ast::Command::Add { dest, left, right } => ast::Command::Add {
                        dest: temp_m.rmem(dest),
                        left: temp_m.rmem(left),
                        right: temp_m.rmem(right),
                    },
                    ast::Command::Sub { dest, left, right } => ast::Command::Sub {
                        dest: temp_m.rmem(dest),
                        left: temp_m.rmem(left),
                        right: temp_m.rmem(right),
                    },
                    ast::Command::Jump { src } => ast::Command::Jump { src: temp_m.rmem(src) },
                    ast::Command::Out { src } => ast::Command::Out { src: temp_m.rmem(src) },
                    ast::Command::In { dest } => ast::Command::In { dest: temp_m.rmem(dest) },
                    ast::Command::Exit => ast::Command::Exit,
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
