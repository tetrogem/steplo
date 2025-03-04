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

            let mut assignment_with_temp_frees = Vec::new();
            let mut freed_temps = BTreeSet::<Arc<ast::TempVar>>::new();

            // temps used in call don't need to be freed because the sub proc is exiting anyway
            // (and so their register could never be used again anyway)
            // only need to handle freeing temps used in statements
            // so, we will add them to freed_temps so the statement scanner below will never
            // choose to free them
            match sp.next_call.as_ref() {
                ast::Call::Func { name: _, params, return_sub_proc: _ } => {
                    for param in params.as_ref() {
                        if let ast::UMemLoc::Temp(temp) = param.as_ref() {
                            freed_temps.insert(temp.clone());
                        }
                    }
                },
                ast::Call::SubProc(_) => {},
                ast::Call::IfBranch { cond, then_sub_proc: _, pop_sub_proc: _ } => {
                    if let ast::UMemLoc::Temp(temp) = cond.as_ref() {
                        freed_temps.insert(temp.clone());
                    }
                },
                ast::Call::IfElseBranch { cond, then_sub_proc: _, else_sub_proc: _ } => {
                    if let ast::UMemLoc::Temp(temp) = cond.as_ref() {
                        freed_temps.insert(temp.clone());
                    }
                },
                ast::Call::Return => {},
                ast::Call::Terminate => {},
            };

            // find when temps are last used (iterate over statements in reverse)
            for assignment in sp.assignments.as_ref().iter().rev() {
                let add = |umems: &[&Arc<ast::UMemLoc>]| {
                    let mut temps = Vec::new();
                    for umem in umems {
                        if let ast::UMemLoc::Temp(temp) = umem.as_ref() {
                            temps.push(temp.clone());
                        }
                    }

                    temps
                };

                let mem_locs: &[&Arc<ast::UMemLoc>] = match assignment.expr.as_ref() {
                    ast::Expr::Set { literal: _ } => &[],
                    ast::Expr::Copy { src } => &[src],
                    ast::Expr::CopyDerefDest { src } => &[src],
                    ast::Expr::Deref { src } => &[src],
                    ast::Expr::Add { left, right } => &[left, right],
                };

                let mem_locs = chain!([&assignment.dest], mem_locs.iter().copied()).collect_vec();
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

                assignment_with_temp_frees.push((assignment, temps_freed))
            }

            // convert statements umem -> rmem
            let mut assignments = Vec::new();
            for (assignment, temp_frees) in assignment_with_temp_frees.into_iter().rev() {
                let expr = match assignment.expr.as_ref() {
                    ast::Expr::Set { literal: value } => ast::Expr::Set { literal: value.clone() },
                    ast::Expr::Copy { src } => ast::Expr::Copy { src: temp_m.rmem(src) },
                    ast::Expr::CopyDerefDest { src } => {
                        ast::Expr::CopyDerefDest { src: temp_m.rmem(src) }
                    },
                    ast::Expr::Deref { src } => ast::Expr::Deref { src: temp_m.rmem(src) },
                    ast::Expr::Add { left, right } => {
                        ast::Expr::Add { left: temp_m.rmem(left), right: temp_m.rmem(right) }
                    },
                };

                let assignment =
                    ast::Assignment { dest: temp_m.rmem(&assignment.dest), expr: Arc::new(expr) };

                assignments.push(Arc::new(assignment));

                // free the registers of temps which will never be used again
                for temp in temp_frees {
                    temp_m.free(&temp);
                }
            }

            // convert next_call umem -> rmem
            let next_call = match sp.next_call.as_ref() {
                ast::Call::Func { name, params, return_sub_proc } => ast::Call::Func {
                    name: name.clone(),
                    params: Arc::new(params.iter().map(|param| temp_m.rmem(param)).collect_vec()),
                    return_sub_proc: *return_sub_proc,
                },
                ast::Call::SubProc(uuid) => ast::Call::SubProc(*uuid),
                ast::Call::IfBranch { cond, then_sub_proc, pop_sub_proc } => ast::Call::IfBranch {
                    cond: temp_m.rmem(cond),
                    then_sub_proc: *then_sub_proc,
                    pop_sub_proc: *pop_sub_proc,
                },
                ast::Call::IfElseBranch { cond, then_sub_proc, else_sub_proc } => {
                    ast::Call::IfElseBranch {
                        cond: temp_m.rmem(cond),
                        then_sub_proc: *then_sub_proc,
                        else_sub_proc: *else_sub_proc,
                    }
                },
                ast::Call::Return => ast::Call::Return,
                ast::Call::Terminate => ast::Call::Terminate,
            };

            let sp = ast::SubProc {
                uuid: sp.uuid,
                next_call: Arc::new(next_call),
                assignments: Arc::new(assignments),
            };

            sps.push(Arc::new(sp));
            reg_m.free_all();
        }

        let proc = ast::Proc { kind: proc.kind.clone(), sub_procs: Arc::new(sps) };

        procs.push(Arc::new(proc));
    }

    procs
}
