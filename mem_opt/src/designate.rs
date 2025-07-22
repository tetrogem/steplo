use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use itertools::chain;

use crate::ast::{self, Call};

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

            let call_umem_locs = match sp.call.as_ref() {
                ast::Call::Exit => Default::default(),
                ast::Call::Jump(to) => to.to_mem_locs(),
                ast::Call::Branch { cond, then_to, else_to } => {
                    chain!(cond.to_mem_locs(), then_to.to_mem_locs(), else_to.to_mem_locs())
                        .collect()
                },
            };

            for umem in call_umem_locs {
                if let ast::UMemLoc::Temp(temp) = umem.as_ref() {
                    freed_temps.insert(temp.clone());
                }
            }

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
                    ast::Command::SetMemLoc { mem_loc, val } => {
                        chain!([mem_loc.clone()], val.to_mem_locs()).collect()
                    },
                    ast::Command::SetStack { addr, val } => {
                        chain!(addr.to_mem_locs(), val.to_mem_locs()).collect()
                    },
                    ast::Command::In => Default::default(),
                    ast::Command::Out(expr) => expr.to_mem_locs(),
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
                    ast::Command::SetMemLoc { mem_loc, val } => ast::Command::SetMemLoc {
                        mem_loc: mem_loc.to_rmem(temp_m),
                        val: val.to_rmem(temp_m),
                    },
                    ast::Command::SetStack { addr, val } => ast::Command::SetStack {
                        addr: addr.to_rmem(temp_m),
                        val: val.to_rmem(temp_m),
                    },
                    ast::Command::In => ast::Command::In,
                    ast::Command::Out(expr) => ast::Command::Out(expr.to_rmem(temp_m)),
                };

                commands.push(Arc::new(command));

                // free the registers of temps which will never be used again
                for temp in temp_frees {
                    temp_m.free(&temp);
                }
            }

            let call = match sp.call.as_ref() {
                ast::Call::Exit => Call::Exit,
                ast::Call::Jump(to) => Call::Jump(to.to_rmem(&mut temp_m)),
                ast::Call::Branch { cond, then_to, else_to } => Call::Branch {
                    cond: cond.to_rmem(&mut temp_m),
                    then_to: then_to.to_rmem(&mut temp_m),
                    else_to: else_to.to_rmem(&mut temp_m),
                },
            };

            let call_umem_locs = match sp.call.as_ref() {
                ast::Call::Exit => Vec::new(),
                ast::Call::Jump(to) => to.to_mem_locs(),
                ast::Call::Branch { cond, then_to, else_to } => {
                    chain!(cond.to_mem_locs(), then_to.to_mem_locs(), else_to.to_mem_locs())
                        .collect()
                },
            };

            for umem in call_umem_locs {
                if let ast::UMemLoc::Temp(temp) = umem.as_ref() {
                    temp_m.free(temp);
                }
            }

            let sp =
                ast::SubProc { uuid: sp.uuid, commands: Arc::new(commands), call: Arc::new(call) };

            sps.push(Arc::new(sp));
            reg_m.free_all();
        }

        let proc = ast::Proc { kind: proc.kind.clone(), sub_procs: Arc::new(sps) };

        procs.push(Arc::new(proc));
    }

    procs
}

trait HasMemLocs {
    type RMem;

    fn to_mem_locs(&self) -> Vec<Arc<ast::UMemLoc>>;
    fn to_rmem(&self, temp_m: &mut TempManager) -> Arc<Self::RMem>;
}

impl HasMemLocs for ast::Expr<ast::UMemLoc> {
    type RMem = ast::Expr<RMemLoc>;

    fn to_mem_locs(&self) -> Vec<Arc<ast::UMemLoc>> {
        match self {
            ast::Expr::MemLoc(mem_loc) => Vec::from([mem_loc.clone()]),
            ast::Expr::Value(_) => Default::default(),
            ast::Expr::Deref(expr) => expr.to_mem_locs(),
            ast::Expr::Add(args) => args.to_mem_locs(),
            ast::Expr::Sub(args) => args.to_mem_locs(),
            ast::Expr::Mul(args) => args.to_mem_locs(),
            ast::Expr::Div(args) => args.to_mem_locs(),
            ast::Expr::Mod(args) => args.to_mem_locs(),
            ast::Expr::Eq(args) => args.to_mem_locs(),
            ast::Expr::Gt(args) => args.to_mem_locs(),
            ast::Expr::Lt(args) => args.to_mem_locs(),
            ast::Expr::Not(expr) => expr.to_mem_locs(),
            ast::Expr::Or(args) => args.to_mem_locs(),
            ast::Expr::And(args) => args.to_mem_locs(),
            ast::Expr::InAnswer => Default::default(),
            ast::Expr::Join(args) => args.to_mem_locs(),
            ast::Expr::Random(args) => args.to_mem_locs(),
        }
    }

    fn to_rmem(&self, temp_m: &mut TempManager) -> Arc<Self::RMem> {
        let expr = match self {
            ast::Expr::MemLoc(umem) => ast::Expr::MemLoc(umem.to_rmem(temp_m)),
            ast::Expr::Value(value) => ast::Expr::Value(value.clone()),
            ast::Expr::Deref(expr) => ast::Expr::Deref(expr.to_rmem(temp_m)),
            ast::Expr::Add(args) => ast::Expr::Add(args.to_rmem(temp_m)),
            ast::Expr::Sub(args) => ast::Expr::Sub(args.to_rmem(temp_m)),
            ast::Expr::Mul(args) => ast::Expr::Mul(args.to_rmem(temp_m)),
            ast::Expr::Div(args) => ast::Expr::Div(args.to_rmem(temp_m)),
            ast::Expr::Mod(args) => ast::Expr::Mod(args.to_rmem(temp_m)),
            ast::Expr::Eq(args) => ast::Expr::Eq(args.to_rmem(temp_m)),
            ast::Expr::Gt(args) => ast::Expr::Gt(args.to_rmem(temp_m)),
            ast::Expr::Lt(args) => ast::Expr::Lt(args.to_rmem(temp_m)),
            ast::Expr::Not(expr) => ast::Expr::Not(expr.to_rmem(temp_m)),
            ast::Expr::Or(args) => ast::Expr::Or(args.to_rmem(temp_m)),
            ast::Expr::And(args) => ast::Expr::And(args.to_rmem(temp_m)),
            ast::Expr::InAnswer => ast::Expr::InAnswer,
            ast::Expr::Join(args) => ast::Expr::Join(args.to_rmem(temp_m)),
            ast::Expr::Random(args) => ast::Expr::Random(args.to_rmem(temp_m)),
        };

        Arc::new(expr)
    }
}

impl HasMemLocs for ast::BinaryArgs<ast::UMemLoc> {
    type RMem = ast::BinaryArgs<RMemLoc>;

    fn to_mem_locs(&self) -> Vec<Arc<ast::UMemLoc>> {
        chain!(self.left.to_mem_locs(), self.right.to_mem_locs()).collect()
    }

    fn to_rmem(&self, temp_m: &mut TempManager) -> Arc<Self::RMem> {
        Arc::new(ast::BinaryArgs {
            left: self.left.to_rmem(temp_m),
            right: self.right.to_rmem(temp_m),
        })
    }
}

impl HasMemLocs for Arc<ast::UMemLoc> {
    type RMem = RMemLoc;

    fn to_mem_locs(&self) -> Vec<Arc<ast::UMemLoc>> {
        Vec::from([self.clone()])
    }

    fn to_rmem(&self, temp_m: &mut TempManager) -> Arc<Self::RMem> {
        temp_m.rmem(self)
    }
}
