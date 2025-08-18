use std::sync::Arc;

use itertools::chain;

use crate::inline::ast as a;
use crate::inline::compile_to_mem_opt::func_manager::FuncManager;
use crate::inline::compile_to_mem_opt::stack_manager::{ProcKind, StackManager, VarInfo};
use crate::mem_opt::ast as o;

pub fn compile(procs: &[Arc<a::Proc>]) -> anyhow::Result<Vec<Arc<o::Proc<o::UMemLoc>>>> {
    let stack_m = StackManager::new(procs);
    let func_m = FuncManager::new(procs);

    procs
        .iter()
        .map(|proc| compile_proc(proc, &stack_m, &func_m).map(Arc::new))
        .collect::<Result<_, _>>()
}

mod stack_manager {
    use itertools::chain;
    use uuid::Uuid;

    use crate::inline::ast as a;
    use std::{collections::BTreeMap, sync::Arc};

    pub struct StackManager {
        proc_kind_to_stack: BTreeMap<ProcKind, Arc<Frame>>,
    }

    impl StackManager {
        pub fn new(procs: &[Arc<a::Proc>]) -> Self {
            let proc_kind_to_stack = procs
                .iter()
                .map(|proc| {
                    let proc_kind = match proc.kind.as_ref() {
                        a::ProcKind::Main => ProcKind::Main,
                        a::ProcKind::Func { name } => ProcKind::Func { name: name.clone() },
                    };

                    let stack = Frame::new(proc);

                    (proc_kind, Arc::new(stack))
                })
                .collect();

            Self { proc_kind_to_stack }
        }

        pub fn get(&self, proc_kind: &ProcKind) -> anyhow::Result<&Arc<Frame>> {
            self.proc_kind_to_stack
                .get(proc_kind)
                .ok_or_else(|| anyhow::anyhow!("Proc kind {proc_kind:?} not found"))
        }
    }

    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub enum ProcKind {
        Main,
        Func { name: Arc<str> },
    }

    pub struct Frame {
        var_uuid_to_info: BTreeMap<Uuid, VarInfo>,
        size: u32,
    }

    #[derive(Clone, Copy, Debug)]
    pub struct VarInfo {
        pub frame_offset: u32,
        pub size: u32,
    }

    impl Frame {
        fn new(proc: &a::Proc) -> Self {
            let mut var_uuid_to_info = BTreeMap::new();

            let infos = chain!(proc.ordered_arg_infos.iter(), proc.ordered_local_infos.iter());
            let mut frame_size = 0;

            for info in infos {
                let var_info = VarInfo { frame_offset: frame_size, size: info.size };
                var_uuid_to_info.insert(info.uuid, var_info);
                frame_size += info.size;
            }

            Self { var_uuid_to_info, size: frame_size }
        }

        pub fn resolve(&self, var_uuid: Uuid) -> anyhow::Result<VarInfo> {
            self.var_uuid_to_info
                .get(&var_uuid)
                .copied()
                .ok_or_else(|| anyhow::anyhow!("Var not found in frame: {var_uuid:?}"))
        }

        pub fn size(&self) -> u32 {
            self.size
        }
    }
}

mod func_manager {
    use std::{collections::BTreeMap, sync::Arc};

    use uuid::Uuid;

    use crate::inline::ast as a;

    pub struct FuncManager {
        func_name_to_head_sp_uuid: BTreeMap<Arc<str>, Uuid>,
    }

    impl FuncManager {
        pub fn new(procs: &[Arc<a::Proc>]) -> Self {
            let func_name_to_head_sp_uuid = procs
                .iter()
                .filter_map(|proc| {
                    let func_name = match proc.kind.as_ref() {
                        a::ProcKind::Main => return None,
                        a::ProcKind::Func { name } => name,
                    };

                    let head_sp = proc.sub_procs.iter().next()?;

                    Some((func_name.clone(), head_sp.uuid))
                })
                .collect();

            Self { func_name_to_head_sp_uuid }
        }

        pub fn get_head_sp_uuid(&self, func_name: &Arc<str>) -> anyhow::Result<Uuid> {
            self.func_name_to_head_sp_uuid
                .get(func_name)
                .copied()
                .ok_or_else(|| anyhow::anyhow!("No head sp found for func: {func_name}"))
        }
    }
}

fn compile_proc(
    proc: &a::Proc,
    stack_m: &StackManager,
    func_m: &FuncManager,
) -> anyhow::Result<o::Proc<o::UMemLoc>> {
    let (o_proc_kind, proc_kind) = match proc.kind.as_ref() {
        a::ProcKind::Main => (o::ProcKind::Main, ProcKind::Main),
        a::ProcKind::Func { name } => {
            (o::ProcKind::Func { name: name.clone() }, ProcKind::Func { name: name.clone() })
        },
    };

    let sub_procs = proc
        .sub_procs
        .iter()
        .enumerate()
        .map(|(i, sp)| compile_sub_proc(sp, stack_m, &proc_kind, i == 0, func_m).map(Arc::new))
        .collect::<Result<_, _>>()?;

    Ok(o::Proc { kind: Arc::new(o_proc_kind), sub_procs: Arc::new(sub_procs) })
}

fn compile_sub_proc(
    sp: &a::SubProc,
    stack_m: &StackManager,
    proc_kind: &ProcKind,
    is_head: bool,
    func_m: &FuncManager,
) -> anyhow::Result<o::SubProc<o::UMemLoc>> {
    let is_main_head = matches!(proc_kind, ProcKind::Main) && is_head;
    let program_setup_commands = match is_main_head {
        false => Vec::new().into_iter(),
        true => Vec::from([o::Command::SetMemLoc {
            mem_loc: Arc::new(o::UMemLoc::StackPointer),
            val: Arc::new(o::Expr::Value(Arc::new(o::Value::Literal(0.to_string().into())))),
        }])
        .into_iter(),
    };

    let stack_setup_commands = match is_head {
        false => Vec::new().into_iter(),
        true => {
            let frame = stack_m.get(proc_kind)?;

            Vec::from([o::Command::SetMemLoc {
                mem_loc: Arc::new(o::UMemLoc::StackPointer),
                val: Arc::new(o::Expr::Add(Arc::new(o::BinaryArgs {
                    left: Arc::new(o::Expr::MemLoc(Arc::new(o::UMemLoc::StackPointer))),
                    right: Arc::new(o::Expr::Value(Arc::new(o::Value::Literal(
                        frame.size().to_string().into(),
                    )))),
                }))),
            }])
            .into_iter()
        },
    };

    let user_commands = sp
        .commands
        .iter()
        .map(|x| compile_command(x, stack_m, proc_kind))
        .collect::<Result<Vec<_>, _>>()?;

    let compiled_call = compile_call(sp.call.as_ref(), stack_m, proc_kind, func_m)?;

    let commands = chain!(
        program_setup_commands,
        stack_setup_commands,
        user_commands,
        compiled_call.prereq_commands
    )
    .map(Arc::new)
    .collect();
    let call = compiled_call.call;

    Ok(o::SubProc { uuid: sp.uuid, commands: Arc::new(commands), call: Arc::new(call) })
}

fn compile_command(
    command: &a::Command,
    stack_m: &StackManager,
    proc_kind: &ProcKind,
) -> anyhow::Result<o::Command<o::UMemLoc>> {
    Ok(match command {
        a::Command::SetLoc { loc, val } => {
            let val = Arc::new(compile_expr(val, stack_m, proc_kind)?);
            match loc.as_ref() {
                a::Loc::Deref(addr) => o::Command::SetStack {
                    addr: Arc::new(compile_expr(addr, stack_m, proc_kind)?),
                    val,
                },
                a::Loc::Temp(temp) => o::Command::SetMemLoc {
                    mem_loc: Arc::new(o::UMemLoc::Temp(Arc::new(compile_temp(temp)))),
                    val,
                },
            }
        },
        a::Command::Out(expr) => o::Command::Out(Arc::new(compile_expr(expr, stack_m, proc_kind)?)),
        a::Command::In => o::Command::In,
        a::Command::ClearStdout => o::Command::ClearStdout,
        a::Command::WriteStdout { index, val } => o::Command::WriteStdout {
            index: Arc::new(compile_expr(index, stack_m, proc_kind)?),
            val: Arc::new(compile_expr(val, stack_m, proc_kind)?),
        },
    })
}

fn compile_call(
    call: &a::Call,
    stack_m: &StackManager,
    proc_kind: &ProcKind,
    func_m: &FuncManager,
) -> anyhow::Result<CompiledCall> {
    Ok(match call {
        a::Call::Exit => CompiledCall { prereq_commands: Default::default(), call: o::Call::Exit },
        a::Call::Jump { to } => CompiledCall {
            prereq_commands: Default::default(),
            call: o::Call::Jump(Arc::new(compile_expr(to, stack_m, proc_kind)?)),
        },
        a::Call::Branch { cond, then_to, else_to } => CompiledCall {
            prereq_commands: Default::default(),
            call: o::Call::Branch {
                cond: Arc::new(compile_expr(cond, stack_m, proc_kind)?),
                then_to: Arc::new(compile_expr(then_to, stack_m, proc_kind)?),
                else_to: Arc::new(compile_expr(else_to, stack_m, proc_kind)?),
            },
        },
        a::Call::Sleep { duration_s, to } => CompiledCall {
            prereq_commands: Default::default(),
            call: o::Call::Sleep {
                duration_s: Arc::new(compile_expr(duration_s, stack_m, proc_kind)?),
                to: Arc::new(compile_expr(to, stack_m, proc_kind)?),
            },
        },
        a::Call::Func { to_func_name, arg_assignments } => {
            let mut prereq_commands = Vec::new();

            // set args
            for aa in arg_assignments.iter() {
                let to_func_proc_kind = ProcKind::Func { name: to_func_name.clone() };
                let to_func_frame = stack_m.get(&to_func_proc_kind)?;
                let arg_var_info = to_func_frame.resolve(aa.arg_uuid)?;

                let arg_indexed_frame_offset = compile_var_indexed_frame_offset(
                    arg_var_info,
                    aa.arg_offset,
                    stack_m,
                    proc_kind,
                )?;

                let arg_addr = o::Expr::Add(Arc::new(o::BinaryArgs {
                    left: Arc::new(o::Expr::Add(Arc::new(o::BinaryArgs {
                        left: Arc::new(o::Expr::MemLoc(Arc::new(o::UMemLoc::StackPointer))),
                        right: Arc::new(o::Expr::Value(Arc::new(o::Value::Literal(
                            1.to_string().into(),
                        )))),
                    }))),
                    right: Arc::new(arg_indexed_frame_offset),
                }));

                prereq_commands.push(o::Command::SetStack {
                    addr: Arc::new(arg_addr),
                    val: Arc::new(compile_expr(&aa.expr, stack_m, proc_kind)?),
                })
            }

            // set func addr temp
            let func_addr_temp = Arc::new(o::TempVar::new());
            let func_head_sp_uuid = func_m.get_head_sp_uuid(to_func_name)?;
            prereq_commands.push(o::Command::SetMemLoc {
                mem_loc: Arc::new(o::UMemLoc::Temp(func_addr_temp.clone())),
                val: Arc::new(o::Expr::Value(Arc::new(o::Value::Label(func_head_sp_uuid)))),
            });

            // then jump to func head
            CompiledCall {
                prereq_commands,
                call: o::Call::Jump(Arc::new(o::Expr::MemLoc(Arc::new(o::UMemLoc::Temp(
                    func_addr_temp.clone(),
                ))))),
            }
        },
        a::Call::Return { to } => {
            let mut prereq_commands = Vec::new();

            // store return address in temp var
            let return_addr_temp = Arc::new(o::TempVar::new());
            prereq_commands.push(o::Command::SetMemLoc {
                mem_loc: Arc::new(o::UMemLoc::Temp(return_addr_temp.clone())),
                val: Arc::new(compile_expr(to, stack_m, proc_kind)?),
            });

            // pop stack frame for func
            let frame = stack_m.get(proc_kind)?;
            prereq_commands.push(o::Command::SetMemLoc {
                mem_loc: Arc::new(o::UMemLoc::StackPointer),
                val: Arc::new(o::Expr::Sub(Arc::new(o::BinaryArgs {
                    left: Arc::new(o::Expr::MemLoc(Arc::new(o::UMemLoc::StackPointer))),
                    right: Arc::new(o::Expr::Value(Arc::new(o::Value::Literal(
                        frame.size().to_string().into(),
                    )))),
                }))),
            });

            // then jump to return address
            CompiledCall {
                prereq_commands,
                call: o::Call::Jump(Arc::new(o::Expr::MemLoc(Arc::new(o::UMemLoc::Temp(
                    return_addr_temp.clone(),
                ))))),
            }
        },
    })
}

struct CompiledCall {
    prereq_commands: Vec<o::Command<o::UMemLoc>>,
    call: o::Call<o::UMemLoc>,
}

fn compile_expr(
    expr: &a::Expr,
    stack_m: &StackManager,
    proc_kind: &ProcKind,
) -> anyhow::Result<o::Expr<o::UMemLoc>> {
    Ok(match expr {
        a::Expr::Loc(loc) => match loc.as_ref() {
            a::Loc::Temp(temp) => {
                o::Expr::MemLoc(Arc::new(o::UMemLoc::Temp(Arc::new(compile_temp(temp)))))
            },
            a::Loc::Deref(addr) => {
                o::Expr::StackDeref(Arc::new(compile_expr(addr, stack_m, proc_kind)?))
            },
        },
        a::Expr::StackAddr(addr) => {
            let uuid = *match addr.as_ref() {
                a::StackAddr::Arg { uuid } => uuid,
                a::StackAddr::Local { uuid } => uuid,
            };

            let frame = stack_m.get(proc_kind)?;
            let var_info = frame.resolve(uuid)?;

            let indexed_frame_offset =
                compile_var_indexed_frame_offset(var_info, 0, stack_m, proc_kind)?;

            let frame_start_addr = o::Expr::Sub(Arc::new(o::BinaryArgs {
                left: Arc::new(o::Expr::MemLoc(Arc::new(o::UMemLoc::StackPointer))),
                right: Arc::new(o::Expr::Value(Arc::new(o::Value::Literal(
                    (frame.size() - 1).to_string().into(),
                )))),
            }));

            o::Expr::Add(Arc::new(o::BinaryArgs {
                left: Arc::new(frame_start_addr),
                right: Arc::new(indexed_frame_offset),
            }))
        },
        a::Expr::StdoutDeref(index) => {
            o::Expr::StdoutDeref(Arc::new(compile_expr(index, stack_m, proc_kind)?))
        },
        a::Expr::Value(val) => {
            let val = match val.as_ref() {
                a::Value::Literal(lit) => o::Value::Literal(lit.clone()),
                a::Value::Label(label) => o::Value::Label(*label),
            };

            o::Expr::Value(Arc::new(val))
        },
        a::Expr::Add(args) => o::Expr::Add(Arc::new(compile_args(args, stack_m, proc_kind)?)),
        a::Expr::And(args) => o::Expr::And(Arc::new(compile_args(args, stack_m, proc_kind)?)),
        a::Expr::Div(args) => o::Expr::Div(Arc::new(compile_args(args, stack_m, proc_kind)?)),
        a::Expr::Eq(args) => o::Expr::Eq(Arc::new(compile_args(args, stack_m, proc_kind)?)),
        a::Expr::Gt(args) => o::Expr::Gt(Arc::new(compile_args(args, stack_m, proc_kind)?)),
        a::Expr::InAnswer => o::Expr::InAnswer,
        a::Expr::Join(args) => o::Expr::Join(Arc::new(compile_args(args, stack_m, proc_kind)?)),
        a::Expr::Lt(args) => o::Expr::Lt(Arc::new(compile_args(args, stack_m, proc_kind)?)),
        a::Expr::Mod(args) => o::Expr::Mod(Arc::new(compile_args(args, stack_m, proc_kind)?)),
        a::Expr::Mul(args) => o::Expr::Mul(Arc::new(compile_args(args, stack_m, proc_kind)?)),
        a::Expr::Not(expr) => o::Expr::Not(Arc::new(compile_expr(expr, stack_m, proc_kind)?)),
        a::Expr::Or(args) => o::Expr::Or(Arc::new(compile_args(args, stack_m, proc_kind)?)),
        a::Expr::Random(args) => o::Expr::Random(Arc::new(compile_args(args, stack_m, proc_kind)?)),
        a::Expr::StdoutLen => o::Expr::StdoutLen,
        a::Expr::Sub(args) => o::Expr::Sub(Arc::new(compile_args(args, stack_m, proc_kind)?)),
        a::Expr::Timer => o::Expr::Timer,
    })
}

fn compile_temp(temp: &a::TempVar) -> o::TempVar {
    o::TempVar { uuid: temp.uuid }
}

fn compile_args(
    args: &a::BinaryArgs,
    stack_m: &StackManager,
    proc_kind: &ProcKind,
) -> anyhow::Result<o::BinaryArgs<o::UMemLoc>> {
    Ok(o::BinaryArgs {
        left: Arc::new(compile_expr(&args.left, stack_m, proc_kind)?),
        right: Arc::new(compile_expr(&args.right, stack_m, proc_kind)?),
    })
}

fn compile_var_indexed_frame_offset(
    var_info: VarInfo,
    offset: u32,
    stack_m: &StackManager,
    proc_kind: &ProcKind,
) -> anyhow::Result<o::Expr<o::UMemLoc>> {
    let index = o::Expr::Value(Arc::new(o::Value::Literal(offset.to_string().into())));

    let frame_offset_indexed = o::Expr::Add(Arc::new(o::BinaryArgs {
        left: Arc::new(o::Expr::Value(Arc::new(o::Value::Literal(
            var_info.frame_offset.to_string().into(),
        )))),
        right: Arc::new(index),
    }));

    Ok(frame_offset_indexed)
}
