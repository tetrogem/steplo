use std::sync::Arc;

use itertools::Itertools;
use itertools::chain;

use crate::high_compiler::compile_to_inline::loc_manager::LocManager;
use crate::high_compiler::compile_to_inline::loc_manager::TypedVarInfo;
use crate::high_compiler::link as l;
use crate::high_compiler::type_resolved_ast as t;
use crate::inline::ast as o;

pub fn compile(procs: &[t::Ref<l::Proc>]) -> anyhow::Result<Vec<Arc<o::Proc>>> {
    let loc_m = LocManager::new(procs);

    procs.iter().map(|proc| compile_proc(&proc.val, &loc_m).map(Arc::new)).collect::<Result<_, _>>()
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum ProcKind {
    Main,
    Func { name: Arc<str> },
}

mod loc_manager {
    use anyhow::bail;
    use itertools::chain;
    use uuid::Uuid;

    use crate::high_compiler::compile_to_inline::ProcKind;
    use crate::high_compiler::link as l;
    use crate::high_compiler::type_resolved_ast as t;
    use crate::inline::ast as o;
    use std::collections::BTreeMap;
    use std::sync::Arc;

    pub struct LocManager {
        proc_kind_to_locs: BTreeMap<ProcKind, ProcLocs>,
    }

    impl LocManager {
        pub fn new(procs: &[t::Ref<l::Proc>]) -> Self {
            let proc_kind_to_locs = procs
                .iter()
                .map(|proc| ((&proc.val.kind).into(), ProcLocs::new(&proc.val)))
                .collect();

            Self { proc_kind_to_locs }
        }

        pub fn get_locs(&self, proc_kind: &ProcKind) -> anyhow::Result<&ProcLocs> {
            let Some(locs) = self.proc_kind_to_locs.get(proc_kind) else {
                bail!("Proc kind not registered: {:?}", proc_kind);
            };

            Ok(locs)
        }
    }

    impl From<&l::ProcKind> for ProcKind {
        fn from(value: &l::ProcKind) -> Self {
            match &value {
                l::ProcKind::Main => ProcKind::Main,
                l::ProcKind::Func { name, .. } => ProcKind::Func { name: name.val.str.clone() },
            }
        }
    }

    pub struct ProcLocs {
        local_name_to_info: BTreeMap<Arc<str>, o::VarInfo>,
        user_arg_name_to_info: BTreeMap<Arc<str>, o::VarInfo>,
        ordered_local_names: Vec<Arc<str>>,
        ordered_user_arg_names: Vec<Arc<str>>,
        // \/ these two are both considered ArgVars, as they are inputs to functions
        return_label_arg_info: o::VarInfo, // where to jump when returning
        return_value_arg_info: o::VarInfo, // reference to store return value of function in
    }

    impl ProcLocs {
        fn new(proc: &l::Proc) -> Self {
            let mut local_name_to_info = BTreeMap::new();
            let mut ordered_local_names = Vec::new();

            for decl in &proc.idents.val {
                local_name_to_info
                    .insert(decl.val.name.val.str.clone(), o::VarInfo::new(decl.val.size));

                ordered_local_names.push(decl.val.name.val.str.clone());
            }

            let (arg_name_to_info, ordered_arg_names) = match &proc.kind {
                l::ProcKind::Main => (Default::default(), Default::default()),
                l::ProcKind::Func { params, .. } => {
                    let mut arg_name_to_info = BTreeMap::new();
                    let mut ordered_arg_names = Vec::new();

                    for decl in &params.val {
                        arg_name_to_info
                            .insert(decl.val.name.val.str.clone(), o::VarInfo::new(decl.val.size));

                        ordered_arg_names.push(decl.val.name.val.str.clone());
                    }

                    (arg_name_to_info, ordered_arg_names)
                },
            };

            Self {
                local_name_to_info,
                user_arg_name_to_info: arg_name_to_info,
                ordered_local_names,
                ordered_user_arg_names: ordered_arg_names,
                return_label_arg_info: o::VarInfo::new(1),
                return_value_arg_info: o::VarInfo::new(1),
            }
        }

        pub fn get_loc(&self, loc_name: &Arc<str>) -> anyhow::Result<TypedVarInfo> {
            if let Some(info) = self.local_name_to_info.get(loc_name) {
                return Ok(TypedVarInfo::Local(*info));
            }

            if let Some(info) = self.user_arg_name_to_info.get(loc_name) {
                return Ok(TypedVarInfo::Arg(*info));
            }

            bail!("Loc name not found: {}", loc_name);
        }

        pub fn get_local(&self, local_name: &Arc<str>) -> anyhow::Result<o::VarInfo> {
            let Some(info) = self.local_name_to_info.get(local_name) else {
                bail!("Local name not found: {}", local_name)
            };

            Ok(*info)
        }

        pub fn get_arg(&self, arg_name: &Arc<str>) -> anyhow::Result<o::VarInfo> {
            let Some(info) = self.user_arg_name_to_info.get(arg_name) else {
                bail!("Arg name not found: {}", arg_name)
            };

            Ok(*info)
        }

        pub fn get_return_label_arg(&self) -> o::VarInfo {
            self.return_label_arg_info
        }

        pub fn get_return_value_arg(&self) -> o::VarInfo {
            self.return_value_arg_info
        }

        pub fn ordered_local_names(&self) -> &Vec<Arc<str>> {
            &self.ordered_local_names
        }

        pub fn ordered_user_arg_names(&self) -> &Vec<Arc<str>> {
            &self.ordered_user_arg_names
        }

        pub fn ordered_local_infos(&self) -> anyhow::Result<Vec<o::VarInfo>> {
            self.ordered_local_names.iter().map(|name| self.get_local(name)).collect()
        }

        pub fn ordered_arg_infos(&self) -> anyhow::Result<Vec<o::VarInfo>> {
            let ordered_user_arg_uuids = self
                .ordered_user_arg_names
                .iter()
                .map(|name| self.get_arg(name))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(chain!(
                [self.return_label_arg_info, self.return_value_arg_info],
                ordered_user_arg_uuids
            )
            .collect())
        }
    }

    pub enum TypedVarInfo {
        Local(o::VarInfo),
        Arg(o::VarInfo),
    }
}

fn compile_proc(proc: &l::Proc, loc_m: &LocManager) -> anyhow::Result<o::Proc> {
    let proc_kind = (&proc.kind).into();
    let locs = loc_m.get_locs(&proc_kind)?;

    Ok(o::Proc {
        kind: Arc::new(match &proc.kind {
            l::ProcKind::Main => o::ProcKind::Main,
            l::ProcKind::Func { name, .. } => o::ProcKind::Func { name: name.val.str.clone() },
        }),
        ordered_local_infos: Arc::new(locs.ordered_local_infos()?),
        ordered_arg_infos: Arc::new(locs.ordered_arg_infos()?),
        sub_procs: Arc::new(
            proc.sub_procs
                .val
                .iter()
                .map(|sp| compile_sp(&sp.val, &proc_kind, loc_m).map(Arc::new))
                .collect::<Result<_, _>>()?,
        ),
    })
}

fn compile_sp(
    sp: &l::SubProc,
    proc_kind: &ProcKind,
    loc_m: &LocManager,
) -> anyhow::Result<o::SubProc> {
    let statement_commands = sp
        .statements
        .val
        .iter()
        .map(|s| compile_statement(&s.val, proc_kind, loc_m))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .flatten()
        .collect_vec();

    let compiled_call = compile_call(&sp.next_call.val, proc_kind, loc_m)?;

    let commands =
        chain!(statement_commands, compiled_call.prereq_commands.into_iter().map(Arc::new))
            .collect();

    Ok(o::SubProc {
        uuid: sp.uuid,
        commands: Arc::new(commands),
        call: Arc::new(compiled_call.call),
    })
}

fn compile_statement(
    item: &l::Statement,
    proc_kind: &ProcKind,
    loc_m: &LocManager,
) -> anyhow::Result<Vec<Arc<o::Command>>> {
    match item {
        l::Statement::Assign(x) => {
            let base_addr =
                Arc::new(compile_place_to_addr_expr(&x.val.place.val, proc_kind, loc_m)?);

            x.val
                .expr
                .val
                .iter()
                .map(|ae| {
                    let addr = o::Expr::Add(Arc::new(o::BinaryArgs {
                        left: base_addr.clone(),
                        right: Arc::new(o::Expr::Value(Arc::new(int_literal(ae.val.offset)))),
                    }));

                    let val = compile_expr(&ae.val.expr.val, proc_kind, loc_m)?;

                    Ok(Arc::new(o::Command::SetLoc {
                        loc: Arc::new(o::Loc::Deref(Arc::new(addr))),
                        val: Arc::new(val),
                    }))
                })
                .collect::<anyhow::Result<Vec<_>>>()
        },
        l::Statement::Native(native) => {
            let commands = match &native.val {
                t::NativeOperation::In { dest_place } => {
                    let dest_loc = compile_place_to_loc(&dest_place.val, proc_kind, loc_m)?;

                    Vec::from([
                        o::Command::In,
                        o::Command::SetLoc {
                            loc: Arc::new(dest_loc),
                            val: Arc::new(o::Expr::InAnswer),
                        },
                    ])
                },
                t::NativeOperation::Out { val } => {
                    let val = compile_expr(&val.val, proc_kind, loc_m)?;

                    Vec::from([o::Command::Out(Arc::new(val))])
                },
                t::NativeOperation::Random { dest_place, min, max } => {
                    let dest_loc = compile_place_to_loc(&dest_place.val, proc_kind, loc_m)?;
                    let min = compile_expr(&min.val, proc_kind, loc_m)?;
                    let max = compile_expr(&max.val, proc_kind, loc_m)?;

                    let args =
                        Arc::new(o::BinaryArgs { left: Arc::new(min), right: Arc::new(max) });

                    Vec::from([o::Command::SetLoc {
                        loc: Arc::new(dest_loc),
                        val: Arc::new(o::Expr::Random(args)),
                    }])
                },
                t::NativeOperation::StdoutClear => Vec::from([o::Command::ClearStdout]),
                t::NativeOperation::StdoutLen { dest_place } => {
                    let dest_loc = compile_place_to_loc(&dest_place.val, proc_kind, loc_m)?;

                    Vec::from([o::Command::SetLoc {
                        loc: Arc::new(dest_loc),
                        val: Arc::new(o::Expr::StdoutLen),
                    }])
                },
                t::NativeOperation::StdoutRead { dest_place, index } => {
                    let dest_loc = compile_place_to_loc(&dest_place.val, proc_kind, loc_m)?;
                    let index = compile_expr(&index.val, proc_kind, loc_m)?;

                    Vec::from([o::Command::SetLoc {
                        loc: Arc::new(dest_loc),
                        val: Arc::new(o::Expr::StdoutDeref(Arc::new(index))),
                    }])
                },
                t::NativeOperation::StdoutWrite { val, index } => {
                    let val = compile_expr(&val.val, proc_kind, loc_m)?;
                    let index = compile_expr(&index.val, proc_kind, loc_m)?;

                    Vec::from([o::Command::WriteStdout {
                        index: Arc::new(index),
                        val: Arc::new(val),
                    }])
                },
                t::NativeOperation::TimerGet { dest_place } => {
                    let dest_loc = compile_place_to_loc(&dest_place.val, proc_kind, loc_m)?;

                    Vec::from([o::Command::SetLoc {
                        loc: Arc::new(dest_loc),
                        val: Arc::new(o::Expr::Timer),
                    }])
                },
                t::NativeOperation::Wait { duration_s } => {
                    let duration_s = compile_expr(&duration_s.val, proc_kind, loc_m)?;

                    Vec::from([o::Command::Wait { duration_s: Arc::new(duration_s) }])
                },
            };

            Ok(commands.into_iter().map(Arc::new).collect())
        },
    }
}

fn compile_call(
    item: &l::Call,
    proc_kind: &ProcKind,
    loc_m: &LocManager,
) -> anyhow::Result<CompiledCall> {
    Ok(match item {
        l::Call::Terminate => {
            CompiledCall { prereq_commands: Default::default(), call: o::Call::Exit }
        },
        l::Call::Return => {
            let return_label_arg = loc_m.get_locs(proc_kind)?.get_return_label_arg();

            CompiledCall {
                prereq_commands: Default::default(),
                call: o::Call::Return {
                    to: Arc::new(o::Expr::Loc(Arc::new(o::Loc::Deref(Arc::new(
                        o::Expr::StackAddr(Arc::new(o::StackAddr::Arg {
                            uuid: return_label_arg.uuid,
                        })),
                    ))))),
                },
            }
        },
        l::Call::SubProc(sp_uuid) => CompiledCall {
            prereq_commands: Default::default(),
            call: o::Call::Jump {
                to: Arc::new(o::Expr::Value(Arc::new(o::Value::Label(*sp_uuid)))),
            },
        },
        l::Call::IfElseBranch { cond_expr, then_sub_proc, else_sub_proc } => {
            let cond = compile_expr(&cond_expr.val, proc_kind, loc_m)?;
            let then_to = o::Expr::Value(Arc::new(o::Value::Label(*then_sub_proc)));
            let else_to = o::Expr::Value(Arc::new(o::Value::Label(*else_sub_proc)));

            CompiledCall {
                prereq_commands: Default::default(),
                call: o::Call::Branch {
                    cond: Arc::new(cond),
                    then_to: Arc::new(then_to),
                    else_to: Arc::new(else_to),
                },
            }
        },
        l::Call::Func { name, param_exprs, return_sub_proc } => {
            let func_kind = ProcKind::Func { name: name.val.str.clone() };
            let func_locs = loc_m.get_locs(&func_kind)?;

            // set return label
            let return_label_arg_assignments = [o::ArgAssignment {
                arg_uuid: func_locs.get_return_label_arg().uuid,
                arg_offset: 0,
                expr: Arc::new(o::Expr::Value(Arc::new(o::Value::Label(*return_sub_proc)))),
            }];

            // return values currently aren't implemented -- skipping

            // set user-defined args
            let mut user_arg_prereq_commands = Vec::new();
            let mut user_arg_assignments = Vec::new();

            for (aes, arg_name) in param_exprs.val.iter().zip(func_locs.ordered_user_arg_names()) {
                let arg_info = func_locs.get_arg(arg_name)?;

                for ae in aes.val.iter() {
                    // assignments to exprs cannot have calls to functions between setting args
                    // so, all commands must be done before we start setting args on the stack
                    // (this is to prevent unexpected behavior if passing a value that should be
                    // copied but a function mutates it, or stacks from function calls overwriting
                    // the stack we're setting up here)
                    // so, this is all just set up for handling that behavior in the future
                    // by making all expr be handled outside of the func call,
                    // and then we just copy from those temps to the arg locs
                    let expr = Arc::new(compile_expr(&ae.val.expr.val, proc_kind, loc_m)?);
                    let temp = Arc::new(o::Loc::Temp(Arc::new(o::TempVar::new())));

                    let prereq_commands = [o::Command::SetLoc { loc: temp.clone(), val: expr }];

                    user_arg_prereq_commands.extend(prereq_commands);

                    user_arg_assignments.push(o::ArgAssignment {
                        arg_uuid: arg_info.uuid,
                        arg_offset: ae.val.offset,
                        expr: Arc::new(o::Expr::Loc(temp.clone())),
                    })
                }
            }

            let arg_assignments =
                chain!(return_label_arg_assignments, user_arg_assignments).collect();

            CompiledCall {
                prereq_commands: user_arg_prereq_commands,
                call: o::Call::Func {
                    to_func_name: name.val.str.clone(),
                    arg_assignments: Arc::new(arg_assignments),
                },
            }
        },
    })
}

struct CompiledCall {
    prereq_commands: Vec<o::Command>,
    call: o::Call,
}

fn compile_place_to_addr_expr(
    item: &t::Place,
    proc_kind: &ProcKind,
    loc_m: &LocManager,
) -> anyhow::Result<o::Expr> {
    let offset = match &item.offset {
        None => o::Expr::Value(Arc::new(o::Value::Literal(0.to_string().into()))),
        Some(offset) => match &offset.val {
            t::Offset::Static(x) => {
                o::Expr::Value(Arc::new(o::Value::Literal(x.val.to_string().into())))
            },
            t::Offset::Expr(x) => compile_expr(&x.val, proc_kind, loc_m)?,
        },
    };

    let place_head = match &item.head.val {
        t::PlaceHead::Ident(ident) => {
            let addr = match loc_m.get_locs(proc_kind)?.get_loc(&ident.val.name.val.str)? {
                TypedVarInfo::Local(info) => o::StackAddr::Local { uuid: info.uuid },
                TypedVarInfo::Arg(info) => o::StackAddr::Arg { uuid: info.uuid },
            };

            o::Expr::StackAddr(Arc::new(addr))
        },
        t::PlaceHead::Deref(deref) => compile_expr(&deref.val.addr.val, proc_kind, loc_m)?,
    };

    Ok(o::Expr::Add(Arc::new(o::BinaryArgs {
        left: Arc::new(place_head),
        right: Arc::new(offset),
    })))
}

fn compile_place_to_loc(
    item: &t::Place,
    proc_kind: &ProcKind,
    loc_m: &LocManager,
) -> anyhow::Result<o::Loc> {
    let addr = compile_place_to_addr_expr(item, proc_kind, loc_m)?;
    Ok(o::Loc::Deref(Arc::new(addr)))
}

fn compile_place_to_loc_expr(
    item: &t::Place,
    proc_kind: &ProcKind,
    loc_m: &LocManager,
) -> anyhow::Result<o::Expr> {
    let loc = compile_place_to_loc(item, proc_kind, loc_m)?;
    Ok(o::Expr::Loc(Arc::new(loc)))
}

fn compile_expr(
    item: &t::Expr,
    proc_kind: &ProcKind,
    loc_m: &LocManager,
) -> anyhow::Result<o::Expr> {
    Ok(match item {
        t::Expr::Literal(x) => {
            let lit: Arc<str> = match &x.val {
                t::Literal::Str(x) => x.clone(),
                t::Literal::Num(x) => format!("{x:?}").into(),
                t::Literal::Int(x) | t::Literal::Uint(x) => format!("{x}").into(),
                t::Literal::Bool(x) => (if *x { "true" } else { "false" }).into(),
            };

            o::Expr::Value(Arc::new(o::Value::Literal(lit)))
        },
        t::Expr::Place(place) => compile_place_to_loc_expr(&place.val, proc_kind, loc_m)?,
        t::Expr::Ref(place) => compile_place_to_addr_expr(&place.val, proc_kind, loc_m)?,
        t::Expr::Paren(paren) => match &paren.val {
            t::ParenExpr::Unary(unary) => {
                let operand = Arc::new(compile_expr(&unary.val.operand.val, proc_kind, loc_m)?);

                match unary.val.op.val {
                    t::UnaryParenExprOp::Not => o::Expr::Not(operand),
                }
            },
            t::ParenExpr::Binary(binary) => {
                let left = Arc::new(compile_expr(&binary.val.left.val, proc_kind, loc_m)?);
                let right = Arc::new(compile_expr(&binary.val.right.val, proc_kind, loc_m)?);
                let args = Arc::new(o::BinaryArgs { left, right });

                match binary.val.op.val {
                    t::BinaryParenExprOp::Add => o::Expr::Add(args),
                    t::BinaryParenExprOp::And => o::Expr::And(args),
                    t::BinaryParenExprOp::Div => o::Expr::Div(args),
                    t::BinaryParenExprOp::Eq => o::Expr::Eq(args),
                    t::BinaryParenExprOp::Gt => o::Expr::Gt(args),
                    t::BinaryParenExprOp::Gte => o::Expr::Not(Arc::new(o::Expr::Lt(args))),
                    t::BinaryParenExprOp::Join => o::Expr::Join(args),
                    t::BinaryParenExprOp::Lt => o::Expr::Lt(args),
                    t::BinaryParenExprOp::Lte => o::Expr::Not(Arc::new(o::Expr::Gt(args))),
                    t::BinaryParenExprOp::Mod => o::Expr::Mod(args),
                    t::BinaryParenExprOp::Mul => o::Expr::Mul(args),
                    t::BinaryParenExprOp::Neq => o::Expr::Not(Arc::new(o::Expr::Eq(args))),
                    t::BinaryParenExprOp::Or => o::Expr::Or(args),
                    t::BinaryParenExprOp::Sub => o::Expr::Sub(args),
                }
            },
        },
    })
}

fn int_literal(int: u32) -> o::Value {
    o::Value::Literal(int.to_string().into())
}
