use std::{collections::HashMap, sync::Arc};

use crate::ast as hast;
use crate::link;
use anyhow::bail;
use itertools::chain;
use itertools::Itertools;
use mem_opt::ast as opt;

struct StackFrame {
    ident_name_to_offset: HashMap<Arc<str>, usize>,
}

impl StackFrame {
    fn new(idents: &[Arc<hast::IdentDeclaration>]) -> Self {
        let mut ident_name_to_offset = HashMap::new();
        let mut total_offset = 0;

        for ident in idents {
            ident_name_to_offset.insert(ident.name().clone(), total_offset);

            let size = match ident.as_ref() {
                hast::IdentDeclaration::Value { .. } => 1,
                hast::IdentDeclaration::Array { length, .. } => *length,
            };

            total_offset += size;
        }

        Self { ident_name_to_offset }
    }

    fn get_offset(&self, name: &str) -> anyhow::Result<usize> {
        let Some(offset) = self.ident_name_to_offset.get(name) else {
            bail!("ident name {} is not in current stack frame", name);
        };

        Ok(*offset)
    }
}

pub fn compile(linked: Vec<Arc<link::Proc>>) -> anyhow::Result<Vec<Arc<opt::Proc<opt::UMemLoc>>>> {
    let opt_procs = linked.into_iter().map(|p| compile_proc(&p)).collect::<Result<Vec<_>, _>>()?;
    Ok(opt_procs)
}

fn compile_proc(proc: &link::Proc) -> anyhow::Result<Arc<opt::Proc<opt::UMemLoc>>> {
    let (kind, stack_params) = match &proc.kind {
        link::ProcKind::Main => (opt::ProcKind::Main, Vec::new()),
        link::ProcKind::Func { name, params } => {
            (opt::ProcKind::Func { name: name.clone() }, params.iter().cloned().collect())
        },
    };

    let stack_vars = proc.idents.iter().cloned();
    let stack_idents = chain!(stack_params, stack_vars).collect_vec();

    let stack_frame = StackFrame::new(&stack_idents);
    let sub_procs = proc
        .sub_procs
        .iter()
        .map(|sp| compile_sub_proc(&stack_frame, sp))
        .collect::<Result<Vec<_>, _>>()?;

    let proc = opt::Proc { kind: Arc::new(kind), sub_procs: Arc::new(sub_procs) };

    Ok(Arc::new(proc))
}

fn compile_sub_proc(
    stack_frame: &StackFrame,
    sp: &link::SubProc,
) -> anyhow::Result<Arc<opt::SubProc<opt::UMemLoc>>> {
    let statement_assignments: Vec<_> = sp
        .statements
        .iter()
        .map(|s| compile_statement(stack_frame, s))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .flatten()
        .collect();

    let compiled_call = compile_call(stack_frame, &sp.next_call)?;
    let assignments = chain!(statement_assignments, compiled_call.assignments).collect();

    let sp = opt::SubProc {
        uuid: sp.uuid,
        assignments: Arc::new(assignments),
        next_call: Arc::new(compiled_call.call),
    };

    Ok(Arc::new(sp))
}

struct CompiledCall {
    call: opt::Call<opt::UMemLoc>,
    assignments: Vec<Arc<opt::Assignment<opt::UMemLoc>>>,
}

fn compile_call(stack_frame: &StackFrame, call: &link::Call) -> anyhow::Result<CompiledCall> {
    let compiled = match call {
        link::Call::Func { name, param_pipelines, return_sub_proc } => {
            let mut param_assignments = Vec::new();
            let mut param_mem_locs = Vec::new();

            for pipeline in param_pipelines.as_ref() {
                let compiled_pipeline = compile_pipeline(stack_frame, pipeline)?;
                param_mem_locs.push(compiled_pipeline.mem_loc);
                param_assignments.extend(compiled_pipeline.assignments);
            }

            let call = opt::Call::Func {
                name: name.clone(),
                params: Arc::new(param_mem_locs),
                return_sub_proc: *return_sub_proc,
            };

            CompiledCall { assignments: param_assignments, call }
        },
        link::Call::SubProc(uuid) => {
            CompiledCall { assignments: Vec::new(), call: opt::Call::SubProc(*uuid) }
        },
        link::Call::IfBranch { cond_pipeline, then_sub_proc, pop_sub_proc } => {
            let compiled_cond = compile_pipeline(stack_frame, cond_pipeline)?;

            let call = opt::Call::IfBranch {
                cond: compiled_cond.mem_loc,
                then_sub_proc: *then_sub_proc,
                pop_sub_proc: *pop_sub_proc,
            };

            CompiledCall { assignments: compiled_cond.assignments, call }
        },
        link::Call::IfElseBranch { cond_pipeline, then_sub_proc, else_sub_proc } => {
            let compiled_cond = compile_pipeline(stack_frame, cond_pipeline)?;

            let call = opt::Call::IfElseBranch {
                cond: compiled_cond.mem_loc,
                then_sub_proc: *then_sub_proc,
                else_sub_proc: *else_sub_proc,
            };

            CompiledCall { assignments: compiled_cond.assignments, call }
        },
        link::Call::Return => CompiledCall { call: opt::Call::Return, assignments: Vec::new() },
        link::Call::Terminate => {
            CompiledCall { call: opt::Call::Terminate, assignments: Vec::new() }
        },
    };

    Ok(compiled)
}

fn compile_statement(
    stack_frame: &StackFrame,
    statement: &link::Statement,
) -> anyhow::Result<Vec<Arc<opt::Assignment<opt::UMemLoc>>>> {
    let assignments = match statement {
        link::Statement::Assign(assign) => {
            let compiled_ident_addr = compile_ident_to_addr(stack_frame, &assign.ident)?;

            let compiled_pipeline = compile_pipeline(stack_frame, &assign.pipeline)?;

            let mut assignments =
                chain!(compiled_pipeline.assignments, compiled_ident_addr.assignments,)
                    .collect_vec();

            if assign.deref_ident {
                // deref var
                assignments.push(Arc::new(opt::Assignment {
                    dest: compiled_pipeline.mem_loc.clone(),
                    expr: Arc::new(opt::Expr::CopyDerefDest {
                        src: compiled_pipeline.mem_loc.clone(),
                    }),
                }));
            }

            // assign to var
            assignments.push(Arc::new(opt::Assignment {
                dest: compiled_ident_addr.mem_loc,
                expr: Arc::new(opt::Expr::CopyDerefDest { src: compiled_pipeline.mem_loc.clone() }),
            }));

            assignments
        },
        link::Statement::Native(native) => {
            todo!();
        },
    };

    Ok(assignments)
}

fn compile_pipeline(
    stack_frame: &StackFrame,
    pipeline: &hast::Pipeline,
) -> anyhow::Result<CompiledExpr> {
    let mut assignments = Vec::new();

    let initial_val = compile_value(stack_frame, &pipeline.initial_val)?;

    let mut val = initial_val.mem_loc;
    assignments.extend(initial_val.assignments);

    for operation in pipeline.operations.as_ref() {
        match operation.as_ref() {
            hast::Operation::Deref => todo!(),
            hast::Operation::Add { operand } => {
                let compiled_value = compile_value(stack_frame, operand)?;
                let value_mem_loc = compiled_value.mem_loc;
                assignments.extend(compiled_value.assignments);

                let output_mem_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));
                assignments.push(Arc::new(opt::Assignment {
                    dest: output_mem_loc.clone(),
                    expr: Arc::new(opt::Expr::Add { left: val.clone(), right: value_mem_loc }),
                }));

                val = output_mem_loc;
            },
            hast::Operation::Sub { operand } => todo!(),
            hast::Operation::Mul { operand } => todo!(),
            hast::Operation::Div { operand } => todo!(),
            hast::Operation::Mod { operand } => todo!(),
            hast::Operation::Eq { operand } => todo!(),
            hast::Operation::Neq { operand } => todo!(),
            hast::Operation::Gt { operand } => todo!(),
            hast::Operation::Lt { operand } => todo!(),
            hast::Operation::Gte { operand } => todo!(),
            hast::Operation::Lte { operand } => todo!(),
            hast::Operation::And { operand } => todo!(),
            hast::Operation::Or { operand } => todo!(),
            hast::Operation::Xor { operand } => todo!(),
            hast::Operation::Not => todo!(),
            hast::Operation::Join { operand } => todo!(),
        }
    }

    let compiled = CompiledExpr { mem_loc: val, assignments };
    Ok(compiled)
}

fn compile_value(stack_frame: &StackFrame, value: &hast::Value) -> anyhow::Result<CompiledExpr> {
    let compiled = match value {
        hast::Value::Literal(literal) => {
            let temp = Arc::new(opt::TempVar::new());
            let assignments = Vec::from([Arc::new(opt::Assignment {
                dest: Arc::new(opt::UMemLoc::Temp(temp.clone())),
                expr: Arc::new(opt::Expr::Set {
                    literal: Arc::new(opt::Literal { value: literal.clone() }),
                }),
            })]);

            CompiledExpr { mem_loc: Arc::new(opt::UMemLoc::Temp(temp)), assignments }
        },
        hast::Value::Ident(ident) => {
            let compiled_ident_addr = compile_ident_to_addr(stack_frame, ident)?;

            let ident_value_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));

            let assignments = chain!(
                compiled_ident_addr.assignments,
                [Arc::new(opt::Assignment {
                    dest: ident_value_loc.clone(),
                    expr: Arc::new(opt::Expr::Deref { src: compiled_ident_addr.mem_loc })
                })]
            )
            .collect();

            CompiledExpr { mem_loc: ident_value_loc, assignments }
        },
        hast::Value::Ref(ident) => compile_ident_to_addr(stack_frame, ident)?,
    };

    Ok(compiled)
}

fn compile_ident_to_addr(
    stack_frame: &StackFrame,
    ident: &hast::Ident,
) -> anyhow::Result<CompiledExpr> {
    let compiled = match ident {
        hast::Ident::Var { name } => {
            let stack_offset = stack_frame.get_offset(name)?;
            let stack_offset_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new()))); // offset of var within the current stack frame
            let stack_addr_loc = Arc::new(opt::UMemLoc::Temp(Arc::new(opt::TempVar::new())));

            let assignments = Vec::from([
                Arc::new(opt::Assignment {
                    dest: stack_offset_loc.clone(),
                    expr: Arc::new(opt::Expr::Set {
                        literal: Arc::new(opt::Literal { value: stack_offset.to_string().into() }),
                    }),
                }),
                Arc::new(opt::Assignment {
                    dest: stack_addr_loc.clone(),
                    expr: Arc::new(opt::Expr::Add {
                        left: Arc::new(opt::UMemLoc::StackPointer),
                        right: stack_offset_loc,
                    }),
                }),
            ]);

            CompiledExpr { mem_loc: stack_addr_loc, assignments }
        },
        hast::Ident::Array { name, index } => {
            // let compiled_index = compile_pipeline(index);

            // CompiledExpr {
            //     mem_loc: Arc::new(opt::UMemLoc::Stack {
            //         var: todo!(),
            //         offset: Some(compiled_index.mem_loc),
            //     }),
            //     assignments: compiled_index.assignments,
            // }

            todo!()
        },
    };

    Ok(compiled)
}

#[derive(Debug)]
struct CompiledExpr {
    mem_loc: Arc<opt::UMemLoc>,
    assignments: Vec<Arc<opt::Assignment<opt::UMemLoc>>>,
}
