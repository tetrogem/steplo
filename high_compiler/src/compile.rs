use anyhow::bail;
use asm_compiler::ast as asm_ast;
use itertools::{chain, Itertools};
use std::{collections::HashMap, sync::Arc};
use uuid::Uuid;

use crate::{
    ast::{Assign, Ident, IdentDeclaration, Index, NativeOperation, Operation, Pipeline, Value},
    link::{Call, Proc, ProcKind, Statement},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PrivRegister(&'static str);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PubRegister(&'static str);

trait Register {
    fn name(&self) -> &'static str;

    fn value(&self) -> Arc<asm_ast::Value> {
        Arc::new(asm_ast::Value::Register(Arc::new(asm_ast::Register { name: self.name().into() })))
    }
}

impl Register for PrivRegister {
    fn name(&self) -> &'static str {
        self.0
    }
}

impl Register for PubRegister {
    fn name(&self) -> &'static str {
        self.0
    }
}

const STACK_POINTER: PrivRegister = PrivRegister("sp");
const RESULT: PrivRegister = PrivRegister("result");
const OPERAND: PrivRegister = PrivRegister("operand");
const TEMP: PubRegister = PubRegister("temp");
const EXPR: PubRegister = PubRegister("expr");

fn data(command: asm_ast::DataCommand) -> Arc<asm_ast::Command> {
    Arc::new(asm_ast::Command::Data(Arc::new(command)))
}

fn control(command: asm_ast::ControlCommand) -> Arc<asm_ast::Command> {
    Arc::new(asm_ast::Command::Control(Arc::new(command)))
}

fn literal(value: impl ToString) -> Arc<asm_ast::Value> {
    Arc::new(asm_ast::Value::Literal(Arc::new(asm_ast::Literal { val: value.to_string().into() })))
}

fn label(value: &str) -> Arc<asm_ast::Value> {
    Arc::new(asm_ast::Value::Label(Arc::new(asm_ast::Label { name: value.into() })))
}

fn compute_stack_addr(dest: PubRegister, offset: usize) -> Vec<Arc<asm_ast::Command>> {
    Vec::from([
        data(asm_ast::DataCommand::Move(asm_ast::BinaryArgs {
            dest: RESULT.value(),
            val: STACK_POINTER.value(),
        })),
        data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
            dest: OPERAND.value(),
            val: literal(offset),
        })),
        data(asm_ast::DataCommand::Sub(asm_ast::TernaryArgs {
            dest: dest.value(),
            left: RESULT.value(),
            right: OPERAND.value(),
        })),
    ])
}

fn compute_param_addr(dest: PubRegister, offset: usize) -> Vec<Arc<asm_ast::Command>> {
    Vec::from([
        data(asm_ast::DataCommand::Move(asm_ast::BinaryArgs {
            dest: RESULT.value(),
            val: STACK_POINTER.value(),
        })),
        data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
            dest: OPERAND.value(),
            val: literal(offset),
        })),
        data(asm_ast::DataCommand::Add(asm_ast::TernaryArgs {
            dest: dest.value(),
            left: RESULT.value(),
            right: OPERAND.value(),
        })),
    ])
}

fn get_stack_value(dest: PubRegister, offset: usize) -> Vec<Arc<asm_ast::Command>> {
    [].into_iter()
        .chain(compute_stack_addr(dest, offset))
        .chain([data(asm_ast::DataCommand::MoveDerefSrc(asm_ast::BinaryArgs {
            dest: dest.value(),
            val: dest.value(),
        }))])
        .collect_vec()
}

pub fn compile(linked: Vec<Arc<Proc>>) -> anyhow::Result<asm_ast::Program> {
    let procs = compile_procs(&linked)?;

    let registers = [STACK_POINTER.0, RESULT.0, OPERAND.0, TEMP.0, EXPR.0]
        .into_iter()
        .map(Into::into)
        .collect();

    let program = asm_ast::Program { registers: Arc::new(registers), procedures: Arc::new(procs) };
    Ok(program)
}

fn compile_procs(linked: &Vec<Arc<Proc>>) -> anyhow::Result<Vec<Arc<asm_ast::Procedure>>> {
    let mut asm_procs = Vec::<Arc<asm_ast::Procedure>>::new();

    let asm_main_setup = asm_ast::Procedure {
        kind: asm_ast::ProcedureKind::Main,
        commands: Vec::from([
            // init stack pointer
            data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                dest: STACK_POINTER.value(), // stored in register
                val: literal("0"),           // stack begins empty
            })),
            // run high-level main method
            data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                dest: RESULT.value(),
                val: label("main.0"),
            })),
            control(asm_ast::ControlCommand::Jump(asm_ast::UnaryArgs { val: RESULT.value() })),
        ]),
    };

    asm_procs.push(Arc::new(asm_main_setup));

    for proc in linked {
        let compiled_asm_procs = compile_proc(proc.as_ref())?;
        asm_procs.extend(compiled_asm_procs);
    }

    Ok(asm_procs)
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum SlotName {
    ReturnAddr,
    Ident(Arc<str>),
}

#[derive(Clone, Debug)]
enum SlotDeclaration {
    ReturnAddr,
    Ident(Arc<IdentDeclaration>),
}

impl SlotDeclaration {
    pub fn name(&self) -> SlotName {
        match self {
            Self::ReturnAddr => SlotName::ReturnAddr,
            Self::Ident(ident) => SlotName::Ident(Arc::clone(ident.name())),
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Self::ReturnAddr => 1,
            Self::Ident(ident) => match ident.as_ref() {
                IdentDeclaration::Value { .. } => 1,
                IdentDeclaration::Array { length, .. } => *length,
            },
        }
    }
}

#[derive(Clone, Debug)]
enum Slot {
    ReturnAddr,
    Ident(Arc<Ident>),
}

impl Slot {
    pub fn name(&self) -> SlotName {
        match self {
            Self::ReturnAddr => SlotName::ReturnAddr,
            Self::Ident(ident) => SlotName::Ident(Arc::clone(ident.name())),
        }
    }

    pub fn index(&self) -> Arc<Index> {
        match self {
            Self::ReturnAddr => Arc::new(Index::Int(0)),
            Self::Ident(ident) => match ident.as_ref() {
                Ident::Var { .. } => Arc::new(Index::Int(0)),
                Ident::Array { index, .. } => Arc::clone(index),
            },
        }
    }
}

struct Frame {
    name_to_offset: HashMap<SlotName, usize>,
    size: usize,
}

impl Frame {
    pub fn size(&self) -> usize {
        self.size
    }

    pub fn get_offset(&self, slot: Slot) -> anyhow::Result<usize> {
        let name = &slot.name();
        let Some(base) = self.name_to_offset.get(name) else {
            bail!("Could not find offset for slot: '{:?}'", name)
        };

        let index = self.compile_index(&slot.index())?;

        Ok(base - index)
    }

    fn compile_index(&self, index: &Index) -> anyhow::Result<usize> {
        let index = match index {
            Index::Int(int) => *int,
            Index::Ident(ident) => todo!(),
        };

        Ok(index)
    }

    pub fn get_ident_offset(&self, ident: &Arc<Ident>) -> anyhow::Result<usize> {
        self.get_offset(Slot::Ident(Arc::clone(ident)))
    }
}

fn compile_frame(proc: &Proc) -> anyhow::Result<Frame> {
    let slot_declarations: Vec<SlotDeclaration> = match &proc.kind {
        ProcKind::Main => {
            proc.idents.iter().map(|ident| SlotDeclaration::Ident(Arc::clone(ident))).collect()
        },
        ProcKind::Func { params, .. } => chain!(
            [SlotDeclaration::ReturnAddr],
            params
                .iter()
                .chain(proc.idents.iter())
                .map(|ident| SlotDeclaration::Ident(Arc::clone(ident))),
        )
        .collect(),
    };

    // init information about stack vars / their offsets
    let mut name_to_offset = HashMap::new();
    let mut offset = 0;
    for slot in slot_declarations.iter().rev() {
        // data goes from lower -> higher mem addrs
        offset += slot.size();
        name_to_offset.insert(slot.name(), offset - 1);
    }

    let frame = Frame { name_to_offset, size: offset };

    Ok(frame)
}

fn compile_proc(proc: &Proc) -> anyhow::Result<Vec<Arc<asm_ast::Procedure>>> {
    let frame = compile_frame(proc)?;

    // init information about sub proc indexes
    let mut sub_proc_uuid_to_index = HashMap::<Uuid, usize>::new();
    for (index, sub_proc) in proc.sub_procs.iter().enumerate() {
        sub_proc_uuid_to_index.insert(sub_proc.uuid, index);
    }

    let mut asm_procs = Vec::new();
    for (sub_proc_index, sub_proc) in proc.sub_procs.iter().enumerate() {
        let is_head = sub_proc_index == 0;
        let is_tail = matches!(sub_proc.next_call.as_ref(), Call::Return | Call::Terminate);

        let mut asm_commands = Vec::<Arc<asm_ast::Command>>::new();

        // allocate stack vars
        if is_head {
            asm_commands.extend([
                data(asm_ast::DataCommand::Move(asm_ast::BinaryArgs {
                    dest: RESULT.value(),
                    val: STACK_POINTER.value(),
                })),
                data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                    dest: OPERAND.value(),
                    val: literal(frame.size()),
                })),
                data(asm_ast::DataCommand::Add(asm_ast::TernaryArgs {
                    dest: STACK_POINTER.value(),
                    left: RESULT.value(),
                    right: OPERAND.value(),
                })),
            ]);
        }

        // run statements
        for statement in sub_proc.statements.iter().map(AsRef::as_ref) {
            let asm_statement_commands = match statement {
                Statement::Assign(assign) => {
                    let Assign { deref_ident, ident, pipeline } = assign.as_ref();

                    let var_offset = frame.get_ident_offset(&ident)?;

                    // value to assign to var should end up in temp_right
                    let mut assign_asm_commands = compile_pipeline(pipeline, &frame)?;

                    // set var addr to assign in temp_left
                    assign_asm_commands.extend(compute_stack_addr(TEMP, var_offset));

                    // deref var addr if its a ref assign
                    if *deref_ident {
                        assign_asm_commands.push(data(asm_ast::DataCommand::MoveDerefSrc(
                            asm_ast::BinaryArgs { dest: TEMP.value(), val: TEMP.value() },
                        )));
                    }

                    // move value at addr in temp_right to addr in temp_left
                    assign_asm_commands.push(data(asm_ast::DataCommand::MoveDerefDest(
                        asm_ast::BinaryArgs { dest: TEMP.value(), val: EXPR.value() },
                    )));

                    assign_asm_commands
                },
                Statement::Native(command) => match command.as_ref() {
                    NativeOperation::Out { ident } => {
                        let var_offset = frame.get_ident_offset(ident)?;

                        chain!(
                            get_stack_value(EXPR, var_offset),
                            [data(asm_ast::DataCommand::Out(asm_ast::UnaryArgs {
                                val: EXPR.value()
                            }))],
                        )
                        .collect()
                    },
                    NativeOperation::In { dest_ident } => {
                        let dest_var_offset = frame.get_ident_offset(dest_ident)?;

                        chain!(
                            [data(asm_ast::DataCommand::In(asm_ast::UnaryArgs {
                                val: EXPR.value(),
                            }))],
                            compute_stack_addr(TEMP, dest_var_offset),
                            [data(asm_ast::DataCommand::MoveDerefDest(asm_ast::BinaryArgs {
                                dest: TEMP.value(),
                                val: EXPR.value()
                            }))],
                        )
                        .collect()
                    },
                },
            };

            asm_commands.extend(asm_statement_commands);
        }

        if is_tail {
            // cleanup stack vars
            asm_commands.extend([
                data(asm_ast::DataCommand::Move(asm_ast::BinaryArgs {
                    dest: RESULT.value(),
                    val: STACK_POINTER.value(),
                })),
                data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                    dest: OPERAND.value(),
                    val: literal(frame.size()),
                })),
                data(asm_ast::DataCommand::Sub(asm_ast::TernaryArgs {
                    dest: STACK_POINTER.value(),
                    left: RESULT.value(),
                    right: OPERAND.value(),
                })),
            ]);
        }

        let asm_proc_name = match &proc.kind {
            ProcKind::Main => "main".into(),
            ProcKind::Func { name, .. } => format!("func_{}", name),
        };

        let asm_call_commands: Vec<Arc<asm_ast::Command>> = match sub_proc.next_call.as_ref() {
            Call::Func { name, param_pipelines, return_sub_proc } => {
                // set param values on func's stack
                let mut param_asm_commands = Vec::new();

                for (param_offset, pipeline) in param_pipelines.iter().enumerate() {
                    let pipeline_asm_commands = compile_pipeline(pipeline, &frame)?;

                    let asm_commands: Vec<Arc<asm_ast::Command>> = chain!(
                        // chain commands to put param value in RIGHT
                        pipeline_asm_commands,
                        // store addr for func param in LEFT
                        compute_param_addr(TEMP, param_offset + 2),
                        // move value in call var to func param
                        [data(asm_ast::DataCommand::MoveDerefDest(asm_ast::BinaryArgs {
                            dest: TEMP.value(),
                            val: EXPR.value(),
                        })),]
                    )
                    .collect();

                    param_asm_commands.extend(asm_commands);
                }

                // set return addr
                let Some(return_sub_proc_index) = sub_proc_uuid_to_index.get(return_sub_proc)
                else {
                    bail!("Failed to find return sub proc index");
                };

                let return_asm_commands: Vec<Arc<asm_ast::Command>> = chain!(
                    // store addr for call var in LEFT
                    compute_param_addr(TEMP, 1),
                    // store return addr in RIGHT
                    [data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                        dest: EXPR.value(),
                        val: label(&format!("{}.{}", asm_proc_name, return_sub_proc_index)),
                    }))],
                    // move value in RIGHT (return addr) to addr in LEFT (return addr var)
                    [data(asm_ast::DataCommand::MoveDerefDest(asm_ast::BinaryArgs {
                        dest: TEMP.value(),
                        val: EXPR.value()
                    }))],
                )
                .collect();

                let broadcast_asm_commands = Vec::from([
                    // broadcast function message
                    data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                        dest: RESULT.value(),
                        val: label(&format!("func_{}.0", name)),
                    })),
                    control(asm_ast::ControlCommand::Jump(asm_ast::UnaryArgs {
                        val: RESULT.value(),
                    })),
                ]);

                chain!(param_asm_commands, return_asm_commands, broadcast_asm_commands).collect()
            },
            Call::SubProc(sub_proc) => {
                let Some(sub_proc_index) = sub_proc_uuid_to_index.get(sub_proc) else {
                    bail!("Failed to find sub proc index");
                };

                Vec::from([
                    // broadcast function message
                    data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                        dest: RESULT.value(),
                        val: label(&format!("{}.{}", asm_proc_name, sub_proc_index)),
                    })),
                    control(asm_ast::ControlCommand::Jump(asm_ast::UnaryArgs {
                        val: RESULT.value(),
                    })),
                ])
            },
            Call::Return => {
                chain!(
                    // store return addr in RIGHT
                    compute_param_addr(EXPR, 1),
                    // broadcast function message
                    [
                        data(asm_ast::DataCommand::MoveDerefSrc(asm_ast::BinaryArgs {
                            dest: RESULT.value(),
                            val: EXPR.value()
                        })),
                        control(asm_ast::ControlCommand::Jump(asm_ast::UnaryArgs {
                            val: RESULT.value()
                        })),
                    ],
                )
                .collect()
            },
            Call::Terminate => Vec::from([control(asm_ast::ControlCommand::Exit)]),
            Call::IfBranch { cond_pipeline, then_sub_proc, pop_sub_proc } => {
                let Some(then_proc_index) = sub_proc_uuid_to_index.get(then_sub_proc) else {
                    bail!("Failed to find then sub proc index");
                };

                let Some(pop_proc_index) = sub_proc_uuid_to_index.get(pop_sub_proc) else {
                    bail!("Failed to find pop sub proc index");
                };

                chain!(
                    compile_pipeline(cond_pipeline, &frame)?,
                    // store `then` sub proc name in OPERAND and `pop` sub proc name in RESULT
                    [
                        data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                            dest: OPERAND.value(),
                            val: label(&format!("{}.{}", asm_proc_name, then_proc_index)),
                        })),
                        data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                            dest: RESULT.value(),
                            val: label(&format!("{}.{}", asm_proc_name, pop_proc_index)),
                        })),
                    ],
                    [
                        control(asm_ast::ControlCommand::Branch(asm_ast::BinaryArgs {
                            dest: OPERAND.value(),
                            val: EXPR.value(),
                        })),
                        control(asm_ast::ControlCommand::Jump(asm_ast::UnaryArgs {
                            val: RESULT.value(),
                        }))
                    ]
                )
                .collect()
            },
            Call::IfElseBranch { cond_pipeline, then_sub_proc, else_sub_proc } => {
                let Some(then_proc_index) = sub_proc_uuid_to_index.get(then_sub_proc) else {
                    bail!("Failed to find then sub proc index");
                };

                let Some(else_proc_index) = sub_proc_uuid_to_index.get(else_sub_proc) else {
                    bail!("Failed to find else sub proc index");
                };

                chain!(
                    compile_pipeline(cond_pipeline, &frame)?,
                    // store `then` sub proc name in OPERAND and `else` sub proc name in RESULT
                    [
                        data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                            dest: OPERAND.value(),
                            val: label(&format!("{}.{}", asm_proc_name, then_proc_index)),
                        })),
                        data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
                            dest: RESULT.value(),
                            val: label(&format!("{}.{}", asm_proc_name, else_proc_index)),
                        })),
                    ],
                    [
                        control(asm_ast::ControlCommand::Branch(asm_ast::BinaryArgs {
                            dest: OPERAND.value(),
                            val: EXPR.value(),
                        })),
                        control(asm_ast::ControlCommand::Jump(asm_ast::UnaryArgs {
                            val: RESULT.value(),
                        }))
                    ]
                )
                .collect()
            },
        };

        asm_commands.extend(asm_call_commands);

        // finalize sub proc
        let asm_sub_proc_name = format!("{}.{}", asm_proc_name, sub_proc_index);
        let asm_proc_kind = asm_ast::ProcedureKind::Sub { name: asm_sub_proc_name.into() };
        let asm_proc = asm_ast::Procedure { kind: asm_proc_kind, commands: asm_commands };
        asm_procs.push(Arc::new(asm_proc));
    }

    Ok(asm_procs)
}

fn compile_value(
    value: &Value,
    frame: &Frame,
    dest: PubRegister,
) -> anyhow::Result<Vec<Arc<asm_ast::Command>>> {
    let asm_commands = match value {
        Value::Literal(val) => Vec::from([data(asm_ast::DataCommand::Set(asm_ast::BinaryArgs {
            dest: dest.value(),
            val: literal(val),
        }))]),
        Value::Ident(ident) => {
            let offset = frame.get_ident_offset(ident)?;
            get_stack_value(dest, offset)
        },
        Value::Ref(ident) => {
            let offset = frame.get_ident_offset(ident)?;
            compute_stack_addr(dest, offset)
        },
    };

    Ok(asm_commands)
}

fn compile_operation(
    operation: &Operation,
    frame: &Frame,
) -> anyhow::Result<Vec<Arc<asm_ast::Command>>> {
    macro_rules! binary_op {
        ($com:ident, $operand:expr $(,)?) => {
            chain!(
                compile_value($operand, frame, TEMP)?,
                [data(asm_ast::DataCommand::$com(asm_ast::TernaryArgs {
                    dest: EXPR.value(),
                    left: EXPR.value(),
                    right: TEMP.value(),
                }))],
            )
            .collect()
        };
    }

    let asm_commands: Vec<Arc<asm_ast::Command>> = match operation {
        Operation::Deref => {
            chain!(
                // deref RIGHT
                [data(asm_ast::DataCommand::MoveDerefSrc(asm_ast::BinaryArgs {
                    dest: EXPR.value(),
                    val: EXPR.value(),
                }))]
            )
            .collect()
        },
        Operation::Add { operand } => binary_op!(Add, operand),
        Operation::Sub { operand } => binary_op!(Sub, operand),
        Operation::Mul { operand } => binary_op!(Mul, operand),
        Operation::Div { operand } => binary_op!(Div, operand),
        Operation::Mod { operand } => binary_op!(Mod, operand),
        Operation::Eq { operand } => binary_op!(Eq, operand),
        Operation::Neq { operand } => binary_op!(Neq, operand),
        Operation::Gt { operand } => binary_op!(Gt, operand),
        Operation::Lt { operand } => binary_op!(Lt, operand),
        Operation::Gte { operand } => binary_op!(Gte, operand),
        Operation::Lte { operand } => binary_op!(Lte, operand),
        Operation::And { operand } => binary_op!(And, operand),
        Operation::Or { operand } => binary_op!(Or, operand),
        Operation::Xor { operand } => binary_op!(Xor, operand),
        Operation::Not => chain!([data(asm_ast::DataCommand::Not(asm_ast::BinaryArgs {
            dest: EXPR.value(),
            val: EXPR.value(),
        }))])
        .collect(),
        Operation::Join { operand } => binary_op!(Join, operand),
    };

    Ok(asm_commands)
}

fn compile_pipeline(
    pipeline: &Pipeline,
    frame: &Frame,
) -> anyhow::Result<Vec<Arc<asm_ast::Command>>> {
    let mut asm_commands = Vec::new();

    asm_commands.extend(compile_value(&pipeline.initial_val, frame, EXPR)?);

    for operation in pipeline.operations.iter().map(AsRef::as_ref) {
        asm_commands.extend(compile_operation(operation, frame)?);
    }

    Ok(asm_commands)
}
