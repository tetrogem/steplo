use std::{fmt::Display, sync::Arc};

use itertools::Itertools;
use serde_json::{Map as JsMap, Value as JsVal, json};
use uuid::Uuid;

use crate::ir::{
    ArgumentOp, Block, Broadcast, ControlOp, DataOp, EventOp, Expr, List, Literal, Monitor, Op,
    OperatorOp, ProcedureOp, Program, SensingOp, Stage, Variable,
};

impl Program {
    pub fn compile(&self) -> JsVal {
        let stages: Vec<JsVal> = self.stages.iter().map(|x| x.compile()).collect();
        let monitors: Vec<JsVal> = self.monitors.iter().map(|x| x.compile()).collect();

        json!({
            "targets": stages,
            "monitors": monitors,
            "extensions": [],
            "meta": {
                "semver": "3.0.0",
                "vm": "2.3.4",
                "agent": "Steplo-v0.0.3" // TODO: Update version number each release
            }
        })
    }
}

impl Monitor {
    pub fn compile(&self) -> JsVal {
        json!({
            "id": self.list.uuid.to_string(),
            "mode": "list",
            "opcode": "data_listcontents",
            "params": { "LIST": self.list.name.to_string() },
            "spriteName": null,
            "value": [],
            "width": self.width,
            "height": self.height,
            "x": self.x,
            "y": self.y,
            "visible": self.visible,
        })
    }
}

impl Stage {
    pub fn compile(&self) -> JsVal {
        let variable_uuid_to_def: JsMap<String, JsVal> = self
            .variables
            .iter()
            .map(|variable| (variable.uuid.to_string(), json!([variable.name.to_string(), ""])))
            .collect();

        let list_uuid_to_def: JsMap<String, JsVal> = self
            .lists
            .iter()
            .map(|list| (list.uuid.to_string(), json!([list.name.to_string(), []])))
            .collect();

        let broadcast_uuid_to_def: JsMap<String, JsVal> = self
            .broadcasts
            .iter()
            .map(|broadcast| {
                (broadcast.uuid.to_string(), JsVal::String(broadcast.name.to_string()))
            })
            .collect();

        let block_uuid_to_def: JsMap<String, JsVal> =
            self.blocks.iter().map(|block| block.compile()).collect();

        json!({
            "isStage": true,
            "name": "Stage",
            "variables": variable_uuid_to_def,
            "lists": list_uuid_to_def,
            "broadcasts": broadcast_uuid_to_def,
            "blocks": block_uuid_to_def,
            "comments": {},
            "currentCostume": 0,
            "costumes": [
                {
                    "name": "backdrop1",
                    "dataFormat": "svg",
                    "assetId": "cd21514d0531fdffb22204e0ec5ed84a",
                    "md5ext": "cd21514d0531fdffb22204e0ec5ed84a.svg",
                    "rotationCenterX": 240,
                    "rotationCenterY": 180,
                },
            ],
            "sounds": [],
            "volume": 100,
            "layerOrder": 0,
            "tempo": 60,
            "videoTransparency": 50,
            "videoState": "on",
            "textToSpeechLanguage": null,
        })
    }
}

impl Block {
    pub fn compile(&self) -> (String, JsVal) {
        let metadata = self.op.compile_metadata();

        let mut value = obj([
            ("opcode", metadata.opcode.into()),
            ("next", Self::compile_block_link(self.next)),
            ("parent", Self::compile_block_link(self.parent)),
            ("inputs", metadata.inputs.into()),
            ("fields", metadata.fields.into()),
            ("topLevel", self.parent.is_none().into()),
            ("x", 0.into()),
            ("y", 0.into()),
        ]);

        if let Some(mutation) = metadata.mutation {
            let mut mutation_json = obj([
                ("tagName", "mutation".into()),
                ("children", JsVal::Array(Vec::new())),
                ("proccode", mutation.proccode.as_ref().into()),
                ("warp", "true".into()),
            ]);

            if let Some(ids) = mutation.argumentids {
                mutation_json.insert(
                    "argumentids".into(),
                    format!(
                        "[{}]",
                        ids.iter().map(|id| format!("\"{}\"", id.escape_default())).join(", ")
                    )
                    .into(),
                );
            }

            if let Some(names) = mutation.argumentnames {
                mutation_json.insert(
                    "argumentnames".into(),
                    format!(
                        "[{}]",
                        names
                            .iter()
                            .map(|name| format!("\"{}\"", name.escape_default()))
                            .join(", ")
                    )
                    .into(),
                );
            }

            if let Some(defaults) = mutation.argumentdefaults {
                mutation_json.insert(
                    "argumentdefaults".into(),
                    format!(
                        "[{}]",
                        defaults
                            .iter()
                            .map(|default| format!("\"{}\"", default.escape_default()))
                            .join(", ")
                    )
                    .into(),
                );
            }

            value = value.into_iter().chain(obj([("mutation", mutation_json.into())])).collect();
        }

        (self.uuid.to_string(), value.into())
    }

    fn compile_block_link(link: Option<Uuid>) -> JsVal {
        match link {
            None => json!(null),
            Some(uuid) => json!(uuid.to_string()),
        }
    }
}

impl Op {
    fn compile_metadata(&self) -> ExprMetadata {
        let compile = |expr: &Expr| expr.compile();
        let compile_option = |expr: &Option<Arc<Expr>>| expr.as_ref().map(|x| compile(x));

        match self {
            Self::Event(op) => match op {
                EventOp::WhenFlagClicked => ExprMetadata {
                    opcode: "event_whenflagclicked",
                    inputs: JsMap::new(),
                    fields: JsMap::new(),
                    mutation: None,
                },
                EventOp::WhenBroadcastReceived { broadcast } => ExprMetadata {
                    opcode: "event_whenbroadcastreceived",
                    inputs: JsMap::new(),
                    fields: obj([("BROADCAST_OPTION", broadcast_ref(broadcast))]),
                    mutation: None,
                },
                EventOp::BroadcastAndWait { input } => ExprMetadata {
                    opcode: "event_broadcastandwait",
                    inputs: obj([("BROADCAST_INPUT", compile(input))]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                EventOp::Broadcast { input } => ExprMetadata {
                    opcode: "event_broadcast",
                    inputs: obj([("BROADCAST_INPUT", compile(input))]),
                    fields: JsMap::new(),
                    mutation: None,
                },
            },
            Self::Data(op) => match op {
                DataOp::AddToList { item, list } => ExprMetadata {
                    opcode: "data_addtolist",
                    inputs: obj([("ITEM", compile(item))]),
                    fields: obj([("LIST", list_ref(list))]),
                    mutation: None,
                },
                DataOp::DeleteAllOfList { list } => ExprMetadata {
                    opcode: "data_deletealloflist",
                    inputs: JsMap::new(),
                    fields: obj([("LIST", list_ref(list))]),
                    mutation: None,
                },
                DataOp::DeleteOfList { index, list } => ExprMetadata {
                    opcode: "data_deleteoflist",
                    inputs: obj([("INDEX", compile(index))]),
                    fields: obj([("LIST", list_ref(list))]),
                    mutation: None,
                },
                DataOp::ReplaceItemOfList { index, item, list } => ExprMetadata {
                    opcode: "data_replaceitemoflist",
                    inputs: obj([("INDEX", compile(index)), ("ITEM", compile(item))]),
                    fields: obj([("LIST", list_ref(list))]),
                    mutation: None,
                },
                DataOp::LengthOfList { list } => ExprMetadata {
                    opcode: "data_lengthoflist",
                    inputs: JsMap::new(),
                    fields: obj([("LIST", list_ref(list))]),
                    mutation: None,
                },
                DataOp::ItemOfList { list, index } => ExprMetadata {
                    opcode: "data_itemoflist",
                    inputs: obj([("INDEX", compile(index))]),
                    fields: obj([("LIST", list_ref(list))]),
                    mutation: None,
                },
                DataOp::SetVariableTo { variable, value } => ExprMetadata {
                    opcode: "data_setvariableto",
                    inputs: obj([("VALUE", compile(value))]),
                    fields: obj([("VARIABLE", variable_ref(variable))]),
                    mutation: None,
                },
            },
            Self::Control(op) => match op {
                ControlOp::If { condition, then_substack } => ExprMetadata {
                    opcode: "control_if",
                    inputs: option_obj([
                        ("CONDITION", Some(compile(condition))),
                        ("SUBSTACK", compile_option(then_substack)),
                    ]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                ControlOp::IfElse { condition, then_substack, else_substack } => ExprMetadata {
                    opcode: "control_if_else",
                    inputs: option_obj([
                        ("CONDITION", Some(compile(condition))),
                        ("SUBSTACK", compile_option(then_substack)),
                        ("SUBSTACK2", compile_option(else_substack)),
                    ]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                ControlOp::Wait { duration_s } => ExprMetadata {
                    opcode: "control_wait",
                    inputs: obj([("DURATION", compile(duration_s))]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                ControlOp::Repeat { times, looped_substack } => ExprMetadata {
                    opcode: "control_repeat",
                    inputs: option_obj([
                        ("TIMES", Some(compile(times))),
                        ("SUBSTACK", compile_option(looped_substack)),
                    ]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                ControlOp::RepeatUntil { condition, then_substack } => ExprMetadata {
                    opcode: "control_repeat_until",
                    inputs: option_obj([
                        ("CONDITION", Some(compile(condition))),
                        ("SUBSTACK", compile_option(then_substack)),
                    ]),
                    fields: JsMap::new(),
                    mutation: None,
                },
            },
            Self::Sensing(op) => match op {
                SensingOp::AskAndWait { question } => ExprMetadata {
                    opcode: "sensing_askandwait",
                    inputs: obj([("QUESTION", compile(question))]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                SensingOp::Answer => ExprMetadata {
                    opcode: "sensing_answer",
                    inputs: JsMap::new(),
                    fields: JsMap::new(),
                    mutation: None,
                },
                SensingOp::Timer => ExprMetadata {
                    opcode: "sensing_timer",
                    inputs: JsMap::new(),
                    fields: JsMap::new(),
                    mutation: None,
                },
                SensingOp::DaysSince2000 => ExprMetadata {
                    opcode: "sensing_dayssince2000",
                    inputs: JsMap::new(),
                    fields: JsMap::new(),
                    mutation: None,
                },
            },
            Self::Operator(op) => match op {
                OperatorOp::Subtract { num_a, num_b } => ExprMetadata {
                    opcode: "operator_subtract",
                    inputs: obj([("NUM1", compile(num_a)), ("NUM2", compile(num_b))]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                OperatorOp::Mod { num_a, num_b } => ExprMetadata {
                    opcode: "operator_mod",
                    inputs: obj([("NUM1", compile(num_a)), ("NUM2", compile(num_b))]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                OperatorOp::Add { num_a, num_b } => ExprMetadata {
                    opcode: "operator_add",
                    inputs: obj([("NUM1", compile(num_a)), ("NUM2", compile(num_b))]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                OperatorOp::Multiply { num_a, num_b } => ExprMetadata {
                    opcode: "operator_multiply",
                    inputs: obj([("NUM1", compile(num_a)), ("NUM2", compile(num_b))]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                OperatorOp::Divide { num_a, num_b } => ExprMetadata {
                    opcode: "operator_divide",
                    inputs: obj([("NUM1", compile(num_a)), ("NUM2", compile(num_b))]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                OperatorOp::Join { string_a, string_b } => ExprMetadata {
                    opcode: "operator_join",
                    inputs: obj([("STRING1", compile(string_a)), ("STRING2", compile(string_b))]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                OperatorOp::Random { from, to } => ExprMetadata {
                    opcode: "operator_random",
                    inputs: obj([("FROM", compile(from)), ("TO", compile(to))]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                OperatorOp::Equals { operand_a, operand_b } => ExprMetadata {
                    opcode: "operator_equals",
                    inputs: obj([
                        ("OPERAND1", compile(operand_a)),
                        ("OPERAND2", compile(operand_b)),
                    ]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                OperatorOp::Not { operand } => ExprMetadata {
                    opcode: "operator_not",
                    inputs: obj([("OPERAND", compile(operand))]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                OperatorOp::GreaterThan { operand_a, operand_b } => ExprMetadata {
                    opcode: "operator_gt",
                    inputs: obj([
                        ("OPERAND1", compile(operand_a)),
                        ("OPERAND2", compile(operand_b)),
                    ]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                OperatorOp::LessThan { operand_a, operand_b } => ExprMetadata {
                    opcode: "operator_lt",
                    inputs: obj([
                        ("OPERAND1", compile(operand_a)),
                        ("OPERAND2", compile(operand_b)),
                    ]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                OperatorOp::And { operand_a, operand_b } => ExprMetadata {
                    opcode: "operator_and",
                    inputs: obj([
                        ("OPERAND1", compile(operand_a)),
                        ("OPERAND2", compile(operand_b)),
                    ]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                OperatorOp::Or { operand_a, operand_b } => ExprMetadata {
                    opcode: "operator_or",
                    inputs: obj([
                        ("OPERAND1", compile(operand_a)),
                        ("OPERAND2", compile(operand_b)),
                    ]),
                    fields: JsMap::new(),
                    mutation: None,
                },
            },
            Self::Procedure(op) => match op {
                ProcedureOp::Definition { prototype_stack } => ExprMetadata {
                    opcode: "procedures_definition",
                    inputs: obj([("custom_block", compile(prototype_stack))]),
                    fields: JsMap::new(),
                    mutation: None,
                },
                ProcedureOp::Prototype { custom_block, arguments_with_stacks } => ExprMetadata {
                    opcode: "procedures_prototype",
                    inputs: obj(arguments_with_stacks
                        .iter()
                        .map(|(argument, stack)| (argument.uuid.to_string(), compile(stack)))),
                    fields: JsMap::new(),
                    mutation: Some(MutationTag {
                        proccode: custom_block.name.clone(),
                        argumentids: Some(Arc::new(
                            arguments_with_stacks
                                .iter()
                                .map(|(argument, _)| argument.uuid.to_string().into())
                                .collect(),
                        )),
                        argumentnames: Some(Arc::new(
                            arguments_with_stacks
                                .iter()
                                .map(|(argument, _)| argument.name.clone())
                                .collect(),
                        )),
                        argumentdefaults: Some(Arc::new(
                            arguments_with_stacks
                                .iter()
                                .map(|(argument, _)| argument.default.clone())
                                .collect(),
                        )),
                    }),
                },
                ProcedureOp::Call { custom_block, argument_inputs } => ExprMetadata {
                    opcode: "procedures_call",
                    inputs: obj(argument_inputs
                        .iter()
                        .map(|(argument, expr)| (argument.uuid.to_string(), compile(expr)))),
                    fields: JsMap::new(),
                    mutation: Some(MutationTag {
                        proccode: custom_block.name.clone(),
                        argumentids: Some(Arc::new(
                            argument_inputs
                                .iter()
                                .map(|(argument, _)| argument.uuid.to_string().into())
                                .collect(),
                        )),
                        argumentnames: None,
                        argumentdefaults: None,
                    }),
                },
            },
            Self::Argument(op) => match op {
                ArgumentOp::ReporterStringNumber { arg } => ExprMetadata {
                    opcode: "argument_reporter_string_number",
                    inputs: JsMap::new(),
                    fields: obj([("VALUE", [arg.name.as_ref().into(), JsVal::Null].into())]),
                    mutation: None,
                },
            },
        }
    }
}

impl Expr {
    pub fn compile(&self) -> JsVal {
        match self {
            Self::Literal(l) => l.compile(),
            Self::Derived(d) => {
                json!([3, d.uuid.to_string(), [7, ""]])
            },
            Self::Variable(v) => {
                json!([3, [12, v.name.to_string(), v.uuid.to_string()]])
            },
            Self::Broadcast(b) => {
                json!([1, [11, b.name.to_string(), b.uuid.to_string()]])
            },
            Self::Stack(s) => json!([1, s.uuid.to_string()]),
        }
    }
}

impl Literal {
    pub fn compile(&self) -> JsVal {
        let value = match self {
            Self::Num(n) => json!([4, n.to_string()]),
            Self::PosNum(n) => json!([5, n.to_string()]),
            Self::PosInt(i) => json!([6, i.to_string()]),
            Self::Int(i) => json!([7, i.to_string()]),
            Self::Angle(a) => json!([8, a.to_string()]),
            Self::Color { r, g, b } => {
                json!([9, format!("#{:02X}{:02X}{:02X}", r, g, b)])
            },
            Self::String(s) => json!([10, s.to_string()]),
        };

        json!([1, value])
    }
}

struct ExprMetadata {
    opcode: &'static str,
    inputs: JsMap<String, JsVal>,
    fields: JsMap<String, JsVal>,
    mutation: Option<MutationTag>,
}

struct MutationTag {
    proccode: Arc<str>,
    argumentids: Option<Arc<Vec<Arc<str>>>>,
    argumentnames: Option<Arc<Vec<Arc<str>>>>,
    argumentdefaults: Option<Arc<Vec<Arc<str>>>>,
}

fn obj<K: Display>(fields: impl IntoIterator<Item = (K, JsVal)>) -> JsMap<String, JsVal> {
    fields.into_iter().map(|(key, value)| (key.to_string(), value)).collect()
}

fn option_obj<K: Display>(
    fields: impl IntoIterator<Item = (K, Option<JsVal>)>,
) -> JsMap<String, JsVal> {
    let fields = fields.into_iter().filter_map(|(key, value)| value.map(|value| (key, value)));
    obj(fields)
}

fn variable_ref(variable: &Variable) -> JsVal {
    json!([variable.name.to_string(), variable.uuid.to_string()])
}

fn list_ref(list: &List) -> JsVal {
    json!([list.name.to_string(), list.uuid.to_string()])
}

fn broadcast_ref(broadcast: &Broadcast) -> JsVal {
    json!([broadcast.name.to_string(), broadcast.uuid.to_string()])
}
