use std::fmt::Display;

use serde_json::{Map as JsMap, Value as JsVal, json};
use uuid::Uuid;

use super::{
    Block, Broadcast, ControlOp, DataOp, EventOp, Expr, List, Literal, Monitor, Op, OperatorOp,
    Program, SensingOp, Stage, Variable,
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
                "agent": "Steplo-v0.0.1" // TODO: Update version number each release
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

        let value = json!({
            "opcode": metadata.opcode,
            "next": Self::compile_block_link(self.next),
            "parent": Self::compile_block_link(self.parent),
            "inputs": metadata.inputs,
            "fields": metadata.fields,
            "topLevel": self.parent.is_none(),
            "x": 0,
            "y": 0,
        });

        (self.uuid.to_string(), value)
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
        match self {
            Self::Event(op) => match op {
                EventOp::WhenFlagClicked => ExprMetadata {
                    opcode: "event_whenflagclicked",
                    inputs: JsMap::new(),
                    fields: JsMap::new(),
                },
                EventOp::WhenBroadcastReceived { broadcast } => ExprMetadata {
                    opcode: "event_whenbroadcastreceived",
                    inputs: JsMap::new(),
                    fields: obj([("BROADCAST_OPTION", broadcast_ref(broadcast))]),
                },
                EventOp::BroadcastAndWait { input } => ExprMetadata {
                    opcode: "event_broadcastandwait",
                    inputs: obj([("BROADCAST_INPUT", compile(input))]),
                    fields: JsMap::new(),
                },
                EventOp::Broadcast { input } => ExprMetadata {
                    opcode: "event_broadcast",
                    inputs: obj([("BROADCAST_INPUT", compile(input))]),
                    fields: JsMap::new(),
                },
            },
            Self::Data(op) => match op {
                DataOp::AddToList { item, list } => ExprMetadata {
                    opcode: "data_addtolist",
                    inputs: obj([("ITEM", compile(item))]),
                    fields: obj([("LIST", list_ref(list))]),
                },
                DataOp::DeleteAllOfList { list } => ExprMetadata {
                    opcode: "data_deletealloflist",
                    inputs: JsMap::new(),
                    fields: obj([("LIST", list_ref(list))]),
                },
                DataOp::DeleteOfList { index, list } => ExprMetadata {
                    opcode: "data_deleteoflist",
                    inputs: obj([("INDEX", compile(index))]),
                    fields: obj([("LIST", list_ref(list))]),
                },
                DataOp::ReplaceItemOfList { index, item, list } => ExprMetadata {
                    opcode: "data_replaceitemoflist",
                    inputs: obj([("INDEX", compile(index)), ("ITEM", compile(item))]),
                    fields: obj([("LIST", list_ref(list))]),
                },
                DataOp::LengthOfList { list } => ExprMetadata {
                    opcode: "data_lengthoflist",
                    inputs: JsMap::new(),
                    fields: obj([("LIST", list_ref(list))]),
                },
                DataOp::ItemOfList { list, index } => ExprMetadata {
                    opcode: "data_itemoflist",
                    inputs: obj([("INDEX", compile(index))]),
                    fields: obj([("LIST", list_ref(list))]),
                },
                DataOp::SetVariableTo { variable, value } => ExprMetadata {
                    opcode: "data_setvariableto",
                    inputs: obj([("VALUE", compile(value))]),
                    fields: obj([("VARIABLE", variable_ref(variable))]),
                },
            },
            Self::Control(op) => match op {
                ControlOp::If { condition, then_substack } => ExprMetadata {
                    opcode: "control_if",
                    inputs: obj([
                        ("CONDITION", compile(condition)),
                        ("SUBSTACK", compile(then_substack)),
                    ]),
                    fields: JsMap::new(),
                },
                ControlOp::IfElse { condition, then_substack, else_substack } => ExprMetadata {
                    opcode: "control_if_else",
                    inputs: obj([
                        ("CONDITION", compile(condition)),
                        ("SUBSTACK", compile(then_substack)),
                        ("SUBSTACK2", compile(else_substack)),
                    ]),
                    fields: JsMap::new(),
                },
                ControlOp::Wait { .. } => {
                    ExprMetadata { opcode: "control_wait", inputs: todo!(), fields: todo!() }
                },
                ControlOp::Repeat { times, looped_substack } => ExprMetadata {
                    opcode: "control_repeat",
                    inputs: obj([
                        ("TIMES", compile(times)),
                        ("SUBSTACK", compile(looped_substack)),
                    ]),
                    fields: JsMap::new(),
                },
            },
            Self::Sensing(op) => match op {
                SensingOp::AskAndWait { question } => ExprMetadata {
                    opcode: "sensing_askandwait",
                    inputs: obj([("QUESTION", compile(question))]),
                    fields: JsMap::new(),
                },
                SensingOp::Answer => ExprMetadata {
                    opcode: "sensing_answer",
                    inputs: JsMap::new(),
                    fields: JsMap::new(),
                },
                SensingOp::Timer => ExprMetadata {
                    opcode: "sensing_timer",
                    inputs: JsMap::new(),
                    fields: JsMap::new(),
                },
            },
            Self::Operator(op) => match op {
                OperatorOp::Subtract { num_a, num_b } => ExprMetadata {
                    opcode: "operator_subtract",
                    inputs: obj([("NUM1", compile(num_a)), ("NUM2", compile(num_b))]),
                    fields: JsMap::new(),
                },
                OperatorOp::Mod { num_a, num_b } => ExprMetadata {
                    opcode: "operator_mod",
                    inputs: obj([("NUM1", compile(num_a)), ("NUM2", compile(num_b))]),
                    fields: JsMap::new(),
                },
                OperatorOp::Add { num_a, num_b } => ExprMetadata {
                    opcode: "operator_add",
                    inputs: obj([("NUM1", compile(num_a)), ("NUM2", compile(num_b))]),
                    fields: JsMap::new(),
                },
                OperatorOp::Multiply { num_a, num_b } => ExprMetadata {
                    opcode: "operator_multiply",
                    inputs: obj([("NUM1", compile(num_a)), ("NUM2", compile(num_b))]),
                    fields: JsMap::new(),
                },
                OperatorOp::Divide { num_a, num_b } => ExprMetadata {
                    opcode: "operator_divide",
                    inputs: obj([("NUM1", compile(num_a)), ("NUM2", compile(num_b))]),
                    fields: JsMap::new(),
                },
                OperatorOp::Join { string_a, string_b } => ExprMetadata {
                    opcode: "operator_join",
                    inputs: obj([("STRING1", compile(string_a)), ("STRING2", compile(string_b))]),
                    fields: JsMap::new(),
                },
                OperatorOp::Random { from, to } => ExprMetadata {
                    opcode: "operator_random",
                    inputs: obj([("FROM", compile(from)), ("TO", compile(to))]),
                    fields: JsMap::new(),
                },
                OperatorOp::Equals { operand_a, operand_b } => ExprMetadata {
                    opcode: "operator_equals",
                    inputs: obj([
                        ("OPERAND1", compile(operand_a)),
                        ("OPERAND2", compile(operand_b)),
                    ]),
                    fields: JsMap::new(),
                },
                OperatorOp::Not { operand } => ExprMetadata {
                    opcode: "operator_not",
                    inputs: obj([("OPERAND", compile(operand))]),
                    fields: JsMap::new(),
                },
                OperatorOp::GreaterThan { operand_a, operand_b } => ExprMetadata {
                    opcode: "operator_gt",
                    inputs: obj([
                        ("OPERAND1", compile(operand_a)),
                        ("OPERAND2", compile(operand_b)),
                    ]),
                    fields: JsMap::new(),
                },
                OperatorOp::LessThan { operand_a, operand_b } => ExprMetadata {
                    opcode: "operator_lt",
                    inputs: obj([
                        ("OPERAND1", compile(operand_a)),
                        ("OPERAND2", compile(operand_b)),
                    ]),
                    fields: JsMap::new(),
                },
                OperatorOp::And { operand_a, operand_b } => ExprMetadata {
                    opcode: "operator_and",
                    inputs: obj([
                        ("OPERAND1", compile(operand_a)),
                        ("OPERAND2", compile(operand_b)),
                    ]),
                    fields: JsMap::new(),
                },
                OperatorOp::Or { operand_a, operand_b } => ExprMetadata {
                    opcode: "operator_or",
                    inputs: obj([
                        ("OPERAND1", compile(operand_a)),
                        ("OPERAND2", compile(operand_b)),
                    ]),
                    fields: JsMap::new(),
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
            Self::Stack(s) => json!([2, s.uuid.to_string()]),
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
}

fn obj<K: Display>(fields: impl IntoIterator<Item = (K, JsVal)>) -> JsMap<String, JsVal> {
    fields.into_iter().map(|(key, value)| (key.to_string(), value)).collect()
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
