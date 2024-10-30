use std::{fmt::Display, fs::Metadata, sync::Arc};

use itertools::Itertools;
use serde_json::{json, Map as JsMap, Value as JsVal};
use uuid::Uuid;

use crate::ir::SensingDerived;

use super::{
    Block, Broadcast, ControlOp, DataDerived, DataOp, Derived, EventOp, Expr, List, Literal, Op,
    OpDerived, Program, SensingOp, Stage,
};

impl Stage {
    pub fn compile(&self) -> JsVal {
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
            self.blocks.iter().flat_map(|block| block.compile()).collect();

        json!({
            "isStage": true,
            "name": "Stage",
            "variables": {},
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
    pub fn compile(&self) -> JsMap<String, JsVal> {
        let data = self.op.compile_metadata(self.uuid);

        let js_val = json!({
            "opcode": data.metadata.opcode,
            "next": Self::compile_block_link(self.next.as_ref().map(|x| x.as_ref())),
            "parent": Self::compile_block_link(self.parent.as_ref().map(|x| x.as_ref())),
            "inputs": data.metadata.inputs,
            "fields": data.metadata.fields,
            "topLevel": self.parent.is_none(),
            "x": 0,
            "y": 0,
        });

        let fields = data.deps.into_iter().map(|dep| (dep.uuid.to_string(), dep.js_val.clone()));
        let fields = fields.chain([(self.uuid.to_string(), js_val)]);
        fields.collect()
    }

    fn compile_block_link(uuid: Option<&Block>) -> JsVal {
        match uuid {
            None => json!(null),
            Some(link) => json!(link.uuid.to_string()),
        }
    }
}

impl Op {
    pub fn compile_metadata(&self, uuid: Uuid) -> ExprData {
        let mut deps = Vec::new();
        let mut compile = |expr| compile(uuid, &mut deps, expr);
        let metadata = match self {
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
                ControlOp::IfElse { .. } => {
                    ExprMetadata { opcode: "control_if_else", inputs: todo!(), fields: todo!() }
                },
                ControlOp::Wait { .. } => {
                    ExprMetadata { opcode: "control_wait", inputs: todo!(), fields: todo!() }
                },
            },
            Self::Sensing(op) => match op {
                SensingOp::AskAndWait { .. } => {
                    ExprMetadata { opcode: "sensing_askandwait", inputs: todo!(), fields: todo!() }
                },
            },
        };

        ExprData { metadata, deps }
    }
}

struct ExprCompiled {
    js_val: JsVal,
    deps: Vec<Arc<Dep>>,
}

impl Expr {
    pub fn compile(&self, parent: Uuid) -> ExprCompiled {
        match self {
            Self::Literal(l) => l.compile(),
            Self::Derived(d) => d.compile(parent),
            Self::Broadcast(b) => ExprCompiled {
                js_val: json!([1, [11, b.name.to_string(), b.uuid.to_string()]]),
                deps: Vec::new(),
            },
            Self::Stack(s) => {
                let blocks = [s.root]s.chain.iter().chain()
                ExprCompiled { js_val: json!([2, s.root.uuid.to_string()]), deps:  }
            }
        }
    }
}

impl Literal {
    pub fn compile(&self) -> ExprCompiled {
        let js_val = match self {
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

        ExprCompiled { js_val, deps: Vec::new() }
    }
}

struct ExprMetadata {
    opcode: &'static str,
    inputs: JsMap<String, JsVal>,
    fields: JsMap<String, JsVal>,
}

struct ExprData {
    metadata: ExprMetadata,
    deps: Vec<Arc<Dep>>,
}

impl Derived {
    pub fn compile(&self, parent: Uuid) -> ExprCompiled {
        let uuid = Uuid::new_v4();
        let compiled = self.compile_metadata(uuid, parent);
        let js_val = json!([3, uuid.to_string(), [7, ""]]);
        ExprCompiled { js_val, deps: compiled.deps }
    }

    fn compile_metadata(&self, uuid: Uuid, parent: Uuid) -> ExprData {
        let mut deps = Vec::new();

        let mut compile = |expr| compile(uuid, &mut deps, expr);

        let metadata = match self {
            Self::Data(op) => match op.as_ref() {
                DataDerived::LengthOfList { list } => ExprMetadata {
                    opcode: "data_lengthoflist",
                    inputs: JsMap::new(),
                    fields: obj([("LIST", list_ref(list))]),
                },
                DataDerived::ItemOfList { list, index } => ExprMetadata {
                    opcode: "data_itemoflist",
                    inputs: obj([("INDEX", compile(index))]),
                    fields: obj([("LIST", list_ref(list))]),
                },
            },
            Self::Op(op) => match op.as_ref() {
                OpDerived::Subtract { num_a, num_b } => ExprMetadata {
                    opcode: "operator_subtract",
                    inputs: obj([("NUM1", compile(num_a)), ("NUM2", compile(num_b))]),
                    fields: JsMap::new(),
                },
                OpDerived::Mod { num_a, num_b } => ExprMetadata {
                    opcode: "operator_mod",
                    inputs: obj([("NUM1", compile(num_a)), ("NUM2", compile(num_b))]),
                    fields: JsMap::new(),
                },
                OpDerived::Add { num_a, num_b } => ExprMetadata {
                    opcode: "operator_add",
                    inputs: obj([("NUM1", compile(num_a)), ("NUM2", compile(num_b))]),
                    fields: JsMap::new(),
                },
                OpDerived::Multiply { num_a, num_b } => ExprMetadata {
                    opcode: "operator_multiply",
                    inputs: obj([("NUM1", compile(num_a)), ("NUM2", compile(num_b))]),
                    fields: JsMap::new(),
                },
                OpDerived::Divide { num_a, num_b } => ExprMetadata {
                    opcode: "operator_divide",
                    inputs: obj([("NUM1", compile(num_a)), ("NUM2", compile(num_b))]),
                    fields: JsMap::new(),
                },
                OpDerived::Join { string_a, string_b } => ExprMetadata {
                    opcode: "operator_join",
                    inputs: obj([("STRING1", compile(string_a)), ("STRING2", compile(string_b))]),
                    fields: JsMap::new(),
                },
                OpDerived::Random { from, to } => ExprMetadata {
                    opcode: "operator_random",
                    inputs: obj([("FROM", compile(from)), ("TO", compile(to))]),
                    fields: JsMap::new(),
                },
            },
            Self::Sensing(op) => match op.as_ref() {
                SensingDerived::Answer => ExprMetadata {
                    opcode: "sensing_answer",
                    inputs: JsMap::new(),
                    fields: JsMap::new(),
                },
                SensingDerived::Timer => ExprMetadata {
                    opcode: "sensing_timer",
                    inputs: JsMap::new(),
                    fields: JsMap::new(),
                },
            },
        };

        deps.push(Arc::new(Dep::new(parent, uuid, &metadata)));

        ExprData { metadata, deps }
    }
}

fn compile(uuid: Uuid, deps: &mut Vec<Arc<Dep>>, expr: &Expr) -> JsVal {
    let compiled = expr.compile(uuid);
    deps.extend(compiled.deps);
    compiled.js_val
}

fn obj<K: Display>(fields: impl IntoIterator<Item = (K, JsVal)>) -> JsMap<String, JsVal> {
    fields.into_iter().map(|(key, value)| (key.to_string(), value)).collect()
}

fn list_ref(list: &List) -> JsVal {
    json!([list.name.to_string(), list.uuid.to_string()])
}

fn broadcast_ref(broadcast: &Broadcast) -> JsVal {
    json!([broadcast.name.to_string(), broadcast.uuid.to_string()])
}

struct Dep {
    uuid: Uuid,
    js_val: JsVal,
}

impl Dep {
    fn new(parent: Uuid, uuid: Uuid, metadata: &ExprMetadata) -> Self {
        let js_val = json!({
            uuid.to_string(): {
                "opcode": metadata.opcode.to_string(),
                "next": null,
                "parent": parent.to_string(),
                "inputs": metadata.inputs,
                "fields": metadata.fields,
                "shadow": false,
                "topLevel": false,
            }
        });

        Self { uuid, js_val }
    }
}
