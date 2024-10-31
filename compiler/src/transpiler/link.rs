// use std::sync::Arc;

// use inter::ez;

// use crate::parser::{
//     command::Command,
//     scope::{Chunk, ScopeHeader},
// };

// use super::{Globals, ScopeTagManager};

// pub struct Link {
//     pub header: Arc<ScopeHeader>,
//     pub tags: ScopeTagManager,
//     pub chunk: Chunk,
//     /// the continue broadcast for this chunk
//     pub continue_broadcast: Arc<ez::Broadcast>,
//     /// the chunk to go to once finished executing
//     pub next_cb: Option<Arc<ez::Broadcast>>,
// }

// pub fn transpile_link(link: &Link, globals: &Globals) {
//     let blocks = link.chunk.body.iter().map(|command| {
//         transpile_command(&command.inner, globals);
//     });
// }

// pub fn transpile_command(command: &Command, globals: &Globals) -> Vec<ez::Op> {
//     match command {
//         Command::Clear => Vec::from([ez::Op::Data(ez::DataOp::DeleteAllOfList {
//             list: Arc::clone(&globals.stdout),
//         })]),
//         Command::Out { msg } => {
//             Vec::from([ez::Op::Data(ez::DataOp::ReplaceItemOfList { list: Arc::clone(&globals.stdout), index: ez::D, item: () })])
//         }
//         _ => todo!(),
//     }
// }
