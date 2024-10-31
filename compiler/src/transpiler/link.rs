// use std::sync::Arc;

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
//     pub continue_broadcast: Arc<ir::Broadcast>,
//     /// the chunk to go to once finished executing
//     pub next_cb: Option<Arc<ir::Broadcast>>,
// }

// pub fn transpile_link(link: &Link, globals: &Globals) {
//     let blocks = link.chunk.body.iter().map(|command| {
//         transpile_command(&command.inner, globals);
//     });
// }

// pub fn transpile_command(command: &Command, globals: &Globals) -> Vec<ir::Op> {
//     match command {
//         Command::Clear => Vec::from([ir::Op::Data(ir::DataOp::DeleteAllOfList {
//             list: Arc::clone(&globals.stdout),
//         })]),
//         Command::Out { msg } => {
//             Vec::from([ir::Op::Data(ir::DataOp::ReplaceItemOfList { list: Arc::clone(&globals.stdout), index: ir::, item: () })])
//         }
//         _ => todo!(),
//     }
// }
