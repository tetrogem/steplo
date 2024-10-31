// pub mod link;

// use std::{collections::HashMap, sync::Arc};

// use anyhow::{bail, Context};
// use inter::ez;
// use itertools::Itertools;
// // use link::{transpile_link, Link};
// use uuid::Uuid;

// use crate::{
//     parser::{
//         command::{ApTag, Command},
//         scope::{Chunk, ScopeHeader, ScopeKind},
//         Parsed,
//     },
//     util::spellcheck,
// };

// pub fn transpile(parsed: Parsed) -> ez::Program {
//     // default lists included in all compiled programs
//     let globals = Arc::new(Globals {
//         stdout: Arc::new(ez::List { uuid: Uuid::new_v4(), name: "::stdout".into() }),
//         stack: Arc::new(ez::List { uuid: Uuid::new_v4(), name: "::stack".into() }),
//         args: Arc::new(ez::List { uuid: Uuid::new_v4(), name: "::args".into() }),
//     });

//     let mut links = Vec::new();
//     let mut cb_name_to_cb = HashMap::<Arc<str>, Arc<ez::Broadcast>>::new();

//     // link together each chunk via continue broadcasts
//     for scope in parsed.scopes {
//         let scope_tag_to_chunk_uuid = Arc::new(scope.tag_to_chunk_uuid);
//         let mut next_cb: Option<Arc<ez::Broadcast>> = None;

//         for (i, chunk) in scope.chunks.into_iter().enumerate().rev() {
//             let is_scope_root = i == 0;

//             // continue broadcast -- jumps to the next chunk once this one finishes executing
//             let cb = create_broadcast(&scope.header.kind, chunk.uuid, is_scope_root);
//             let cb = Arc::new(cb);
//             cb_name_to_cb.insert(Arc::clone(&cb.name), Arc::clone(&cb));

//             links.push(Link {
//                 chunk,
//                 header: Arc::clone(&scope.header),
//                 tags: ScopeTagManager {
//                     scope_tag_to_chunk_uuid: Arc::clone(&scope_tag_to_chunk_uuid),
//                 },
//                 continue_broadcast: Arc::clone(&cb),
//                 next_cb: next_cb.map(|cb| Arc::clone(&cb)),
//             });

//             next_cb = Some(Arc::clone(&cb));
//         }
//     }

//     let chains = links.into_iter().map(|link| transpile_link(&link, &globals)).collect_vec();

//     todo!()
// }

// pub struct Globals {
//     pub stdout: Arc<ez::List>,
//     pub stack: Arc<ez::List>,
//     pub args: Arc<ez::List>,
// }

// fn create_broadcast(kind: &ScopeKind, chunk_uuid: Uuid, is_scope_root: bool) -> ez::Broadcast {
//     let scope_name = match kind {
//         ScopeKind::Main => "::main",
//         ScopeKind::Fn { name } => name.as_str(),
//     };

//     let broadcast_name = match is_scope_root {
//         true => scope_name.into(),
//         false => format!("{}@{}", scope_name, chunk_uuid),
//     };

//     let broadcast_name: Arc<str> = broadcast_name.into();
//     ez::Broadcast { uuid: Uuid::new_v4(), name: Arc::clone(&broadcast_name) }
// }

// struct ScopeTagManager {
//     scope_tag_to_chunk_uuid: Arc<HashMap<Arc<String>, Uuid>>,
// }

// impl ScopeTagManager {
//     pub fn get_chunk_uuid(&self, tag: &ApTag) -> anyhow::Result<Uuid> {
//         self.get_chunk_uuid_by_name(&tag.inner).with_context(|| format!("In position {}", tag.pos))
//     }

//     fn get_chunk_uuid_by_name(&self, tag: &String) -> anyhow::Result<Uuid> {
//         let Some(chunk_uuid) = self.scope_tag_to_chunk_uuid.get(tag) else {
//             let mut err_msg = format!("No tag named '{}' found within this scope", tag);

//             if let Some(maybe_tag) =
//                 spellcheck(tag, self.scope_tag_to_chunk_uuid.keys().map(|k| k.as_str()))
//             {
//                 err_msg += &format!("; Perhaps you meant '{}'?", maybe_tag.term);
//             }

//             bail!(err_msg);
//         };

//         Ok(*chunk_uuid)
//     }
// }
