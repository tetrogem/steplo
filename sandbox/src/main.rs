use std::sync::Arc;

use ir::{Block, DataOp, EventOp, Expr, List, Literal, Op, OperatorOp, Program, Stage};
use uuid::Uuid;

fn main() {
    let dummy_list = Arc::new(List { uuid: Uuid::new_v4(), name: "dummy".into() });

    let root_block_uuid = Uuid::new_v4();
    let sum_block_uuid = Uuid::new_v4();
    let push_block_uuid = Uuid::new_v4();

    let root_block = Arc::new(Block {
        uuid: root_block_uuid,
        parent: None,
        next: Some(push_block_uuid),
        op: Op::Event(EventOp::WhenFlagClicked),
    });

    let sum_block = Arc::new(Block {
        uuid: sum_block_uuid,
        op: Op::Operator(OperatorOp::Add {
            num_a: Arc::new(Expr::Literal(Arc::new(Literal::Num(1.)))),
            num_b: Arc::new(Expr::Literal(Arc::new(Literal::Num(2.)))),
        }),
        next: None,
        parent: Some(push_block_uuid),
    });

    let push_block = Arc::new(Block {
        uuid: push_block_uuid,
        parent: Some(root_block_uuid),
        next: None,
        op: Op::Data(DataOp::AddToList {
            list: Arc::clone(&dummy_list),
            item: Arc::new(Expr::Derived(Arc::clone(&sum_block))),
        }),
    });

    let stage = Arc::new(Stage {
        lists: Arc::new(Vec::from([dummy_list])),
        broadcasts: Arc::new(Vec::new()),
        blocks: Arc::new(Vec::from([root_block, push_block, sum_block])),
    });

    let program = Program { stages: Arc::new(Vec::from([stage])), monitors: Arc::new(Vec::new()) };

    println!("{:#}", program.compile());
}
