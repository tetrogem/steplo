use std::sync::Arc;

use inter::{ez, ir};
use uuid::Uuid;

fn main() {
    ez_2();
}

fn ir() {
    let dummy_list = Arc::new(ir::List { uuid: Uuid::new_v4(), name: "dummy".into() });

    let root_block_uuid = Uuid::new_v4();
    let sum_block_uuid = Uuid::new_v4();
    let push_block_uuid = Uuid::new_v4();

    let root_block = Arc::new(ir::Block {
        uuid: root_block_uuid,
        parent: None,
        next: Some(push_block_uuid),
        op: ir::Op::Event(ir::EventOp::WhenFlagClicked),
    });

    let sum_block = Arc::new(ir::Block {
        uuid: sum_block_uuid,
        op: ir::Op::Operator(ir::OperatorOp::Add {
            num_a: Arc::new(ir::Expr::Literal(Arc::new(ir::Literal::Num(1.)))),
            num_b: Arc::new(ir::Expr::Literal(Arc::new(ir::Literal::Num(2.)))),
        }),
        next: None,
        parent: Some(push_block_uuid),
    });

    let push_block = Arc::new(ir::Block {
        uuid: push_block_uuid,
        parent: Some(root_block_uuid),
        next: None,
        op: ir::Op::Data(ir::DataOp::AddToList {
            list: Arc::clone(&dummy_list),
            item: Arc::new(ir::Expr::Derived(Arc::clone(&sum_block))),
        }),
    });

    let stage = Arc::new(ir::Stage {
        lists: Arc::new(Vec::from([dummy_list])),
        broadcasts: Arc::new(Vec::new()),
        blocks: Arc::new(Vec::from([root_block, push_block, sum_block])),
    });

    let program =
        ir::Program { stages: Arc::new(Vec::from([stage])), monitors: Arc::new(Vec::new()) };

    println!("{:#}", program.compile());
}

fn ez() {
    let dummy_list = Arc::new(ez::List { uuid: Uuid::new_v4(), name: "dummy".into() });

    let root_block = Arc::new(ez::Op::Event(ez::EventOp::WhenFlagClicked));

    let sum_block = Arc::new(ez::Op::Operator(ez::OperatorOp::Add {
        num_a: ez::Expr::num(1.),
        num_b: ez::Expr::num(2.),
    }));

    let push_block = Arc::new(ez::Op::Data(ez::DataOp::AddToList {
        list: Arc::clone(&dummy_list),
        item: ez::Expr::derived(&sum_block),
    }));

    let stack = Arc::new(ez::Stack { ops: Arc::new(Vec::from([root_block, push_block])) });

    let stage = Arc::new(ez::Stage {
        lists: Arc::new(Vec::from([dummy_list])),
        broadcasts: Arc::new(Vec::new()),
        stack,
    });

    let program =
        ez::Program { stages: Arc::new(Vec::from([stage])), monitors: Arc::new(Vec::new()) };

    let ir = program.compile();
    println!("{:#}", ir.compile());
}

fn ez_2() {
    // create stack
    let stack_list = Arc::new(ez::List { uuid: Uuid::new_v4(), name: "stack".into() });

    // start on green flag event
    let start_op = Arc::new(ez::Op::Event(ez::EventOp::WhenFlagClicked));

    // push first number onto stack
    let push_1_op = Arc::new(ez::Op::Data(ez::DataOp::AddToList {
        list: Arc::clone(&stack_list),
        item: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::Num(10.)))),
    }));

    // push second number onto stack
    let push_2_op = Arc::new(ez::Op::Data(ez::DataOp::AddToList {
        list: Arc::clone(&stack_list),
        item: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::Num(25.)))),
    }));

    // ops you can put into derived and statement ops should really be two separate things
    // derive ops = pills?

    // push sum of both numbers onto stack
    let push_sum_op = Arc::new(ez::Op::Data(ez::DataOp::AddToList {
        list: Arc::clone(&stack_list),
        item: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Operator(ez::OperatorOp::Add {
            num_a: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                list: Arc::clone(&stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::PosInt(1)))),
            })))),
            num_b: Arc::new(ez::Expr::Derived(Arc::new(ez::Op::Data(ez::DataOp::ItemOfList {
                list: Arc::clone(&stack_list),
                index: Arc::new(ez::Expr::Literal(Arc::new(ez::Literal::PosInt(2)))),
            })))),
        })))),
    }));

    // finalize
    let stack = Arc::new(ez::Stack {
        ops: Arc::new(Vec::from([start_op, push_1_op, push_2_op, push_sum_op])),
    });

    let stage = Arc::new(ez::Stage {
        broadcasts: Arc::new(Vec::new()),
        lists: Arc::new(Vec::from([stack_list])),
        stack,
    });

    let program =
        ez::Program { monitors: Arc::new(Vec::new()), stages: Arc::new(Vec::from([stage])) };

    let ir = program.compile();
    let json = ir.compile();
    println!("{:#}", json);
}
