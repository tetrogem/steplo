use std::sync::Arc;

use designate::designate_registers;
use uuid::Uuid;

pub mod ast;
mod designate;

fn main() -> anyhow::Result<()> {
    let t1 = Arc::new(ast::TempVar::new());
    let t2 = Arc::new(ast::TempVar::new());
    let t3 = Arc::new(ast::TempVar::new());

    let ast = Vec::from([Arc::new(ast::Proc {
        kind: Arc::new(ast::ProcKind::Main),
        sub_procs: Arc::new(Vec::from([
            Arc::new(ast::SubProc {
                uuid: Uuid::new_v4(),
                next_call: Arc::new(ast::Call::Terminate),
                assignments: Arc::new(Vec::from([
                    Arc::new(ast::Assignment {
                        dest: Arc::new(ast::UMemLoc::Temp(t1.clone())),
                        expr: Arc::new(ast::Expr::Add {
                            left: Arc::new(ast::UMemLoc::StackPointer),
                            right: Arc::new(ast::UMemLoc::StackPointer),
                        }),
                    }),
                    Arc::new(ast::Assignment {
                        dest: Arc::new(ast::UMemLoc::Temp(t2.clone())),
                        expr: Arc::new(ast::Expr::Add {
                            left: Arc::new(ast::UMemLoc::StackPointer),
                            right: Arc::new(ast::UMemLoc::StackPointer),
                        }),
                    }),
                    Arc::new(ast::Assignment {
                        dest: Arc::new(ast::UMemLoc::StackPointer),
                        expr: Arc::new(ast::Expr::Add {
                            left: Arc::new(ast::UMemLoc::Temp(t1.clone())),
                            right: Arc::new(ast::UMemLoc::StackPointer),
                        }),
                    }),
                ])),
            }),
            Arc::new(ast::SubProc {
                uuid: Uuid::new_v4(),
                next_call: Arc::new(ast::Call::Terminate),
                assignments: Arc::new(Vec::from([
                    Arc::new(ast::Assignment {
                        dest: Arc::new(ast::UMemLoc::Temp(t3.clone())),
                        expr: Arc::new(ast::Expr::Add {
                            left: Arc::new(ast::UMemLoc::StackPointer),
                            right: Arc::new(ast::UMemLoc::StackPointer),
                        }),
                    }),
                    Arc::new(ast::Assignment {
                        dest: Arc::new(ast::UMemLoc::Temp(t2.clone())),
                        expr: Arc::new(ast::Expr::Add {
                            left: Arc::new(ast::UMemLoc::StackPointer),
                            right: Arc::new(ast::UMemLoc::StackPointer),
                        }),
                    }),
                    Arc::new(ast::Assignment {
                        dest: Arc::new(ast::UMemLoc::Temp(t1.clone())),
                        expr: Arc::new(ast::Expr::Add {
                            left: Arc::new(ast::UMemLoc::Temp(t3.clone())),
                            right: Arc::new(ast::UMemLoc::Temp(t2.clone())),
                        }),
                    }),
                ])),
            }),
        ])),
    })]);

    let designated = designate_registers(&ast);
    dbg!(designated);

    Ok(())
}
