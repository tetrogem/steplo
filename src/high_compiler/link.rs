use std::{iter::Peekable, sync::Arc};

use itertools::Itertools;
use uuid::Uuid;

use crate::{logic_ast as ast, srced::Srced};

#[derive(Debug, Clone)]
pub struct Proc {
    pub kind: ProcKind,
    pub idents: ast::Ref<Vec<ast::Ref<ast::IdentDeclaration>>>,
    pub sub_procs: ast::Ref<Vec<ast::Ref<SubProc>>>,
}

#[derive(Debug, Clone)]
pub enum ProcKind {
    Main,
    Func { name: ast::Ref<ast::Name>, params: ast::Ref<Vec<ast::Ref<ast::IdentDeclaration>>> },
}

#[derive(Debug, Clone)]
pub struct SubProc {
    pub uuid: Uuid,
    pub statements: ast::Ref<Vec<ast::Ref<Statement>>>,
    pub next_call: ast::Ref<Call>,
}

#[derive(Debug, Clone)]
pub enum Call {
    Func {
        name: ast::Ref<ast::Name>,
        param_exprs: ast::Ref<Vec<ast::Ref<ast::AssignExpr>>>,
        return_sub_proc: Uuid,
    },
    SubProc(Uuid),
    IfElseBranch {
        cond_expr: ast::Ref<ast::Expr>,
        then_sub_proc: Uuid,
        else_sub_proc: Uuid,
    },
    Return,
    Terminate,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(ast::Ref<ast::Assign>),
    Native(ast::Ref<ast::NativeOperation>),
}

pub fn link(ast: &ast::Ref<ast::Program>) -> Vec<ast::Ref<Proc>> {
    let top_items = ast.val.items.val.iter().cloned().collect_vec();

    // parse user top items
    let mut procs = Vec::<ast::Ref<Proc>>::new();

    for top_item in top_items.iter().map(AsRef::as_ref) {
        let (proc_kind, ast_proc) = match &top_item.val {
            ast::TopItem::Main(main) => (ProcKind::Main, Arc::clone(&main.val.proc)),
            ast::TopItem::Func(func) => (
                ProcKind::Func {
                    name: Arc::clone(&func.val.name),
                    params: Arc::clone(&func.val.params),
                },
                Arc::clone(&func.val.proc),
            ),
        };

        let mut body_items = ast_proc.val.body.val.items.val.iter().peekable();

        let create_sub_proc_res =
            create_sub_proc(&mut body_items, matches!(proc_kind, ProcKind::Main), None);

        let sub_procs = Vec::from([create_sub_proc_res.root])
            .into_iter()
            .chain(create_sub_proc_res.rest)
            .collect_vec();

        let proc = Proc {
            kind: proc_kind,
            idents: Arc::clone(&ast_proc.val.idents),
            sub_procs: Arc::new(Srced {
                range: sub_procs.iter().fold(Default::default(), |acc, x| acc.merge(x.range)),
                val: sub_procs,
            }),
        };

        procs.push(Arc::new(Srced {
            range: proc.idents.range.merge(proc.sub_procs.range),
            val: proc,
        }));
    }

    procs
}

struct CreateSubProcRes {
    root: ast::Ref<SubProc>,
    rest: Vec<ast::Ref<SubProc>>,
}

fn create_sub_proc<'a>(
    body_items: &mut Peekable<impl Iterator<Item = &'a ast::Ref<ast::BodyItem>>>,
    main: bool,
    pop_sub_proc: Option<Uuid>,
) -> CreateSubProcRes {
    let mut rest_sps = Vec::<ast::Ref<SubProc>>::new();

    macro_rules! next_sp {
        ($body_items:expr, $pop_sp:expr $(,)?) => {{
            let sp_res = create_sub_proc($body_items, main, $pop_sp);

            let sp = Arc::new(sp_res.root);
            rest_sps.push(Arc::clone(&sp));
            rest_sps.extend(sp_res.rest);

            sp
        }};
    }

    let mut statements = Vec::<ast::Ref<Statement>>::new();

    let mut next_call = None;

    while let Some(body_item) = body_items.next() {
        match &body_item.val {
            ast::BodyItem::If(if_item) => {
                let pop_sp = next_sp!(body_items, pop_sub_proc);

                let then_sp = next_sp!(
                    &mut if_item.val.then_body.val.items.val.iter().peekable(),
                    Some(pop_sp.val.uuid)
                );

                let call = match &if_item.val.else_item {
                    None => Call::IfElseBranch {
                        cond_expr: Arc::clone(&if_item.val.condition),
                        then_sub_proc: then_sp.val.uuid,
                        else_sub_proc: pop_sp.val.uuid,
                    },
                    Some(else_item) => {
                        let else_sp = next_sp!(
                            &mut else_item.val.body.val.items.val.iter().peekable(),
                            Some(pop_sp.val.uuid)
                        );

                        Call::IfElseBranch {
                            cond_expr: Arc::clone(&if_item.val.condition),
                            then_sub_proc: then_sp.val.uuid,
                            else_sub_proc: else_sp.val.uuid,
                        }
                    },
                };

                next_call = Some(call);
            },
            ast::BodyItem::While(while_item) => {
                // where to go after exiting the loop
                let pop_sp = next_sp!(body_items, pop_sub_proc);

                let check_uuid = Uuid::new_v4();

                // body of the while loop
                let then_sp = next_sp!(
                    &mut while_item.val.body.val.items.val.iter().peekable(),
                    // tell the body to go back to the check condition once done
                    Some(check_uuid),
                );

                // check if the condition is still true... if so, go to body, else pop out
                let check_sp = SubProc {
                    uuid: check_uuid,
                    statements: Arc::new(Srced { val: Vec::new(), range: Default::default() }),
                    next_call: Arc::new(Srced {
                        val: Call::IfElseBranch {
                            cond_expr: Arc::clone(&while_item.val.condition),
                            then_sub_proc: then_sp.val.uuid,
                            else_sub_proc: pop_sp.val.uuid,
                        },
                        range: Default::default(),
                    }),
                };

                // start loop by going to check sp
                let call = Call::SubProc(check_sp.uuid);

                // register manually created sps
                rest_sps.push(Arc::new(Srced { val: check_sp, range: Default::default() }));

                // finish!
                next_call = Some(call);
            },
            ast::BodyItem::Statement(statement) => match &statement.val {
                ast::Statement::Assign(assign) => {
                    statements.push(Arc::new(Srced {
                        val: Statement::Assign(Arc::clone(assign)),
                        range: assign.range,
                    }));
                },
                ast::Statement::Native(native) => {
                    statements.push(Arc::new(Srced {
                        val: Statement::Native(Arc::clone(native)),
                        range: native.range,
                    }));
                },
                ast::Statement::Call(call) => {
                    let return_sp = next_sp!(body_items, pop_sub_proc);

                    next_call = Some(Call::Func {
                        name: Arc::clone(&call.val.func_name),
                        param_exprs: Arc::clone(&call.val.param_exprs),
                        return_sub_proc: return_sp.val.uuid,
                    });
                },
            },
        }
    }

    let next_call = match next_call {
        Some(next_call) => next_call,
        None => {
            if body_items.peek().is_none() {
                match pop_sub_proc {
                    Some(pop_sub_proc) => Call::SubProc(pop_sub_proc),
                    None => match main {
                        true => Call::Terminate,
                        false => Call::Return,
                    },
                }
            } else {
                let return_sp = next_sp!(body_items, None);
                Call::SubProc(return_sp.val.uuid)
            }
        },
    };

    let sp = SubProc {
        uuid: Uuid::new_v4(),
        statements: Arc::new(Srced {
            range: statements.iter().fold(Default::default(), |acc, x| acc.merge(x.range)),
            val: statements,
        }),
        next_call: Arc::new(Srced { val: next_call, range: Default::default() }),
    };

    CreateSubProcRes {
        root: Arc::new(Srced { range: sp.statements.range, val: sp }),
        rest: rest_sps,
    }
}
