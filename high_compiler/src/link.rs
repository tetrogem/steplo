use std::{iter::Peekable, sync::Arc};

use itertools::{chain, Itertools};
use uuid::Uuid;

use crate::logic_ast as ast;

#[derive(Debug, Clone)]
pub struct Proc {
    pub kind: ProcKind,
    pub idents: Arc<Vec<Arc<ast::IdentDeclaration>>>,
    pub sub_procs: Arc<Vec<Arc<SubProc>>>,
}

#[derive(Debug, Clone)]
pub enum ProcKind {
    Main,
    Func { name: Arc<ast::Name>, params: Arc<Vec<Arc<ast::IdentDeclaration>>> },
}

#[derive(Debug, Clone)]
pub struct SubProc {
    pub uuid: Uuid,
    pub statements: Arc<Vec<Arc<Statement>>>,
    pub next_call: Arc<Call>,
}

#[derive(Debug, Clone)]
pub enum Call {
    Func {
        name: Arc<ast::Name>,
        param_exprs: Arc<Vec<Arc<ast::AssignExpr>>>,
        return_sub_proc: Uuid,
    },
    SubProc(Uuid),
    IfElseBranch {
        cond_expr: Arc<ast::Expr>,
        then_sub_proc: Uuid,
        else_sub_proc: Uuid,
    },
    Return,
    Terminate,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(Arc<ast::Assign>),
    Native(Arc<ast::NativeOperation>),
}

pub fn link(ast: &ast::Program) -> anyhow::Result<Vec<Arc<Proc>>> {
    let top_items = ast.items.iter().cloned().collect_vec();

    // parse user top items
    let mut procs = Vec::<Arc<Proc>>::new();

    for top_item in top_items.iter().map(AsRef::as_ref) {
        let (proc_kind, ast_proc) = match top_item {
            ast::TopItem::Main(main) => (ProcKind::Main, Arc::clone(&main.proc)),
            ast::TopItem::Func(func) => (
                ProcKind::Func { name: Arc::clone(&func.name), params: Arc::clone(&func.params) },
                Arc::clone(&func.proc),
            ),
        };

        let mut body_items = ast_proc.body.items.iter().map(AsRef::as_ref).peekable();

        let create_sub_proc_res =
            create_sub_proc(&mut body_items, matches!(proc_kind, ProcKind::Main), None);

        let sub_procs = Vec::from([Arc::new(create_sub_proc_res.root)])
            .into_iter()
            .chain(create_sub_proc_res.rest)
            .collect_vec();

        let proc = Proc {
            kind: proc_kind,
            idents: Arc::clone(&ast_proc.idents),
            sub_procs: Arc::new(sub_procs),
        };

        procs.push(Arc::new(proc));
    }

    Ok(procs)
}

struct CreateSubProcRes {
    root: SubProc,
    rest: Vec<Arc<SubProc>>,
}

fn create_sub_proc<'a>(
    body_items: &mut Peekable<impl Iterator<Item = &'a ast::BodyItem>>,
    main: bool,
    pop_sub_proc: Option<Uuid>,
) -> CreateSubProcRes {
    let mut rest_sps = Vec::<Arc<SubProc>>::new();

    macro_rules! next_sp {
        ($body_items:expr, $pop_sp:expr $(,)?) => {{
            let sp_res = create_sub_proc($body_items, main, $pop_sp);

            let sp = Arc::new(sp_res.root);
            rest_sps.push(Arc::clone(&sp));
            rest_sps.extend(sp_res.rest);

            sp
        }};
    }

    let mut statements = Vec::<Arc<Statement>>::new();

    let mut next_call = None;

    while let Some(body_item) = body_items.next() {
        match body_item {
            ast::BodyItem::If(if_item) => {
                let pop_sp = next_sp!(body_items, pop_sub_proc);

                let then_sp = next_sp!(
                    &mut if_item.then_body.items.iter().map(AsRef::as_ref).peekable(),
                    Some(pop_sp.uuid)
                );

                let call = match &if_item.else_item {
                    None => Call::IfElseBranch {
                        cond_expr: Arc::clone(&if_item.condition),
                        then_sub_proc: then_sp.uuid,
                        else_sub_proc: pop_sp.uuid,
                    },
                    Some(else_item) => {
                        let else_sp = next_sp!(
                            &mut else_item.body.items.iter().map(AsRef::as_ref).peekable(),
                            Some(pop_sp.uuid)
                        );

                        Call::IfElseBranch {
                            cond_expr: Arc::clone(&if_item.condition),
                            then_sub_proc: then_sp.uuid,
                            else_sub_proc: else_sp.uuid,
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
                    &mut while_item.body.items.iter().map(AsRef::as_ref).peekable(),
                    // tell the body to go back to the check condition once done
                    Some(check_uuid),
                );

                // check if the condition is still true... if so, go to body, else pop out
                let check_sp = SubProc {
                    uuid: check_uuid,
                    statements: Arc::new(Vec::new()),
                    next_call: Arc::new(Call::IfElseBranch {
                        cond_expr: Arc::clone(&while_item.condition),
                        then_sub_proc: then_sp.uuid,
                        else_sub_proc: pop_sp.uuid,
                    }),
                };

                // start loop by going to check sp
                let call = Call::SubProc(check_sp.uuid);

                // register manually created sps
                rest_sps.push(Arc::new(check_sp));

                // finish!
                next_call = Some(call);
            },
            ast::BodyItem::Statement(statement) => match statement.as_ref() {
                ast::Statement::Assign(assign) => {
                    statements.push(Arc::new(Statement::Assign(Arc::clone(assign))));
                },
                ast::Statement::Native(native) => {
                    statements.push(Arc::new(Statement::Native(Arc::clone(native))));
                },
                ast::Statement::Call(call) => {
                    let return_sp = next_sp!(body_items, pop_sub_proc);

                    next_call = Some(Call::Func {
                        name: Arc::clone(&call.func_name),
                        param_exprs: Arc::clone(&call.param_exprs),
                        return_sub_proc: return_sp.uuid,
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
                Call::SubProc(return_sp.uuid)
            }
        },
    };

    let sp = SubProc {
        uuid: Uuid::new_v4(),
        statements: Arc::new(statements),
        next_call: Arc::new(next_call),
    };

    CreateSubProcRes { root: sp, rest: rest_sps }
}
