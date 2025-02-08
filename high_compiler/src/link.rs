use std::{iter::Peekable, mem, sync::Arc};

use itertools::Itertools;
use uuid::Uuid;

use crate::ast;

#[derive(Debug, Clone)]
pub struct Proc {
    pub kind: ProcKind,
    pub vars: Arc<Vec<Arc<str>>>,
    pub sub_procs: Arc<Vec<Arc<SubProc>>>,
}

#[derive(Debug, Clone)]
pub enum ProcKind {
    Main,
    Func { name: Arc<str>, params: Arc<Vec<Arc<str>>> },
}

#[derive(Debug, Clone)]
pub struct SubProc {
    pub uuid: Uuid,
    pub statements: Arc<Vec<Arc<Statement>>>,
    pub next_call: Arc<Call>,
}

#[derive(Debug, Clone)]
pub enum Call {
    Func { name: Arc<str>, param_coms: Arc<Vec<Arc<ast::Command>>>, return_sub_proc: Uuid },
    SubProc(Uuid),
    IfBranch { cond_com: Arc<ast::Command>, then_sub_proc: Uuid, pop_sub_proc: Uuid },
    IfElseBranch { cond_com: Arc<ast::Command>, then_sub_proc: Uuid, else_sub_proc: Uuid },
    Return,
    Terminate,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(Arc<ast::Assign>),
    Native(Arc<ast::NativeCommand>),
}

pub fn link(mut ast: Vec<Arc<ast::TopItem>>) -> anyhow::Result<Vec<Arc<Proc>>> {
    // add built-in native functions
    let out_func = ast::Func {
        name: "out".into(),
        params: Arc::new(Vec::from(["val".into()])),
        proc: Arc::new(ast::Proc {
            vars: Arc::new(Vec::new()),
            body: Arc::new(Vec::from([Arc::new(ast::BodyItem::Statement(Arc::new(
                ast::Statement::Native(Arc::new(ast::NativeCommand::Out { var: "val".into() })),
            )))])),
        }),
    };

    ast.push(Arc::new(ast::TopItem::Func(Arc::new(out_func))));

    let mut procs = Vec::<Arc<Proc>>::new();

    for top_item in ast.iter().map(AsRef::as_ref) {
        let (proc_kind, ast_proc) = match top_item {
            ast::TopItem::Main(main) => (ProcKind::Main, Arc::clone(&main.proc)),
            ast::TopItem::Func(func) => (
                ProcKind::Func { name: Arc::clone(&func.name), params: Arc::clone(&func.params) },
                Arc::clone(&func.proc),
            ),
        };

        let mut body_items = ast_proc.body.iter().map(AsRef::as_ref).peekable();

        let create_sub_proc_res =
            create_sub_proc(&mut body_items, matches!(proc_kind, ProcKind::Main), None);

        let sub_procs = Vec::from([Arc::new(create_sub_proc_res.root)])
            .into_iter()
            .chain(create_sub_proc_res.rest)
            .collect_vec();

        let proc = Proc {
            kind: proc_kind,
            vars: Arc::clone(&ast_proc.vars),
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
            ast::BodyItem::If { cond_com, then_body, else_body } => {
                let pop_sp = next_sp!(body_items, pop_sub_proc);

                let then_sp = next_sp!(
                    &mut then_body.iter().map(AsRef::as_ref).peekable(),
                    Some(pop_sp.uuid)
                );

                let call = match else_body {
                    None => Call::IfBranch {
                        cond_com: Arc::clone(cond_com),
                        then_sub_proc: then_sp.uuid,
                        pop_sub_proc: pop_sp.uuid,
                    },
                    Some(else_body) => {
                        let else_sp = next_sp!(
                            &mut else_body.iter().map(AsRef::as_ref).peekable(),
                            Some(pop_sp.uuid)
                        );

                        Call::IfElseBranch {
                            cond_com: Arc::clone(cond_com),
                            then_sub_proc: then_sp.uuid,
                            else_sub_proc: else_sp.uuid,
                        }
                    },
                };

                next_call = Some(call);
            },
            ast::BodyItem::While { cond_com, body } => {
                // where to go after exiting the loop
                let pop_sp = next_sp!(body_items, pop_sub_proc);

                let check_uuid = Uuid::new_v4();

                // body of the while loop
                let then_sp = next_sp!(
                    &mut body.iter().map(AsRef::as_ref).peekable(),
                    // tell the body to go back to the check condition once done
                    Some(check_uuid),
                );

                // check if the condition is still true... if so, go to body, else pop out
                let check_sp = SubProc {
                    uuid: check_uuid,
                    statements: Arc::new(Vec::new()),
                    next_call: Arc::new(Call::IfElseBranch {
                        cond_com: Arc::clone(cond_com),
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
                ast::Statement::Call { func_name, param_coms: param_vars } => {
                    let return_sp = next_sp!(body_items, pop_sub_proc);

                    next_call = Some(Call::Func {
                        name: Arc::clone(func_name),
                        param_coms: Arc::clone(param_vars),
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
