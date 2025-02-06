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
    Func { name: Arc<str>, param_vars: Arc<Vec<Arc<str>>>, return_sub_proc: Arc<SubProc> },
    SubProc(Arc<SubProc>),
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
            create_sub_proc(&mut body_items, matches!(proc_kind, ProcKind::Main));

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
) -> CreateSubProcRes {
    println!("run");
    let mut rest_sps = Vec::<Arc<SubProc>>::new();
    let mut statements = Vec::<Arc<Statement>>::new();

    let mut next_call = None;

    while let Some(body_item) = body_items.next() {
        dbg!(body_item);
        match body_item {
            ast::BodyItem::If { cond_var, body } => {
                todo!()
            },
            ast::BodyItem::Statement(statement) => match statement.as_ref() {
                ast::Statement::Assign(assign) => {
                    statements.push(Arc::new(Statement::Assign(Arc::clone(assign))));
                },
                ast::Statement::Native(native) => {
                    statements.push(Arc::new(Statement::Native(Arc::clone(native))));
                },
                ast::Statement::Call { func_name, param_vars } => {
                    let return_sp_res = create_sub_proc(body_items, main);

                    let return_sp = Arc::new(return_sp_res.root);
                    rest_sps.push(Arc::clone(&return_sp));
                    rest_sps.extend(return_sp_res.rest);

                    next_call = Some(Call::Func {
                        name: Arc::clone(func_name),
                        param_vars: Arc::clone(param_vars),
                        return_sub_proc: return_sp,
                    });
                },
            },
        }
    }

    let next_call = match next_call {
        Some(next_call) => next_call,
        None => {
            if body_items.peek().is_none() {
                match main {
                    true => Call::Terminate,
                    false => Call::Return,
                }
            } else {
                let sp_res = create_sub_proc(body_items, main);

                let return_sp = Arc::new(sp_res.root);
                rest_sps.push(Arc::clone(&return_sp));
                rest_sps.extend(sp_res.rest);

                Call::SubProc(return_sp)
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
