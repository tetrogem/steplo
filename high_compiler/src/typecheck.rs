use std::{
    collections::HashMap,
    ops::Not,
    sync::{Arc, LazyLock},
};

use anyhow::{anyhow, bail};

use crate::logic_ast as l;

static ANY_TYPE: LazyLock<Arc<l::Type>> =
    LazyLock::new(|| Arc::new(l::Type::Base(Arc::new(l::BaseType::Any))));

static VAL_TYPE: LazyLock<Arc<l::Type>> =
    LazyLock::new(|| Arc::new(l::Type::Base(Arc::new(l::BaseType::Val))));

static NUM_TYPE: LazyLock<Arc<l::Type>> =
    LazyLock::new(|| Arc::new(l::Type::Base(Arc::new(l::BaseType::Num))));

static INT_TYPE: LazyLock<Arc<l::Type>> =
    LazyLock::new(|| Arc::new(l::Type::Base(Arc::new(l::BaseType::Int))));

static UINT_TYPE: LazyLock<Arc<l::Type>> =
    LazyLock::new(|| Arc::new(l::Type::Base(Arc::new(l::BaseType::Uint))));

static BOOL_TYPE: LazyLock<Arc<l::Type>> =
    LazyLock::new(|| Arc::new(l::Type::Base(Arc::new(l::BaseType::Bool))));

type IdentToType<'a> = HashMap<&'a l::Name, Arc<l::Type>>;
type FuncToParams<'a> = HashMap<&'a l::Name, Arc<Vec<Arc<l::IdentDeclaration>>>>;

pub fn typecheck(program: &l::Program) -> anyhow::Result<()> {
    let func_to_params = program
        .items
        .iter()
        .filter_map(|item| match item.as_ref() {
            l::TopItem::Main(_) => None,
            l::TopItem::Func(func) => Some((func.name.as_ref(), func.params.clone())),
        })
        .collect::<FuncToParams>();

    program.items.iter().try_for_each(|x| typecheck_top_item(x, &func_to_params))
}

fn typecheck_top_item(item: &l::TopItem, func_to_params: &FuncToParams) -> anyhow::Result<()> {
    let (proc, params) = match item {
        l::TopItem::Main(x) => (x.proc.as_ref(), [].iter()),
        l::TopItem::Func(x) => (x.proc.as_ref(), x.params.iter()),
    };

    let idents = proc.idents.iter().chain(params).map(AsRef::as_ref);
    let ident_to_type = idents.map(|i| (i.name.as_ref(), i.ty.clone())).collect::<IdentToType>();

    typecheck_body(&proc.body, &ident_to_type, func_to_params)
}

fn typecheck_body(
    body: &l::Body,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> anyhow::Result<()> {
    body.items.iter().try_for_each(|x| typecheck_body_item(x, ident_to_type, func_to_params))
}

fn typecheck_body_item(
    item: &l::BodyItem,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> anyhow::Result<()> {
    match item {
        l::BodyItem::Statement(x) => typecheck_statement(x, ident_to_type, func_to_params),
        l::BodyItem::If(x) => typecheck_if(x, ident_to_type, func_to_params),
        l::BodyItem::While(x) => typecheck_while(x, ident_to_type, func_to_params),
    }
}

fn typecheck_if(
    item: &l::IfItem,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> anyhow::Result<()> {
    let condition_type = eval_expr(&item.condition, ident_to_type)?;
    if condition_type.is_assignable_to(&VAL_TYPE).not() {
        bail!("If condition expected to be of type `val`");
    }

    typecheck_body(&item.then_body, ident_to_type, func_to_params)?;
    if let Some(else_item) = &item.else_item {
        typecheck_body(&else_item.body, ident_to_type, func_to_params)?;
    }

    Ok(())
}

fn typecheck_while(
    item: &l::WhileItem,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> anyhow::Result<()> {
    let condition_type = eval_expr(&item.condition, ident_to_type)?;
    if condition_type.is_assignable_to(&VAL_TYPE).not() {
        bail!("While condition expected to be of type `val`");
    }

    typecheck_body(&item.body, ident_to_type, func_to_params)?;

    Ok(())
}

fn typecheck_statement(
    item: &l::Statement,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> anyhow::Result<()> {
    match item {
        l::Statement::Assign(x) => typecheck_assign(x, ident_to_type),
        l::Statement::Call(x) => typecheck_call(x, ident_to_type, func_to_params),
        l::Statement::Native(_) => Ok(()),
    }
}

fn typecheck_assign(item: &l::Assign, ident_to_type: &IdentToType) -> anyhow::Result<()> {
    let place_type = eval_place(&item.place, ident_to_type)?;
    let expr_type = eval_assign_expr(&item.expr, &place_type, ident_to_type)?;
    if expr_type.is_assignable_to(&place_type).not() {
        bail!(
            "Type of expr is not assignable to type of place, Expected: {:?}, found: {:?}",
            place_type,
            expr_type
        );
    }

    Ok(())
}

fn typecheck_call(
    item: &l::FunctionCall,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> anyhow::Result<()> {
    let Some(param_decls) = func_to_params.get(item.func_name.as_ref()) else {
        bail!("No function with name `{}` exists", item.func_name.str);
    };

    let mut expr_iter = item.param_exprs.iter();
    let mut decl_iter = param_decls.iter();

    loop {
        let (expr, decl) = (expr_iter.next(), decl_iter.next());
        match (expr, decl) {
            (None, None) => break,
            (Some(expr), Some(decl)) => {
                let expr_type = eval_assign_expr(expr, &decl.ty, ident_to_type)?;
                if expr_type.is_assignable_to(&decl.ty).not() {
                    bail!("Argument type mismatch, expected {:?} found {:?}", decl.ty, expr_type);
                }
            },
            _ => {
                bail!(
                    "Argument count mismatch for calling function `{}`: Expected {} args, found {} args",
                    item.func_name.str,
                    param_decls.len(),
                    item.param_exprs.len(),
                );
            },
        }
    }

    Ok(())
}

fn eval_place(item: &l::Place, ident_to_type: &IdentToType) -> anyhow::Result<Arc<l::Type>> {
    let head_type = eval_place_head(&item.head, ident_to_type)?;
    Ok(match item.offset {
        None => head_type,
        Some(_) => {
            let l::Type::Array { ty, .. } = head_type.as_ref() else {
                bail!("Cannot index non-array type")
            };

            ty.clone()
        },
    })
}

fn eval_place_head(
    item: &l::PlaceHead,
    ident_to_type: &IdentToType,
) -> anyhow::Result<Arc<l::Type>> {
    match item {
        l::PlaceHead::Ident(ident) => eval_ident(ident, ident_to_type),
        l::PlaceHead::Deref(deref) => eval_deref(deref, ident_to_type),
    }
}

fn eval_ident(item: &l::Ident, ident_to_type: &IdentToType) -> anyhow::Result<Arc<l::Type>> {
    ident_to_type
        .get(item.name.as_ref())
        .ok_or_else(|| anyhow!("Identifier `{}` was not found in scope", item.name.str))
        .cloned()
}

fn eval_deref(item: &l::Deref, ident_to_type: &IdentToType) -> anyhow::Result<Arc<l::Type>> {
    let addr_type = eval_expr(&item.addr, ident_to_type)?;
    let l::Type::Ref(deref_type) = addr_type.as_ref() else {
        bail!("Cannot dereference non-reference type: {:?}", addr_type)
    };

    Ok(deref_type.clone())
}

fn eval_assign_expr(
    item: &l::AssignExpr,
    expected_type: &Arc<l::Type>,
    ident_to_type: &IdentToType,
) -> anyhow::Result<Arc<l::Type>> {
    match item {
        l::AssignExpr::Expr(x) => eval_expr(x, ident_to_type),
        l::AssignExpr::Span(elements) => {
            let l::Type::Array { ty: expected_el_type, .. } = expected_type.as_ref() else {
                bail!("Assignment is not expecting span")
            };

            for el in elements.iter() {
                let el_type = eval_expr(el, ident_to_type)?;
                if el_type.is_assignable_to(expected_el_type).not() {
                    bail!("Array element is not the correct type for this array, Expected: {:?}, found: {:?}", expected_el_type, el_type);
                }
            }

            Ok(Arc::new(l::Type::Array {
                ty: expected_el_type.clone(),
                len: elements.len() as u32,
            }))
        },
        l::AssignExpr::Slice { place, .. } => eval_place(place, ident_to_type),
    }
}

fn eval_expr(item: &l::Expr, ident_to_type: &IdentToType) -> anyhow::Result<Arc<l::Type>> {
    match item {
        l::Expr::Literal(x) => Ok(match x.as_ref() {
            l::Literal::Val(_) => VAL_TYPE.clone(),
            l::Literal::Num(_) => NUM_TYPE.clone(),
            l::Literal::Int(_) => INT_TYPE.clone(),
            l::Literal::Uint(_) => UINT_TYPE.clone(),
            l::Literal::Bool(_) => BOOL_TYPE.clone(),
        }),
        l::Expr::Place(x) => eval_place(x, ident_to_type),
        l::Expr::Ref(place) => {
            let place_type = eval_place(place, ident_to_type)?;
            Ok(Arc::new(l::Type::Ref(place_type)))
        },
        l::Expr::Paren(x) => eval_paren_expr(x, ident_to_type),
        l::Expr::Cast { ty, expr } => {
            let expr_ty = eval_expr(expr, ident_to_type)?;
            if expr_ty.can_cast_to(ty).not() {
                bail!("Cannot perform typecast from {:?} to {:?}", expr_ty, ty);
            }

            Ok(ty.clone())
        },
        l::Expr::Transmute { ty, expr } => {
            let expr_ty = eval_expr(expr, ident_to_type)?;
            if ty.can_transmute_to(&expr_ty).not() {
                bail!("Cannot perform transmute from {:?} to {:?}", expr_ty, ty);
            }

            Ok(ty.clone())
        },
    }
}

fn eval_paren_expr(
    item: &l::ParenExpr,
    ident_to_type: &IdentToType,
) -> anyhow::Result<Arc<l::Type>> {
    match item {
        l::ParenExpr::Unary(x) => eval_unary_expr(x, ident_to_type),
        l::ParenExpr::Binary(x) => eval_binary_expr(x, ident_to_type),
    }
}

fn eval_unary_expr(
    item: &l::UnaryParenExpr,
    ident_to_type: &IdentToType,
) -> anyhow::Result<Arc<l::Type>> {
    let operand_type = eval_expr(&item.operand, ident_to_type)?;
    match item.op {
        l::UnaryParenExprOp::Not => {
            if operand_type.is_assignable_to(&BOOL_TYPE).not() {
                bail!("Not operation expects operand to be of type `bool`");
            }

            Ok(BOOL_TYPE.clone())
        },
    }
}

fn eval_binary_expr(
    item: &l::BinaryParenExpr,
    ident_to_type: &IdentToType,
) -> anyhow::Result<Arc<l::Type>> {
    let left_type = eval_expr(&item.left, ident_to_type)?;
    let right_type = eval_expr(&item.right, ident_to_type)?;

    macro_rules! expect_types {
        (
            $($left:expr, $right:expr => $out:expr;)*
        ) => {'expect: {
            $(
                if left_type.is_assignable_to(&$left) && right_type.is_assignable_to(&$right) {
                    break 'expect Ok($out.clone());
                }
            )*

            break 'expect Err(anyhow!("Bad operands"));
        }};
    }

    match item.op {
        l::BinaryParenExprOp::Add => expect_types!(
            UINT_TYPE, UINT_TYPE => UINT_TYPE;
            INT_TYPE, INT_TYPE => INT_TYPE;
            NUM_TYPE, NUM_TYPE => NUM_TYPE;
        ),
        l::BinaryParenExprOp::Sub => expect_types!(
            UINT_TYPE, UINT_TYPE => INT_TYPE;
            INT_TYPE, INT_TYPE => INT_TYPE;
            NUM_TYPE, NUM_TYPE => NUM_TYPE;
        ),
        l::BinaryParenExprOp::Mul => expect_types!(
            UINT_TYPE, UINT_TYPE => UINT_TYPE;
            INT_TYPE, INT_TYPE => INT_TYPE;
            NUM_TYPE, NUM_TYPE => NUM_TYPE;
        ),
        l::BinaryParenExprOp::Div => expect_types!(
            UINT_TYPE, UINT_TYPE => NUM_TYPE;
            INT_TYPE, INT_TYPE => NUM_TYPE;
            NUM_TYPE, NUM_TYPE => NUM_TYPE;
        ),
        l::BinaryParenExprOp::Mod => expect_types!(
            UINT_TYPE, UINT_TYPE => NUM_TYPE;
            INT_TYPE, INT_TYPE => NUM_TYPE;
            NUM_TYPE, NUM_TYPE => NUM_TYPE;
        ),
        l::BinaryParenExprOp::Eq => expect_types!(
            ANY_TYPE, ANY_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::Neq => expect_types!(
            ANY_TYPE, ANY_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::Gt => expect_types!(
            ANY_TYPE, ANY_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::Lt => expect_types!(
            ANY_TYPE, ANY_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::Gte => expect_types!(
            ANY_TYPE, ANY_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::Lte => expect_types!(
            ANY_TYPE, ANY_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::And => expect_types!(
            BOOL_TYPE, BOOL_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::Or => expect_types!(
            BOOL_TYPE, BOOL_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::Join => expect_types!(
            VAL_TYPE, VAL_TYPE => VAL_TYPE;
        ),
    }
}
