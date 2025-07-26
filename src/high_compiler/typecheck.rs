use std::{
    collections::HashMap,
    ops::Not,
    sync::{Arc, LazyLock},
};

use crate::{
    compile_error::{CompileError, CompileErrorSet, TypeError, VagueType},
    logic_ast as l,
};

static ANY_TYPE: LazyLock<Arc<l::Type>> = LazyLock::new(|| Arc::new(l::Type::Any));

static VAL_TYPE: LazyLock<Arc<l::Type>> =
    LazyLock::new(|| Arc::new(l::Type::Primitive(l::PrimitiveType::Val)));

static NUM_TYPE: LazyLock<Arc<l::Type>> =
    LazyLock::new(|| Arc::new(l::Type::Primitive(l::PrimitiveType::Num)));

static INT_TYPE: LazyLock<Arc<l::Type>> =
    LazyLock::new(|| Arc::new(l::Type::Primitive(l::PrimitiveType::Int)));

static UINT_TYPE: LazyLock<Arc<l::Type>> =
    LazyLock::new(|| Arc::new(l::Type::Primitive(l::PrimitiveType::Uint)));

static BOOL_TYPE: LazyLock<Arc<l::Type>> =
    LazyLock::new(|| Arc::new(l::Type::Primitive(l::PrimitiveType::Bool)));

type IdentToType = HashMap<Arc<str>, Arc<l::Type>>;
type FuncToParams = HashMap<Arc<str>, l::Ref<Vec<l::Ref<l::IdentDeclaration>>>>;

pub fn typecheck(program: &l::Ref<l::Program>) -> Result<(), CompileErrorSet> {
    let func_to_params = program
        .val
        .items
        .val
        .iter()
        .filter_map(|item| match &item.val {
            l::TopItem::Main(_) => None,
            l::TopItem::Func(func) => {
                Some((func.val.name.val.str.clone(), func.val.params.clone()))
            },
        })
        .collect::<FuncToParams>();

    program.val.items.val.iter().try_for_each(|x| typecheck_top_item(x, &func_to_params))
}

fn typecheck_top_item(
    item: &l::Ref<l::TopItem>,
    func_to_params: &FuncToParams,
) -> Result<(), CompileErrorSet> {
    let (proc, params) = match &item.val {
        l::TopItem::Main(x) => (&x.val.proc, [].iter()),
        l::TopItem::Func(x) => (&x.val.proc, x.val.params.val.iter()),
    };

    let idents = proc.val.idents.val.iter().chain(params).map(AsRef::as_ref);
    let ident_to_type =
        idents.map(|i| (i.val.name.val.str.clone(), i.val.ty.clone())).collect::<IdentToType>();

    typecheck_body(&proc.val.body, &ident_to_type, func_to_params)
}

fn typecheck_body(
    body: &l::Ref<l::Body>,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> Result<(), CompileErrorSet> {
    body.val
        .items
        .val
        .iter()
        .try_for_each(|x| typecheck_body_item(x, ident_to_type, func_to_params))
}

fn typecheck_body_item(
    item: &l::Ref<l::BodyItem>,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> Result<(), CompileErrorSet> {
    match &item.val {
        l::BodyItem::Statement(x) => typecheck_statement(x, ident_to_type, func_to_params),
        l::BodyItem::If(x) => typecheck_if(x, ident_to_type, func_to_params),
        l::BodyItem::While(x) => typecheck_while(x, ident_to_type, func_to_params),
    }
}

fn typecheck_if(
    item: &l::Ref<l::IfItem>,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> Result<(), CompileErrorSet> {
    let condition_type = eval_expr(&item.val.condition, ident_to_type)?;
    if condition_type.is_subtype_of(&VAL_TYPE).not() {
        return Err(CompileErrorSet::new_error(
            item.val.condition.range,
            CompileError::Type(TypeError::Mismatch {
                expected: vague(&VAL_TYPE),
                found: vague(&condition_type),
            }),
        ));
    }

    typecheck_body(&item.val.then_body, ident_to_type, func_to_params)?;
    if let Some(else_item) = &item.val.else_item {
        typecheck_body(&else_item.val.body, ident_to_type, func_to_params)?;
    }

    Ok(())
}

fn typecheck_while(
    item: &l::Ref<l::WhileItem>,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> Result<(), CompileErrorSet> {
    let condition_type = eval_expr(&item.val.condition, ident_to_type)?;
    if condition_type.is_subtype_of(&VAL_TYPE).not() {
        return Err(CompileErrorSet::new_error(
            item.val.condition.range,
            CompileError::Type(TypeError::Mismatch {
                expected: vague(&VAL_TYPE),
                found: vague(&condition_type),
            }),
        ));
    }

    typecheck_body(&item.val.body, ident_to_type, func_to_params)?;

    Ok(())
}

fn typecheck_statement(
    item: &l::Ref<l::Statement>,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> Result<(), CompileErrorSet> {
    match &item.val {
        l::Statement::Assign(x) => typecheck_assign(x, ident_to_type),
        l::Statement::Call(x) => typecheck_call(x, ident_to_type, func_to_params),
        l::Statement::Native(_) => Ok(()),
    }
}

fn typecheck_assign(
    item: &l::Ref<l::Assign>,
    ident_to_type: &IdentToType,
) -> Result<(), CompileErrorSet> {
    let place_type = eval_place(&item.val.place, ident_to_type)?;
    let expr = &item.val.expr;
    let expr_type = eval_assign_expr(expr, &place_type, ident_to_type)?;
    if expr_type.is_subtype_of(&place_type).not() {
        return Err(CompileErrorSet::new_error(
            expr.range,
            CompileError::Type(TypeError::Mismatch {
                expected: vague(&place_type),
                found: vague(&expr_type),
            }),
        ));
    }

    Ok(())
}

fn typecheck_call(
    item: &l::Ref<l::FunctionCall>,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> Result<(), CompileErrorSet> {
    let func_name = &item.val.func_name;
    let Some(param_decls) = func_to_params.get(func_name.val.str.as_ref()) else {
        return Err(CompileErrorSet::new_error(
            func_name.range,
            CompileError::Type(TypeError::FuncNotFound { name: func_name.val.str.clone() }),
        ));
    };

    let mut expr_iter = item.val.param_exprs.val.iter();
    let mut decl_iter = param_decls.val.iter();

    loop {
        let (expr, decl) = (expr_iter.next(), decl_iter.next());
        match (expr, decl) {
            (None, None) => break,
            (Some(expr), Some(decl)) => {
                let expr_type = eval_assign_expr(expr, &decl.val.ty, ident_to_type)?;
                if expr_type.is_subtype_of(&decl.val.ty).not() {
                    return Err(CompileErrorSet::new_error(
                        expr.range,
                        CompileError::Type(TypeError::Mismatch {
                            expected: vague(&decl.val.ty),
                            found: vague(&expr_type),
                        }),
                    ));
                }
            },
            _ => {
                return Err(CompileErrorSet::new_error(
                    item.val.param_exprs.range,
                    CompileError::Type(TypeError::ArgsLenMismatch {
                        func_name: item.val.func_name.val.str.clone(),
                        expected: param_decls.val.len(),
                        found: item.val.param_exprs.val.len(),
                    }),
                ));
            },
        }
    }

    Ok(())
}

fn vague(ty: &Arc<l::Type>) -> Arc<VagueType> {
    Arc::new(ty.as_ref().into())
}

fn eval_place(
    item: &l::Ref<l::Place>,
    ident_to_type: &IdentToType,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let head_type = eval_place_head(&item.val.head, ident_to_type)?;
    let offset = &item.val.offset;
    Ok(match &offset {
        None => head_type,
        Some(offset) => {
            let l::Type::Array { ty, .. } = head_type.as_ref() else {
                return Err(CompileErrorSet::new_error(
                    offset.range,
                    CompileError::Type(TypeError::IndexNonArray { found: vague(&head_type) }),
                ));
            };

            ty.clone()
        },
    })
}

fn eval_place_head(
    item: &l::Ref<l::PlaceHead>,
    ident_to_type: &IdentToType,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    match &item.val {
        l::PlaceHead::Ident(ident) => eval_ident(ident, ident_to_type),
        l::PlaceHead::Deref(deref) => eval_deref(deref, ident_to_type),
    }
}

fn eval_ident(
    item: &l::Ref<l::Ident>,
    ident_to_type: &IdentToType,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let name_str = &item.val.name.val.str;
    ident_to_type
        .get(name_str.as_ref())
        .ok_or_else(|| {
            CompileErrorSet::new_error(
                item.range,
                CompileError::Type(TypeError::IdentNotFound { name: name_str.clone() }),
            )
        })
        .cloned()
}

fn eval_deref(
    item: &l::Ref<l::Deref>,
    ident_to_type: &IdentToType,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let addr_type = eval_expr(&item.val.addr, ident_to_type)?;
    let l::Type::Ref(deref_type) = addr_type.as_ref() else {
        return Err(CompileErrorSet::new_error(
            item.range,
            CompileError::Type(TypeError::DerefNonRef { found: vague(&addr_type) }),
        ));
    };

    Ok(deref_type.clone())
}

fn eval_assign_expr(
    item: &l::Ref<l::AssignExpr>,
    expected_type: &Arc<l::Type>,
    ident_to_type: &IdentToType,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    match &item.val {
        l::AssignExpr::Expr(x) => eval_expr(x, ident_to_type),
        l::AssignExpr::Span(elements) => {
            let len = elements.val.len() as u32;

            let l::Type::Array { ty: expected_el_type, .. } = expected_type.as_ref() else {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::Mismatch {
                        expected: vague(expected_type),
                        found: Arc::new(VagueType::Array {
                            ty: Arc::new(VagueType::Unknown),
                            len: Some(len),
                        }),
                    }),
                ));
            };

            for el in elements.val.iter() {
                let el_type = eval_expr(el, ident_to_type)?;
                if el_type.is_subtype_of(expected_el_type).not() {
                    return Err(CompileErrorSet::new_error(
                        el.range,
                        CompileError::Type(TypeError::Mismatch {
                            expected: vague(expected_el_type),
                            found: vague(&el_type),
                        }),
                    ));
                }
            }

            Ok(Arc::new(l::Type::Array { ty: expected_el_type.clone(), len }))
        },
        l::AssignExpr::Slice { place, .. } => eval_place(place, ident_to_type),
    }
}

fn eval_expr(
    item: &l::Ref<l::Expr>,
    ident_to_type: &IdentToType,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    match &item.val {
        l::Expr::Literal(x) => Ok(match &x.val {
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
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::InvalidCast {
                        from: vague(&expr_ty),
                        to: vague(ty),
                    }),
                ));
            }

            Ok(ty.clone())
        },
        l::Expr::Transmute { ty, expr } => {
            let expr_ty = eval_expr(expr, ident_to_type)?;
            if ty.can_transmute_to(&expr_ty).not() {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::InvalidTransmute {
                        from: vague(&expr_ty),
                        to: vague(ty),
                    }),
                ));
            }

            Ok(ty.clone())
        },
    }
}

fn eval_paren_expr(
    item: &l::Ref<l::ParenExpr>,
    ident_to_type: &IdentToType,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    match &item.val {
        l::ParenExpr::Unary(x) => eval_unary_expr(x, ident_to_type),
        l::ParenExpr::Binary(x) => eval_binary_expr(x, ident_to_type),
    }
}

fn eval_unary_expr(
    item: &l::Ref<l::UnaryParenExpr>,
    ident_to_type: &IdentToType,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let operand = &item.val.operand;
    let operand_type = eval_expr(operand, ident_to_type)?;
    match item.val.op.val {
        l::UnaryParenExprOp::Not => {
            if operand_type.is_subtype_of(&BOOL_TYPE).not() {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::Mismatch {
                        expected: vague(&BOOL_TYPE),
                        found: vague(&operand_type),
                    }),
                ));
            }

            Ok(BOOL_TYPE.clone())
        },
    }
}

fn eval_binary_expr(
    item: &l::Ref<l::BinaryParenExpr>,
    ident_to_type: &IdentToType,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let left_type = eval_expr(&item.val.left, ident_to_type)?;
    let right_type = eval_expr(&item.val.right, ident_to_type)?;

    macro_rules! expect_types {
        (
            for $op:expr;
            $($left:expr, $right:expr => $out:expr;)*
        ) => {'expect: {
            $(
                if left_type.is_subtype_of(&$left) && right_type.is_subtype_of(&$right) {
                    break 'expect Ok($out.clone());
                }
            )*

            break 'expect Err(CompileErrorSet::new_error(
                item.range,
                CompileError::Type(TypeError::BinaryOpOperandsMismatch {
                    op: $op.into(),
                    expected: [
                        $(
                            (vague(&$left), vague(&$right)),
                        )*
                    ].into(),
                    found: (vague(&left_type), vague(&right_type)),
                }),
            ));
        }};
    }

    match item.val.op.val {
        l::BinaryParenExprOp::Add => expect_types!(
            for "+";
            UINT_TYPE, UINT_TYPE => UINT_TYPE;
            INT_TYPE, INT_TYPE => INT_TYPE;
            NUM_TYPE, NUM_TYPE => NUM_TYPE;
        ),
        l::BinaryParenExprOp::Sub => expect_types!(
            for "-";
            INT_TYPE, INT_TYPE => INT_TYPE;
            NUM_TYPE, NUM_TYPE => NUM_TYPE;
        ),
        l::BinaryParenExprOp::Mul => expect_types!(
            for "*";
            UINT_TYPE, UINT_TYPE => UINT_TYPE;
            INT_TYPE, INT_TYPE => INT_TYPE;
            NUM_TYPE, NUM_TYPE => NUM_TYPE;
        ),
        l::BinaryParenExprOp::Div => expect_types!(
            for "/";
            NUM_TYPE, NUM_TYPE => NUM_TYPE;
        ),
        l::BinaryParenExprOp::Mod => expect_types!(
            for "%";
            NUM_TYPE, NUM_TYPE => NUM_TYPE;
        ),
        l::BinaryParenExprOp::Eq => expect_types!(
            for "==";
            ANY_TYPE, ANY_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::Neq => expect_types!(
            for "!=";
            ANY_TYPE, ANY_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::Gt => expect_types!(
            for ">";
            ANY_TYPE, ANY_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::Lt => expect_types!(
            for "<";
            ANY_TYPE, ANY_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::Gte => expect_types!(
            for ">=";
            ANY_TYPE, ANY_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::Lte => expect_types!(
            for "<=";
            ANY_TYPE, ANY_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::And => expect_types!(
            for "&&";
            BOOL_TYPE, BOOL_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::Or => expect_types!(
            for "||";
            BOOL_TYPE, BOOL_TYPE => BOOL_TYPE;
        ),
        l::BinaryParenExprOp::Join => expect_types!(
            for "~";
            VAL_TYPE, VAL_TYPE => VAL_TYPE;
        ),
    }
}
