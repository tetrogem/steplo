use std::{
    collections::HashMap,
    ops::Not,
    sync::{Arc, LazyLock},
};

use itertools::Itertools;

use crate::high_compiler::srced::{SrcRange, Srced};

use super::{
    compile_error::{CompileError, CompileErrorSet, TypeError, VagueType},
    logic_ast as l, type_resolved_ast as t,
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

fn tr<L, T, F: FnOnce(&l::Ref<L>) -> T>(l_ref: &l::Ref<L>, f: F) -> t::Ref<T> {
    let t = f(l_ref);
    Arc::new(Srced { val: t, range: l_ref.range })
}

fn try_tr<L, T, Error, F: FnOnce(&l::Ref<L>) -> Result<T, Error>>(
    l_ref: &l::Ref<L>,
    f: F,
) -> Result<t::Ref<T>, Error> {
    let t = f(l_ref)?;
    Ok(Arc::new(Srced { val: t, range: l_ref.range }))
}

pub fn typecheck(program: &l::Ref<l::Program>) -> Result<t::Ref<t::Program>, CompileErrorSet> {
    let func_to_params = program
        .val
        .items
        .val
        .iter()
        .filter_map(|item| {
            if let l::TopItem::Exe(exe) = &item.val
                && let l::ExeItem::Func(func) = &exe.val
            {
                Some((func.val.name.val.str.clone(), func.val.params.clone()))
            } else {
                None
            }
        })
        .collect::<FuncToParams>();

    // let typename_to_type = program.val.items.val.iter().filter_map(|item| {
    //     let l::TopItem::Type(item) = item else { return None };
    //     match &item.val {
    //         l::TypeItem::Struct(x) =>
    //     }
    // });

    let items = try_tr(&program.val.items, |x| {
        Ok(x.val
            .iter()
            .map(|x| try_tr(x, |x| typecheck_top_item(x, &func_to_params)))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .filter_map(|x| x.val.clone())
            .collect_vec())
    })?;

    Ok(Arc::new(Srced { range: program.range, val: t::Program { items } }))
}

fn typecheck_top_item(
    item: &l::Ref<l::TopItem>,
    func_to_params: &FuncToParams,
) -> Result<Option<t::Ref<t::ExeItem>>, CompileErrorSet> {
    Ok(match &item.val {
        l::TopItem::Exe(x) => Some(try_tr(x, |x| typecheck_exe_item(x, func_to_params))?),
        l::TopItem::Type(x) => {
            typecheck_type_item(x);
            None
        },
    })
}

fn typecheck_exe_item(
    item: &l::Ref<l::ExeItem>,
    func_to_params: &FuncToParams,
) -> Result<t::ExeItem, CompileErrorSet> {
    Ok(match &item.val {
        l::ExeItem::Main(x) => t::ExeItem::Main(try_tr(x, |x| typecheck_main(x, func_to_params))?),
        l::ExeItem::Func(x) => t::ExeItem::Func(try_tr(x, |x| typecheck_func(x, func_to_params))?),
    })
}

fn typecheck_type_item(_item: &l::Ref<l::TypeItem>) {}

fn typecheck_main(
    item: &l::Ref<l::Main>,
    func_to_params: &FuncToParams,
) -> Result<t::Main, CompileErrorSet> {
    Ok(t::Main { proc: try_tr(&item.val.proc, |x| typecheck_proc(x, [].iter(), func_to_params))? })
}

fn tr_vec<L, T>(
    ts: &l::Ref<Vec<l::Ref<L>>>,
    typechecker: impl Fn(&l::Ref<L>) -> T,
) -> t::Ref<Vec<t::Ref<T>>> {
    tr(ts, |x| x.val.iter().map(|x| tr(x, |x| typechecker(x))).collect())
}

fn try_tr_vec<L, T, Error>(
    ts: &l::Ref<Vec<l::Ref<L>>>,
    typechecker: impl Fn(&l::Ref<L>) -> Result<T, Error>,
) -> Result<t::Ref<Vec<t::Ref<T>>>, Error> {
    try_tr(ts, |x| {
        x.val.iter().map(|x| try_tr(x, |x| typechecker(x))).collect::<Result<Vec<_>, _>>()
    })
}

fn typecheck_func(
    item: &l::Ref<l::Func>,
    func_to_params: &FuncToParams,
) -> Result<t::Func, CompileErrorSet> {
    Ok(t::Func {
        name: tr(&item.val.name, typecheck_name),
        params: tr_vec(&item.val.params, typecheck_ident_decl),
        proc: try_tr(&item.val.proc, |x| {
            typecheck_proc(x, item.val.params.val.iter(), func_to_params)
        })?,
    })
}

fn typecheck_name(item: &l::Ref<l::Name>) -> t::Name {
    t::Name { str: item.val.str.clone() }
}

fn typecheck_ident_decl(item: &l::Ref<l::IdentDeclaration>) -> t::IdentDeclaration {
    t::IdentDeclaration { name: tr(&item.val.name, typecheck_name), size: item.val.ty.size() }
}

fn typecheck_proc<'a>(
    item: &'a l::Ref<l::Proc>,
    params: impl Iterator<Item = &'a l::Ref<l::IdentDeclaration>>,
    func_to_params: &FuncToParams,
) -> Result<t::Proc, CompileErrorSet> {
    let idents = item.val.idents.val.iter().chain(params).map(AsRef::as_ref);
    let ident_to_type =
        idents.map(|i| (i.val.name.val.str.clone(), i.val.ty.clone())).collect::<IdentToType>();

    Ok(t::Proc {
        idents: tr_vec(&item.val.idents, typecheck_ident_decl),
        body: try_tr(&item.val.body, |x| typecheck_body(x, &ident_to_type, func_to_params))?,
    })
}

fn typecheck_body(
    item: &l::Ref<l::Body>,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> Result<t::Body, CompileErrorSet> {
    Ok(t::Body {
        items: try_tr(&item.val.items, |x| {
            x.val
                .iter()
                .map(|x| try_tr(x, |x| typecheck_body_item(x, ident_to_type, func_to_params)))
                .collect::<Result<_, _>>()
        })?,
    })
}

fn typecheck_body_item(
    item: &l::Ref<l::BodyItem>,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> Result<t::BodyItem, CompileErrorSet> {
    Ok(match &item.val {
        l::BodyItem::Statement(x) => t::BodyItem::Statement(try_tr(x, |x| {
            typecheck_statement(x, ident_to_type, func_to_params)
        })?),
        l::BodyItem::If(x) => {
            t::BodyItem::If(try_tr(x, |x| typecheck_if(x, ident_to_type, func_to_params))?)
        },
        l::BodyItem::While(x) => {
            t::BodyItem::While(try_tr(x, |x| typecheck_while(x, ident_to_type, func_to_params))?)
        },
    })
}

fn typecheck_if(
    item: &l::Ref<l::IfItem>,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> Result<t::IfItem, CompileErrorSet> {
    let condition_type = eval_expr(&item.val.condition, ident_to_type)?;
    if condition_type.is_subtype_of(&VAL_TYPE).not() {
        return Err(CompileErrorSet::new_error(
            item.val.condition.range,
            CompileError::Type(TypeError::Mismatch {
                expected: vague(&VAL_TYPE),
                found: vague(&condition_type),
            }),
        ));
    };

    Ok(t::IfItem {
        condition: try_tr(&item.val.condition, |x| typecheck_expr(x, ident_to_type))?,
        then_body: try_tr(&item.val.then_body, |x| {
            typecheck_body(x, ident_to_type, func_to_params)
        })?,
        else_item: match &item.val.else_item {
            None => None,
            Some(x) => Some(try_tr(x, |x| {
                Ok(t::ElseItem {
                    body: try_tr(&x.val.body, |x| {
                        typecheck_body(x, ident_to_type, func_to_params)
                    })?,
                })
            })?),
        },
    })
}

fn typecheck_while(
    item: &l::Ref<l::WhileItem>,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> Result<t::WhileItem, CompileErrorSet> {
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

    Ok(t::WhileItem {
        condition: try_tr(&item.val.condition, |x| typecheck_expr(x, ident_to_type))?,
        body: try_tr(&item.val.body, |x| typecheck_body(x, ident_to_type, func_to_params))?,
    })
}

fn typecheck_statement(
    item: &l::Ref<l::Statement>,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> Result<t::Statement, CompileErrorSet> {
    Ok(match &item.val {
        l::Statement::Assign(x) => {
            t::Statement::Assign(try_tr(x, |x| typecheck_assign(x, ident_to_type))?)
        },
        l::Statement::Call(x) => {
            t::Statement::Call(try_tr(x, |x| typecheck_call(x, ident_to_type, func_to_params))?)
        },
        l::Statement::Native(x) => {
            t::Statement::Native(try_tr(x, |x| typecheck_native_op(x, ident_to_type))?)
        },
    })
}

fn typecheck_native_op(
    item: &l::Ref<l::NativeOperation>,
    ident_to_type: &IdentToType,
) -> Result<t::NativeOperation, CompileErrorSet> {
    Ok(match &item.val {
        l::NativeOperation::Out { place } => {
            t::NativeOperation::Out { place: try_tr(place, |x| typecheck_place(x, ident_to_type))? }
        },
        l::NativeOperation::In { dest_place } => t::NativeOperation::In {
            dest_place: try_tr(dest_place, |x| typecheck_place(x, ident_to_type))?,
        },
        l::NativeOperation::Random { dest_place, min, max } => t::NativeOperation::Random {
            dest_ident: try_tr(dest_place, |x| typecheck_place(x, ident_to_type))?,
            min: try_tr(min, |x| typecheck_expr(x, ident_to_type))?,
            max: try_tr(max, |x| typecheck_expr(x, ident_to_type))?,
        },
    })
}

fn typecheck_assign(
    item: &l::Ref<l::Assign>,
    ident_to_type: &IdentToType,
) -> Result<t::Assign, CompileErrorSet> {
    let place_type = eval_place(&item.val.place, ident_to_type)?.ty;
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

    Ok(t::Assign {
        place: try_tr(&item.val.place, |x| typecheck_place(x, ident_to_type))?,
        expr: try_tr(&item.val.expr, |x| typecheck_assign_expr(x, &place_type, ident_to_type))?,
    })
}

fn typecheck_call(
    item: &l::Ref<l::FunctionCall>,
    ident_to_type: &IdentToType,
    func_to_params: &FuncToParams,
) -> Result<t::FunctionCall, CompileErrorSet> {
    let func_name = &item.val.func_name;
    let Some(param_decls) = func_to_params.get(func_name.val.str.as_ref()) else {
        return Err(CompileErrorSet::new_error(
            func_name.range,
            CompileError::Type(TypeError::FuncNotFound { name: func_name.val.str.clone() }),
        ));
    };

    let mut expr_iter = item.val.param_exprs.val.iter();
    let mut decl_iter = param_decls.val.iter();
    let mut typechecked_param_exprs = Vec::new();

    loop {
        let (expr, decl) = (expr_iter.next(), decl_iter.next());
        let typechecked_param_expr = match (expr, decl) {
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

                try_tr(expr, |x| typecheck_assign_expr(x, &decl.val.ty, ident_to_type))?
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
        };

        typechecked_param_exprs.push(typechecked_param_expr);
    }

    Ok(t::FunctionCall {
        func_name: tr(&item.val.func_name, typecheck_name),
        param_exprs: tr(&item.val.param_exprs, |_| typechecked_param_exprs),
    })
}

fn vague(ty: &Arc<l::Type>) -> Arc<VagueType> {
    Arc::new(ty.as_ref().into())
}

fn typecheck_place(
    item: &l::Ref<l::Place>,
    ident_to_type: &IdentToType,
) -> Result<t::Place, CompileErrorSet> {
    let place_eval = eval_place(item, ident_to_type)?;
    Ok(t::Place {
        head: try_tr(&item.val.head, |x| typecheck_place_head(x, ident_to_type))?,
        size: place_eval.ty.size(),
        offset: match place_eval.offset {
            None => None,
            Some(offset) => Some(try_tr(&offset, |x| typecheck_expr(x, ident_to_type))?),
        },
    })
}

struct LReffer {
    range: SrcRange,
}

impl LReffer {
    fn of<T>(&self, t: T) -> l::Ref<T> {
        Arc::new(Srced { range: self.range, val: t })
    }
}

struct EvalPlaceRes {
    ty: Arc<l::Type>,
    offset: Option<l::Ref<l::Expr>>,
}

impl EvalPlaceRes {
    fn update_index(&mut self, ty: Arc<l::Type>, offset: l::Ref<l::Expr>) {
        self.ty = ty;
        self.offset = Some(match &self.offset {
            None => offset,
            Some(curr_offset) => {
                let offset_range = curr_offset.range.merge(offset.range);
                let lr = LReffer { range: offset_range };

                lr.of(l::Expr::Paren(lr.of(l::ParenExpr::Binary(lr.of(l::BinaryParenExpr {
                    left: curr_offset.clone(),
                    op: lr.of(l::BinaryParenExprOp::Add),
                    right: offset,
                })))))
            },
        });
    }
}

fn eval_place(
    item: &l::Ref<l::Place>,
    ident_to_type: &IdentToType,
) -> Result<EvalPlaceRes, CompileErrorSet> {
    let ty = eval_place_head(&item.val.head, ident_to_type)?;
    let mut res = EvalPlaceRes { ty: ty.clone(), offset: None };

    for index in item.val.index_chain.val.iter() {
        match &index.val {
            l::PlaceIndex::Offset(offset) => {
                let l::Type::Array { ty, .. } = res.ty.as_ref() else {
                    return Err(CompileErrorSet::new_error(
                        offset.range,
                        CompileError::Type(TypeError::IndexNonArray { found: vague(&ty) }),
                    ));
                };

                let lr = LReffer { range: index.range };

                let index_offset =
                    lr.of(l::Expr::Paren(lr.of(l::ParenExpr::Binary(lr.of(l::BinaryParenExpr {
                        left: offset.clone(),
                        op: lr.of(l::BinaryParenExprOp::Mul),
                        right:
                            lr.of(l::Expr::Literal(lr.of(l::Literal::Uint(f64::from(ty.size()))))),
                    })))));

                res.update_index(ty.clone(), index_offset);
            },
            l::PlaceIndex::Field(index_field) => {
                let l::Type::Struct(struct_fields) = ty.as_ref() else {
                    todo!() // cannot index field of non-struct
                };

                let mut index_field_ty: Option<Arc<l::Type>> = None;
                let mut offset = 0;
                for struct_field in struct_fields.iter() {
                    if struct_field.name == index_field.val.str {
                        index_field_ty = Some(struct_field.ty.clone());
                        break;
                    }

                    offset += struct_field.ty.size();
                }

                let Some(index_field_ty) = index_field_ty else {
                    todo!(); // field is not in struct type
                };

                let lr = LReffer { range: index.range };

                let index_offset =
                    lr.of(l::Expr::Literal(lr.of(l::Literal::Uint(f64::from(offset)))));

                res.update_index(index_field_ty, index_offset);
            },
        };
    }

    Ok(res)
}

fn typecheck_place_head(
    item: &l::Ref<l::PlaceHead>,
    ident_to_type: &IdentToType,
) -> Result<t::PlaceHead, CompileErrorSet> {
    Ok(match &item.val {
        l::PlaceHead::Ident(x) => t::PlaceHead::Ident(tr(x, typecheck_ident)),
        l::PlaceHead::Deref(x) => {
            t::PlaceHead::Deref(try_tr(x, |x| typecheck_deref(x, ident_to_type))?)
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

fn typecheck_ident(item: &l::Ref<l::Ident>) -> t::Ident {
    t::Ident { name: tr(&item.val.name, typecheck_name) }
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

fn typecheck_deref(
    item: &l::Ref<l::Deref>,
    ident_to_type: &IdentToType,
) -> Result<t::Deref, CompileErrorSet> {
    Ok(t::Deref { addr: try_tr(&item.val.addr, |x| typecheck_expr(x, ident_to_type))? })
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

fn typecheck_assign_expr(
    item: &l::Ref<l::AssignExpr>,
    expected_type: &Arc<l::Type>,
    ident_to_type: &IdentToType,
) -> Result<t::AssignExpr, CompileErrorSet> {
    Ok(match &item.val {
        l::AssignExpr::Expr(x) => match expected_type.size() {
            1 => t::AssignExpr::Expr(try_tr(x, |x| typecheck_expr(x, ident_to_type))?),
            size => {
                fn typecheck_copy(
                    expr: &l::Ref<l::Expr>,
                    size: u32,
                    ident_to_type: &IdentToType,
                ) -> Result<t::AssignExpr, CompileErrorSet> {
                    Ok(match &expr.val {
                        l::Expr::Place(place) => t::AssignExpr::Copy {
                            place: try_tr(place, |x| typecheck_place(x, ident_to_type))?,
                            start_in: 0,
                            end_ex: size,
                        },
                        l::Expr::Cast { expr, .. } | l::Expr::Transmute { expr, .. } => {
                            typecheck_copy(expr, size, ident_to_type)?
                        },
                        _ => {
                            todo!() // cannot assign expr to non-size 1 type
                        },
                    })
                }

                typecheck_copy(x, size, ident_to_type)?
            },
        },
        l::AssignExpr::Span(x) => {
            t::AssignExpr::Span(try_tr_vec(x, |x| typecheck_expr(x, ident_to_type))?)
        },
        l::AssignExpr::Slice { place, start_in, end_ex } => {
            let place_eval = eval_place(place, ident_to_type)?;
            t::AssignExpr::Copy {
                place: try_tr(place, |x| typecheck_place(x, ident_to_type))?,
                start_in: *start_in * place_eval.ty.size(),
                end_ex: *end_ex * place_eval.ty.size(),
            }
        },
    })
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
        l::AssignExpr::Slice { place, start_in, end_ex } => {
            let place_res = eval_place(place, ident_to_type)?;

            let Some(len) = end_ex.checked_sub(*start_in) else {
                todo!() // error, negative length
            };

            let l::Type::Array { ty: el_type, .. } = place_res.ty.as_ref() else {
                todo!() // error, cannot slice non-array
            };

            Ok(Arc::new(l::Type::Array { ty: el_type.clone(), len }))
        },
    }
}

fn typecheck_expr(
    item: &l::Ref<l::Expr>,
    ident_to_type: &IdentToType,
) -> Result<t::Expr, CompileErrorSet> {
    Ok(match &item.val {
        l::Expr::Literal(x) => t::Expr::Literal(tr(x, |x| match &x.val {
            l::Literal::Val(x) => t::Literal::Val(x.clone()),
            l::Literal::Num(x) => t::Literal::Num(*x),
            l::Literal::Int(x) => t::Literal::Int(*x),
            l::Literal::Uint(x) => t::Literal::Uint(*x),
            l::Literal::Bool(x) => t::Literal::Bool(*x),
        })),
        l::Expr::Place(x) => t::Expr::Place(try_tr(x, |x| typecheck_place(x, ident_to_type))?),
        l::Expr::Ref(x) => t::Expr::Ref(try_tr(x, |x| typecheck_place(x, ident_to_type))?),
        l::Expr::Paren(x) => t::Expr::Paren(try_tr(x, |x| typecheck_paren_expr(x, ident_to_type))?),
        l::Expr::Cast { expr, .. } => typecheck_expr(expr, ident_to_type)?,
        l::Expr::Transmute { expr, .. } => typecheck_expr(expr, ident_to_type)?,
    })
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
        l::Expr::Place(x) => eval_place(x, ident_to_type).map(|res| res.ty),
        l::Expr::Ref(place) => {
            let place_type = eval_place(place, ident_to_type)?;
            Ok(Arc::new(l::Type::Ref(place_type.ty)))
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

fn typecheck_paren_expr(
    item: &l::Ref<l::ParenExpr>,
    ident_to_type: &IdentToType,
) -> Result<t::ParenExpr, CompileErrorSet> {
    Ok(match &item.val {
        l::ParenExpr::Unary(x) => {
            t::ParenExpr::Unary(try_tr(x, |x| typecheck_unary_expr(x, ident_to_type))?)
        },
        l::ParenExpr::Binary(x) => {
            t::ParenExpr::Binary(try_tr(x, |x| typecheck_binary_expr(x, ident_to_type))?)
        },
    })
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

macro_rules! typecheck_variants {
    ($expr:expr, $ty:ident => [$($variant:ident),* $(,)?]) => {
        match $expr {
            $(
                l::$ty::$variant => t::$ty::$variant,
            )*
        }
    };
}

fn typecheck_unary_expr(
    item: &l::Ref<l::UnaryParenExpr>,
    ident_to_type: &IdentToType,
) -> Result<t::UnaryParenExpr, CompileErrorSet> {
    Ok(t::UnaryParenExpr {
        operand: try_tr(&item.val.operand, |x| typecheck_expr(x, ident_to_type))?,
        op: tr(&item.val.op, |x| typecheck_variants!(&x.val, UnaryParenExprOp => [Not])),
    })
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

fn typecheck_binary_expr(
    item: &l::Ref<l::BinaryParenExpr>,
    ident_to_type: &IdentToType,
) -> Result<t::BinaryParenExpr, CompileErrorSet> {
    Ok(t::BinaryParenExpr {
        left: try_tr(&item.val.left, |x| typecheck_expr(x, ident_to_type))?,
        right: try_tr(&item.val.right, |x| typecheck_expr(x, ident_to_type))?,
        op: tr(&item.val.op, |x| {
            typecheck_variants!(&x.val, BinaryParenExprOp => [
                Add,
                Sub,
                Mul,
                Div,
                Mod,
                Eq,
                Neq,
                Gt,
                Lt,
                Gte,
                Lte,
                And,
                Or,
                Join,
            ])
        }),
    })
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
