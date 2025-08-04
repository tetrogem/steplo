use std::{
    collections::{HashMap, HashSet},
    ops::Not,
    sync::{Arc, LazyLock},
};

use itertools::Itertools;

use crate::high_compiler::{
    logic_ast::TypeHint,
    srced::{SrcRange, Srced},
};

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

type IdentToTypeHint = HashMap<Arc<str>, l::Ref<l::TypeHint>>;
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

struct TypeAliasManager {
    enum_to_variants: HashMap<Arc<str>, Arc<Vec<Arc<str>>>>,
    alias_to_type_hint: HashMap<Arc<str>, l::Ref<TypeHint>>,
}

impl TypeAliasManager {
    pub fn new(program: &l::Ref<l::Program>) -> Self {
        let mut enum_to_variants = HashMap::new();
        let mut alias_to_type_hint = HashMap::new();

        for item in program.val.items.val.iter() {
            let l::TopItem::Type(item) = &item.val else { continue };

            match &item.val {
                l::TypeItem::Alias(x) => {
                    alias_to_type_hint.insert(x.val.name.val.str.clone(), x.val.ty.clone());
                },
                l::TypeItem::Enum(x) => {
                    enum_to_variants.insert(
                        x.val.name.val.str.clone(),
                        Arc::new(x.val.variants.val.iter().map(|v| v.val.str.clone()).collect()),
                    );
                },
            }
        }

        Self { enum_to_variants, alias_to_type_hint }
    }

    pub fn resolve(&self, hint: &l::Ref<l::TypeHint>) -> Result<l::Type, CompileErrorSet> {
        Ok(match &hint.val {
            l::TypeHint::Nominal(name) => {
                if self.enum_to_variants.get(&name.val.str).is_some() {
                    l::Type::Enum { name: name.val.str.clone() }
                } else {
                    match self.alias_to_type_hint.get(&name.val.str) {
                        Some(hint) => self.resolve(hint)?,
                        None => {
                            return Err(CompileErrorSet::new_error(
                                name.range,
                                CompileError::Type(TypeError::UnknownAlias {
                                    name: name.val.str.clone(),
                                }),
                            ));
                        },
                    }
                }
            },
            l::TypeHint::Any => l::Type::Any,
            l::TypeHint::Primitive(p) => l::Type::Primitive(*p),
            l::TypeHint::Ref(hint) => l::Type::Ref(Arc::new(self.resolve(hint)?)),
            l::TypeHint::Array { ty, len } => {
                l::Type::Array { ty: Arc::new(self.resolve(ty)?), len: *len }
            },
            l::TypeHint::Struct(fields) => {
                let fields = fields
                    .val
                    .iter()
                    .map(|field| {
                        Ok(Arc::new(l::FieldType {
                            name: field.val.name.val.str.clone(),
                            ty: Arc::new(self.resolve(&field.val.ty)?),
                        }))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                l::Type::Struct(Arc::new(fields))
            },
        })
    }

    pub fn id_variant(
        &self,
        enum_name: &l::Ref<l::Name>,
        variant_name: &l::Ref<l::Name>,
    ) -> Result<u32, CompileErrorSet> {
        let Some(variants) = self.enum_to_variants.get(&enum_name.val.str) else {
            todo!(); // enum with this name not found
        };

        let Some((id, _)) = variants.iter().enumerate().find(|(_, v)| *v == &variant_name.val.str)
        else {
            todo!(); // variant with this name not found in enum
        };

        Ok(id as u32)
    }

    pub fn all_variants(
        &self,
        enum_name: &Arc<str>,
    ) -> Result<&Arc<Vec<Arc<str>>>, CompileErrorSet> {
        let Some(variants) = self.enum_to_variants.get(enum_name) else {
            todo!(); // enum with this name not found
        };

        Ok(variants)
    }
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

    let type_alias_m = TypeAliasManager::new(program);

    let items = try_tr(&program.val.items, |x| {
        Ok(x.val
            .iter()
            .map(|x| try_tr(x, |x| typecheck_top_item(x, &func_to_params, &type_alias_m)))
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
    type_alias_m: &TypeAliasManager,
) -> Result<Option<t::Ref<t::ExeItem>>, CompileErrorSet> {
    Ok(match &item.val {
        l::TopItem::Exe(x) => {
            Some(try_tr(x, |x| typecheck_exe_item(x, func_to_params, type_alias_m))?)
        },
        l::TopItem::Type(x) => {
            typecheck_type_item(x);
            None
        },
    })
}

fn typecheck_exe_item(
    item: &l::Ref<l::ExeItem>,
    func_to_params: &FuncToParams,
    type_alias_m: &TypeAliasManager,
) -> Result<t::ExeItem, CompileErrorSet> {
    Ok(match &item.val {
        l::ExeItem::Main(x) => {
            t::ExeItem::Main(try_tr(x, |x| typecheck_main(x, func_to_params, type_alias_m))?)
        },
        l::ExeItem::Func(x) => {
            t::ExeItem::Func(try_tr(x, |x| typecheck_func(x, func_to_params, type_alias_m))?)
        },
    })
}

fn typecheck_type_item(_item: &l::Ref<l::TypeItem>) {}

fn typecheck_main(
    item: &l::Ref<l::Main>,
    func_to_params: &FuncToParams,
    type_alias_m: &TypeAliasManager,
) -> Result<t::Main, CompileErrorSet> {
    Ok(t::Main {
        proc: try_tr(&item.val.proc, |x| {
            typecheck_proc(x, [].iter(), func_to_params, type_alias_m)
        })?,
    })
}

#[expect(unused)]
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
    type_alias_m: &TypeAliasManager,
) -> Result<t::Func, CompileErrorSet> {
    Ok(t::Func {
        name: tr(&item.val.name, typecheck_name),
        params: try_tr_vec(&item.val.params, |x| typecheck_ident_decl(x, type_alias_m))?,
        proc: try_tr(&item.val.proc, |x| {
            typecheck_proc(x, item.val.params.val.iter(), func_to_params, type_alias_m)
        })?,
    })
}

fn typecheck_name(item: &l::Ref<l::Name>) -> t::Name {
    t::Name { str: item.val.str.clone() }
}

fn typecheck_ident_decl(
    item: &l::Ref<l::IdentDeclaration>,
    type_alias_m: &TypeAliasManager,
) -> Result<t::IdentDeclaration, CompileErrorSet> {
    Ok(t::IdentDeclaration {
        name: tr(&item.val.name, typecheck_name),
        size: type_alias_m.resolve(&item.val.ty)?.size(),
    })
}

fn typecheck_proc<'a>(
    item: &'a l::Ref<l::Proc>,
    params: impl Iterator<Item = &'a l::Ref<l::IdentDeclaration>>,
    func_to_params: &FuncToParams,
    type_alias_m: &TypeAliasManager,
) -> Result<t::Proc, CompileErrorSet> {
    let idents = item.val.idents.val.iter().chain(params).map(AsRef::as_ref);
    let ident_to_type =
        idents.map(|i| (i.val.name.val.str.clone(), i.val.ty.clone())).collect::<IdentToTypeHint>();

    Ok(t::Proc {
        idents: try_tr_vec(&item.val.idents, |x| typecheck_ident_decl(x, type_alias_m))?,
        body: try_tr(&item.val.body, |x| {
            typecheck_body(x, &ident_to_type, func_to_params, type_alias_m)
        })?,
    })
}

fn typecheck_body(
    item: &l::Ref<l::Body>,
    ident_to_type: &IdentToTypeHint,
    func_to_params: &FuncToParams,
    type_alias_m: &TypeAliasManager,
) -> Result<t::Body, CompileErrorSet> {
    Ok(t::Body {
        items: try_tr(&item.val.items, |x| {
            x.val
                .iter()
                .map(|x| {
                    let item = try_tr(x, |x| {
                        let body_item =
                            typecheck_body_item(x, ident_to_type, func_to_params, type_alias_m)?;

                        Ok(body_item.map(|bi| tr(x, |_| bi)))
                    })?;

                    Ok(item.val.as_ref().map(|val| val.clone()))
                })
                .filter_map(|x| x.transpose())
                .collect::<Result<Vec<_>, _>>()
        })?,
    })
}

fn typecheck_body_item(
    item: &l::Ref<l::BodyItem>,
    ident_to_type: &IdentToTypeHint,
    func_to_params: &FuncToParams,
    type_alias_m: &TypeAliasManager,
) -> Result<Option<t::BodyItem>, CompileErrorSet> {
    Ok(match &item.val {
        l::BodyItem::Statement(x) => Some(t::BodyItem::Statement(try_tr(x, |x| {
            typecheck_statement(x, ident_to_type, func_to_params, type_alias_m)
        })?)),
        l::BodyItem::If(x) => Some(t::BodyItem::If(try_tr(x, |x| {
            typecheck_if(x, ident_to_type, func_to_params, type_alias_m)
        })?)),
        l::BodyItem::While(x) => Some(t::BodyItem::While(try_tr(x, |x| {
            typecheck_while(x, ident_to_type, func_to_params, type_alias_m)
        })?)),
        l::BodyItem::Match(match_item) => {
            let expr_ty = eval_expr(&match_item.val.expr, ident_to_type, type_alias_m)?;
            let l::Type::Enum { name: enum_name } = expr_ty.as_ref() else {
                todo!(); // cannot match non-enum
            };

            let mut unmatched_variants =
                type_alias_m.all_variants(enum_name)?.iter().collect::<HashSet<_>>();

            // TODO: make this do a binary search instead to save on the number of checks?
            let mut if_item: Option<t::Ref<t::IfItem>> = None;

            for case in match_item.val.cases.val.iter().rev() {
                let variant = &case.val.variant;

                if &variant.val.enum_name.val.str != enum_name {
                    todo!(); // wrong enum
                }

                if unmatched_variants.remove(&variant.val.variant_name.val.str).not() {
                    todo!(); // double matched variant
                }

                let case_condition = try_tr(variant, |v| {
                    Ok(t::Expr::Paren(try_tr(v, |v| {
                        Ok(t::ParenExpr::Binary(try_tr(v, |v| {
                            Ok(t::BinaryParenExpr {
                                left: try_tr(&match_item.val.expr, |expr| {
                                    typecheck_expr(expr, ident_to_type, type_alias_m)
                                })?,
                                op: tr(v, |_| t::BinaryParenExprOp::Eq),
                                right: try_tr(v, |v| {
                                    Ok(t::Expr::Literal(try_tr(v, |v| {
                                        typecheck_variant_literal(v, type_alias_m)
                                    })?))
                                })?,
                            })
                        })?))
                    })?))
                })?;

                if_item = Some(try_tr(case, |_| {
                    Ok(t::IfItem {
                        condition: case_condition,
                        then_body: try_tr(&case.val.body, |x| {
                            typecheck_body(x, ident_to_type, func_to_params, type_alias_m)
                        })?,
                        else_item: match if_item {
                            None => None,
                            Some(if_item) => Some(tr(&if_item, |x| t::ElseItem {
                                body: tr(x, |x| t::Body {
                                    items: tr(x, |x| {
                                        Vec::from([tr(x, |x| t::BodyItem::If(x.clone()))])
                                    }),
                                }),
                            })),
                        },
                    })
                })?);
            }

            if unmatched_variants.is_empty().not() {
                todo!(); // not all variants are matched
            }

            if_item.map(t::BodyItem::If)
        },
    })
}

fn typecheck_if(
    item: &l::Ref<l::IfItem>,
    ident_to_type: &IdentToTypeHint,
    func_to_params: &FuncToParams,
    type_alias_m: &TypeAliasManager,
) -> Result<t::IfItem, CompileErrorSet> {
    let condition_type = eval_expr(&item.val.condition, ident_to_type, type_alias_m)?;
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
        condition: try_tr(&item.val.condition, |x| typecheck_expr(x, ident_to_type, type_alias_m))?,
        then_body: try_tr(&item.val.then_body, |x| {
            typecheck_body(x, ident_to_type, func_to_params, type_alias_m)
        })?,
        else_item: match &item.val.else_item {
            None => None,
            Some(x) => Some(try_tr(x, |x| {
                Ok(t::ElseItem {
                    body: try_tr(&x.val.body, |x| {
                        typecheck_body(x, ident_to_type, func_to_params, type_alias_m)
                    })?,
                })
            })?),
        },
    })
}

fn typecheck_while(
    item: &l::Ref<l::WhileItem>,
    ident_to_type: &IdentToTypeHint,
    func_to_params: &FuncToParams,
    type_alias_m: &TypeAliasManager,
) -> Result<t::WhileItem, CompileErrorSet> {
    let condition_type = eval_expr(&item.val.condition, ident_to_type, type_alias_m)?;
    if condition_type.is_subtype_of(&VAL_TYPE).not() {
        return Err(CompileErrorSet::new_error(
            item.val.condition.range,
            CompileError::Type(TypeError::Mismatch {
                expected: vague(&VAL_TYPE),
                found: vague(&condition_type),
            }),
        ));
    }

    typecheck_body(&item.val.body, ident_to_type, func_to_params, type_alias_m)?;

    Ok(t::WhileItem {
        condition: try_tr(&item.val.condition, |x| typecheck_expr(x, ident_to_type, type_alias_m))?,
        body: try_tr(&item.val.body, |x| {
            typecheck_body(x, ident_to_type, func_to_params, type_alias_m)
        })?,
    })
}

fn typecheck_statement(
    item: &l::Ref<l::Statement>,
    ident_to_type: &IdentToTypeHint,
    func_to_params: &FuncToParams,
    type_alias_m: &TypeAliasManager,
) -> Result<t::Statement, CompileErrorSet> {
    Ok(match &item.val {
        l::Statement::Assign(x) => {
            t::Statement::Assign(try_tr(x, |x| typecheck_assign(x, ident_to_type, type_alias_m))?)
        },
        l::Statement::Call(x) => t::Statement::Call(try_tr(x, |x| {
            typecheck_call(x, ident_to_type, func_to_params, type_alias_m)
        })?),
        l::Statement::Native(x) => t::Statement::Native(try_tr(x, |x| {
            typecheck_native_op(x, ident_to_type, type_alias_m)
        })?),
    })
}

fn typecheck_native_op(
    item: &l::Ref<l::NativeOperation>,
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<t::NativeOperation, CompileErrorSet> {
    Ok(match &item.val {
        l::NativeOperation::Out { place } => t::NativeOperation::Out {
            place: try_tr(place, |x| typecheck_place(x, ident_to_type, type_alias_m))?,
        },
        l::NativeOperation::In { dest_place } => t::NativeOperation::In {
            dest_place: try_tr(dest_place, |x| typecheck_place(x, ident_to_type, type_alias_m))?,
        },
        l::NativeOperation::Random { dest_place, min, max } => t::NativeOperation::Random {
            dest_place: try_tr(dest_place, |x| typecheck_place(x, ident_to_type, type_alias_m))?,
            min: try_tr(min, |x| typecheck_expr(x, ident_to_type, type_alias_m))?,
            max: try_tr(max, |x| typecheck_expr(x, ident_to_type, type_alias_m))?,
        },
        l::NativeOperation::StdoutClear => t::NativeOperation::StdoutClear,
        l::NativeOperation::StdoutRead { dest_place, index } => t::NativeOperation::StdoutRead {
            dest_place: try_tr(dest_place, |x| typecheck_place(x, ident_to_type, type_alias_m))?,
            index: try_tr(index, |x| typecheck_expr(x, ident_to_type, type_alias_m))?,
        },
        l::NativeOperation::StdoutWrite { val, index } => t::NativeOperation::StdoutWrite {
            val: try_tr(val, |x| typecheck_expr(x, ident_to_type, type_alias_m))?,
            index: try_tr(index, |x| typecheck_expr(x, ident_to_type, type_alias_m))?,
        },
        l::NativeOperation::StdoutLen { dest_place } => t::NativeOperation::StdoutLen {
            dest_place: try_tr(dest_place, |x| typecheck_place(x, ident_to_type, type_alias_m))?,
        },
        l::NativeOperation::Wait { duration_s } => t::NativeOperation::Wait {
            duration_s: try_tr(duration_s, |x| typecheck_expr(x, ident_to_type, type_alias_m))?,
        },
        l::NativeOperation::TimerGet { dest_place } => t::NativeOperation::TimerGet {
            dest_place: try_tr(dest_place, |x| typecheck_place(x, ident_to_type, type_alias_m))?,
        },
    })
}

fn typecheck_assign(
    item: &l::Ref<l::Assign>,
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<t::Assign, CompileErrorSet> {
    let place_type = eval_place(&item.val.place, ident_to_type, type_alias_m)?.ty;
    let expr = &item.val.expr;
    let expr_type = eval_assign_expr(expr, &place_type, ident_to_type, type_alias_m)?;
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
        place: try_tr(&item.val.place, |x| typecheck_place(x, ident_to_type, type_alias_m))?,
        expr: try_tr(&item.val.expr, |x| {
            Ok(typecheck_assign_expr(x, &place_type, ident_to_type, type_alias_m)?
                .into_iter()
                .map(|expr| tr(x, |_| expr))
                .collect())
        })?,
    })
}

fn typecheck_call(
    item: &l::Ref<l::FunctionCall>,
    ident_to_type: &IdentToTypeHint,
    func_to_params: &FuncToParams,
    type_alias_m: &TypeAliasManager,
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
    let mut typechecked_param_exprs_vec = Vec::new();

    loop {
        let (expr, decl) = (expr_iter.next(), decl_iter.next());
        let typechecked_param_exprs = match (expr, decl) {
            (None, None) => break,
            (Some(expr), Some(decl)) => {
                let decl_type = Arc::new(type_alias_m.resolve(&decl.val.ty)?);
                let expr_type = eval_assign_expr(expr, &decl_type, ident_to_type, type_alias_m)?;
                if expr_type.is_subtype_of(&decl_type).not() {
                    return Err(CompileErrorSet::new_error(
                        expr.range,
                        CompileError::Type(TypeError::Mismatch {
                            expected: vague(&decl_type),
                            found: vague(&expr_type),
                        }),
                    ));
                }

                try_tr(expr, |x| {
                    Ok(typecheck_assign_expr(x, &decl_type, ident_to_type, type_alias_m)?
                        .into_iter()
                        .map(|expr| tr(x, |_| expr))
                        .collect())
                })?
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

        typechecked_param_exprs_vec.push(typechecked_param_exprs);
    }

    Ok(t::FunctionCall {
        func_name: tr(&item.val.func_name, typecheck_name),
        param_exprs: tr(&item.val.param_exprs, |_| typechecked_param_exprs_vec),
    })
}

fn vague(ty: &Arc<l::Type>) -> Arc<VagueType> {
    Arc::new(ty.as_ref().into())
}

fn typecheck_place(
    item: &l::Ref<l::Place>,
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<t::Place, CompileErrorSet> {
    let place_eval = eval_place(item, ident_to_type, type_alias_m)?;
    Ok(t::Place {
        head: try_tr(&item.val.head, |x| typecheck_place_head(x, ident_to_type, type_alias_m))?,
        offset: match place_eval.offset {
            None => None,
            Some(offset) => {
                Some(try_tr(&offset, |x| typecheck_expr(x, ident_to_type, type_alias_m))?)
            },
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
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<EvalPlaceRes, CompileErrorSet> {
    let ty = eval_place_head(&item.val.head, ident_to_type, type_alias_m)?;
    let mut res = EvalPlaceRes { ty: ty.clone(), offset: None };

    for index in item.val.index_chain.val.iter() {
        match &index.val {
            l::PlaceIndex::Offset(offset) => {
                let l::Type::Array { ty, .. } = res.ty.as_ref() else {
                    return Err(CompileErrorSet::new_error(
                        offset.range,
                        CompileError::Type(TypeError::OffsetIndexNonArray { found: vague(&ty) }),
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
                let l::Type::Struct(struct_fields) = res.ty.as_ref() else {
                    return Err(CompileErrorSet::new_error(
                        index_field.range,
                        CompileError::Type(TypeError::FieldIndexNonStruct { found: vague(&ty) }),
                    ));
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
                    return Err(CompileErrorSet::new_error(
                        index_field.range,
                        CompileError::Type(TypeError::IndexInvalidField {
                            struct_type: vague(&ty),
                            field_name: index_field.val.str.clone(),
                        }),
                    ));
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
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<t::PlaceHead, CompileErrorSet> {
    Ok(match &item.val {
        l::PlaceHead::Ident(x) => t::PlaceHead::Ident(tr(x, typecheck_ident)),
        l::PlaceHead::Deref(x) => {
            t::PlaceHead::Deref(try_tr(x, |x| typecheck_deref(x, ident_to_type, type_alias_m))?)
        },
    })
}

fn eval_place_head(
    item: &l::Ref<l::PlaceHead>,
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    match &item.val {
        l::PlaceHead::Ident(ident) => eval_ident(ident, ident_to_type, type_alias_m),
        l::PlaceHead::Deref(deref) => eval_deref(deref, ident_to_type, type_alias_m),
    }
}

fn typecheck_ident(item: &l::Ref<l::Ident>) -> t::Ident {
    t::Ident { name: tr(&item.val.name, typecheck_name) }
}

fn eval_ident(
    item: &l::Ref<l::Ident>,
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let name_str = &item.val.name.val.str;
    let type_hint = match ident_to_type.get(name_str.as_ref()) {
        Some(hint) => hint,
        None => {
            return Err(CompileErrorSet::new_error(
                item.range,
                CompileError::Type(TypeError::IdentNotFound { name: name_str.clone() }),
            ));
        },
    };

    type_alias_m.resolve(type_hint).map(Arc::new)
}

fn typecheck_deref(
    item: &l::Ref<l::Deref>,
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<t::Deref, CompileErrorSet> {
    Ok(t::Deref {
        addr: try_tr(&item.val.addr, |x| typecheck_expr(x, ident_to_type, type_alias_m))?,
    })
}

fn eval_deref(
    item: &l::Ref<l::Deref>,
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let addr_type = eval_expr(&item.val.addr, ident_to_type, type_alias_m)?;
    let l::Type::Ref(deref_type) = addr_type.as_ref() else {
        return Err(CompileErrorSet::new_error(
            item.range,
            CompileError::Type(TypeError::DerefNonRef { found: vague(&addr_type) }),
        ));
    };

    Ok(deref_type.clone())
}

fn typecheck_copy(
    expr: &l::Ref<l::Expr>,
    expected_place_size: u32,
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<Vec<t::AssignExpr>, CompileErrorSet> {
    Ok(match &expr.val {
        l::Expr::Place(place) => {
            let place = try_tr(place, |x| typecheck_place(x, ident_to_type, type_alias_m))?;
            (0..expected_place_size)
                .map(|offset| t::AssignExpr {
                    offset,
                    expr: tr(&place, |x| {
                        t::Expr::Place(tr(x, |x| {
                            let offset_expr =
                                t::Expr::Literal(tr(x, |_| t::Literal::Uint(offset as f64)));

                            let offset_expr = match &place.val.offset {
                                None => offset_expr,
                                Some(place_offset_expr) => t::Expr::Paren(tr(x, |x| {
                                    t::ParenExpr::Binary(tr(x, |x| t::BinaryParenExpr {
                                        left: tr(x, |_| offset_expr),
                                        op: tr(x, |_| t::BinaryParenExprOp::Add),
                                        right: place_offset_expr.clone(),
                                    }))
                                })),
                            };

                            t::Place {
                                head: x.val.head.clone(),
                                offset: Some(tr(x, |_| offset_expr)),
                            }
                        }))
                    }),
                })
                .collect()
        },
        l::Expr::Cast { expr, .. } | l::Expr::Transmute { expr, .. } => {
            typecheck_copy(expr, expected_place_size, ident_to_type, type_alias_m)?
        },
        _ => {
            return Err(CompileErrorSet::new_error(
                expr.range,
                CompileError::Type(TypeError::AssignCellExprToCompoundPlace {
                    place_size: expected_place_size,
                }),
            ));
        },
    })
}

fn typecheck_assign_expr(
    item: &l::Ref<l::AssignExpr>,
    expected_type: &Arc<l::Type>,
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<Vec<t::AssignExpr>, CompileErrorSet> {
    let assign_expr_ty = eval_assign_expr(item, expected_type, ident_to_type, type_alias_m)?;

    Ok(match &item.val {
        l::AssignExpr::Expr(x) => match expected_type.size() {
            1 => Vec::from([t::AssignExpr {
                offset: 0,
                expr: try_tr(x, |x| typecheck_expr(x, ident_to_type, type_alias_m))?,
            }]),
            size => typecheck_copy(x, size, ident_to_type, type_alias_m)?,
        },
        l::AssignExpr::Array { single_exprs, spread_expr } => {
            let l::Type::Array { ty: expected_el_type, len: expected_len } = expected_type.as_ref()
            else {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::Mismatch {
                        expected: vague(expected_type),
                        found: vague(&assign_expr_ty),
                    }),
                ));
            };

            let el_size = expected_el_type.size();
            let mut single_expr_iter = single_exprs.val.iter();
            let mut typechecked_assign_exprs = Vec::new();

            for i in 0..(*expected_len) {
                let el = match single_expr_iter.next() {
                    Some(x) => x,
                    None => match spread_expr {
                        Some(x) => x,
                        None => {
                            return Err(CompileErrorSet::new_error(
                                item.range,
                                CompileError::Type(TypeError::Mismatch {
                                    expected: vague(expected_type),
                                    found: vague(&assign_expr_ty),
                                }),
                            ));
                        },
                    },
                };

                let exprs =
                    typecheck_assign_expr(el, expected_el_type, ident_to_type, type_alias_m)?;

                let el_assign_exprs = exprs.iter().map(|el| t::AssignExpr {
                    offset: i * el_size + el.offset,
                    expr: el.expr.clone(),
                });

                typechecked_assign_exprs.extend(el_assign_exprs);
            }

            typechecked_assign_exprs
        },
        l::AssignExpr::Struct(assign_fields) => {
            let l::Type::Struct(expected_fields) = expected_type.as_ref() else {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::Mismatch {
                        expected: vague(expected_type),
                        found: vague(&assign_expr_ty),
                    }),
                ));
            };

            let mut struct_assign_exprs = Vec::new();
            let mut field_offset = 0;

            for expected_field in expected_fields.iter() {
                let Some(assign_field) = assign_fields
                    .val
                    .iter()
                    .find(|assign_field| assign_field.val.name.val.str == expected_field.name)
                else {
                    return Err(CompileErrorSet::new_error(
                        assign_fields.range,
                        CompileError::Type(TypeError::StructLiteralMissingField {
                            field_type: vague(&expected_field.ty),
                            field_name: expected_field.name.clone(),
                        }),
                    ));
                };

                let field_assign_exprs = typecheck_assign_expr(
                    &assign_field.val.assign,
                    &expected_field.ty,
                    ident_to_type,
                    type_alias_m,
                )?;

                for field_assign_expr in field_assign_exprs {
                    struct_assign_exprs.push(t::AssignExpr {
                        offset: field_offset + field_assign_expr.offset,
                        expr: field_assign_expr.expr,
                    });
                }

                field_offset += expected_field.ty.size();
            }

            struct_assign_exprs
        },
    })
}

fn eval_assign_expr(
    item: &l::Ref<l::AssignExpr>,
    expected_type: &Arc<l::Type>,
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    match &item.val {
        l::AssignExpr::Expr(x) => eval_expr(x, ident_to_type, type_alias_m),
        l::AssignExpr::Array { single_exprs, spread_expr } => {
            let l::Type::Array { ty: expected_el_type, len: expected_len } = expected_type.as_ref()
            else {
                let len = match spread_expr {
                    None => Some(single_exprs.val.len() as u32),
                    Some(_) => None,
                };

                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::Mismatch {
                        expected: vague(expected_type),
                        found: Arc::new(VagueType::Array { ty: Arc::new(VagueType::Unknown), len }),
                    }),
                ));
            };

            let elements = single_exprs.val.iter().chain(spread_expr.as_ref());

            for el in elements {
                let el_type = eval_assign_expr(el, expected_el_type, ident_to_type, type_alias_m)?;
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

            let min_len = single_exprs.val.len() as u32;

            let len = match spread_expr {
                None => min_len,
                Some(_) => min_len.max(*expected_len),
            };

            Ok(Arc::new(l::Type::Array { ty: expected_el_type.clone(), len }))
        },
        l::AssignExpr::Struct(assign_fields) => {
            let l::Type::Struct(expected_fields) = expected_type.as_ref() else {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::Mismatch {
                        expected: vague(expected_type),
                        found: Arc::new(VagueType::Struct(None)),
                    }),
                ));
            };

            Ok(Arc::new(l::Type::Struct(Arc::new(
                expected_fields
                    .iter()
                    .map(|expected_field| {
                        let Some(assign_field) = assign_fields.val.iter().find(|assign_field| {
                            expected_field.name == assign_field.val.name.val.str
                        }) else {
                            return Err(CompileErrorSet::new_error(
                                assign_fields.range,
                                CompileError::Type(TypeError::StructLiteralMissingField {
                                    field_type: vague(&expected_field.ty),
                                    field_name: expected_field.name.clone(),
                                }),
                            ));
                        };

                        let assign_field_ty = eval_assign_expr(
                            &assign_field.val.assign,
                            &expected_field.ty,
                            ident_to_type,
                            type_alias_m,
                        )?;

                        if assign_field_ty.is_subtype_of(&expected_field.ty).not() {
                            return Err(CompileErrorSet::new_error(
                                assign_field.val.assign.range,
                                CompileError::Type(TypeError::Mismatch {
                                    expected: vague(&expected_field.ty),
                                    found: vague(&assign_field_ty),
                                }),
                            ));
                        }

                        Ok(Arc::new(l::FieldType {
                            name: expected_field.name.clone(),
                            ty: expected_field.ty.clone(),
                        }))
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            ))))
        },
    }
}

fn typecheck_expr(
    item: &l::Ref<l::Expr>,
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<t::Expr, CompileErrorSet> {
    Ok(match &item.val {
        l::Expr::Literal(x) => t::Expr::Literal(try_tr(x, |x| match &x.val {
            l::Literal::Val(x) => Ok(t::Literal::Val(x.clone())),
            l::Literal::Num(x) => Ok(t::Literal::Num(*x)),
            l::Literal::Int(x) => Ok(t::Literal::Int(*x)),
            l::Literal::Uint(x) => Ok(t::Literal::Uint(*x)),
            l::Literal::Bool(x) => Ok(t::Literal::Bool(*x)),
            l::Literal::Variant(x) => Ok(typecheck_variant_literal(&x, type_alias_m)?),
        })?),
        l::Expr::Place(x) => {
            t::Expr::Place(try_tr(x, |x| typecheck_place(x, ident_to_type, type_alias_m))?)
        },
        l::Expr::Ref(x) => {
            t::Expr::Ref(try_tr(x, |x| typecheck_place(x, ident_to_type, type_alias_m))?)
        },
        l::Expr::Paren(x) => {
            t::Expr::Paren(try_tr(x, |x| typecheck_paren_expr(x, ident_to_type, type_alias_m))?)
        },
        l::Expr::Cast { expr, .. } => typecheck_expr(expr, ident_to_type, type_alias_m)?,
        l::Expr::Transmute { expr, .. } => typecheck_expr(expr, ident_to_type, type_alias_m)?,
    })
}

fn eval_expr(
    item: &l::Ref<l::Expr>,
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    match &item.val {
        l::Expr::Literal(x) => Ok(match &x.val {
            l::Literal::Val(_) => VAL_TYPE.clone(),
            l::Literal::Num(_) => NUM_TYPE.clone(),
            l::Literal::Int(_) => INT_TYPE.clone(),
            l::Literal::Uint(_) => UINT_TYPE.clone(),
            l::Literal::Bool(_) => BOOL_TYPE.clone(),
            l::Literal::Variant(x) => {
                Arc::new(l::Type::Enum { name: x.val.enum_name.val.str.clone() })
            },
        }),
        l::Expr::Place(x) => eval_place(x, ident_to_type, type_alias_m).map(|res| res.ty),
        l::Expr::Ref(place) => {
            let place_type = eval_place(place, ident_to_type, type_alias_m)?;
            Ok(Arc::new(l::Type::Ref(place_type.ty)))
        },
        l::Expr::Paren(x) => eval_paren_expr(x, ident_to_type, type_alias_m),
        l::Expr::Cast { ty, expr } => {
            let cast_ty = Arc::new(type_alias_m.resolve(ty)?);
            let expr_ty = eval_expr(expr, ident_to_type, type_alias_m)?;
            if expr_ty.can_cast_to(&cast_ty).not() {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::InvalidCast {
                        from: vague(&expr_ty),
                        to: vague(&cast_ty),
                    }),
                ));
            }

            Ok(cast_ty.clone())
        },
        l::Expr::Transmute { ty, expr } => {
            let cast_ty = Arc::new(type_alias_m.resolve(ty)?);
            let expr_ty = eval_expr(expr, ident_to_type, type_alias_m)?;
            if expr_ty.can_transmute_to(&cast_ty).not() {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::InvalidTransmute {
                        from: vague(&expr_ty),
                        to: vague(&cast_ty),
                    }),
                ));
            }

            Ok(cast_ty.clone())
        },
    }
}

fn typecheck_variant_literal(
    item: &l::Ref<l::VariantLiteral>,
    type_alias_m: &TypeAliasManager,
) -> Result<t::Literal, CompileErrorSet> {
    let variant_id = type_alias_m.id_variant(&item.val.enum_name, &item.val.variant_name)?;
    Ok(t::Literal::Uint(f64::from(variant_id)))
}

fn typecheck_paren_expr(
    item: &l::Ref<l::ParenExpr>,
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<t::ParenExpr, CompileErrorSet> {
    Ok(match &item.val {
        l::ParenExpr::Unary(x) => t::ParenExpr::Unary(try_tr(x, |x| {
            typecheck_unary_expr(x, ident_to_type, type_alias_m)
        })?),
        l::ParenExpr::Binary(x) => t::ParenExpr::Binary(try_tr(x, |x| {
            typecheck_binary_expr(x, ident_to_type, type_alias_m)
        })?),
    })
}

fn eval_paren_expr(
    item: &l::Ref<l::ParenExpr>,
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    match &item.val {
        l::ParenExpr::Unary(x) => eval_unary_expr(x, ident_to_type, type_alias_m),
        l::ParenExpr::Binary(x) => eval_binary_expr(x, ident_to_type, type_alias_m),
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
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<t::UnaryParenExpr, CompileErrorSet> {
    Ok(t::UnaryParenExpr {
        operand: try_tr(&item.val.operand, |x| typecheck_expr(x, ident_to_type, type_alias_m))?,
        op: tr(&item.val.op, |x| typecheck_variants!(&x.val, UnaryParenExprOp => [Not])),
    })
}

fn eval_unary_expr(
    item: &l::Ref<l::UnaryParenExpr>,
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let operand = &item.val.operand;
    let operand_type = eval_expr(operand, ident_to_type, type_alias_m)?;
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
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<t::BinaryParenExpr, CompileErrorSet> {
    Ok(t::BinaryParenExpr {
        left: try_tr(&item.val.left, |x| typecheck_expr(x, ident_to_type, type_alias_m))?,
        right: try_tr(&item.val.right, |x| typecheck_expr(x, ident_to_type, type_alias_m))?,
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
    ident_to_type: &IdentToTypeHint,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let left_type = eval_expr(&item.val.left, ident_to_type, type_alias_m)?;
    let right_type = eval_expr(&item.val.right, ident_to_type, type_alias_m)?;

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
