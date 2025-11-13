use std::{
    collections::{HashMap, HashSet},
    ops::Not,
    sync::Arc,
};

use itertools::{Itertools, chain};
use uuid::Uuid;

use crate::high_compiler::{
    logic_ast::{
        BOOL_TYPE, INTEGER_TYPE, NUM_TYPE, STR_TYPE, Type, TypeHint, UINTEGER_TYPE, UNIT_TYPE,
        VAL_TYPE, bool_type_hint, unit_type_hint,
    },
    srced::{SrcRange, Srced},
    typecheck::ident_manager::IdentManager,
};

use super::{
    compile_error::{CompileError, CompileErrorSet, TypeError, VagueType},
    logic_ast as l, type_erased_ast as t,
};

struct FuncInfo {
    params: l::Ref<Vec<l::Ref<l::IdentDef>>>,
    return_ty: l::Ref<l::TypeHint>,
}

struct FuncManager {
    func_name_to_info: HashMap<Arc<str>, FuncInfo>,
}

impl FuncManager {
    pub fn new(program: &l::Ref<l::Program>) -> Self {
        let func_name_to_info = program
            .val
            .items
            .val
            .iter()
            .filter_map(|item| {
                if let l::TopItem::Exe(exe) = &item.val
                    && let l::ExeItem::Func(func) = &exe.val
                {
                    Some((
                        func.val.name.val.str.clone(),
                        FuncInfo {
                            params: func.val.params.clone(),
                            return_ty: func.val.return_ty.clone(),
                        },
                    ))
                } else {
                    None
                }
            })
            .collect::<HashMap<_, _>>();

        Self { func_name_to_info }
    }

    pub fn info(&self, func_name: &l::Ref<l::Name>) -> Result<&FuncInfo, CompileErrorSet> {
        match self.func_name_to_info.get(&func_name.val.str) {
            Some(info) => Ok(info),
            None => Err(CompileErrorSet::new_error(
                func_name.range,
                CompileError::Type(TypeError::FuncNotFound { name: func_name.val.str.clone() }),
            )),
        }
    }

    pub fn params(
        &self,
        func_name: &l::Ref<l::Name>,
    ) -> Result<&l::Ref<Vec<l::Ref<l::IdentDef>>>, CompileErrorSet> {
        Ok(&self.info(func_name)?.params)
    }

    pub fn return_ty(
        &self,
        func_name: &l::Ref<l::Name>,
    ) -> Result<&l::Ref<l::TypeHint>, CompileErrorSet> {
        Ok(&self.info(func_name)?.return_ty)
    }
}

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

fn sift_stmt_deps<T>(stmt_deps: &mut Vec<t::Ref<t::Statement>>, dependent: StmtDependent<T>) -> T {
    let StmtDependent { stmt_deps: these_deps, value } = dependent;
    stmt_deps.extend(these_deps);
    value
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
            l::TypeHint::Nominal(name) => match name.val.str.as_ref() {
                Type::ANY_NAME => l::Type::Any,
                Type::VAL_NAME => l::Type::Primitive(l::PrimitiveType::Val),
                Type::STR_NAME => l::Type::Primitive(l::PrimitiveType::Str),
                Type::NUM_NAME => l::Type::Primitive(l::PrimitiveType::Num),
                Type::INTEGER_NAME => l::Type::Primitive(l::PrimitiveType::Int),
                Type::UINTEGER_NAME => l::Type::Primitive(l::PrimitiveType::Uint),
                Type::BOOL_NAME => l::Type::Primitive(l::PrimitiveType::Bool),
                _ => {
                    if self.enum_to_variants.contains_key(&name.val.str) {
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
            },
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
        enum_name: &EnumName,
        variant_name: &l::Ref<l::Name>,
        variant_literal_range: SrcRange,
    ) -> Result<u32, CompileErrorSet> {
        let variants = self.all_variants(enum_name, variant_literal_range)?;

        let Some((id, _)) = variants.iter().enumerate().find(|(_, v)| *v == &variant_name.val.str)
        else {
            return Err(CompileErrorSet::new_error(
                variant_name.range,
                CompileError::Type(TypeError::UnknownVariant {
                    enum_name: enum_name.str().clone(),
                    variant_name: variant_name.val.str.clone(),
                }),
            ));
        };

        Ok(id as u32)
    }

    pub fn all_variants(
        &self,
        enum_name: &EnumName,
        variant_literal_range: SrcRange,
    ) -> Result<&Arc<Vec<Arc<str>>>, CompileErrorSet> {
        let enum_name_range = match enum_name {
            EnumName::Name(name) => name.range,
            EnumName::Str(_) => variant_literal_range,
        };

        let Some(variants) = self.enum_to_variants.get(enum_name.str()) else {
            return Err(CompileErrorSet::new_error(
                enum_name_range,
                CompileError::Type(TypeError::UnknownEnum { name: enum_name.str().clone() }),
            ));
        };

        Ok(variants)
    }
}

enum EnumName {
    Name(l::Ref<l::Name>),
    Str(Arc<str>),
}

impl EnumName {
    pub fn str(&self) -> &Arc<str> {
        match self {
            Self::Name(name) => &name.val.str,
            Self::Str(str) => str,
        }
    }
}

struct StaticItems {
    items: Vec<l::Ref<l::Static>>,
}

pub fn typecheck(program: &l::Ref<l::Program>) -> Result<t::Ref<t::Program>, CompileErrorSet> {
    let func_m = FuncManager::new(program);
    let type_alias_m = TypeAliasManager::new(program);
    let mut ident_m = IdentManager::new(program);

    let static_items = StaticItems {
        items: program
            .val
            .items
            .val
            .iter()
            .filter_map(|item| {
                if let l::TopItem::Exe(item) = &item.val
                    && let l::ExeItem::Static(item) = &item.val
                {
                    Some(item.clone())
                } else {
                    None
                }
            })
            .collect(),
    };

    let items = Arc::new(
        program
            .val
            .items
            .val
            .iter()
            .map(|x| {
                try_tr(x, |x| {
                    typecheck_top_item(x, &mut ident_m, &func_m, &type_alias_m, &static_items)
                })
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .filter_map(|x| x.val.clone())
            .collect_vec(),
    );

    let static_defs = Arc::new(
        program
            .val
            .items
            .val
            .iter()
            .filter_map(|x| {
                if let l::TopItem::Exe(x) = &x.val
                    && let l::ExeItem::Static(x) = &x.val
                {
                    Some(x)
                } else {
                    None
                }
            })
            .map(|x| {
                try_tr(x, |x| {
                    let info = ident_m.get(&x.val.ident_init.val.def.val.ident.val)?;
                    let ty = type_alias_m.resolve(&info.def.val.ty)?;
                    Ok(t::IdentDef {
                        name: tr(&x.val.ident_init.val.def.val.ident, |x| {
                            t::Name::User(tr(x, |_| t::UserName { str: info.unique_name.clone() }))
                        }),
                        size: ty.size(),
                    })
                })
            })
            .collect::<Result<Vec<_>, _>>()?,
    );

    Ok(Arc::new(Srced { range: program.range, val: t::Program { items, statics: static_defs } }))
}

fn typecheck_top_item(
    item: &l::Ref<l::TopItem>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
    static_items: &StaticItems,
) -> Result<Option<t::Ref<t::ExeItem>>, CompileErrorSet> {
    Ok(match &item.val {
        l::TopItem::Exe(x) => {
            match typecheck_exe_item(x, ident_m, func_m, type_alias_m, static_items)? {
                Some(item) => Some(tr(x, |_| item)),
                None => None,
            }
        },
        l::TopItem::Type(x) => {
            typecheck_type_item(x);
            None
        },
    })
}

fn typecheck_exe_item(
    item: &l::Ref<l::ExeItem>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
    static_items: &StaticItems,
) -> Result<Option<t::ExeItem>, CompileErrorSet> {
    Ok(match &item.val {
        l::ExeItem::Main(x) => Some(t::ExeItem::Main(try_tr(x, |x| {
            typecheck_main(x, ident_m, func_m, type_alias_m, static_items)
        })?)),
        l::ExeItem::Func(x) => {
            Some(t::ExeItem::Func(try_tr(x, |x| typecheck_func(x, ident_m, func_m, type_alias_m))?))
        },
        l::ExeItem::Static(_) => None, // statics are scanned & registered earlier and then will be inserted into the main function
    })
}

fn typecheck_type_item(_item: &l::Ref<l::TypeItem>) {}

fn typecheck_main(
    item: &l::Ref<l::Main>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
    static_items: &StaticItems,
) -> Result<t::Main, CompileErrorSet> {
    ident_m.layer(|ident_m| {
        Ok(t::Main {
            proc: try_tr(&item.val.body, |x| {
                typecheck_proc(
                    x,
                    &l::UNIT_TYPE,
                    ProcCompilation::Main { static_items },
                    ident_m,
                    func_m,
                    type_alias_m,
                )
            })?,
        })
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
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<t::Func, CompileErrorSet> {
    ident_m.layer(|ident_m| {
        for param in item.val.params.val.iter() {
            ident_m.reg_let(param.clone());
        }

        let return_ty = Arc::new(type_alias_m.resolve(&item.val.return_ty)?);
        let return_param_ty = Type::Ref(return_ty.clone());

        let user_params_defs =
            try_tr_vec(&item.val.params, |x| typecheck_ident_def(x, ident_m, type_alias_m))?;

        let return_param_def = Arc::new(Srced {
            range: item.val.return_ty.range,
            val: t::IdentDef {
                name: Arc::new(Srced {
                    range: item.val.return_ty.range,
                    val: t::Name::Internal(Arc::new(Srced {
                        range: item.val.return_ty.range,
                        val: t::InternalName::Return,
                    })),
                }),
                size: return_param_ty.size(),
            },
        });

        let params = Arc::new(Srced {
            range: user_params_defs.range,
            val: chain!([return_param_def], user_params_defs.val.iter().cloned()).collect(),
        });

        let func_name = tr(&item.val.name, typecheck_name);

        let t_proc = typecheck_proc(
            &item.val.body,
            &return_ty,
            ProcCompilation::Func,
            ident_m,
            func_m,
            type_alias_m,
        )?;

        Ok(t::Func {
            name: Arc::new(Srced { range: func_name.range, val: t::Name::User(func_name) }),
            params,
            proc: tr(&item.val.body, |_| t_proc),
        })
    })
}

fn typecheck_name(item: &l::Ref<l::Name>) -> t::UserName {
    t::UserName { str: item.val.str.clone() }
}

fn typecheck_ident_def(
    item: &l::Ref<l::IdentDef>,
    ident_m: &IdentManager,
    type_alias_m: &TypeAliasManager,
) -> Result<t::IdentDef, CompileErrorSet> {
    let info = ident_m.get(&item.val.ident.val)?;

    Ok(t::IdentDef {
        name: tr(item.val.ident.val.name(), |x| {
            t::Name::User(tr(x, |_| t::UserName { str: info.unique_name.clone() }))
        }),
        size: type_alias_m.resolve(&item.val.ty)?.size(),
    })
}

mod ident_manager {
    use crate::{
        high_compiler::compile_error::{CompileError, CompileErrorSet, TypeError},
        logic_ast as l,
    };
    use std::{collections::HashMap, mem, sync::Arc};

    use itertools::chain;
    use uuid::Uuid;

    pub struct IdentInfo {
        pub def: l::Ref<l::IdentDef>,
        pub unique_name: Arc<str>,
    }

    #[derive(PartialEq, Eq, Hash)]
    enum Ident {
        User { name: Arc<str> },
        Internal { name: Arc<str>, uuid: Uuid },
    }

    type IdentToInfo = HashMap<Arc<Ident>, Arc<IdentInfo>>;

    #[derive(Default)]
    struct IdentLayer {
        infos: Vec<Arc<IdentInfo>>,
        name_to_count: HashMap<Arc<str>, usize>,
        prev_ident_to_info_scopes: Vec<IdentToInfo>,
        curr_ident_to_info_scope: IdentToInfo,
    }

    #[derive(Default)]
    pub struct IdentManager {
        static_layer: IdentLayer,
        prev_let_layers: Vec<IdentLayer>,
        curr_let_layer: IdentLayer,
    }

    impl IdentLayer {
        pub fn reg(&mut self, def: l::Ref<l::IdentDef>, unique_name: Arc<str>) -> Arc<IdentInfo> {
            let m_ident = match &def.val.ident.val {
                l::Ident::User { name } => Ident::User { name: name.val.str.clone() },
                l::Ident::Internal { name, uuid } => {
                    Ident::Internal { name: name.val.str.clone(), uuid: *uuid }
                },
            };

            let ident_info = Arc::new(IdentInfo { def, unique_name });

            self.curr_ident_to_info_scope.insert(Arc::new(m_ident), ident_info.clone());
            self.infos.push(ident_info.clone());
            ident_info
        }

        pub fn get(&self, name: &Ident) -> Option<&Arc<IdentInfo>> {
            let scopes =
                chain!(self.prev_ident_to_info_scopes.iter(), [&self.curr_ident_to_info_scope]);

            for scope in scopes.rev() {
                if let Some(info) = scope.get(name) {
                    return Some(info);
                }
            }

            None
        }
    }

    impl IdentManager {
        pub fn new(program: &l::Ref<l::Program>) -> Self {
            let mut ident_m = Self::default();

            for item in program.val.items.val.iter() {
                if let l::TopItem::Exe(exe) = &item.val
                    && let l::ExeItem::Static(x) = &exe.val
                {
                    ident_m.reg_static(x.val.ident_init.val.def.clone());
                }
            }

            ident_m
        }

        pub fn layer<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
            self.prev_let_layers.push(mem::take(&mut self.curr_let_layer));

            let res = f(self);

            self.curr_let_layer = self.prev_let_layers.pop().unwrap_or_default();
            res
        }

        pub fn scope<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
            let layer = &mut self.curr_let_layer;
            layer.prev_ident_to_info_scopes.push(mem::take(&mut layer.curr_ident_to_info_scope));

            let res = f(self);

            let layer = &mut self.curr_let_layer;
            layer.curr_ident_to_info_scope =
                layer.prev_ident_to_info_scopes.pop().unwrap_or_default();

            res
        }

        pub fn reg_static(&mut self, def: l::Ref<l::IdentDef>) -> Arc<IdentInfo> {
            let ident_name = &def.val.ident.val.name().val.str;
            let name_count = self.static_layer.name_to_count.entry(ident_name.clone()).or_default();

            let unique_name = format!("{ident_name}$static${name_count}", name_count = *name_count);

            *name_count += 1;

            self.static_layer.reg(def, unique_name.into())
        }

        pub fn reg_let(&mut self, def: l::Ref<l::IdentDef>) -> Arc<IdentInfo> {
            let ident_name = def.val.ident.val.name().val.str.clone();
            let name_count = self
                .let_layers()
                .map(|layer| layer.name_to_count.get(&ident_name).copied().unwrap_or_default())
                .sum::<usize>();

            let unique_name = format!("{ident_name}$let${name_count}");

            let name_count = self.curr_let_layer.name_to_count.entry(ident_name).or_default();
            *name_count += 1;

            self.curr_let_layer.reg(def, unique_name.into())
        }

        pub fn get(&self, ident: &l::Ident) -> Result<&Arc<IdentInfo>, CompileErrorSet> {
            self.get_from_layers(ident, self.layers())
        }

        pub fn get_static(&self, ident: &l::Ident) -> Result<&Arc<IdentInfo>, CompileErrorSet> {
            self.get_from_layers(ident, [&self.static_layer].into_iter())
        }

        fn get_from_layers<'a>(
            &self,
            ident: &l::Ident,
            layers: impl DoubleEndedIterator<Item = &'a IdentLayer>,
        ) -> Result<&'a Arc<IdentInfo>, CompileErrorSet> {
            let m_ident = match ident {
                l::Ident::User { name } => Ident::User { name: name.val.str.clone() },
                l::Ident::Internal { name, uuid } => {
                    Ident::Internal { name: name.val.str.clone(), uuid: *uuid }
                },
            };

            for layer in layers.rev() {
                if let Some(info) = layer.get(&m_ident) {
                    return Ok(info);
                }
            }

            Err(CompileErrorSet::new_error(
                ident.name().range,
                CompileError::Type(TypeError::IdentNotFound { name: ident.name().val.str.clone() }),
            ))
        }

        fn let_layers(&self) -> impl DoubleEndedIterator<Item = &IdentLayer> {
            chain!(self.prev_let_layers.iter(), [&self.curr_let_layer])
        }

        fn layers(&self) -> impl DoubleEndedIterator<Item = &IdentLayer> {
            chain!([&self.static_layer], self.prev_let_layers.iter(), [&self.curr_let_layer])
        }

        pub fn layer_infos(&self) -> &Vec<Arc<IdentInfo>> {
            &self.curr_let_layer.infos
        }
    }
}

enum ProcCompilation<'a> {
    Main { static_items: &'a StaticItems },
    Func,
}

fn typecheck_proc(
    body: &l::Ref<l::Expr>,
    return_ty: &Arc<l::Type>,
    compilation: ProcCompilation,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<t::Proc, CompileErrorSet> {
    ident_m.layer(|ident_m| {
        let mut stmt_deps = Vec::new();

        // set up all static memory
        if let ProcCompilation::Main { static_items } = compilation {
            for static_item in static_items.items.iter() {
                let ident_init = &static_item.val.ident_init;

                if ident_init.val.expr.val.is_const().not() {
                    return Err(CompileErrorSet::new_error(
                        ident_init.val.expr.range,
                        CompileError::Type(TypeError::ExpectedConstExpr),
                    ));
                }

                let assign = sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_var_init(VarInit::Static(ident_init), ident_m, func_m, type_alias_m)?,
                );

                stmt_deps.push(tr(static_item, |x| t::Statement::Assign(tr(x, |_| assign))));
            }
        }

        let body_expr = try_tr(body, |x| {
            Ok(sift_stmt_deps(
                &mut stmt_deps,
                typecheck_expr(x, Some(return_ty), ident_m, func_m, type_alias_m)?,
            ))
        })?;

        if let ProcCompilation::Func = compilation {
            let return_value = tr(body, |x| {
                t::Statement::Assign(tr(x, |x| t::Assign {
                    place: tr(x, |x| t::Place {
                        head: tr(x, |x| {
                            t::PlaceHead::Deref(tr(x, |x| t::Deref {
                                addr: tr(x, |x| {
                                    t::Expr::Place(tr(x, |x| t::Place {
                                        head: tr(x, |x| {
                                            t::PlaceHead::Ident(tr(x, |x| t::Ident {
                                                name: tr(x, |x| {
                                                    t::Name::Internal(tr(x, |_| {
                                                        t::InternalName::Return
                                                    }))
                                                }),
                                            }))
                                        }),
                                        offset: None,
                                    }))
                                }),
                            }))
                        }),
                        offset: None,
                    }),
                    exprs: body_expr,
                }))
            });

            stmt_deps.push(return_value);
        }

        let body = tr(body, |x| t::Body { items: tr(x, |_| stmt_deps) });

        Ok(t::Proc {
            body,
            idents: Arc::new(
                ident_m
                    .layer_infos()
                    .iter()
                    .map(|info| {
                        try_tr(&info.def, |x| {
                            Ok(t::IdentDef {
                                name: tr(x.val.ident.val.name(), |x| {
                                    t::Name::User(tr(x, |_| t::UserName {
                                        str: info.unique_name.clone(),
                                    }))
                                }),
                                size: type_alias_m.resolve(&x.val.ty)?.size(),
                            })
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            ),
        })
    })
}

fn typecheck_block(
    item: &l::Ref<l::Block>,
    expected_type: Option<&Arc<l::Type>>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<Vec<t::Ref<t::AssignExpr>>>, CompileErrorSet> {
    ident_m.scope(|ident_m| {
        let mut final_expr = None;
        let mut stmt_deps = Vec::new();

        let trail = &item.val.items;

        let mut l_items = trail.val.items.val.iter().peekable();
        while let Some(l_item) = l_items.next() {
            let is_last = l_items.peek().is_none();
            let expected_item_ty = if is_last && trail.val.trailing.not() {
                expected_type
            } else {
                None
            };

            let expr = typecheck_expr(l_item, expected_item_ty, ident_m, func_m, type_alias_m)?;

            stmt_deps.extend(expr.stmt_deps);
            final_expr = Some(expr.value);
        }

        if trail.val.trailing {
            final_expr = None;
        }

        Ok(StmtDependent { stmt_deps, value: final_expr.unwrap_or_default() })
    })
}

fn typecheck_statement(
    item: &l::Ref<l::Statement>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<Vec<t::Ref<t::AssignExpr>>>, CompileErrorSet> {
    let mut stmt_deps = Vec::new();

    let body_items = match &item.val {
        l::Statement::Let(x) => {
            let stmt = t::Statement::Assign(try_tr(x, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_var_init(
                        VarInit::Let(&x.val.ident_init),
                        ident_m,
                        func_m,
                        type_alias_m,
                    )?,
                ))
            })?);

            Vec::from([tr(x, |_| stmt)])
        },
        l::Statement::Assign(x) => {
            let stmt = t::Statement::Assign(try_tr(x, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_assign(x, ident_m, func_m, type_alias_m)?,
                ))
            })?);

            Vec::from([tr(x, |_| stmt)])
        },
        l::Statement::Native(x) => {
            let stmt = t::Statement::Native(try_tr(x, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_native_op(x, ident_m, func_m, type_alias_m)?,
                ))
            })?);

            Vec::from([tr(x, |_| stmt)])
        },
    };

    let stmt_deps = chain!(stmt_deps.into_iter(), body_items).collect();

    Ok(StmtDependent { stmt_deps, value: Default::default() })
}

fn typecheck_if(
    item: &l::Ref<l::IfItem>,
    expected_type: Option<&Arc<l::Type>>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<Vec<t::Ref<t::AssignExpr>>>, CompileErrorSet> {
    let condition_type = eval_expr(&item.val.condition, None, ident_m, func_m, type_alias_m)?;
    if condition_type.is_subtype_of(&BOOL_TYPE).not() {
        return Err(CompileErrorSet::new_error(
            item.val.condition.range,
            CompileError::Type(TypeError::Mismatch {
                expected_any_of: Arc::new(Vec::from([vague(&BOOL_TYPE)])),
                found: vague(&condition_type),
            }),
        ));
    };

    let mut stmt_deps = Vec::new();

    let expr_ident = tr(item, |x| l::Ident::Internal {
        name: tr(x, |_| l::Name { str: "if".into() }),
        uuid: Uuid::new_v4(),
    });

    let expr_ident_ty = eval_if(item, expected_type, ident_m, func_m, type_alias_m)?;

    let expr_ident_info = ident_m.reg_let(tr(item, |x| l::IdentDef {
        ident: expr_ident.clone(),
        ty: tr(x, |x| compute_expr_ty_hint(Some(&expr_ident_ty), x.range)),
    }));

    let if_item = t::IfItem {
        condition: {
            let exprs = try_tr(&item.val.condition, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                ))
            })?;

            typecheck_to_t_expr(exprs)?
        },
        then_body: {
            let body =
                typecheck_expr(&item.val.then_body, expected_type, ident_m, func_m, type_alias_m)?;

            let ident_assignment = tr(&item.val.then_body, |x| {
                t::Statement::Assign(tr(x, |x| t::Assign {
                    place: tr(x, |x| t::Place {
                        offset: None,
                        head: tr(x, |x| {
                            t::PlaceHead::Ident(tr(x, |x| t::Ident {
                                name: tr(x, |x| {
                                    t::Name::User(tr(x, |_| t::UserName {
                                        str: expr_ident_info.unique_name.clone(),
                                    }))
                                }),
                            }))
                        }),
                    }),
                    exprs: tr(x, |_| body.value),
                }))
            });

            tr(&item.val.then_body, |x| t::Body {
                items: tr(x, |_| chain!(body.stmt_deps, [ident_assignment]).collect()),
            })
        },
        else_item: match &item.val.else_item {
            None => None,
            Some(x) => Some(try_tr(x, |x| {
                Ok(t::ElseItem {
                    body: {
                        let body = typecheck_expr(
                            &x.val.body,
                            expected_type,
                            ident_m,
                            func_m,
                            type_alias_m,
                        )?;

                        let ident_assignment = tr(&item.val.then_body, |x| {
                            t::Statement::Assign(tr(x, |x| t::Assign {
                                place: tr(x, |x| t::Place {
                                    offset: None,
                                    head: tr(x, |x| {
                                        t::PlaceHead::Ident(tr(x, |x| t::Ident {
                                            name: tr(x, |x| {
                                                t::Name::User(tr(x, |_| t::UserName {
                                                    str: expr_ident_info.unique_name.clone(),
                                                }))
                                            }),
                                        }))
                                    }),
                                }),
                                exprs: tr(x, |_| body.value),
                            }))
                        });

                        tr(&item.val.then_body, |x| t::Body {
                            items: tr(x, |_| chain!(body.stmt_deps, [ident_assignment]).collect()),
                        })
                    },
                })
            })?),
        },
    };

    let if_ident_expr = tr(&expr_ident, |x| {
        l::Expr::Place(tr(x, |x| l::Place {
            head: tr(x, |x| l::PlaceHead::Ident(x.clone())),
            index_chain: tr(x, |_| Default::default()),
        }))
    });

    let mut if_ident_expr_stmt_deps = Vec::new();
    let if_assign_exprs = sift_stmt_deps(
        &mut if_ident_expr_stmt_deps,
        typecheck_expr(&if_ident_expr, expected_type, ident_m, func_m, type_alias_m)?,
    );

    let if_item = tr(item, |x| t::Statement::If(tr(x, |_| if_item)));
    let stmt_deps = chain!(stmt_deps, [if_item], if_ident_expr_stmt_deps).collect();

    Ok(StmtDependent { stmt_deps, value: if_assign_exprs })
}

fn typecheck_while(
    item: &l::Ref<l::WhileItem>,
    expected_type: Option<&Arc<l::Type>>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<Vec<t::Ref<t::AssignExpr>>>, CompileErrorSet> {
    let condition_type = eval_expr(&item.val.condition, None, ident_m, func_m, type_alias_m)?;
    if condition_type.is_subtype_of(&BOOL_TYPE).not() {
        return Err(CompileErrorSet::new_error(
            item.val.condition.range,
            CompileError::Type(TypeError::Mismatch {
                expected_any_of: Arc::new(Vec::from([vague(&BOOL_TYPE)])),
                found: vague(&condition_type),
            }),
        ));
    }

    // the condition expr of t::WhileItem will be "constant", as in it will just be the final expr
    // so we need to add code to rerun the condition after each loop so that
    // it works how you would expect
    let condition_ident = tr(&item.val.condition, |x| l::Ident::Internal {
        name: tr(x, |_| l::Name { str: "condition".into() }),
        uuid: Uuid::new_v4(),
    });

    let _condition_ident_info = ident_m.reg_let(tr(item, |x| l::IdentDef {
        ident: condition_ident.clone(),
        ty: tr(x, |x| bool_type_hint(x.range)),
    }));

    let mut stmt_deps = Vec::new();

    // let exprs = try_tr(&item.val.condition, |x| {
    //     Ok(sift_stmt_deps(&mut stmt_deps, typecheck_expr(x, None, ident_m, func_m, type_alias_m)?))
    // })?;

    // let condition_expr = typecheck_to_t_expr(exprs)?;

    let define_condition_stmt = tr(&item.val.condition, |x| {
        l::Expr::Statement(tr(x, |x| {
            l::Statement::Let(tr(x, |x| l::Let {
                ident_init: tr(x, |x| l::IdentInit {
                    def: tr(x, |x| l::IdentDef {
                        ident: condition_ident.clone(),
                        ty: tr(x, |x| bool_type_hint(x.range)),
                    }),
                    expr: item.val.condition.clone(),
                }),
            }))
        }))
    });

    sift_stmt_deps(
        &mut stmt_deps,
        typecheck_expr(&define_condition_stmt, Some(&UNIT_TYPE), ident_m, func_m, type_alias_m)?,
    );

    let update_condition_stmt = tr(&item.val.condition, |x| {
        l::Expr::Statement(tr(x, |x| {
            l::Statement::Assign(tr(x, |x| l::Assign {
                place: tr(x, |x| l::Place {
                    head: tr(x, |_| l::PlaceHead::Ident(condition_ident.clone())),
                    index_chain: tr(x, |_| Vec::new()),
                }),
                expr: item.val.condition.clone(),
            }))
        }))
    });

    let while_item = t::WhileItem {
        condition: {
            let exprs = try_tr(&item.val.condition, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_expr(
                        &tr(x, |x| {
                            l::Expr::Place(tr(x, |x| l::Place {
                                head: tr(x, |_| l::PlaceHead::Ident(condition_ident.clone())),
                                index_chain: tr(x, |_| Vec::new()),
                            }))
                        }),
                        Some(&BOOL_TYPE),
                        ident_m,
                        func_m,
                        type_alias_m,
                    )?,
                ))
            })?;

            typecheck_to_t_expr(exprs)?
        },
        body: {
            let body_exprs =
                typecheck_expr(&item.val.body, expected_type, ident_m, func_m, type_alias_m)?;

            let loop_exprs =
                typecheck_expr(&update_condition_stmt, None, ident_m, func_m, type_alias_m)?;

            tr(&item.val.body, |x| t::Body {
                items: tr(x, |_| chain!(body_exprs.stmt_deps, loop_exprs.stmt_deps).collect()),
            })
        },
    };

    let while_item = tr(item, |x| t::Statement::While(tr(x, |_| while_item)));
    let stmt_deps = chain!(stmt_deps, [while_item]).collect();

    Ok(StmtDependent { stmt_deps, value: Default::default() })
}

fn typecheck_match(
    item: &l::Ref<l::MatchItem>,
    expected_type: Option<&Arc<l::Type>>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<Vec<t::Ref<t::AssignExpr>>>, CompileErrorSet> {
    let mut stmt_deps = Vec::new();

    let expr_ident = tr(item, |x| l::Ident::Internal {
        name: tr(x, |_| l::Name { str: "match".into() }),
        uuid: Uuid::new_v4(),
    });

    let expr_ident_ty = eval_match(item, expected_type, ident_m, func_m, type_alias_m)?;

    let expr_ident_info = ident_m.reg_let(tr(item, |x| l::IdentDef {
        ident: expr_ident.clone(),
        ty: tr(x, |x| compute_expr_ty_hint(Some(&expr_ident_ty), x.range)),
    }));

    let expr_ty = eval_expr(&item.val.expr, None, ident_m, func_m, type_alias_m)?;
    let l::Type::Enum { name: enum_name } = expr_ty.as_ref() else {
        return Err(CompileErrorSet::new_error(
            item.val.expr.range,
            CompileError::Type(TypeError::Mismatch {
                expected_any_of: Arc::new(Vec::from([Arc::new(VagueType::Enum { name: None })])),
                found: vague(&expr_ty),
            }),
        ));
    };

    let all_variants =
        type_alias_m.all_variants(&EnumName::Str(enum_name.clone()), item.val.expr.range)?;

    let mut unmatched_variants = all_variants.iter().collect::<HashSet<_>>();

    struct CaseBranch {
        condition: t::Ref<t::Expr>,
        body: t::Ref<t::Body>,
    }

    let mut branches = Vec::new();

    for case in item.val.cases.val.iter() {
        let variant = &case.val.variant;

        if let Some(lit_enum_name) = &variant.val.enum_name
            && &lit_enum_name.val.str != enum_name
        {
            return Err(CompileErrorSet::new_error(
                variant.range,
                CompileError::Type(TypeError::Mismatch {
                    expected_any_of: Arc::new(Vec::from([Arc::new(VagueType::Enum {
                        name: Some(enum_name.clone()),
                    })])),
                    found: Arc::new(VagueType::Enum { name: Some(lit_enum_name.val.str.clone()) }),
                }),
            ));
        }

        if unmatched_variants.remove(&variant.val.variant_name.val.str).not() {
            return Err(CompileErrorSet::new_error(
                variant.range,
                CompileError::Type(TypeError::UnmatchableCase),
            ));
        }

        let case_condition = try_tr(variant, |v| {
            Ok(t::Expr::Paren(try_tr(v, |v| {
                Ok(t::ParenExpr::Binary(try_tr(v, |v| {
                    Ok(t::BinaryParenExpr {
                        left: {
                            let exprs = try_tr(&item.val.expr, |expr| {
                                let exprs = sift_stmt_deps(
                                    &mut stmt_deps,
                                    typecheck_expr(expr, None, ident_m, func_m, type_alias_m)?,
                                );

                                Ok(exprs)
                            })?;

                            typecheck_to_t_expr(exprs)?
                        },
                        op: tr(v, |_| t::BinaryParenExprOp::Eq),
                        right: try_tr(v, |v| {
                            Ok(t::Expr::Literal(try_tr(v, |v| {
                                typecheck_variant_literal(
                                    &match &v.val.enum_name {
                                        Some(name) => EnumName::Name(name.clone()),
                                        None => EnumName::Str(enum_name.clone()),
                                    },
                                    &v.val.variant_name,
                                    v.range,
                                    type_alias_m,
                                )
                            })?))
                        })?,
                    })
                })?))
            })?))
        })?;

        let mut case_body_stmt_deps = Vec::new();
        let case_body_exprs = try_tr(&case.val.body, |x| {
            Ok(sift_stmt_deps(
                &mut case_body_stmt_deps,
                typecheck_expr(x, expected_type, ident_m, func_m, type_alias_m)?,
            ))
        })?;

        let case_body = tr(&case.val.body, |x| t::Body {
            items: tr(x, |x| {
                let expr_assignment = tr(x, |x| {
                    t::Statement::Assign(tr(x, |x| t::Assign {
                        place: tr(x, |x| t::Place {
                            head: tr(x, |x| {
                                t::PlaceHead::Ident(tr(x, |x| t::Ident {
                                    name: tr(x, |x| {
                                        t::Name::User(tr(x, |_| t::UserName {
                                            str: expr_ident_info.unique_name.clone(),
                                        }))
                                    }),
                                }))
                            }),
                            offset: None,
                        }),
                        exprs: case_body_exprs,
                    }))
                });

                // let expr_assignment = l::Assign {
                //     place: tr(x, |x| l::Place {
                //         head: tr(x, |x| l::PlaceHead::Ident(expr_ident.clone())),
                //         index_chain: tr(x, |_| Default::default()),
                //     }),
                //     expr:
                // };

                chain!(case_body_stmt_deps, [expr_assignment]).collect()
            }),
        });

        branches.push(Arc::new(Srced {
            range: case.range,
            val: CaseBranch { condition: case_condition, body: case_body },
        }));
    }

    // TODO: make this do a binary search instead to save on the number of checks?
    let mut if_item: Option<t::Ref<t::IfItem>> = None;

    for branch in branches.into_iter().rev() {
        if_item = Some(try_tr(&branch, |branch| {
            Ok(t::IfItem {
                condition: branch.val.condition.clone(),
                then_body: branch.val.body.clone(),
                else_item: match if_item {
                    None => None,
                    Some(if_item) => Some(tr(&if_item, |x| t::ElseItem {
                        body: tr(x, |x| t::Body {
                            items: tr(x, |x| Vec::from([tr(x, |x| t::Statement::If(x.clone()))])),
                        }),
                    })),
                },
            })
        })?);
    }

    if unmatched_variants.is_empty().not() {
        let unmatched_variants =
            all_variants.iter().filter(|v| unmatched_variants.contains(v)).cloned().collect_vec();

        return Err(CompileErrorSet::new_error(
            item.range,
            CompileError::Type(TypeError::NonExhaustiveMatch {
                unmatched_cases: Arc::new(unmatched_variants),
            }),
        ));
    }

    let match_expr = tr(&expr_ident, |x| {
        l::Expr::Place(tr(x, |x| l::Place {
            head: tr(x, |x| l::PlaceHead::Ident(x.clone())),
            index_chain: tr(x, |_| Default::default()),
        }))
    });

    let mut match_expr_stmt_deps = Vec::new();
    let match_assign_exprs = sift_stmt_deps(
        &mut match_expr_stmt_deps,
        typecheck_expr(&match_expr, expected_type, ident_m, func_m, type_alias_m)?,
    );

    let stmt_deps = chain!(
        stmt_deps,
        if_item.map(|i| tr(&i, |i| t::Statement::If(i.clone()))),
        match_expr_stmt_deps
    )
    .collect();

    Ok(StmtDependent { stmt_deps, value: match_assign_exprs })
}

#[derive(Clone, Copy)]
enum VarInit<'a> {
    Let(&'a l::Ref<l::IdentInit>),
    Static(&'a l::Ref<l::IdentInit>),
}

fn typecheck_var_init(
    item: VarInit,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<t::Assign>, CompileErrorSet> {
    let (ident_init, info) = match item {
        VarInit::Let(item) => (item, ident_m.reg_let(item.val.def.clone())),
        VarInit::Static(item) => (item, ident_m.get_static(&item.val.def.val.ident.val)?.clone()),
    };

    let ident_ty = Arc::new(type_alias_m.resolve(&ident_init.val.def.val.ty)?);
    let expr_ty = eval_expr(&ident_init.val.expr, Some(&ident_ty), ident_m, func_m, type_alias_m)?;

    if expr_ty.is_subtype_of(&ident_ty).not() {
        return Err(CompileErrorSet::new_error(
            ident_init.range,
            CompileError::Type(TypeError::Mismatch {
                expected_any_of: Arc::new(Vec::from([vague(&ident_ty)])),
                found: vague(&expr_ty),
            }),
        ));
    }

    let mut stmt_deps = Vec::new();

    let assign = t::Assign {
        place: tr(&ident_init.val.def, |x| t::Place {
            head: tr(x, |x| {
                t::PlaceHead::Ident(tr(x.val.ident.val.name(), |x| t::Ident {
                    name: tr(x, |x| {
                        t::Name::User(tr(x, |_| t::UserName { str: info.unique_name.clone() }))
                    }),
                }))
            }),
            offset: None,
        }),
        exprs: try_tr(&ident_init.val.expr, |x| {
            Ok(sift_stmt_deps(
                &mut stmt_deps,
                typecheck_expr(x, Some(&ident_ty), ident_m, func_m, type_alias_m)?,
            )
            .into_iter()
            .collect())
        })?,
    };

    Ok(StmtDependent { stmt_deps, value: assign })
}

fn typecheck_native_op(
    item: &l::Ref<l::NativeOperation>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<t::NativeOperation>, CompileErrorSet> {
    let mut stmt_deps = Vec::new();

    let native_op = match &item.val {
        l::NativeOperation::Out { val } => t::NativeOperation::Out {
            val: {
                let exprs = try_tr(val, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                    ))
                })?;

                typecheck_to_t_expr(exprs)?
            },
        },
        l::NativeOperation::In { dest_place } => t::NativeOperation::In {
            dest_place: try_tr(dest_place, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_place(x, ident_m, func_m, type_alias_m)?,
                ))
            })?,
        },
        l::NativeOperation::Random { dest_place, min, max } => t::NativeOperation::Random {
            dest_place: try_tr(dest_place, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_place(x, ident_m, func_m, type_alias_m)?,
                ))
            })?,
            min: {
                let exprs = try_tr(min, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                    ))
                })?;

                typecheck_to_t_expr(exprs)?
            },
            max: {
                let exprs = try_tr(max, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                    ))
                })?;

                typecheck_to_t_expr(exprs)?
            },
        },
        l::NativeOperation::StdoutClear => t::NativeOperation::StdoutClear,
        l::NativeOperation::StdoutRead { dest_place, index } => t::NativeOperation::StdoutRead {
            dest_place: try_tr(dest_place, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_place(x, ident_m, func_m, type_alias_m)?,
                ))
            })?,
            index: {
                let exprs = try_tr(index, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                    ))
                })?;

                typecheck_to_t_expr(exprs)?
            },
        },
        l::NativeOperation::StdoutWrite { val, index } => t::NativeOperation::StdoutWrite {
            val: {
                let exprs = try_tr(val, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                    ))
                })?;

                typecheck_to_t_expr(exprs)?
            },
            index: {
                let exprs = try_tr(index, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                    ))
                })?;

                typecheck_to_t_expr(exprs)?
            },
        },
        l::NativeOperation::StdoutLen { dest_place } => t::NativeOperation::StdoutLen {
            dest_place: try_tr(dest_place, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_place(x, ident_m, func_m, type_alias_m)?,
                ))
            })?,
        },
        l::NativeOperation::KeyEventsKeyQueueClear => t::NativeOperation::KeyEventsKeyQueueClear,
        l::NativeOperation::KeyEventsKeyQueueDelete { index } => {
            t::NativeOperation::KeyEventsKeyQueueDelete {
                index: {
                    let exprs = try_tr(index, |x| {
                        Ok(sift_stmt_deps(
                            &mut stmt_deps,
                            typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                        ))
                    })?;

                    typecheck_to_t_expr(exprs)?
                },
            }
        },
        l::NativeOperation::KeyEventsKeyQueueRead { dest_place, index } => {
            t::NativeOperation::KeyEventsKeyQueueRead {
                dest_place: try_tr(dest_place, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_place(x, ident_m, func_m, type_alias_m)?,
                    ))
                })?,
                index: {
                    let exprs = try_tr(index, |x| {
                        Ok(sift_stmt_deps(
                            &mut stmt_deps,
                            typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                        ))
                    })?;

                    typecheck_to_t_expr(exprs)?
                },
            }
        },
        l::NativeOperation::KeyEventsKeyQueueLen { dest_place } => {
            t::NativeOperation::KeyEventsKeyQueueLen {
                dest_place: try_tr(dest_place, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_place(x, ident_m, func_m, type_alias_m)?,
                    ))
                })?,
            }
        },
        l::NativeOperation::KeyEventsTimeQueueClear => t::NativeOperation::KeyEventsTimeQueueClear,
        l::NativeOperation::KeyEventsTimeQueueDelete { index } => {
            t::NativeOperation::KeyEventsTimeQueueDelete {
                index: {
                    let exprs = try_tr(index, |x| {
                        Ok(sift_stmt_deps(
                            &mut stmt_deps,
                            typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                        ))
                    })?;

                    typecheck_to_t_expr(exprs)?
                },
            }
        },
        l::NativeOperation::KeyEventsTimeQueueRead { dest_place, index } => {
            t::NativeOperation::KeyEventsTimeQueueRead {
                dest_place: try_tr(dest_place, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_place(x, ident_m, func_m, type_alias_m)?,
                    ))
                })?,
                index: {
                    let exprs = try_tr(index, |x| {
                        Ok(sift_stmt_deps(
                            &mut stmt_deps,
                            typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                        ))
                    })?;

                    typecheck_to_t_expr(exprs)?
                },
            }
        },
        l::NativeOperation::KeyEventsTimeQueueLen { dest_place } => {
            t::NativeOperation::KeyEventsTimeQueueLen {
                dest_place: try_tr(dest_place, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_place(x, ident_m, func_m, type_alias_m)?,
                    ))
                })?,
            }
        },
        l::NativeOperation::Wait { duration_s } => t::NativeOperation::Wait {
            duration_s: {
                let exprs = try_tr(duration_s, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                    ))
                })?;

                typecheck_to_t_expr(exprs)?
            },
        },
        l::NativeOperation::TimerGet { dest_place } => t::NativeOperation::TimerGet {
            dest_place: try_tr(dest_place, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_place(x, ident_m, func_m, type_alias_m)?,
                ))
            })?,
        },
        l::NativeOperation::DaysSince2000Get { dest_place } => {
            t::NativeOperation::DaysSince2000Get {
                dest_place: try_tr(dest_place, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_place(x, ident_m, func_m, type_alias_m)?,
                    ))
                })?,
            }
        },
        l::NativeOperation::Round { dest_place, num } => t::NativeOperation::Round {
            dest_place: try_tr(dest_place, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_place(x, ident_m, func_m, type_alias_m)?,
                ))
            })?,
            num: {
                let exprs = try_tr(num, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                    ))
                })?;

                typecheck_to_t_expr(exprs)?
            },
        },
        l::NativeOperation::Floor { dest_place, num } => t::NativeOperation::Floor {
            dest_place: try_tr(dest_place, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_place(x, ident_m, func_m, type_alias_m)?,
                ))
            })?,
            num: {
                let exprs = try_tr(num, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                    ))
                })?;

                typecheck_to_t_expr(exprs)?
            },
        },
        l::NativeOperation::Ceil { dest_place, num } => t::NativeOperation::Ceil {
            dest_place: try_tr(dest_place, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_place(x, ident_m, func_m, type_alias_m)?,
                ))
            })?,
            num: {
                let exprs = try_tr(num, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                    ))
                })?;

                typecheck_to_t_expr(exprs)?
            },
        },
        l::NativeOperation::Abs { dest_place, num } => t::NativeOperation::Abs {
            dest_place: try_tr(dest_place, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_place(x, ident_m, func_m, type_alias_m)?,
                ))
            })?,
            num: {
                let exprs = try_tr(num, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                    ))
                })?;

                typecheck_to_t_expr(exprs)?
            },
        },
    };

    Ok(StmtDependent { stmt_deps, value: native_op })
}

fn typecheck_assign(
    item: &l::Ref<l::Assign>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<t::Assign>, CompileErrorSet> {
    let place_type = eval_place(&item.val.place, ident_m, func_m, type_alias_m)?.ty;
    let expr = &item.val.expr;
    let expr_type = eval_expr(expr, Some(&place_type), ident_m, func_m, type_alias_m)?;

    if expr_type.is_subtype_of(&place_type).not() {
        return Err(CompileErrorSet::new_error(
            expr.range,
            CompileError::Type(TypeError::Mismatch {
                expected_any_of: Arc::new(Vec::from([vague(&place_type)])),
                found: vague(&expr_type),
            }),
        ));
    }

    let mut stmt_deps = Vec::new();

    let assign = t::Assign {
        place: try_tr(&item.val.place, |x| {
            Ok(sift_stmt_deps(&mut stmt_deps, typecheck_place(x, ident_m, func_m, type_alias_m)?))
        })?,
        exprs: try_tr(&item.val.expr, |x| {
            Ok(sift_stmt_deps(
                &mut stmt_deps,
                typecheck_expr(x, Some(&place_type), ident_m, func_m, type_alias_m)?,
            )
            .into_iter()
            .collect())
        })?,
    };

    Ok(StmtDependent { stmt_deps, value: assign })
}

fn typecheck_call(
    item: &l::Ref<l::FunctionCall>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<Vec<t::Ref<t::AssignExpr>>>, CompileErrorSet> {
    let func_name = &item.val.func_name;
    let param_decls = func_m.params(func_name)?;

    let expr_ident = tr(item, |x| l::Ident::Internal {
        name: tr(x, |_| l::Name { str: "if".into() }),
        uuid: Uuid::new_v4(),
    });

    let expr_ident_ty = func_m.return_ty(func_name)?;
    let expr_ident_ty = Arc::new(type_alias_m.resolve(expr_ident_ty)?);

    let _expr_ident_info = ident_m.reg_let(tr(item, |x| l::IdentDef {
        ident: expr_ident.clone(),
        ty: tr(x, |x| compute_expr_ty_hint(Some(&expr_ident_ty), x.range)),
    }));

    let mut expr_iter = item.val.param_exprs.val.iter();
    let mut decl_iter = param_decls.val.iter();
    let mut typechecked_param_exprs_vec = Vec::new();
    let mut stmt_deps = Vec::new();

    let call_ident_place = tr(&expr_ident, |x| l::Place {
        head: tr(x, |x| l::PlaceHead::Ident(x.clone())),
        index_chain: tr(x, |_| Default::default()),
    });

    let call_ident_expr = tr(&call_ident_place, |x| l::Expr::Place(x.clone()));

    let expr_ident_ref_expr = sift_stmt_deps(
        &mut stmt_deps,
        typecheck_expr(
            &tr(&call_ident_place, |x| l::Expr::Ref(x.clone())),
            Some(&Arc::new(l::Type::Ref(expr_ident_ty.clone()))),
            ident_m,
            func_m,
            type_alias_m,
        )?,
    );

    typechecked_param_exprs_vec.push(Arc::new(Srced {
        range: expr_ident_ref_expr
            .iter()
            .map(|x| x.range)
            .fold(Default::default(), |a, b| a.merge(b)),
        val: expr_ident_ref_expr,
    }));

    loop {
        let (expr, decl) = (expr_iter.next(), decl_iter.next());
        let typechecked_param_exprs = match (expr, decl) {
            (None, None) => break,
            (Some(expr), Some(decl)) => {
                let decl_type = Arc::new(type_alias_m.resolve(&decl.val.ty)?);
                let expr_type = eval_expr(expr, Some(&decl_type), ident_m, func_m, type_alias_m)?;
                if expr_type.is_subtype_of(&decl_type).not() {
                    return Err(CompileErrorSet::new_error(
                        expr.range,
                        CompileError::Type(TypeError::Mismatch {
                            expected_any_of: Arc::new(Vec::from([vague(&decl_type)])),
                            found: vague(&expr_type),
                        }),
                    ));
                }

                try_tr(expr, |x| {
                    Ok(sift_stmt_deps(
                        &mut stmt_deps,
                        typecheck_expr(x, Some(&decl_type), ident_m, func_m, type_alias_m)?,
                    )
                    .into_iter()
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

    let func_call = t::FunctionCall {
        func_name: tr(&item.val.func_name, |x| t::Name::User(tr(x, typecheck_name))),
        param_exprs: tr(&item.val.param_exprs, |_| typechecked_param_exprs_vec),
    };

    stmt_deps.push(tr(item, |x| t::Statement::Call(tr(x, |_| func_call))));

    let mut call_ident_expr_stmt_deps = Vec::new();
    let call_assign_exprs = sift_stmt_deps(
        &mut call_ident_expr_stmt_deps,
        typecheck_expr(&call_ident_expr, Some(&expr_ident_ty), ident_m, func_m, type_alias_m)?,
    );

    let stmt_deps = chain!(stmt_deps, call_ident_expr_stmt_deps).collect();

    Ok(StmtDependent { stmt_deps, value: call_assign_exprs })
}

fn vague(ty: &Arc<l::Type>) -> Arc<VagueType> {
    Arc::new(ty.as_ref().into())
}

fn typecheck_place(
    item: &l::Ref<l::Place>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<t::Place>, CompileErrorSet> {
    let place_eval = eval_place(item, ident_m, func_m, type_alias_m)?;

    let mut stmt_deps = Vec::new();

    let place = t::Place {
        head: try_tr(&item.val.head, |x| {
            Ok(sift_stmt_deps(
                &mut stmt_deps,
                typecheck_place_head(x, ident_m, func_m, type_alias_m)?,
            ))
        })?,
        offset: match place_eval.offset {
            None => None,
            Some(offset) => Some(match offset.as_ref() {
                PlaceOffset::Static(index) => tr(index, |_| t::Offset::Static(index.clone())),
                PlaceOffset::Expr(expr) => try_tr(expr, |x| {
                    Ok(t::Offset::Expr({
                        let exprs = try_tr(x, |x| {
                            Ok(sift_stmt_deps(
                                &mut stmt_deps,
                                typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                            ))
                        })?;

                        typecheck_to_t_expr(exprs)?
                    }))
                })?,
            }),
        },
    };

    Ok(StmtDependent { stmt_deps, value: place })
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
    offset: Option<Arc<PlaceOffset>>,
}

enum PlaceOffset {
    Static(l::Ref<u32>),
    Expr(l::Ref<l::Expr>),
}

impl PlaceOffset {
    pub fn range(&self) -> SrcRange {
        match self {
            Self::Static(x) => x.range,
            Self::Expr(x) => x.range,
        }
    }

    pub fn update_index(&self, offset: PlaceOffset) -> PlaceOffset {
        let to_expr = |offset: &PlaceOffset| match offset {
            PlaceOffset::Expr(expr) => expr.clone(),
            PlaceOffset::Static(index) => {
                let lr = LReffer { range: index.range };
                lr.of(l::Expr::Literal(lr.of(l::Literal::Int(f64::from(index.val)))))
            },
        };

        let offset_range = self.range().merge(offset.range());
        let lr = LReffer { range: offset_range };

        match (&self, offset) {
            (PlaceOffset::Static(curr_static), PlaceOffset::Static(next_static)) => {
                PlaceOffset::Static(lr.of(curr_static.val + next_static.val))
            },
            (curr, next) => PlaceOffset::Expr(lr.of(l::Expr::Paren(lr.of(l::ParenExpr::Binary(
                lr.of(l::BinaryParenExpr {
                    left: to_expr(curr),
                    op: lr.of(l::BinaryParenExprOp::Add),
                    right: to_expr(&next),
                }),
            ))))),
        }
    }
}

impl EvalPlaceRes {
    fn update_index(&mut self, ty: Arc<l::Type>, offset: PlaceOffset) {
        let updated_offset = match &self.offset {
            None => offset,
            Some(curr_offset) => curr_offset.update_index(offset),
        };

        self.ty = ty;
        self.offset = Some(Arc::new(updated_offset));
    }
}

fn eval_place(
    item: &l::Ref<l::Place>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<EvalPlaceRes, CompileErrorSet> {
    let ty = eval_place_head(&item.val.head, ident_m, func_m, type_alias_m)?;
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

                res.update_index(ty.clone(), PlaceOffset::Expr(index_offset));
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

                res.update_index(index_field_ty, PlaceOffset::Static(lr.of(offset)));
            },
        };
    }

    Ok(res)
}

fn typecheck_place_head(
    item: &l::Ref<l::PlaceHead>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<t::PlaceHead>, CompileErrorSet> {
    let mut stmt_deps = Vec::new();

    let place_head = match &item.val {
        l::PlaceHead::Ident(x) => t::PlaceHead::Ident(try_tr(x, |x| typecheck_ident(x, ident_m))?),
        l::PlaceHead::Deref(x) => t::PlaceHead::Deref(try_tr(x, |x| {
            Ok(sift_stmt_deps(&mut stmt_deps, typecheck_deref(x, ident_m, func_m, type_alias_m)?))
        })?),
    };

    Ok(StmtDependent { stmt_deps, value: place_head })
}

fn eval_place_head(
    item: &l::Ref<l::PlaceHead>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    match &item.val {
        l::PlaceHead::Ident(ident) => eval_ident(ident, ident_m, type_alias_m),
        l::PlaceHead::Deref(deref) => eval_deref(deref, ident_m, func_m, type_alias_m),
    }
}

fn typecheck_ident(
    item: &l::Ref<l::Ident>,
    ident_m: &IdentManager,
) -> Result<t::Ident, CompileErrorSet> {
    let info = ident_m.get(&item.val)?;
    let unique_name = tr(item.val.name(), |x| {
        t::Name::User(tr(x, |_| t::UserName { str: info.unique_name.clone() }))
    });

    Ok(t::Ident { name: unique_name })
}

fn eval_ident(
    item: &l::Ref<l::Ident>,
    ident_m: &mut IdentManager,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let info = ident_m.get(&item.val)?;
    type_alias_m.resolve(&info.def.val.ty).map(Arc::new)
}

fn typecheck_deref(
    item: &l::Ref<l::Deref>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<t::Deref>, CompileErrorSet> {
    let mut stmt_deps = Vec::new();

    let exprs = try_tr(&item.val.addr, |x| {
        Ok(sift_stmt_deps(&mut stmt_deps, typecheck_expr(x, None, ident_m, func_m, type_alias_m)?))
    })?;

    let deref = t::Deref { addr: typecheck_to_t_expr(exprs)? };

    Ok(StmtDependent { stmt_deps, value: deref })
}

fn eval_deref(
    item: &l::Ref<l::Deref>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let addr_type = eval_expr(&item.val.addr, None, ident_m, func_m, type_alias_m)?;
    let l::Type::Ref(deref_type) = addr_type.as_ref() else {
        return Err(CompileErrorSet::new_error(
            item.range,
            CompileError::Type(TypeError::DerefNonRef { found: vague(&addr_type) }),
        ));
    };

    Ok(deref_type.clone())
}

fn merge_t_offsets(
    curr_offset: &t::Ref<t::Offset>,
    next_offset: &t::Ref<t::Offset>,
) -> t::Ref<t::Offset> {
    let to_expr = |offset: &t::Offset| match offset {
        t::Offset::Expr(expr) => expr.clone(),
        t::Offset::Static(index) => tr(index, |index| {
            t::Expr::Literal(tr(index, |index| t::Literal::Int(f64::from(index.val))))
        }),
    };

    let lr = LReffer { range: curr_offset.range.merge(next_offset.range) };

    let merged_offset = match (&curr_offset.val, &next_offset.val) {
        (t::Offset::Static(curr_static), t::Offset::Static(next_static)) => {
            t::Offset::Static(lr.of(curr_static.val + next_static.val))
        },
        (curr, next) => t::Offset::Expr(lr.of(t::Expr::Paren(lr.of(t::ParenExpr::Binary(lr.of(
            t::BinaryParenExpr {
                left: to_expr(curr),
                op: lr.of(t::BinaryParenExprOp::Add),
                right: to_expr(next),
            },
        )))))),
    };

    lr.of(merged_offset)
}

fn infer_enum_name(
    variant_literal: &l::Ref<l::VariantLiteral>,
    expected_type: Option<&Arc<l::Type>>,
) -> Result<Arc<str>, CompileErrorSet> {
    let enum_name = match &variant_literal.val.enum_name {
        Some(enum_name) => &enum_name.val.str,
        None => match expected_type.map(AsRef::as_ref) {
            Some(l::Type::Enum { name }) => name,
            _ => {
                return Err(CompileErrorSet::new_error(
                    variant_literal.range,
                    CompileError::Type(TypeError::CannotInfer),
                ));
            },
        },
    };

    Ok(enum_name.clone())
}

#[derive(Debug)]
struct StmtDependent<T> {
    stmt_deps: Vec<t::Ref<t::Statement>>,
    value: T,
}

fn typecheck_expr(
    item: &l::Ref<l::Expr>,
    expected_type: Option<&Arc<l::Type>>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<Vec<t::Ref<t::AssignExpr>>>, CompileErrorSet> {
    let expr_ty = eval_expr(item, expected_type, ident_m, func_m, type_alias_m)?;

    let mut stmt_deps = Vec::new();

    let exprs: Vec<t::Ref<t::AssignExpr>> = match &item.val {
        l::Expr::Literal(x) => {
            let expr = t::Expr::Literal(try_tr(x, |x| match &x.val {
                l::Literal::Str(x) => Ok(t::Literal::Str(x.clone())),
                l::Literal::Num(x) => Ok(t::Literal::Num(*x)),
                l::Literal::Int(x) => Ok(t::Literal::Int(*x)),
                l::Literal::Uint(x) => Ok(t::Literal::Uint(*x)),
                l::Literal::Bool(x) => Ok(t::Literal::Bool(*x)),
                l::Literal::Variant(x) => Ok(typecheck_variant_literal(
                    &EnumName::Str(infer_enum_name(x, expected_type)?),
                    &x.val.variant_name,
                    x.range,
                    type_alias_m,
                )?),
            })?);

            Vec::from([tr(x, |x| t::AssignExpr { expr: tr(x, |_| expr), offset: 0 })])
        },
        l::Expr::Ref(x) => {
            let expr = t::Expr::Ref(try_tr(x, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_place(x, ident_m, func_m, type_alias_m)?,
                ))
            })?);

            Vec::from([tr(x, |x| t::AssignExpr { expr: tr(x, |_| expr), offset: 0 })])
        },
        l::Expr::Paren(x) => {
            let expr = t::Expr::Paren(try_tr(x, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_paren_expr(x, ident_m, func_m, type_alias_m)?,
                ))
            })?);

            Vec::from([tr(x, |x| t::AssignExpr { expr: tr(x, |_| expr), offset: 0 })])
        },
        l::Expr::Place(place) => {
            let place_type = eval_place(place, ident_m, func_m, type_alias_m)?;

            let place = try_tr(place, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_place(x, ident_m, func_m, type_alias_m)?,
                ))
            })?;

            (0..(place_type.ty.size()))
                .map(|offset| {
                    tr(&place, |x| t::AssignExpr {
                        offset,
                        expr: tr(x, |x| {
                            t::Expr::Place(tr(x, |x| {
                                let curr_offset = tr(x, |x| t::Offset::Static(tr(x, |_| offset)));

                                let offset = match &place.val.offset {
                                    None => curr_offset,
                                    Some(next_offset) => merge_t_offsets(&curr_offset, next_offset),
                                };

                                t::Place { head: x.val.head.clone(), offset: Some(offset) }
                            }))
                        }),
                    })
                })
                .collect()
        },
        l::Expr::Cast { expr, ty: _ } | l::Expr::Transmute { expr, ty: _ } => sift_stmt_deps(
            &mut stmt_deps,
            typecheck_expr(expr, None, ident_m, func_m, type_alias_m)?,
        ),
        l::Expr::If(x) => sift_stmt_deps(
            &mut stmt_deps,
            typecheck_if(x, expected_type, ident_m, func_m, type_alias_m)?,
        ),
        l::Expr::While(x) => sift_stmt_deps(
            &mut stmt_deps,
            typecheck_while(x, expected_type, ident_m, func_m, type_alias_m)?,
        ),
        l::Expr::Match(match_item) => sift_stmt_deps(
            &mut stmt_deps,
            typecheck_match(match_item, expected_type, ident_m, func_m, type_alias_m)?,
        ),
        l::Expr::Call(call) => {
            sift_stmt_deps(&mut stmt_deps, typecheck_call(call, ident_m, func_m, type_alias_m)?)
        },
        l::Expr::Statement(stmt) => sift_stmt_deps(
            &mut stmt_deps,
            typecheck_statement(stmt, ident_m, func_m, type_alias_m)?,
        ),
        l::Expr::Block(block) => sift_stmt_deps(
            &mut stmt_deps,
            typecheck_block(block, expected_type, ident_m, func_m, type_alias_m)?,
        ),
        l::Expr::Array { single_exprs, spread_expr } => {
            let Some(expected_type) = expected_type else {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::CannotInfer),
                ));
            };

            let l::Type::Array { ty: expected_el_type, len: expected_len } = expected_type.as_ref()
            else {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::Mismatch {
                        expected_any_of: Arc::new(Vec::from([vague(expected_type)])),
                        found: vague(&expr_ty),
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
                                    expected_any_of: Arc::new(Vec::from([vague(expected_type)])),
                                    found: vague(&expr_ty),
                                }),
                            ));
                        },
                    },
                };

                let exprs = sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_expr(el, Some(expected_el_type), ident_m, func_m, type_alias_m)?,
                );

                let el_assign_exprs = exprs.iter().map(|el| {
                    tr(el, |el| t::AssignExpr {
                        offset: i * el_size + el.val.offset,
                        expr: el.val.expr.clone(),
                    })
                });

                typechecked_assign_exprs.extend(el_assign_exprs);
            }

            typechecked_assign_exprs
        },
        l::Expr::Struct(assign_fields) => {
            let Some(expected_type) = expected_type else {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::CannotInfer),
                ));
            };

            let l::Type::Struct(expected_fields) = expected_type.as_ref() else {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::Mismatch {
                        expected_any_of: Arc::new(Vec::from([vague(expected_type)])),
                        found: vague(&expr_ty),
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

                let field_assign_exprs = sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_expr(
                        &assign_field.val.assign,
                        Some(&expected_field.ty),
                        ident_m,
                        func_m,
                        type_alias_m,
                    )?,
                );

                for field_assign_expr in field_assign_exprs {
                    struct_assign_exprs.push(tr(&field_assign_expr, |field_assign_expr| {
                        t::AssignExpr {
                            offset: field_offset + field_assign_expr.val.offset,
                            expr: field_assign_expr.val.expr.clone(),
                        }
                    }));
                }

                field_offset += expected_field.ty.size();
            }

            struct_assign_exprs
        },
        l::Expr::Undefined => Default::default(),
    };

    Ok(StmtDependent { stmt_deps, value: exprs })
}

fn compute_expr_ty_hint(expected_type: Option<&Arc<l::Type>>, range: SrcRange) -> l::TypeHint {
    expected_type.map(|ty| TypeHint::from_type(ty, range)).unwrap_or_else(|| unit_type_hint(range))
}

fn eval_if(
    item: &l::Ref<l::IfItem>,
    expected_type: Option<&Arc<l::Type>>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let then_ty = eval_expr(&item.val.then_body, expected_type, ident_m, func_m, type_alias_m)?;

    let Some(else_item) = &item.val.else_item else {
        return if then_ty.is_subtype_of(&l::UNIT_TYPE) {
            Ok(l::UNIT_TYPE.clone())
        } else {
            Err(CompileErrorSet::new_error(
                item.val.then_body.range,
                CompileError::Type(TypeError::Mismatch {
                    expected_any_of: Arc::new(Vec::from([vague(&l::UNIT_TYPE)])),
                    found: vague(&then_ty),
                }),
            ))
        };
    };

    let else_ty = eval_expr(&else_item.val.body, expected_type, ident_m, func_m, type_alias_m)?;

    match Type::closest_common_supertype(&then_ty, &else_ty) {
        Some(if_ty) => Ok(if_ty),
        None => Err(CompileErrorSet::new_error(
            else_item.val.body.range,
            CompileError::Type(TypeError::Mismatch {
                expected_any_of: Arc::new(Vec::from([vague(&then_ty)])),
                found: vague(&else_ty),
            }),
        )),
    }
}

fn eval_match(
    item: &l::Ref<l::MatchItem>,
    expected_type: Option<&Arc<l::Type>>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let case_tys_with_expr_range = item
        .val
        .cases
        .val
        .iter()
        .map(|case| {
            Ok((
                eval_expr(&case.val.body, expected_type, ident_m, func_m, type_alias_m)?,
                case.val.body.range,
            ))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let Some((match_ty, _)) = case_tys_with_expr_range.first() else {
        return Err(CompileErrorSet::new_error(
            item.val.cases.range,
            CompileError::Type(TypeError::NonExhaustiveMatch {
                unmatched_cases: Arc::new(Vec::from(["_".into()])),
            }),
        ));
    };

    let mut match_ty = match_ty.clone();
    for (case_ty, case_expr_range) in &case_tys_with_expr_range {
        match_ty = match Type::closest_common_supertype(&match_ty, case_ty) {
            Some(ty) => ty,
            None => {
                return Err(CompileErrorSet::new_error(
                    *case_expr_range,
                    CompileError::Type(TypeError::Mismatch {
                        expected_any_of: Arc::new(Vec::from([vague(&match_ty)])),
                        found: vague(case_ty),
                    }),
                ));
            },
        };
    }

    Ok(match_ty)
}

fn eval_expr(
    item: &l::Ref<l::Expr>,
    expected_type: Option<&Arc<l::Type>>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let ty = match &item.val {
        l::Expr::Literal(x) => Ok(match &x.val {
            l::Literal::Str(_) => STR_TYPE.clone(),
            l::Literal::Num(_) => NUM_TYPE.clone(),
            l::Literal::Int(_) => INTEGER_TYPE.clone(),
            l::Literal::Uint(_) => UINTEGER_TYPE.clone(),
            l::Literal::Bool(_) => BOOL_TYPE.clone(),
            l::Literal::Variant(x) => {
                Arc::new(l::Type::Enum { name: infer_enum_name(x, expected_type)?.clone() })
            },
        }),
        l::Expr::Place(x) => Ok(eval_place(x, ident_m, func_m, type_alias_m).map(|res| res.ty)?),
        l::Expr::Ref(place) => {
            let place_type = eval_place(place, ident_m, func_m, type_alias_m)?;
            Ok(Arc::new(l::Type::Ref(place_type.ty)))
        },
        l::Expr::Paren(x) => Ok(eval_paren_expr(x, ident_m, func_m, type_alias_m)?),
        l::Expr::Cast { ty, expr } => {
            let cast_ty = Arc::new(type_alias_m.resolve(ty)?);
            let expr_ty = eval_expr(expr, None, ident_m, func_m, type_alias_m)?;
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
            let expr_ty = eval_expr(expr, None, ident_m, func_m, type_alias_m)?;
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
        l::Expr::Statement(_) => Ok(UNIT_TYPE.clone()),
        l::Expr::Call(call) => {
            let return_ty = func_m.return_ty(&call.val.func_name)?;
            let return_ty = type_alias_m.resolve(return_ty)?;
            Ok(Arc::new(return_ty))
        },
        l::Expr::If(if_item) => eval_if(if_item, expected_type, ident_m, func_m, type_alias_m),
        l::Expr::While(while_item) => {
            // right now, whiles cannot eval to anything but the unit type
            let body = &while_item.val.body;
            let body_ty = eval_expr(body, Some(&UNIT_TYPE), ident_m, func_m, type_alias_m)?;
            if body_ty.is_subtype_of(&UNIT_TYPE).not() {
                return Err(CompileErrorSet::new_error(
                    body.range,
                    CompileError::Type(TypeError::Mismatch {
                        expected_any_of: Arc::new(Vec::from([vague(&UNIT_TYPE)])),
                        found: vague(&body_ty),
                    }),
                ));
            }

            Ok(UNIT_TYPE.clone())
        },
        l::Expr::Match(match_item) => {
            eval_match(match_item, expected_type, ident_m, func_m, type_alias_m)
        },
        l::Expr::Block(block) => 'block: {
            let trail = &block.val.items.val;
            if trail.trailing {
                break 'block Ok(UNIT_TYPE.clone());
            }

            ident_m.scope(|ident_m| {
                for l_item in trail.items.val.iter() {
                    // register all ident defs to ident_m
                    if let l::Expr::Statement(statement) = &l_item.val
                        && let l::Statement::Let(ident_init) = &statement.val
                    {
                        ident_m.reg_let(ident_init.val.ident_init.val.def.clone());
                    }
                }

                match trail.items.val.last() {
                    None => Ok(UNIT_TYPE.clone()),
                    Some(last_expr) => {
                        eval_expr(last_expr, expected_type, ident_m, func_m, type_alias_m)
                    },
                }
            })
        },
        l::Expr::Array { single_exprs, spread_expr } => {
            let Some(expected_type) = expected_type else {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::CannotInfer),
                ));
            };

            let l::Type::Array { ty: expected_el_type, len: expected_len } = expected_type.as_ref()
            else {
                let len = match spread_expr {
                    None => Some(single_exprs.val.len() as u32),
                    Some(_) => None,
                };

                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::Mismatch {
                        expected_any_of: Arc::new(Vec::from([vague(expected_type)])),
                        found: Arc::new(VagueType::Array { ty: Arc::new(VagueType::Unknown), len }),
                    }),
                ));
            };

            let elements = single_exprs.val.iter().chain(spread_expr.as_ref());

            for el in elements {
                let el_type = eval_expr(el, Some(expected_el_type), ident_m, func_m, type_alias_m)?;
                if el_type.is_subtype_of(expected_el_type).not() {
                    return Err(CompileErrorSet::new_error(
                        el.range,
                        CompileError::Type(TypeError::Mismatch {
                            expected_any_of: Arc::new(Vec::from([vague(expected_el_type)])),
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
        l::Expr::Struct(assign_fields) => {
            let Some(expected_type) = expected_type else {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::CannotInfer),
                ));
            };

            let l::Type::Struct(expected_fields) = expected_type.as_ref() else {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::Mismatch {
                        expected_any_of: Arc::new(Vec::from([vague(expected_type)])),
                        found: Arc::new(VagueType::Struct(None)),
                    }),
                ));
            };

            let evaled_fields = expected_fields
                .iter()
                .map(|expected_field| {
                    let Some(assign_field) = assign_fields
                        .val
                        .iter()
                        .find(|assign_field| expected_field.name == assign_field.val.name.val.str)
                    else {
                        return Err(CompileErrorSet::new_error(
                            assign_fields.range,
                            CompileError::Type(TypeError::StructLiteralMissingField {
                                field_type: vague(&expected_field.ty),
                                field_name: expected_field.name.clone(),
                            }),
                        ));
                    };

                    let assign_field_ty = eval_expr(
                        &assign_field.val.assign,
                        Some(&expected_field.ty),
                        ident_m,
                        func_m,
                        type_alias_m,
                    )?;

                    if assign_field_ty.is_subtype_of(&expected_field.ty).not() {
                        return Err(CompileErrorSet::new_error(
                            assign_field.val.assign.range,
                            CompileError::Type(TypeError::Mismatch {
                                expected_any_of: Arc::new(Vec::from([vague(&expected_field.ty)])),
                                found: vague(&assign_field_ty),
                            }),
                        ));
                    }

                    Ok(Arc::new(l::FieldType {
                        name: expected_field.name.clone(),
                        ty: expected_field.ty.clone(),
                    }))
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(Arc::new(l::Type::Struct(Arc::new(evaled_fields))))
        },
        l::Expr::Undefined => {
            let Some(expected_type) = expected_type else {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::CannotInfer),
                ));
            };

            Ok(expected_type.clone())
        },
    }?;

    if let Some(expected_ty) = expected_type {
        if ty.is_subtype_of(expected_ty).not() {
            return Err(CompileErrorSet::new_error(
                item.range,
                CompileError::Type(TypeError::Mismatch {
                    expected_any_of: Arc::new(Vec::from([vague(expected_ty)])),
                    found: vague(&ty),
                }),
            ));
        }
    }

    Ok(ty)
}

fn typecheck_variant_literal(
    enum_name: &EnumName,
    variant_name: &l::Ref<l::Name>,
    variant_literal_range: SrcRange,
    type_alias_m: &TypeAliasManager,
) -> Result<t::Literal, CompileErrorSet> {
    let variant_id = type_alias_m.id_variant(enum_name, variant_name, variant_literal_range)?;
    Ok(t::Literal::Uint(f64::from(variant_id)))
}

fn typecheck_paren_expr(
    item: &l::Ref<l::ParenExpr>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<t::ParenExpr>, CompileErrorSet> {
    let mut stmt_deps = Vec::new();

    let paren_expr = match &item.val {
        l::ParenExpr::Unary(x) => t::ParenExpr::Unary(try_tr(x, |x| {
            Ok(sift_stmt_deps(
                &mut stmt_deps,
                typecheck_unary_expr(x, ident_m, func_m, type_alias_m)?,
            ))
        })?),
        l::ParenExpr::Binary(x) => t::ParenExpr::Binary(try_tr(x, |x| {
            Ok(sift_stmt_deps(
                &mut stmt_deps,
                typecheck_binary_expr(x, ident_m, func_m, type_alias_m)?,
            ))
        })?),
    };

    Ok(StmtDependent { stmt_deps, value: paren_expr })
}

fn eval_paren_expr(
    item: &l::Ref<l::ParenExpr>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    match &item.val {
        l::ParenExpr::Unary(x) => eval_unary_expr(x, ident_m, func_m, type_alias_m),
        l::ParenExpr::Binary(x) => eval_binary_expr(x, ident_m, func_m, type_alias_m),
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
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<t::UnaryParenExpr>, CompileErrorSet> {
    let mut stmt_deps = Vec::new();

    let unary_expr = t::UnaryParenExpr {
        operand: {
            let exprs = try_tr(&item.val.operand, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                ))
            })?;

            typecheck_to_t_expr(exprs)?
        },
        op: tr(&item.val.op, |x| typecheck_variants!(&x.val, UnaryParenExprOp => [Not])),
    };

    Ok(StmtDependent { stmt_deps, value: unary_expr })
}

fn eval_unary_expr(
    item: &l::Ref<l::UnaryParenExpr>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let operand = &item.val.operand;
    let operand_type = eval_expr(operand, None, ident_m, func_m, type_alias_m)?;
    match item.val.op.val {
        l::UnaryParenExprOp::Not => {
            if operand_type.is_subtype_of(&BOOL_TYPE).not() {
                return Err(CompileErrorSet::new_error(
                    item.range,
                    CompileError::Type(TypeError::Mismatch {
                        expected_any_of: Arc::new(Vec::from([vague(&BOOL_TYPE)])),
                        found: vague(&operand_type),
                    }),
                ));
            }

            Ok(BOOL_TYPE.clone())
        },
    }
}

fn typecheck_to_t_expr(
    exprs: t::Ref<Vec<t::Ref<t::AssignExpr>>>,
) -> Result<t::Ref<t::Expr>, CompileErrorSet> {
    let expr = 'expr: {
        let [expr] = exprs.val.as_slice() else { break 'expr None };
        if expr.val.offset != 0 {
            break 'expr None;
        };

        Some(expr.val.expr.clone())
    };

    expr.ok_or_else(|| {
        CompileErrorSet::new_error(
            exprs.range,
            CompileError::Type(TypeError::Mismatch {
                expected_any_of: Arc::new(Vec::from([
                    Arc::new(VagueType::Primitive(l::PrimitiveType::Val)),
                    Arc::new(VagueType::Ref(Arc::new(VagueType::Unknown))),
                    Arc::new(VagueType::Enum { name: None }),
                    Arc::new(VagueType::Any),
                ])),
                found: Arc::new(VagueType::Unknown),
            }),
        )
    })
}

fn typecheck_binary_expr(
    item: &l::Ref<l::BinaryParenExpr>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<StmtDependent<t::BinaryParenExpr>, CompileErrorSet> {
    let mut stmt_deps = Vec::new();

    let binary_expr = t::BinaryParenExpr {
        left: {
            let exprs = try_tr(&item.val.left, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                ))
            })?;

            typecheck_to_t_expr(exprs)?
        },
        right: {
            let exprs = try_tr(&item.val.right, |x| {
                Ok(sift_stmt_deps(
                    &mut stmt_deps,
                    typecheck_expr(x, None, ident_m, func_m, type_alias_m)?,
                ))
            })?;

            typecheck_to_t_expr(exprs)?
        },
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
    };

    Ok(StmtDependent { stmt_deps, value: binary_expr })
}

fn eval_binary_expr(
    item: &l::Ref<l::BinaryParenExpr>,
    ident_m: &mut IdentManager,
    func_m: &FuncManager,
    type_alias_m: &TypeAliasManager,
) -> Result<Arc<l::Type>, CompileErrorSet> {
    let left_type = eval_expr(&item.val.left, None, ident_m, func_m, type_alias_m)?;
    let right_type = eval_expr(&item.val.right, None, ident_m, func_m, type_alias_m)?;

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
            UINTEGER_TYPE, UINTEGER_TYPE => UINTEGER_TYPE;
            INTEGER_TYPE, INTEGER_TYPE => INTEGER_TYPE;
            NUM_TYPE, NUM_TYPE => NUM_TYPE;
        ),
        l::BinaryParenExprOp::Sub => expect_types!(
            for "-";
            INTEGER_TYPE, INTEGER_TYPE => INTEGER_TYPE;
            NUM_TYPE, NUM_TYPE => NUM_TYPE;
        ),
        l::BinaryParenExprOp::Mul => expect_types!(
            for "*";
            UINTEGER_TYPE, UINTEGER_TYPE => UINTEGER_TYPE;
            INTEGER_TYPE, INTEGER_TYPE => INTEGER_TYPE;
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
        l::BinaryParenExprOp::Eq | l::BinaryParenExprOp::Neq => 'expect: {
            let comparable = 'comparable: {
                if left_type.is_subtype_of(&STR_TYPE) && right_type.is_subtype_of(&STR_TYPE) {
                    break 'comparable true;
                }

                if left_type.is_subtype_of(&NUM_TYPE) && right_type.is_subtype_of(&NUM_TYPE) {
                    break 'comparable true;
                }

                if left_type.is_subtype_of(&BOOL_TYPE) && right_type.is_subtype_of(&BOOL_TYPE) {
                    break 'comparable true;
                }

                if let l::Type::Enum { name: left_name } = left_type.as_ref()
                    && let l::Type::Enum { name: right_name } = right_type.as_ref()
                    && left_name == right_name
                {
                    break 'comparable true;
                }

                false
            };

            if comparable {
                break 'expect Ok(BOOL_TYPE.clone());
            }

            Err(CompileErrorSet::new_error(
                item.range,
                CompileError::Type(TypeError::Uncomparable {
                    left: vague(&left_type),
                    right: vague(&right_type),
                }),
            ))
        },
        l::BinaryParenExprOp::Gt
        | l::BinaryParenExprOp::Lt
        | l::BinaryParenExprOp::Gte
        | l::BinaryParenExprOp::Lte => 'expect: {
            let comparable = 'comparable: {
                if left_type.is_subtype_of(&STR_TYPE) && right_type.is_subtype_of(&STR_TYPE) {
                    break 'comparable true;
                }

                if left_type.is_subtype_of(&NUM_TYPE) && right_type.is_subtype_of(&NUM_TYPE) {
                    break 'comparable true;
                }

                if let l::Type::Enum { name: left_name } = left_type.as_ref()
                    && let l::Type::Enum { name: right_name } = right_type.as_ref()
                    && left_name == right_name
                {
                    break 'comparable true;
                }

                false
            };

            if comparable {
                break 'expect Ok(BOOL_TYPE.clone());
            }

            Err(CompileErrorSet::new_error(
                item.range,
                CompileError::Type(TypeError::Uncomparable {
                    left: vague(&left_type),
                    right: vague(&right_type),
                }),
            ))
        },
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
            VAL_TYPE, VAL_TYPE => STR_TYPE;
        ),
    }
}
