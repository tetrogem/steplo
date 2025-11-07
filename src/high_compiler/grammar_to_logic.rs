use std::collections::VecDeque;
use std::ops::Not;
use std::str::FromStr;
use std::sync::Arc;

use itertools::Itertools;

use crate::compile_error::CompileError;
use crate::compile_error::CompileErrorSet;
use crate::compile_error::LogicError;
use crate::grammar_ast as g;
use crate::high_compiler::srced::SrcRange;
use crate::logic_ast as l;
use crate::srced::Srced;
use crate::utils::IsInteger;

trait FromList<T> {
    fn from_list(value: T) -> Self;
}

fn convert<From, To>(from: &g::Ref<From>) -> l::Ref<To>
where
    for<'a> &'a g::Ref<From>: Into<To>,
{
    Arc::new(Srced { range: from.range, val: from.into() })
}

fn try_convert<From, To, Error>(from: &g::Ref<From>) -> Result<l::Ref<To>, Error>
where
    for<'a> &'a g::Ref<From>: TryInto<To, Error = Error>,
{
    from.try_into().map(|val| Arc::new(Srced { val, range: from.range }))
}

fn try_convert_list<List, From, To, Error>(
    list: &g::Ref<List>,
) -> Result<l::Ref<Vec<l::Ref<To>>>, Error>
where
    for<'a> VecDeque<g::Ref<From>>: FromList<&'a g::Ref<List>>,
    for<'a> &'a g::Ref<From>: TryInto<To, Error = Error>,
{
    let deque = VecDeque::from_list(list);
    let tos = deque.iter().map(|x| -> Result<l::Ref<To>, Error> { try_convert(x) });
    let tos = tos.collect::<Result<Vec<_>, _>>()?;

    Ok(Arc::new(Srced { val: tos, range: list.range }))
}

fn convert_list<List, From, To>(list: &g::Ref<List>) -> l::Ref<Vec<l::Ref<To>>>
where
    for<'a> VecDeque<g::Ref<From>>: FromList<&'a g::Ref<List>>,
    for<'a> &'a g::Ref<From>: Into<To>,
{
    let deque = VecDeque::from_list(list);
    let tos = deque.iter().map(|x| -> l::Ref<To> { convert(x) });
    let tos = tos.collect::<Vec<_>>();

    Arc::new(Srced { val: tos, range: list.range })
}

pub fn grammar_to_ast(grammar: &g::Ref<g::Program>) -> Result<l::Ref<l::Program>, CompileErrorSet> {
    try_convert(grammar)
}

impl TryFrom<&g::Ref<g::Program>> for l::Program {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Program>) -> Result<Self, Self::Error> {
        Ok(Self { items: try_convert_list(&value.val.items)? })
    }
}

impl<T> FromList<&g::Ref<g::List<T>>> for VecDeque<g::Ref<T>> {
    fn from_list(value: &g::Ref<g::List<T>>) -> Self {
        match &value.val {
            g::List::Empty(_) => Default::default(),
            g::List::Link(link) => {
                let mut vec = VecDeque::from_list(&link.val.next);
                vec.push_front(link.val.item.clone());
                vec
            },
        }
    }
}

impl<T> FromList<&g::Ref<g::CommaList<T>>> for VecDeque<g::Ref<T>> {
    fn from_list(value: &g::Ref<g::CommaList<T>>) -> Self {
        match &value.val {
            g::CommaList::Empty(_) => Default::default(),
            g::CommaList::Tail(tail) => VecDeque::from([tail.clone()]),
            g::CommaList::Link(link) => {
                let mut vec = VecDeque::from_list(&link.val.next);
                vec.push_front(link.val.item.clone());
                vec
            },
        }
    }
}

impl<T> FromList<&g::Ref<g::SemiList<T>>> for VecDeque<g::Ref<T>> {
    fn from_list(value: &g::Ref<g::SemiList<T>>) -> Self {
        match &value.val {
            g::SemiList::Empty(_) => Default::default(),
            g::SemiList::Tail(tail) => VecDeque::from([tail.clone()]),
            g::SemiList::Link(link) => {
                let mut vec = VecDeque::from_list(&link.val.next);
                vec.push_front(link.val.item.clone());
                vec
            },
        }
    }
}

impl<T> FromList<&g::Ref<g::PipeList<T>>> for VecDeque<g::Ref<T>> {
    fn from_list(value: &g::Ref<g::PipeList<T>>) -> Self {
        match &value.val {
            g::PipeList::Empty(_) => Default::default(),
            g::PipeList::Tail(tail) => VecDeque::from([tail.clone()]),
            g::PipeList::Link(link) => {
                let mut vec = VecDeque::from_list(&link.val.next);
                vec.push_front(link.val.item.clone());
                vec
            },
        }
    }
}

impl TryFrom<&g::Ref<g::TopItem>> for l::TopItem {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::TopItem>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::TopItem::Main(x) => Self::Exe(Arc::new(Srced {
                range: x.range,
                val: l::ExeItem::Main(try_convert(x)?),
            })),
            g::TopItem::Func(x) => Self::Exe(Arc::new(Srced {
                range: x.range,
                val: l::ExeItem::Func(try_convert(x)?),
            })),
            g::TopItem::TypeAlias(x) => Self::Type(Arc::new(Srced {
                range: x.range,
                val: l::TypeItem::Alias(try_convert(x)?),
            })),
            g::TopItem::Enum(x) => {
                Self::Type(Arc::new(Srced { range: x.range, val: l::TypeItem::Enum(convert(x)) }))
            },
        })
    }
}

impl TryFrom<&g::Ref<g::Main>> for l::Main {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Main>) -> Result<Self, Self::Error> {
        Ok(Self { body: try_convert_free_trail_expr(&value.val.body)? })
    }
}

impl TryFrom<&g::Ref<g::Func>> for l::Func {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Func>) -> Result<Self, Self::Error> {
        let return_ty = match convert_maybe(&value.val.return_ty) {
            Some(return_ty) => try_convert(&return_ty.val.ty)?,
            None => Arc::new(Srced {
                range: value.val.return_ty.range,
                val: l::unit_type_hint(value.val.return_ty.range),
            }),
        };

        Ok(Self {
            name: convert(&value.val.name),
            params: try_convert_list(&value.val.params)?,
            return_ty,
            body: try_convert_free_trail_expr(&value.val.body)?,
        })
    }
}

impl TryFrom<&g::Ref<g::TypeAlias>> for l::TypeAlias {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::TypeAlias>) -> Result<Self, Self::Error> {
        Ok(Self { name: convert(&value.val.name), ty: try_convert(&value.val.ty)? })
    }
}

impl From<&g::Ref<g::EnumItem>> for l::EnumItem {
    fn from(value: &g::Ref<g::EnumItem>) -> Self {
        Self { name: convert(&value.val.name), variants: convert_list(&value.val.variants) }
    }
}

impl From<&g::Ref<g::Name>> for l::Name {
    fn from(value: &g::Ref<g::Name>) -> Self {
        Self { str: value.val.str.clone() }
    }
}

impl TryFrom<&g::Ref<g::IdentDef>> for l::IdentDef {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::IdentDef>) -> Result<Self, Self::Error> {
        Ok(Self {
            ident: Arc::new(Srced {
                range: value.val.name.range,
                val: l::Ident::User { name: convert(&value.val.name) },
            }),
            ty: try_convert(&value.val.ty)?,
        })
    }
}

impl TryFrom<&g::Ref<g::IdentInit>> for l::IdentInit {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::IdentInit>) -> Result<Self, Self::Error> {
        Ok(Self {
            def: try_convert(&value.val.def)?,
            expr: try_convert_free_trail_expr(&value.val.expr)?,
        })
    }
}

impl TryFrom<&g::Ref<g::Type>> for l::TypeHint {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Type>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::Type::Nominal(base) => base.into(),
            g::Type::Ref(ref_ty) => Self::Ref(try_convert(&ref_ty.val.ty)?),
            g::Type::Array(array) => {
                let len = &array.val.len;
                let Ok(len) = parse_num_literal(&array.val.len) else {
                    return Err(CompileErrorSet::new_error(
                        len.range,
                        CompileError::Logic(LogicError::InvalidArrayTypeLen),
                    ));
                };

                Self::Array { ty: try_convert(&array.val.ty)?, len }
            },
            g::Type::Struct(struct_ty) => {
                let fields: l::Ref<Vec<l::Ref<l::IdentDef>>> =
                    try_convert_list(&struct_ty.val.fields)?;

                let fields = fields
                    .val
                    .iter()
                    .map(|field| {
                        let ident = &field.val.ident;
                        let field_name = match &ident.val {
                            l::Ident::User { name } => name,
                            l::Ident::Internal { .. } => {
                                return Err(CompileErrorSet::new_error(
                                    ident.range,
                                    CompileError::Logic(LogicError::FoundInternalIdent),
                                ));
                            },
                        };

                        Ok(Arc::new(Srced {
                            range: field.range,
                            val: l::FieldTypeHint {
                                name: field_name.clone(),
                                ty: field.val.ty.clone(),
                            },
                        }))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Self::Struct(Arc::new(Srced {
                    range: fields.iter().fold(Default::default(), |acc, x| acc.merge(x.range)),
                    val: fields,
                }))
            },
        })
    }
}

impl From<&g::Ref<g::NominalType>> for l::TypeHint {
    fn from(value: &g::Ref<g::NominalType>) -> Self {
        let name = &value.val.name;
        Self::Nominal(convert(name))
    }
}

impl TryFrom<&g::Ref<g::Block>> for l::Block {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Block>) -> Result<Self, Self::Error> {
        let mut prev_was_semi = false;
        let mut l_items = Vec::new();

        let g_items = VecDeque::from_list(&value.val.items);
        for g_item in g_items.into_iter() {
            match &g_item.val {
                g::ExprOrSemi::Semi(_) => prev_was_semi = true,
                g::ExprOrSemi::Expr(expr) => {
                    let expr = try_convert_free_trail_expr(expr)?;
                    l_items.push(expr);
                    prev_was_semi = false;
                },
            }
        }

        Ok(l::Block {
            items: Arc::new(Srced {
                range: value.range,
                val: l::Trail {
                    trailing: prev_was_semi,
                    items: Arc::new(Srced { range: value.range, val: l_items }),
                },
            }),
        })
    }
}

impl TryFrom<&g::Ref<g::TrailingExpr>> for l::Expr {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::TrailingExpr>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::TrailingExpr::IdentInit(x) => Self::Statement(Arc::new(Srced {
                range: value.range,
                val: l::Statement::IdentInit(try_convert(x)?),
            })),
            g::TrailingExpr::Assign(x) => Self::Statement(Arc::new(Srced {
                range: value.range,
                val: l::Statement::Assign(try_convert(x)?),
            })),
            g::TrailingExpr::If(x) => Self::If(try_convert(x)?),
            g::TrailingExpr::While(x) => Self::While(try_convert(x)?),
        })
    }
}

impl TryFrom<&g::Ref<g::IfItem>> for l::IfItem {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::IfItem>) -> Result<Self, Self::Error> {
        Ok(Self {
            condition: try_convert_free_trail_expr(&value.val.condition)?,
            then_body: try_convert_free_trail_expr(&value.val.then_body)?,
            else_item: match convert_maybe(&value.val.else_item) {
                None => None,
                Some(x) => Some(try_convert(x)?),
            },
        })
    }
}

impl TryFrom<&g::Ref<g::WhileItem>> for l::WhileItem {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::WhileItem>) -> Result<Self, Self::Error> {
        Ok(Self {
            condition: try_convert_free_trail_expr(&value.val.condition)?,
            body: try_convert_free_trail_expr(&value.val.body)?,
        })
    }
}

impl TryFrom<&g::Ref<g::MatchItem>> for l::MatchItem {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::MatchItem>) -> Result<Self, Self::Error> {
        Ok(Self {
            expr: try_convert_free_trail_expr(&value.val.expr)?,
            cases: try_convert_list(&value.val.cases)?,
        })
    }
}

impl TryFrom<&g::Ref<g::MatchCase>> for l::MatchCase {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::MatchCase>) -> Result<Self, Self::Error> {
        Ok(Self {
            variant: convert(&value.val.variant),
            body: try_convert_free_trail_expr(&value.val.body)?,
        })
    }
}

impl From<&g::Ref<g::VariantLiteral>> for l::VariantLiteral {
    fn from(value: &g::Ref<g::VariantLiteral>) -> Self {
        Self {
            enum_name: convert_maybe(&value.val.enum_name).map(convert),
            variant_name: convert(&value.val.variant_name),
        }
    }
}

impl TryFrom<&g::Ref<g::Assign>> for l::Assign {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Assign>) -> Result<Self, Self::Error> {
        Ok(Self {
            place: try_convert(&value.val.place)?,
            expr: {
                let expr = unnest(&value.val.expr);
                match &expr.val {
                    g::Priority::First(expr) => try_convert(expr),
                    g::Priority::Second(expr) => try_convert(expr),
                }?
            },
        })
    }
}

fn try_convert_free_trail_expr(
    expr: &g::Ref<g::FreeTrailExpr>,
) -> Result<l::Ref<l::Expr>, CompileErrorSet> {
    match &unnest(expr).val {
        g::Priority::First(expr) => try_convert(expr),
        g::Priority::Second(expr) => try_convert(expr),
    }
}

fn try_convert_paren_trail_expr(
    expr: &g::Ref<g::ParenTrailExpr>,
) -> Result<l::Ref<l::Expr>, CompileErrorSet> {
    match &expr.val {
        g::Priority::First(expr) => try_convert(expr),
        g::Priority::Second(expr) => try_convert_free_trail_expr(expr),
    }
}

impl TryFrom<&g::Ref<g::FunctionCall>> for l::FunctionCall {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::FunctionCall>) -> Result<Self, Self::Error> {
        Ok(Self {
            func_name: convert(&value.val.func_name),
            param_exprs: {
                let list = &value.val.param_exprs;
                let param_exprs = VecDeque::from_list(list);

                let param_exprs = param_exprs
                    .into_iter()
                    .map(|expr| try_convert_free_trail_expr(&expr))
                    .collect::<Result<Vec<_>, _>>()?;

                Arc::new(Srced { range: list.range, val: param_exprs })
            },
        })
    }
}

impl TryFrom<&g::Ref<g::ContainedExpr>> for l::Expr {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::ContainedExpr>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::ContainedExpr::Literal(x) => Self::Literal(try_convert(x)?),
            g::ContainedExpr::Place(x) => Self::Place(try_convert(x)?),
            g::ContainedExpr::Ref(x) => Self::Ref(try_convert(unnest(&x.val.place))?),
            g::ContainedExpr::Paren(x) => Self::Paren(try_convert(x)?),
            g::ContainedExpr::Cast(x) => {
                Self::Cast { ty: try_convert(&x.val.ty)?, expr: try_convert(unnest(&x.val.item))? }
            },
            g::ContainedExpr::Transmute(x) => Self::Transmute {
                ty: try_convert(&x.val.ty)?,
                expr: try_convert(unnest(&x.val.item))?,
            },
            g::ContainedExpr::Call(x) => Self::Call(try_convert(x)?),
            g::ContainedExpr::Match(x) => Self::Match(try_convert(x)?),
            g::ContainedExpr::Block(x) => Self::Block(try_convert(x)?),
            g::ContainedExpr::Struct(st) => Self::Struct(try_convert_list(&st.val.fields)?),
            g::ContainedExpr::Undefined(_) => Self::Undefined,
            g::ContainedExpr::Array(array) => {
                let assign_exprs = VecDeque::from_list(&array.val.elements);

                let mut singles = Vec::new();
                let mut spread: Option<g::Ref<g::FreeTrailExpr>> = None;
                for assign_expr in assign_exprs {
                    if spread.is_some() {
                        return Err(CompileErrorSet::new_error(
                            assign_expr.range,
                            CompileError::Logic(LogicError::ElementAfterSpread),
                        ));
                    }

                    match &assign_expr.val {
                        g::ArrayAssignExpr::Single(x) => singles.push(x.clone()),
                        g::ArrayAssignExpr::Spread(x) => spread = Some(x.val.expr.clone()),
                    }
                }

                let single_exprs = singles
                    .into_iter()
                    .map(|x| try_convert_free_trail_expr(&x))
                    .collect::<Result<Vec<_>, _>>()?;

                let spread_expr = spread.map(|x| try_convert_free_trail_expr(&x)).transpose()?;

                Self::Array {
                    single_exprs: Arc::new(Srced {
                        range: single_exprs
                            .iter()
                            .fold(Default::default(), |acc, x| acc.merge(x.range)),
                        val: single_exprs,
                    }),
                    spread_expr,
                }
            },
        })
    }
}

impl TryFrom<&g::Ref<g::ElseItem>> for l::ElseItem {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::ElseItem>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::ElseItem::Body(x) => l::ElseItem { body: try_convert_free_trail_expr(&x.val.body)? },
            g::ElseItem::If(x) => l::ElseItem {
                body: Arc::new(Srced {
                    range: x.range,
                    val: l::Expr::Block(Arc::new(Srced {
                        range: x.range,
                        val: l::Block {
                            items: Arc::new(Srced {
                                range: x.val.if_item.range,
                                val: l::Trail {
                                    trailing: false,
                                    items: Arc::new(Srced {
                                        range: x.val.if_item.range,
                                        val: Vec::from([Arc::new(Srced {
                                            range: x.val.if_item.range,
                                            val: l::Expr::If(try_convert(&x.val.if_item)?),
                                        })]),
                                    }),
                                },
                            }),
                        },
                    })),
                }),
            },
        })
    }
}

impl TryFrom<&g::Ref<g::StructAssignField>> for l::StructAssignField {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::StructAssignField>) -> Result<Self, Self::Error> {
        Ok(Self {
            name: convert(&value.val.name),
            assign: try_convert_free_trail_expr(&value.val.assign)?,
        })
    }
}

impl TryFrom<&g::Ref<g::Place>> for l::Place {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Place>) -> Result<Self, Self::Error> {
        let mut index_chain = Vec::new();
        let mut index_link = convert_maybe(&value.val.index_link);

        while let Some(link) = index_link {
            index_chain.push(try_convert(&link.val.index)?);
            index_link = convert_maybe(&link.val.next_link);
        }

        let index_chain = Arc::new(Srced {
            range: index_chain.iter().fold(SrcRange::default(), |acc, x| acc.merge(x.range)),
            val: index_chain,
        });

        Ok(Self { head: try_convert(unnest(&value.val.head))?, index_chain })
    }
}

impl TryFrom<&g::Ref<g::PlaceIndex>> for l::PlaceIndex {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::PlaceIndex>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::PlaceIndex::Offset(x) => Self::Offset(try_convert_free_trail_expr(&x.val.expr)?),
            g::PlaceIndex::Field(x) => Self::Field(convert(&x.val.name)),
        })
    }
}

impl TryFrom<&g::Ref<g::Literal>> for l::Literal {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Literal>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::Literal::Str(x) => l::Literal::Str(x.val.str.clone()),
            g::Literal::Num(x) => {
                let num: f64 = parse_num_literal(x)?;
                if num.is_integer().not() {
                    Self::Num(num)
                } else {
                    match num.is_sign_positive() {
                        false => Self::Int(num),
                        true => Self::Uint(num),
                    }
                }
            },
            g::Literal::Bool(x) => match &x.val {
                g::BoolLiteral::True(_) => l::Literal::Bool(true),
                g::BoolLiteral::False(_) => l::Literal::Bool(false),
            },
            g::Literal::Variant(x) => l::Literal::Variant(convert(x)),
        })
    }
}

fn parse_num_literal<T: FromStr>(lit: &g::Ref<g::NumLiteral>) -> Result<T, CompileErrorSet> {
    let num = display_num_literal(&lit.val);
    match num.parse() {
        Ok(t) => Ok(t),
        Err(_) => Err(CompileErrorSet::new_error(
            lit.range,
            CompileError::Logic(LogicError::InvalidNumLiteral),
        )),
    }
}

fn display_num_literal(lit: &g::NumLiteral) -> String {
    let mut num = String::new();

    if convert_maybe(&lit.negative).is_some() {
        num += "-";
    }

    num += &lit.int.val.digits;

    if let Some(dec) = convert_maybe(&lit.dec) {
        num += ".";
        num += &dec.val.digits.val.digits;
    }

    num
}

impl TryFrom<&g::Ref<g::ParenExpr>> for l::ParenExpr {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::ParenExpr>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::ParenExpr::Unary(x) => Self::Unary(try_convert(x)?),
            g::ParenExpr::Binary(x) => Self::Binary(try_convert(x)?),
        })
    }
}

impl TryFrom<&g::Ref<g::PlaceHead>> for l::PlaceHead {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::PlaceHead>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::PlaceHead::Ident(ident) => Self::Ident(convert(ident)),
            g::PlaceHead::Deref(deref) => Self::Deref(try_convert(deref)?),
        })
    }
}

fn unnest<T>(nest: &g::Ref<g::ParensNest<T>>) -> &g::Ref<T> {
    match &nest.val {
        g::ParensNest::Root(x) => x,
        g::ParensNest::Wrapped(wrapped) => unnest(&wrapped.val.item),
    }
}

fn convert_maybe<T>(maybe: &g::Ref<g::Maybe<T>>) -> Option<&g::Ref<T>> {
    match &maybe.val {
        g::Maybe::Item(x) => Some(x),
        g::Maybe::Empty(_) => None,
    }
}

impl TryFrom<&g::Ref<g::UnaryParenExpr>> for l::UnaryParenExpr {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::UnaryParenExpr>) -> Result<Self, Self::Error> {
        Ok(Self {
            op: convert(&value.val.op),
            operand: try_convert_free_trail_expr(&value.val.operand)?,
        })
    }
}

impl TryFrom<&g::Ref<g::BinaryParenExpr>> for l::BinaryParenExpr {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::BinaryParenExpr>) -> Result<Self, Self::Error> {
        Ok(Self {
            left: try_convert_paren_trail_expr(&value.val.left)?,
            op: convert(&value.val.op),
            right: try_convert_paren_trail_expr(&value.val.right)?,
        })
    }
}

impl From<&g::Ref<g::Ident>> for l::Ident {
    fn from(value: &g::Ref<g::Ident>) -> Self {
        Self::User { name: convert(&value.val.name) }
    }
}

impl TryFrom<&g::Ref<g::Deref>> for l::Deref {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Deref>) -> Result<Self, Self::Error> {
        Ok(Self {
            addr: match &value.val.addr.val {
                g::Priority::First(expr) => try_convert(expr),
                g::Priority::Second(expr) => match &unnest(expr).val {
                    g::Priority::First(expr) => try_convert(expr),
                    g::Priority::Second(expr) => try_convert(expr),
                },
            }?,
        })
    }
}

impl From<&g::Ref<g::UnaryParenExprOp>> for l::UnaryParenExprOp {
    fn from(value: &g::Ref<g::UnaryParenExprOp>) -> Self {
        match &value.val {
            g::UnaryParenExprOp::Not(_) => Self::Not,
        }
    }
}

impl From<&g::Ref<g::BinaryParenExprOp>> for l::BinaryParenExprOp {
    fn from(value: &g::Ref<g::BinaryParenExprOp>) -> Self {
        match &value.val {
            g::BinaryParenExprOp::Add(_) => Self::Add,
            g::BinaryParenExprOp::Sub(_) => Self::Sub,
            g::BinaryParenExprOp::Mul(_) => Self::Mul,
            g::BinaryParenExprOp::Div(_) => Self::Div,
            g::BinaryParenExprOp::Mod(_) => Self::Mod,
            g::BinaryParenExprOp::Eq(_) => Self::Eq,
            g::BinaryParenExprOp::Neq(_) => Self::Neq,
            g::BinaryParenExprOp::Gt(_) => Self::Gt,
            g::BinaryParenExprOp::Lt(_) => Self::Lt,
            g::BinaryParenExprOp::Gte(_) => Self::Gte,
            g::BinaryParenExprOp::Lte(_) => Self::Lte,
            g::BinaryParenExprOp::And(_) => Self::And,
            g::BinaryParenExprOp::Or(_) => Self::Or,
            g::BinaryParenExprOp::Join(_) => Self::Join,
        }
    }
}
