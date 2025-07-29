use std::collections::VecDeque;
use std::str::FromStr;
use std::sync::Arc;

use crate::compile_error::CompileError;
use crate::compile_error::CompileErrorSet;
use crate::compile_error::LogicError;
use crate::grammar_ast as g;
use crate::high_compiler::srced::SrcRange;
use crate::logic_ast as l;
use crate::srced::Srced;

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
        })
    }
}

impl TryFrom<&g::Ref<g::Main>> for l::Main {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Main>) -> Result<Self, Self::Error> {
        Ok(Self { proc: try_convert(&value.val.proc)? })
    }
}

impl TryFrom<&g::Ref<g::Func>> for l::Func {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Func>) -> Result<Self, Self::Error> {
        Ok(Self {
            name: convert(&value.val.name),
            params: try_convert_list(&value.val.params)?,
            proc: try_convert(&value.val.proc)?,
        })
    }
}

impl TryFrom<&g::Ref<g::TypeAlias>> for l::TypeAlias {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::TypeAlias>) -> Result<Self, Self::Error> {
        Ok(Self { name: convert(&value.val.name), ty: try_convert(&value.val.ty)? })
    }
}

impl From<&g::Ref<g::Name>> for l::Name {
    fn from(value: &g::Ref<g::Name>) -> Self {
        Self { str: value.val.str.clone() }
    }
}

impl TryFrom<&g::Ref<g::IdentDeclaration>> for l::IdentDeclaration {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::IdentDeclaration>) -> Result<Self, Self::Error> {
        Ok(Self { name: convert(&value.val.name), ty: try_convert(&value.val.ty)? })
    }
}

impl TryFrom<&g::Ref<g::Type>> for l::TypeHint {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Type>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::Type::Base(base) => base.into(),
            g::Type::Ref(ref_ty) => Self::Ref(try_convert(&ref_ty.val.ty)?),
            g::Type::Array(array) => {
                let len = &array.val.len;
                let Ok(len) = parse_num_literal(&array.val.len) else {
                    return Err(CompileErrorSet::new_error(
                        len.range,
                        CompileError::Convert(LogicError::InvalidArrayTypeLen),
                    ));
                };

                Self::Array { ty: try_convert(&array.val.ty)?, len }
            },
            g::Type::Struct(struct_ty) => {
                let fields: l::Ref<Vec<l::Ref<l::IdentDeclaration>>> =
                    try_convert_list(&struct_ty.val.fields)?;

                let fields = fields
                    .val
                    .iter()
                    .map(|field| {
                        Ok(Arc::new(Srced {
                            range: field.range,
                            val: l::FieldTypeHint {
                                name: field.val.name.clone(),
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

impl From<&g::Ref<g::BaseType>> for l::TypeHint {
    fn from(value: &g::Ref<g::BaseType>) -> Self {
        let name = &value.val.name;
        match name.val.str.as_ref() {
            "any" => Self::Any,
            "val" => Self::Primitive(l::PrimitiveType::Val),
            "num" => Self::Primitive(l::PrimitiveType::Num),
            "int" => Self::Primitive(l::PrimitiveType::Int),
            "uint" => Self::Primitive(l::PrimitiveType::Uint),
            "bool" => Self::Primitive(l::PrimitiveType::Bool),
            _ => Self::Alias(convert(name)),
        }
    }
}

impl TryFrom<&g::Ref<g::Proc>> for l::Proc {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Proc>) -> Result<Self, Self::Error> {
        Ok(Self {
            idents: try_convert_list(&value.val.idents)?,
            body: try_convert(&value.val.body)?,
        })
    }
}

impl TryFrom<&g::Ref<g::Body>> for l::Body {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Body>) -> Result<Self, Self::Error> {
        Ok(Self { items: try_convert_list(&value.val.items)? })
    }
}

impl TryFrom<&g::Ref<g::BodyItem>> for l::BodyItem {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::BodyItem>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::BodyItem::Statement(x) => Self::Statement(try_convert(x)?),
            g::BodyItem::If(x) => Self::If(try_convert(x)?),
            g::BodyItem::While(x) => Self::While(try_convert(x)?),
        })
    }
}

impl TryFrom<&g::Ref<g::StatementItem>> for l::Statement {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::StatementItem>) -> Result<Self, Self::Error> {
        (&value.val.statement).try_into()
    }
}

impl TryFrom<&g::Ref<g::Statement>> for l::Statement {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Statement>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::Statement::Assign(x) => Self::Assign(try_convert(x)?),
            g::Statement::Call(x) => Self::Call(try_convert(x)?),
        })
    }
}

impl TryFrom<&g::Ref<g::IfItem>> for l::IfItem {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::IfItem>) -> Result<Self, Self::Error> {
        Ok(Self {
            condition: try_convert(&value.val.condition)?,
            then_body: try_convert(&value.val.then_body)?,
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
            condition: try_convert(&value.val.condition)?,
            body: try_convert(&value.val.body)?,
        })
    }
}

impl TryFrom<&g::Ref<g::Assign>> for l::Assign {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Assign>) -> Result<Self, Self::Error> {
        Ok(Self {
            place: try_convert(&value.val.place)?,
            expr: try_convert(unnest(&value.val.expr))?,
        })
    }
}

impl TryFrom<&g::Ref<g::FunctionCall>> for l::FunctionCall {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::FunctionCall>) -> Result<Self, Self::Error> {
        Ok(Self {
            func_name: convert(&value.val.func_name),
            param_exprs: try_convert_list(&value.val.param_exprs)?,
        })
    }
}

impl TryFrom<&g::Ref<g::Expr>> for l::Expr {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Expr>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::Expr::Literal(x) => Self::Literal(try_convert(x)?),
            g::Expr::Place(x) => Self::Place(try_convert(x)?),
            g::Expr::Ref(x) => Self::Ref(try_convert(unnest(&x.val.place))?),
            g::Expr::Paren(x) => Self::Paren(try_convert(x)?),
            g::Expr::Cast(x) => {
                Self::Cast { ty: try_convert(&x.val.ty)?, expr: try_convert(unnest(&x.val.item))? }
            },
            g::Expr::Transmute(x) => Self::Transmute {
                ty: try_convert(&x.val.ty)?,
                expr: try_convert(unnest(&x.val.item))?,
            },
        })
    }
}

impl TryFrom<&g::Ref<g::ElseItem>> for l::ElseItem {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::ElseItem>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::ElseItem::Body(x) => l::ElseItem { body: try_convert(&x.val.body)? },
            g::ElseItem::If(x) => l::ElseItem {
                body: Arc::new(Srced {
                    range: x.range,
                    val: l::Body {
                        items: Arc::new(Srced {
                            range: x.val.if_item.range,
                            val: Vec::from([Arc::new(Srced {
                                range: x.val.if_item.range,
                                val: l::BodyItem::If(try_convert(&x.val.if_item)?),
                            })]),
                        }),
                    },
                }),
            },
        })
    }
}

impl TryFrom<&g::Ref<g::AssignExpr>> for l::AssignExpr {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::AssignExpr>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::AssignExpr::Expr(expr) => Self::Expr(try_convert(expr)?),
            g::AssignExpr::Struct(st) => Self::Struct(try_convert_list(&st.val.fields)?),
            g::AssignExpr::Array(array) => {
                let assign_exprs = VecDeque::from_list(&array.val.elements);

                let mut singles = Vec::new();
                let mut spread: Option<g::Ref<g::AssignExpr>> = None;
                for assign_expr in assign_exprs {
                    if spread.is_some() {
                        return Err(CompileErrorSet::new_error(
                            assign_expr.range,
                            CompileError::Convert(LogicError::ElementAfterSpread),
                        ));
                    }

                    match &assign_expr.val {
                        g::ArrayAssignExpr::Single(x) => singles.push(x.clone()),
                        g::ArrayAssignExpr::Spread(x) => spread = Some(x.val.expr.clone()),
                    }
                }

                let single_exprs =
                    singles.into_iter().map(|x| try_convert(&x)).collect::<Result<Vec<_>, _>>()?;

                let spread_expr = spread.map(|x| try_convert(&x)).transpose()?;

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

impl TryFrom<&g::Ref<g::StructAssignField>> for l::StructAssignField {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::StructAssignField>) -> Result<Self, Self::Error> {
        Ok(Self { name: convert(&value.val.name), assign: try_convert(&value.val.assign)? })
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
            g::PlaceIndex::Offset(x) => Self::Offset(try_convert(&x.val.expr)?),
            g::PlaceIndex::Field(x) => Self::Field(convert(&x.val.name)),
        })
    }
}

impl TryFrom<&g::Ref<g::Literal>> for l::Literal {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Literal>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::Literal::Str(x) => l::Literal::Val(x.val.str.clone()),
            g::Literal::Num(x) => {
                let num: f64 = parse_num_literal(x)?;
                if num.is_infinite() || num.is_nan() || num.is_subnormal() || num.round() != num {
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
        })
    }
}

fn parse_num_literal<T: FromStr>(lit: &g::Ref<g::NumLiteral>) -> Result<T, CompileErrorSet> {
    let num = display_num_literal(&lit.val);
    match num.parse() {
        Ok(t) => Ok(t),
        Err(_) => Err(CompileErrorSet::new_error(
            lit.range,
            CompileError::Convert(LogicError::InvalidNumLiteral),
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
        Ok(Self { op: convert(&value.val.op), operand: try_convert(&value.val.operand)? })
    }
}

impl TryFrom<&g::Ref<g::BinaryParenExpr>> for l::BinaryParenExpr {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::BinaryParenExpr>) -> Result<Self, Self::Error> {
        Ok(Self {
            left: try_convert(&value.val.left)?,
            op: convert(&value.val.op),
            right: try_convert(&value.val.right)?,
        })
    }
}

impl From<&g::Ref<g::Ident>> for l::Ident {
    fn from(value: &g::Ref<g::Ident>) -> Self {
        Self { name: convert(&value.val.name) }
    }
}

impl TryFrom<&g::Ref<g::Deref>> for l::Deref {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Deref>) -> Result<Self, Self::Error> {
        Ok(Self { addr: try_convert(&value.val.addr)? })
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
