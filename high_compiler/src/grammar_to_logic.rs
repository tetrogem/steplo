use std::collections::VecDeque;
use std::str::FromStr;
use std::sync::Arc;

use crate::compile_error::CompileError;
use crate::compile_error::CompileErrorSet;
use crate::compile_error::LogicError;
use crate::grammar_ast as g;
use crate::logic_ast as l;

trait FromList<T> {
    fn from_list(value: T) -> Self;
}

fn convert<From, To>(from: &g::Ref<From>) -> Arc<To>
where
    for<'a> &'a g::Ref<From>: Into<To>,
{
    Arc::new(from.into())
}

fn try_convert<From, To, Error>(from: &g::Ref<From>) -> Result<Arc<To>, Error>
where
    for<'a> &'a g::Ref<From>: TryInto<To, Error = Error>,
{
    from.try_into().map(Arc::new)
}

impl TryFrom<&g::Ref<g::Program>> for l::Program {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Program>) -> Result<Self, Self::Error> {
        Ok(Self {
            items: Arc::new(
                VecDeque::from_list(&value.val.items)
                    .into_iter()
                    .map(|x| try_convert(&x))
                    .collect::<Result<_, _>>()?,
            ),
        })
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
            g::TopItem::Main(main) => Self::Main(try_convert(main)?),
            g::TopItem::Func(func) => Self::Func(try_convert(func)?),
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
            params: Arc::new(
                VecDeque::from_list(&value.val.params)
                    .into_iter()
                    .map(|x| try_convert(&x))
                    .collect::<Result<_, _>>()?,
            ),
            proc: try_convert(&value.val.proc)?,
        })
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
        Ok(Self { name: convert(&value.val.name), ty: Arc::new((&value.val.ty).try_into()?) })
    }
}

impl TryFrom<&g::Ref<g::Type>> for l::Type {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Type>) -> Result<Self, Self::Error> {
        Ok(match &value.val {
            g::Type::Base(base) => Self::Base(try_convert(base)?),
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
            g::Type::Ref(ref_ty) => Self::Ref(try_convert(&ref_ty.val.ty)?),
        })
    }
}

impl TryFrom<&g::Ref<g::BaseType>> for l::BaseType {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::BaseType>) -> Result<Self, Self::Error> {
        let name = &value.val.name;
        Ok(match name.val.str.as_ref() {
            "any" => Self::Any,
            "val" => Self::Val,
            "num" => Self::Num,
            "int" => Self::Int,
            "uint" => Self::Uint,
            "bool" => Self::Bool,
            _ => {
                return Err(CompileErrorSet::new_error(
                    name.range,
                    CompileError::Convert(LogicError::InvalidType),
                ));
            },
        })
    }
}

impl TryFrom<&g::Ref<g::Proc>> for l::Proc {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Proc>) -> Result<Self, Self::Error> {
        Ok(Self {
            idents: Arc::new(
                VecDeque::from_list(&value.val.idents)
                    .into_iter()
                    .map(|x| try_convert(&x))
                    .collect::<Result<_, _>>()?,
            ),
            body: try_convert(&value.val.body)?,
        })
    }
}

impl TryFrom<&g::Ref<g::Body>> for l::Body {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Body>) -> Result<Self, Self::Error> {
        Ok(Self {
            items: Arc::new(
                VecDeque::from_list(&value.val.items)
                    .into_iter()
                    .map(|x| try_convert(&x))
                    .collect::<Result<_, _>>()?,
            ),
        })
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
        Ok(Self { place: try_convert(&value.val.place)?, expr: try_convert(&value.val.expr)? })
    }
}

impl TryFrom<&g::Ref<g::FunctionCall>> for l::FunctionCall {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::FunctionCall>) -> Result<Self, Self::Error> {
        Ok(Self {
            func_name: convert(&value.val.func_name),
            param_exprs: Arc::new(
                VecDeque::from_list(&value.val.param_exprs)
                    .into_iter()
                    .map(|x| try_convert(&x))
                    .collect::<Result<_, _>>()?,
            ),
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
            g::Expr::Cast(x) => Self::Cast {
                ty: Arc::new((&x.val.ty).try_into()?),
                expr: try_convert(unnest(&x.val.item))?,
            },
            g::Expr::Transmute(x) => Self::Transmute {
                ty: Arc::new((&x.val.ty).try_into()?),
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
                body: Arc::new(l::Body {
                    items: Arc::new(Vec::from([Arc::new(l::BodyItem::If(try_convert(
                        &x.val.if_item,
                    )?))])),
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
            g::AssignExpr::Span(span) => Self::Span(Arc::new(
                VecDeque::from_list(&span.val.elements)
                    .into_iter()
                    .map(|x| try_convert(&x))
                    .collect::<Result<_, _>>()?,
            )),
            g::AssignExpr::Slice(slice) => {
                let start_in = convert_maybe(&slice.val.start_in);

                let start_in = match start_in {
                    None => 0,
                    Some(x) => match parse_num_literal(x) {
                        Ok(x) => x,
                        Err(_) => {
                            return Err(CompileErrorSet::new_error(
                                x.range,
                                CompileError::Convert(LogicError::InvalidRangeStartIncl),
                            ));
                        },
                    },
                };

                let end_ex = &slice.val.end_ex;
                let Ok(end_ex) = parse_num_literal(end_ex) else {
                    return Err(CompileErrorSet::new_error(
                        end_ex.range,
                        CompileError::Convert(LogicError::InvalidRangeEndExcl),
                    ));
                };

                Self::Slice { place: try_convert(&slice.val.place)?, start_in, end_ex }
            },
        })
    }
}

impl TryFrom<&g::Ref<g::Place>> for l::Place {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Place>) -> Result<Self, Self::Error> {
        Ok(Self {
            head: try_convert(unnest(&value.val.head))?,
            offset: match convert_maybe(&value.val.offset) {
                None => None,
                Some(offset) => Some(try_convert(offset)?),
            },
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

impl TryFrom<&g::Ref<g::Offset>> for l::Offset {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::Offset>) -> Result<Self, Self::Error> {
        Ok(Self { expr: try_convert(&value.val.expr)? })
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
        Ok(Self { op: *convert(&value.val.op), operand: try_convert(&value.val.operand)? })
    }
}

impl TryFrom<&g::Ref<g::BinaryParenExpr>> for l::BinaryParenExpr {
    type Error = CompileErrorSet;

    fn try_from(value: &g::Ref<g::BinaryParenExpr>) -> Result<Self, Self::Error> {
        Ok(Self {
            left: try_convert(&value.val.left)?,
            op: *convert(&value.val.op),
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
