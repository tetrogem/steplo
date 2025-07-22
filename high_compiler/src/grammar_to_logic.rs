use std::collections::VecDeque;
use std::sync::Arc;

use anyhow::Context;

use crate::grammar_ast as g;
use crate::logic_ast as l;

fn convert<From, To>(from: impl AsRef<From>) -> Arc<To>
where
    for<'a> &'a From: Into<To>,
{
    Arc::new(from.as_ref().into())
}

fn try_convert<From, To, Error>(from: impl AsRef<From>) -> Result<Arc<To>, Error>
where
    for<'a> &'a From: TryInto<To, Error = Error>,
{
    from.as_ref().try_into().map(Arc::new)
}

impl TryFrom<&g::Program> for l::Program {
    type Error = anyhow::Error;

    fn try_from(value: &g::Program) -> Result<Self, Self::Error> {
        Ok(Self {
            items: Arc::new(
                VecDeque::from(value.items.as_ref())
                    .into_iter()
                    .map(try_convert)
                    .collect::<Result<_, _>>()?,
            ),
        })
    }
}

impl<T> From<&g::List<T>> for VecDeque<Arc<T>> {
    fn from(value: &g::List<T>) -> Self {
        match value {
            g::List::Empty(_) => Default::default(),
            g::List::Link(link) => {
                let mut vec = VecDeque::from(link.next.as_ref());
                vec.push_front(link.item.clone());
                vec
            },
        }
    }
}

impl<T> From<&g::CommaList<T>> for VecDeque<Arc<T>> {
    fn from(value: &g::CommaList<T>) -> Self {
        match value {
            g::CommaList::Empty(_) => Default::default(),
            g::CommaList::Tail(tail) => VecDeque::from([tail.clone()]),
            g::CommaList::Link(link) => {
                let mut vec = VecDeque::from(link.next.as_ref());
                vec.push_front(link.item.clone());
                vec
            },
        }
    }
}

impl<T> From<&g::SemiList<T>> for VecDeque<Arc<T>> {
    fn from(value: &g::SemiList<T>) -> Self {
        match value {
            g::SemiList::Empty(_) => Default::default(),
            g::SemiList::Tail(tail) => VecDeque::from([tail.clone()]),
            g::SemiList::Link(link) => {
                let mut vec = VecDeque::from(link.next.as_ref());
                vec.push_front(link.item.clone());
                vec
            },
        }
    }
}

impl TryFrom<&g::TopItem> for l::TopItem {
    type Error = anyhow::Error;

    fn try_from(value: &g::TopItem) -> Result<Self, Self::Error> {
        Ok(match value {
            g::TopItem::Main(main) => Self::Main(try_convert(main)?),
            g::TopItem::Func(func) => Self::Func(try_convert(func)?),
        })
    }
}

impl TryFrom<&g::Main> for l::Main {
    type Error = anyhow::Error;

    fn try_from(value: &g::Main) -> Result<Self, Self::Error> {
        Ok(Self { proc: try_convert(&value.proc)? })
    }
}

impl TryFrom<&g::Func> for l::Func {
    type Error = anyhow::Error;

    fn try_from(value: &g::Func) -> Result<Self, Self::Error> {
        Ok(Self {
            name: convert(&value.name),
            params: Arc::new(
                VecDeque::from(value.params.as_ref())
                    .into_iter()
                    .map(try_convert)
                    .collect::<Result<_, _>>()?,
            ),
            proc: try_convert(&value.proc)?,
        })
    }
}

impl From<&g::Name> for l::Name {
    fn from(value: &g::Name) -> Self {
        Self { str: value.str.clone() }
    }
}

impl TryFrom<&g::IdentDeclaration> for l::IdentDeclaration {
    type Error = anyhow::Error;

    fn try_from(value: &g::IdentDeclaration) -> Result<Self, Self::Error> {
        Ok(Self { name: convert(&value.name), ty: try_convert(&value.ty)? })
    }
}

impl TryFrom<&g::Type> for l::Type {
    type Error = anyhow::Error;

    fn try_from(value: &g::Type) -> Result<Self, Self::Error> {
        Ok(match value {
            g::Type::Base(base) => Self::Base(convert(base)),
            g::Type::Array(array) => Self::Array(try_convert(array)?),
            g::Type::Ref(ref_ty) => Self::Ref(try_convert(ref_ty)?),
        })
    }
}

impl From<&g::BaseType> for l::BaseType {
    fn from(value: &g::BaseType) -> Self {
        Self { name: convert(&value.name) }
    }
}

impl TryFrom<&g::ArrayType> for l::ArrayType {
    type Error = anyhow::Error;

    fn try_from(value: &g::ArrayType) -> Result<Self, Self::Error> {
        let len = &value.len.str;
        let len = len
            .parse()
            .context(format!("Array type length should be a positive integer; Found: {}", len))?;

        Ok(Self { ty: try_convert(&value.ty)?, len })
    }
}

impl TryFrom<&g::RefType> for l::RefType {
    type Error = anyhow::Error;

    fn try_from(value: &g::RefType) -> Result<Self, Self::Error> {
        Ok(Self { ty: try_convert(&value.ty)? })
    }
}

impl TryFrom<&g::Proc> for l::Proc {
    type Error = anyhow::Error;

    fn try_from(value: &g::Proc) -> Result<Self, Self::Error> {
        Ok(Self {
            idents: Arc::new(
                VecDeque::from(value.idents.as_ref())
                    .into_iter()
                    .map(try_convert)
                    .collect::<Result<_, _>>()?,
            ),
            body: try_convert(&value.body)?,
        })
    }
}

impl TryFrom<&g::Body> for l::Body {
    type Error = anyhow::Error;

    fn try_from(value: &g::Body) -> Result<Self, Self::Error> {
        Ok(Self {
            items: Arc::new(
                VecDeque::from(value.items.as_ref())
                    .into_iter()
                    .map(try_convert)
                    .collect::<Result<_, _>>()?,
            ),
        })
    }
}

impl TryFrom<&g::BodyItem> for l::BodyItem {
    type Error = anyhow::Error;

    fn try_from(value: &g::BodyItem) -> Result<Self, Self::Error> {
        Ok(match value {
            g::BodyItem::Statement(x) => Self::Statement(try_convert(x)?),
            g::BodyItem::If(x) => Self::If(try_convert(x)?),
            g::BodyItem::While(x) => Self::While(try_convert(x)?),
        })
    }
}

impl TryFrom<&g::Statement> for l::Statement {
    type Error = anyhow::Error;

    fn try_from(value: &g::Statement) -> Result<Self, Self::Error> {
        Ok(match value {
            g::Statement::Assign(x) => Self::Assign(try_convert(x)?),
            g::Statement::Call(x) => Self::Call(try_convert(x)?),
        })
    }
}

impl TryFrom<&g::IfItem> for l::IfItem {
    type Error = anyhow::Error;

    fn try_from(value: &g::IfItem) -> Result<Self, Self::Error> {
        Ok(Self {
            condition: convert(&value.condition),
            then_body: try_convert(&value.then_body)?,
            else_item: match convert_maybe(&value.else_item) {
                None => None,
                Some(x) => Some(try_convert(x)?),
            },
        })
    }
}

impl TryFrom<&g::WhileItem> for l::WhileItem {
    type Error = anyhow::Error;

    fn try_from(value: &g::WhileItem) -> Result<Self, Self::Error> {
        Ok(Self { condition: convert(&value.condition), body: try_convert(&value.body)? })
    }
}

impl TryFrom<&g::Assign> for l::Assign {
    type Error = anyhow::Error;

    fn try_from(value: &g::Assign) -> Result<Self, Self::Error> {
        Ok(Self { place: convert(&value.place), expr: try_convert(&value.expr)? })
    }
}

impl TryFrom<&g::FunctionCall> for l::FunctionCall {
    type Error = anyhow::Error;

    fn try_from(value: &g::FunctionCall) -> Result<Self, Self::Error> {
        Ok(Self {
            func_name: convert(&value.func_name),
            param_exprs: Arc::new(
                VecDeque::from(value.param_exprs.as_ref())
                    .into_iter()
                    .map(try_convert)
                    .collect::<Result<_, _>>()?,
            ),
        })
    }
}

impl From<&g::Expr> for l::Expr {
    fn from(value: &g::Expr) -> Self {
        match value {
            g::Expr::Literal(x) => Self::Literal(convert(x)),
            g::Expr::Place(x) => Self::Place(convert(x)),
            g::Expr::Ref(x) => Self::Ref(convert(x)),
            g::Expr::Paren(x) => Self::Paren(convert(x)),
        }
    }
}

impl TryFrom<&g::ElseItem> for l::ElseItem {
    type Error = anyhow::Error;

    fn try_from(value: &g::ElseItem) -> Result<Self, Self::Error> {
        Ok(match value {
            g::ElseItem::Body(x) => l::ElseItem { body: try_convert(&x.body)? },
            g::ElseItem::If(x) => l::ElseItem {
                body: Arc::new(l::Body {
                    items: Arc::new(Vec::from([Arc::new(l::BodyItem::If(try_convert(
                        &x.if_item,
                    )?))])),
                }),
            },
        })
    }
}

impl TryFrom<&g::AssignExpr> for l::AssignExpr {
    type Error = anyhow::Error;

    fn try_from(value: &g::AssignExpr) -> Result<Self, Self::Error> {
        Ok(match value {
            g::AssignExpr::Expr(expr) => Self::Expr(convert(expr)),
            g::AssignExpr::Span(span) => Self::Span(convert(span)),
            g::AssignExpr::Slice(slice) => Self::Slice(try_convert(slice)?),
        })
    }
}

impl From<&g::Place> for l::Place {
    fn from(value: &g::Place) -> Self {
        Self {
            head: convert(unnest(&value.head)),
            offset: convert_maybe(&value.offset).map(convert),
        }
    }
}

impl From<&g::Literal> for l::Literal {
    fn from(value: &g::Literal) -> Self {
        Self { str: value.str.clone() }
    }
}

impl From<&g::RefExpr> for l::RefExpr {
    fn from(value: &g::RefExpr) -> Self {
        Self { place: convert(unnest(&value.place)) }
    }
}

impl From<&g::ParenExpr> for l::ParenExpr {
    fn from(value: &g::ParenExpr) -> Self {
        match value {
            g::ParenExpr::Unary(x) => Self::Unary(convert(x)),
            g::ParenExpr::Binary(x) => Self::Binary(convert(x)),
        }
    }
}

impl From<&g::Span> for l::Span {
    fn from(value: &g::Span) -> Self {
        Self {
            elements: Arc::new(
                VecDeque::from(value.elements.as_ref()).into_iter().map(convert).collect(),
            ),
        }
    }
}

impl TryFrom<&g::Slice> for l::Slice {
    type Error = anyhow::Error;

    fn try_from(value: &g::Slice) -> Result<Self, Self::Error> {
        let start_in = convert_maybe(value.start_in.as_ref()).map(convert::<_, l::Literal>);

        let start_in = match start_in {
            None => 0,
            Some(x) => x
                .str
                .parse()
                .context("Inclusive start of slice range should be a positive integer")?,
        };

        let end_ex = value
            .end_ex
            .str
            .parse()
            .context("Exclusive end of slice range should be a positive integer")?;

        Ok(Self { place: convert(&value.place), start_in, end_ex })
    }
}

impl From<&g::PlaceHead> for l::PlaceHead {
    fn from(value: &g::PlaceHead) -> Self {
        match value {
            g::PlaceHead::Ident(ident) => Self::Ident(convert(ident)),
            g::PlaceHead::Deref(deref) => Self::Deref(convert(deref)),
        }
    }
}

fn unnest<T>(nest: &g::ParensNest<T>) -> &Arc<T> {
    match nest {
        g::ParensNest::Root(x) => x,
        g::ParensNest::Wrapped(wrapped) => unnest(&wrapped.item),
    }
}

impl From<&g::Offset> for l::Offset {
    fn from(value: &g::Offset) -> Self {
        Self { expr: convert(&value.expr) }
    }
}

fn convert_maybe<T>(maybe: &g::Maybe<T>) -> Option<&Arc<T>> {
    match maybe {
        g::Maybe::Item(x) => Some(x),
        g::Maybe::Empty(_) => None,
    }
}

impl From<&g::UnaryParenExpr> for l::UnaryParenExpr {
    fn from(value: &g::UnaryParenExpr) -> Self {
        Self { op: value.op.into(), operand: convert(&value.operand) }
    }
}

impl From<&g::BinaryParenExpr> for l::BinaryParenExpr {
    fn from(value: &g::BinaryParenExpr) -> Self {
        Self { left: convert(&value.left), op: value.op.into(), right: convert(&value.right) }
    }
}

impl From<&g::Ident> for l::Ident {
    fn from(value: &g::Ident) -> Self {
        Self { name: convert(&value.name) }
    }
}

impl From<&g::Deref> for l::Deref {
    fn from(value: &g::Deref) -> Self {
        Self { addr: convert(&value.addr) }
    }
}

impl From<g::UnaryParenExprOp> for l::UnaryParenExprOp {
    fn from(value: g::UnaryParenExprOp) -> Self {
        match value {
            g::UnaryParenExprOp::Not(_) => Self::Not,
        }
    }
}

impl From<g::BinaryParenExprOp> for l::BinaryParenExprOp {
    fn from(value: g::BinaryParenExprOp) -> Self {
        match value {
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
