use std::{
    cmp::Ordering,
    ops::{Add, Div, Mul, Neg, Not, Rem, Sub},
    str::FromStr,
    sync::Arc,
};

use anyhow::bail;

use crate::{
    inline::{
        ast::{BinaryArgs, Expr, Loc, Value},
        opt::{MaybeOptimized, OptimizationFn, tracked_exhaust_optimize},
    },
    utils::IsInteger,
};

pub fn optimize_expr(expr: &Arc<Expr>) -> MaybeOptimized<Arc<Expr>> {
    let mut optimized = false;

    macro_rules! expr {
        ($expr:expr) => {
            tracked_exhaust_optimize(&mut optimized, $expr.clone(), |expr| optimize_expr(&expr))
        };
    }

    macro_rules! args {
        ($args:expr) => {
            Arc::new(BinaryArgs { left: expr!(&$args.left), right: expr!(&$args.right) })
        };
    }

    let mut expr = match expr.as_ref() {
        Expr::Loc(loc) => {
            let loc = match loc.as_ref() {
                Loc::Temp(temp) => Loc::Temp(temp.clone()),
                Loc::Deref(addr) => Loc::Deref(expr!(addr)),
            };

            Arc::new(Expr::Loc(Arc::new(loc)))
        },
        Expr::StackAddr(addr) => Arc::new(Expr::StackAddr(addr.clone())),
        Expr::Value(value) => Arc::new(Expr::Value(value.clone())),
        Expr::StdoutDeref(expr) => Arc::new(Expr::StdoutDeref(expr!(expr))),
        Expr::StdoutLen => Arc::new(Expr::StdoutLen),
        Expr::Timer => Arc::new(Expr::Timer),
        Expr::DaysSince2000 => Arc::new(Expr::DaysSince2000),
        Expr::Add(args) => Arc::new(Expr::Add(args!(args))),
        Expr::Sub(args) => Arc::new(Expr::Sub(args!(args))),
        Expr::Mul(args) => Arc::new(Expr::Mul(args!(args))),
        Expr::Div(args) => Arc::new(Expr::Div(args!(args))),
        Expr::Mod(args) => Arc::new(Expr::Mod(args!(args))),
        Expr::Eq(args) => Arc::new(Expr::Eq(args!(args))),
        Expr::Lt(args) => Arc::new(Expr::Lt(args!(args))),
        Expr::Gt(args) => Arc::new(Expr::Gt(args!(args))),
        Expr::Not(expr) => Arc::new(Expr::Not(expr!(expr))),
        Expr::Or(args) => Arc::new(Expr::Or(args!(args))),
        Expr::And(args) => Arc::new(Expr::And(args!(args))),
        Expr::InAnswer => Arc::new(Expr::InAnswer),
        Expr::Join(args) => Arc::new(Expr::Join(args!(args))),
        Expr::Random(args) => Arc::new(Expr::Random(args!(args))),
    };

    const OPTIMIZATIONS: [OptimizationFn<Arc<Expr>>; 1] = [optimization_const_evaluate_exprs];

    for optimization in OPTIMIZATIONS {
        expr = tracked_exhaust_optimize(&mut optimized, expr, |expr| optimization(&expr));
    }

    MaybeOptimized { optimized, val: expr }
}

fn optimization_const_evaluate_exprs(expr: &Arc<Expr>) -> MaybeOptimized<Arc<Expr>> {
    let mut optimized = false;

    let expr = match maybe_const_eval_expr(expr) {
        None => expr.clone(),
        Some(expr) => {
            optimized = true;
            expr
        },
    };

    MaybeOptimized { optimized, val: expr }
}

fn maybe_const_eval_expr(expr: &Arc<Expr>) -> Option<Arc<Expr>> {
    fn make_str_literal(str: &str) -> Arc<Expr> {
        Arc::new(Expr::Value(Arc::new(Value::Literal(str.into()))))
    }

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    struct Bool(bool);

    impl Bool {
        pub fn to_literal(self) -> Arc<Expr> {
            let lit = if self.0 { "true" } else { "false" };
            Arc::new(Expr::Value(Arc::new(Value::Literal(lit.into()))))
        }

        pub fn and(self, rhs: Self) -> Self {
            Self(self.0 && rhs.0)
        }

        pub fn or(self, rhs: Self) -> Self {
            Self(self.0 || rhs.0)
        }
    }

    impl FromStr for Bool {
        type Err = anyhow::Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(match s {
                "true" => Self(true),
                "false" => Self(false),
                _ => bail!("str is not \"true\" or \"false\""),
            })
        }
    }

    impl Not for Bool {
        type Output = Bool;

        fn not(self) -> Self::Output {
            Self(self.0.not())
        }
    }

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    struct Int(f64);

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    struct Float(f64);

    impl FromStr for Int {
        type Err = anyhow::Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            if s.contains('.') {
                bail!("Int literal cannot contain decimal");
            }

            let num: f64 = s.parse()?;
            if num.is_integer().not() {
                bail!("Int's f64 repr is not an integer")
            }

            Ok(Self(num))
        }
    }

    impl FromStr for Float {
        type Err = anyhow::Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(Self(s.parse()?))
        }
    }

    impl Int {
        pub fn to_literal(self) -> Arc<Expr> {
            make_str_literal(&format!("{}", self.0))
        }
    }

    impl Float {
        pub fn to_literal(self) -> Arc<Expr> {
            make_str_literal(&format!("{:?}", self.0))
        }
    }

    impl Add<Int> for Int {
        type Output = Int;

        fn add(self, rhs: Self) -> Self::Output {
            Self(self.0 + rhs.0)
        }
    }

    impl Add<Float> for Float {
        type Output = Float;

        fn add(self, rhs: Self) -> Self::Output {
            Self(self.0 + rhs.0)
        }
    }

    impl Sub<Int> for Int {
        type Output = Int;

        fn sub(self, rhs: Int) -> Self::Output {
            Self(self.0 - rhs.0)
        }
    }

    impl Sub<Float> for Float {
        type Output = Float;

        fn sub(self, rhs: Float) -> Self::Output {
            Self(self.0 - rhs.0)
        }
    }

    impl Mul<Int> for Int {
        type Output = Int;

        fn mul(self, rhs: Int) -> Self::Output {
            Self(self.0 * rhs.0)
        }
    }

    impl Mul<Float> for Float {
        type Output = Float;

        fn mul(self, rhs: Float) -> Self::Output {
            Self(self.0 * rhs.0)
        }
    }

    impl Div<Float> for Float {
        type Output = Float;

        fn div(self, rhs: Float) -> Self::Output {
            Self(self.0 / rhs.0)
        }
    }

    impl Rem<Float> for Float {
        type Output = Float;

        fn rem(self, rhs: Float) -> Self::Output {
            Self(self.0 % rhs.0)
        }
    }

    impl Neg for Int {
        type Output = Int;

        fn neg(self) -> Self::Output {
            Self(-self.0)
        }
    }

    impl Neg for Float {
        type Output = Float;

        fn neg(self) -> Self::Output {
            Self(-self.0)
        }
    }

    #[deprecated]
    fn is_staticly_comparable(expr: &Expr) -> bool {
        macro_rules! args {
            ($args:expr) => {
                is_staticly_comparable(&$args.left) && is_staticly_comparable(&$args.right)
            };
        }

        match expr {
            Expr::Loc(loc) => match loc.as_ref() {
                Loc::Temp(_) => true,
                Loc::Deref(addr) => is_staticly_comparable(addr),
            },
            Expr::StackAddr(_) => true,
            Expr::Value(_) => true,
            Expr::StdoutDeref(expr) => is_staticly_comparable(expr),
            Expr::StdoutLen => true,

            // may change between when checked on left and checked on right?
            // not sure, but better to be safe
            Expr::Timer => false,
            Expr::DaysSince2000 => false,

            Expr::Add(args) => args!(args),
            Expr::Sub(args) => args!(args),
            Expr::Mul(args) => args!(args),
            Expr::Div(args) => args!(args),
            Expr::Mod(args) => args!(args),
            Expr::Eq(args) => args!(args),
            Expr::Lt(args) => args!(args),
            Expr::Gt(args) => args!(args),
            Expr::Not(expr) => is_staticly_comparable(expr),
            Expr::Or(args) => args!(args),
            Expr::And(args) => args!(args),
            Expr::InAnswer => true,
            Expr::Join(args) => args!(args),
            Expr::Random(args) => args!(args),
        }
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    enum Equality {
        Eq,
        Neq(Option<Inequality>),
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    enum Inequality {
        Lt,
        Gt,
    }

    /// None = Cannot statically compare, Some = Result of static comparison
    fn staticly_compare(this: &Expr, other: &Expr) -> Option<Equality> {
        match (this, other) {
            (Expr::Value(this), Expr::Value(other)) => match (this.as_ref(), other.as_ref()) {
                (Value::Label(this), Value::Label(other)) => Some(match this == other {
                    true => Equality::Eq,
                    false => Equality::Neq(None),
                }),
                (Value::Literal(this), Value::Literal(other)) => 'lit: {
                    // scratch detects if both are numbers, and if so, compares them as numbers
                    if let Ok(this) = this.parse::<Int>()
                        && let Ok(other) = other.parse::<Int>()
                    {
                        break 'lit this.partial_cmp(&other).map(|ord| match ord {
                            Ordering::Equal => Equality::Eq,
                            Ordering::Greater => Equality::Neq(Some(Inequality::Gt)),
                            Ordering::Less => Equality::Neq(Some(Inequality::Lt)),
                        });
                    }

                    if let Ok(this) = this.parse::<Float>()
                        && let Ok(other) = other.parse::<Float>()
                    {
                        break 'lit this.partial_cmp(&other).map(|ord| match ord {
                            Ordering::Equal => Equality::Eq,
                            Ordering::Greater => Equality::Neq(Some(Inequality::Gt)),
                            Ordering::Less => Equality::Neq(Some(Inequality::Lt)),
                        });
                    }

                    // otherwise it just uses string comparison
                    Some(match this.cmp(other) {
                        Ordering::Equal => Equality::Eq,
                        Ordering::Greater => Equality::Neq(Some(Inequality::Gt)),
                        Ordering::Less => Equality::Neq(Some(Inequality::Lt)),
                    })
                },
                _ => None,
            },
            (Expr::Loc(this), Expr::Loc(other)) => match (this.as_ref(), other.as_ref()) {
                (Loc::Temp(this), Loc::Temp(other)) => Some(match this == other {
                    true => Equality::Eq,
                    false => Equality::Neq(None),
                }),
                (Loc::Deref(this), Loc::Deref(other)) => match staticly_compare(this, other) {
                    None => None,
                    Some(eq) => match eq {
                        Equality::Eq => Some(Equality::Eq), // same address must have same value
                        Equality::Neq(_) => None, // different address may or may not have eq value
                    },
                },
                _ => None,
            },
            (Expr::StackAddr(this), Expr::StackAddr(other)) => Some(match this == other {
                true => Equality::Eq,
                false => Equality::Neq(None),
            }),
            (Expr::StdoutDeref(this), Expr::StdoutDeref(other)) => staticly_compare(this, other),
            (Expr::StdoutLen, Expr::StdoutLen) => Some(Equality::Eq),
            (Expr::Not(this), Expr::Not(other)) => staticly_compare(this, other),
            (Expr::InAnswer, Expr::InAnswer) => Some(Equality::Eq),
            _ => None,
        }
    }

    match expr.as_ref() {
        Expr::Add(args) => {
            if let Some(a) =
                a_side_if_b_is(args, |expr| is_literal("0")(expr) || is_literal("0.0")(expr))
            {
                return Some(a.clone());
            }

            if let Some((a, b)) = lr_sides_filter_map(args, parse_filter_map::<Int>) {
                return Some((a + b).to_literal());
            }

            if let Some((a, b)) = lr_sides_filter_map(args, parse_filter_map::<Float>) {
                return Some((a + b).to_literal());
            }
        },
        Expr::Sub(args) => {
            if let Some(a) =
                l_side_if_r_is(args, |expr| is_literal("0")(expr) || is_literal("0.0")(expr))
            {
                return Some(a.clone());
            }

            if let Some((a, b)) = lr_sides_filter_map(args, parse_filter_map::<Int>) {
                return Some((a - b).to_literal());
            }

            if let Some((a, b)) = lr_sides_filter_map(args, parse_filter_map::<Float>) {
                return Some((a - b).to_literal());
            }
        },
        Expr::Mul(args) => {
            if a_side_if_b_is(args, is_literal("0")).is_some() {
                return Some(Int(0.).to_literal());
            }

            if a_side_if_b_is(args, is_literal("0.0")).is_some() {
                return Some(Float(0.).to_literal());
            }

            if let Some(a) =
                a_side_if_b_is(args, |expr| is_literal("1")(expr) || is_literal("1.0")(expr))
            {
                return Some(a.clone());
            }

            if let Some((a, b)) = lr_sides_filter_map(args, parse_filter_map::<Int>) {
                return Some((a * b).to_literal());
            }

            if let Some((a, b)) = lr_sides_filter_map(args, parse_filter_map::<Float>) {
                return Some((a * b).to_literal());
            }
        },
        Expr::Div(args) => {
            if let Some(other) =
                l_side_if_r_is(args, |expr| is_literal("1")(expr) || is_literal("1.0")(expr))
            {
                return Some(other.clone());
            }

            if let Some((a, b)) = lr_sides_filter_map(args, parse_filter_map::<Float>) {
                return Some((a / b).to_literal());
            }
        },
        Expr::Mod(args) => {
            if let Some((a, b)) = lr_sides_filter_map(args, parse_filter_map::<Float>) {
                let res = a % b;
                return Some(res.to_literal());
            }
        },
        Expr::Eq(args) => {
            if let Some(eq) = staticly_compare(&args.left, &args.right) {
                let eq = match eq {
                    Equality::Eq => true,
                    Equality::Neq(_) => false,
                };

                return Some(Bool(eq).to_literal());
            }
        },
        Expr::Lt(args) => {
            if let Some(eq) = staticly_compare(&args.left, &args.right) {
                let lt = match eq {
                    Equality::Eq => Some(false),
                    Equality::Neq(neq) => match neq {
                        None => None,
                        Some(Inequality::Gt) => Some(false),
                        Some(Inequality::Lt) => Some(true),
                    },
                };

                if let Some(lt) = lt {
                    return Some(Bool(lt).to_literal());
                }
            }
        },
        Expr::Gt(args) => {
            if let Some(eq) = staticly_compare(&args.left, &args.right) {
                let gt = match eq {
                    Equality::Eq => Some(false),
                    Equality::Neq(neq) => match neq {
                        None => None,
                        Some(Inequality::Gt) => Some(true),
                        Some(Inequality::Lt) => Some(false),
                    },
                };

                if let Some(gt) = gt {
                    return Some(Bool(gt).to_literal());
                }
            }
        },
        Expr::Not(expr) => {
            if let Some(bool) = parse_filter_map::<Bool>(expr) {
                return Some(bool.not().to_literal());
            }
        },
        Expr::And(args) => {
            if let Some((l, r)) = lr_sides_filter_map(args, parse_filter_map::<Bool>) {
                return Some(l.and(r).to_literal());
            }
        },
        Expr::Or(args) => {
            if let Some((l, r)) = lr_sides_filter_map(args, parse_filter_map::<Bool>) {
                return Some(l.or(r).to_literal());
            }
        },
        _ => {},
    }

    None
}

fn l_side_if_r_is(args: &BinaryArgs, predicate: impl Fn(&Arc<Expr>) -> bool) -> Option<&Arc<Expr>> {
    if predicate(&args.right) {
        return Some(&args.left);
    }

    None
}

fn a_side_if_b_is(args: &BinaryArgs, predicate: impl Fn(&Arc<Expr>) -> bool) -> Option<&Arc<Expr>> {
    if predicate(&args.left) {
        return Some(&args.right);
    }

    if predicate(&args.right) {
        return Some(&args.left);
    }

    None
}

fn lr_sides_filter_map<T>(
    args: &BinaryArgs,
    filter_map: impl Fn(&Arc<Expr>) -> Option<T>,
) -> Option<(T, T)> {
    let left = filter_map(&args.left)?;
    let right = filter_map(&args.right)?;
    Some((left, right))
}

fn ab_sides_filter_map<A, B>(
    args: &BinaryArgs,
    filter_map_a: impl Fn(&Arc<Expr>) -> Option<A>,
    filter_map_b: impl Fn(&Arc<Expr>) -> Option<B>,
) -> Option<(A, B)> {
    if let Some(a) = filter_map_a(&args.left)
        && let Some(b) = filter_map_b(&args.right)
    {
        return Some((a, b));
    }

    if let Some(b) = filter_map_b(&args.left)
        && let Some(a) = filter_map_a(&args.right)
    {
        return Some((a, b));
    }

    None
}

fn is_literal(expected: &str) -> impl Fn(&Arc<Expr>) -> bool {
    move |expr| {
        let Expr::Value(value) = expr.as_ref() else { return false };
        let Value::Literal(lit) = value.as_ref() else { return false };
        lit.as_ref() == expected
    }
}

fn parse_filter_map<T: FromStr>(expr: &Arc<Expr>) -> Option<T> {
    let Expr::Value(value) = expr.as_ref() else { return None };
    let Value::Literal(lit) = value.as_ref() else { return None };
    lit.parse::<T>().ok()
}
