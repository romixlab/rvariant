//! [rhai](https://rhai.rs) expressions over [`Variant`] values, e.g. for computed table columns.
//!
//! ```
//! use rvariant::{Variant, VariantTy, script::Expr};
//!
//! let expr = Expr::compile("qty * 2 + 1")?;
//! assert_eq!(expr.variables(), ["qty"]);
//! let out = expr.eval_as(|_| Some(Variant::u32(20)), &VariantTy::u32())?;
//! assert_eq!(out, Variant::u32(41));
//! # Ok::<(), rvariant::Error>(())
//! ```
//!
//! # Value mapping
//!
//! Simple kinds become native rhai values, so the whole rhai standard library works on them:
//!
//! | Variant | rhai |
//! |---|---|
//! | `Empty` | `()` |
//! | `Bool`, `Str` | `bool`, `string` |
//! | `Number` | `i64` (other integers must fit, otherwise an error), `f64` |
//! | `StrList`, `List` | array |
//! | `Map` | object map (keys must be strings, rhai sorts them by key) |
//! | `Binary` | blob |
//!
//! Values come back as `Empty` / `Bool` / `Str` / `I64` / `F64` / `List` / `Map` / `Binary`, use
//! [`Expr::eval_as`] to convert the result to a column type.
//!
//! All other kinds (`SI`, `SIRange`, `Enum`, `MultiSelectionEnum`, `Date`, `Time`, `DateTime`,
//! `Instant`, `Money`, `Tolerance`) stay a `Variant` custom type, keeping units, currency, etc.
//!
//! # Operators on rich values
//!
//! * A string operand is parsed as the type of the other operand: `r > "1 kΩ"`,
//!   `price + "5 EUR"`, `color == "red"`, `date >= "2024-01-01"`.
//!   The only exception is `"text" + value`, which concatenates.
//! * `SI ± SI` with compatible units (rescaled to the left unit), `SI * n`, `n * SI`, `SI / n`,
//!   `SI / SI` gives a plain ratio. Other unit arithmetic (`V / A`) is not supported.
//! * `Money ± Money` with the same currency (result has the larger precision),
//!   `Money / Money` gives a plain ratio. `Money * n` and `Money / n` round half away from zero
//!   to the money precision, except multiplication by an integer, which is exact.
//! * `Instant ± Instant`.
//! * Comparisons: `SI` (compatible units), `Money` (same currency), `Date`, `Time`, `DateTime`,
//!   `Instant`, `Enum` (by variant declaration order). `==` / `!=` work for all kinds of the
//!   same type.
//! * `x in range` for `SIRange` (respects inclusivity), `"Red" in multi_enum`.
//!
//! Integer arithmetic on rich values keeps the number type of the left operand and fails on
//! overflow; a division with a remainder produces `F64`.
//!
//! Properties: `kind` (all), `value` / `unit` / `base` (SI), `start` / `end` / `unit` (SIRange),
//! `selected` / `variants` (enums), `currency` / `amount` / `minor` / `precision` (Money),
//! `year` / `month` / `day` / `hour` / `minute` / `second` (date and time), `ns` / `secs`
//! (Instant), `min` / `max` (Tolerance).

use crate::si::{rescale, shift_number, unit_exp10};
use crate::{Error, F32, F64, Map, Number, NumberTy, Variant, VariantTy, VariantTyDiscriminants};
use chrono::{Datelike, Timelike};
use rhai::{AST, ASTNode, Array, Dynamic, Engine, EvalAltResult, FLOAT, INT, ImmutableString};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::sync::OnceLock;

pub use rhai;

type RhaiResult<T> = Result<T, Box<EvalAltResult>>;

/// Compiled expression.
#[derive(Debug, Clone)]
pub struct Expr {
    ast: AST,
    variables: Vec<String>,
}

impl Expr {
    /// Compile an expression (no statements, loops or assignments) with the default [`engine`].
    pub fn compile(source: &str) -> Result<Self, Error> {
        Self::compile_with(engine(), source)
    }

    pub fn compile_with(engine: &Engine, source: &str) -> Result<Self, Error> {
        let ast = engine.compile_expression(source).map_err(script_error)?;
        let mut variables: Vec<String> = Vec::new();
        ast.walk(&mut |path: &[ASTNode]| {
            if let Some(ASTNode::Expr(rhai::Expr::Variable(x, ..))) = path.last()
                && x.2.is_empty()
                && !variables.iter().any(|v| v == x.1.as_str())
            {
                variables.push(x.1.to_string());
            }
            true
        });
        Ok(Expr { ast, variables })
    }

    /// Names of the variables used by this expression, in order of first appearance.
    pub fn variables(&self) -> &[String] {
        &self.variables
    }

    /// Evaluate with the default [`engine`]. `vars` is only called for [`Expr::variables`],
    /// returning `None` is an error.
    pub fn eval(&self, vars: impl Fn(&str) -> Option<Variant>) -> Result<Variant, Error> {
        self.eval_with(engine(), vars)
    }

    /// Evaluate and strictly convert the result, see [`Variant::convert_to`].
    pub fn eval_as(
        &self,
        vars: impl Fn(&str) -> Option<Variant>,
        ty: &VariantTy,
    ) -> Result<Variant, Error> {
        self.eval(vars)?.convert_to(ty)
    }

    /// Evaluate with a custom engine, which must have been set up with [`register`].
    pub fn eval_with(
        &self,
        engine: &Engine,
        vars: impl Fn(&str) -> Option<Variant>,
    ) -> Result<Variant, Error> {
        let mut scope = rhai::Scope::new();
        for name in &self.variables {
            let value =
                vars(name).ok_or_else(|| Error::Script(format!("unknown variable {name:?}")))?;
            scope.push_dynamic(name.as_str(), value.to_dynamic()?);
        }
        let result: Dynamic = engine
            .eval_ast_with_scope(&mut scope, &self.ast)
            .map_err(script_error)?;
        Variant::try_from_dynamic(result)
    }
}

/// Shared engine created by [`new_engine`].
pub fn engine() -> &'static Engine {
    static ENGINE: OnceLock<Engine> = OnceLock::new();
    ENGINE.get_or_init(new_engine)
}

/// Engine with [`register`]ed `Variant` support and limits suitable for evaluating untrusted,
/// short expressions (e.g. once per table cell). `print` and `debug` output is discarded.
pub fn new_engine() -> Engine {
    let mut engine = Engine::new();
    engine
        .set_max_operations(10_000)
        .set_max_expr_depths(64, 32)
        .set_max_call_levels(16)
        .set_max_string_size(64 * 1024)
        .set_max_array_size(16 * 1024)
        .set_max_map_size(16 * 1024)
        .on_print(|_| {})
        .on_debug(|_, _, _| {});
    engine.disable_symbol("eval");
    register(&mut engine);
    engine
}

/// Register the `Variant` custom type, its operators and properties.
pub fn register(engine: &mut Engine) {
    engine.register_type_with_name::<Variant>("Variant");
    engine.register_fn("to_string", |v: &mut Variant| v.to_string());
    engine.register_fn("to_debug", |v: &mut Variant| format!("{v:?}"));

    macro_rules! binary {
        ($name:expr, $f:expr) => {{
            let f: fn(Variant, Variant) -> RhaiResult<_> = $f;
            engine.register_fn($name, move |a: Variant, b: Variant| f(a, b));
            engine.register_fn($name, move |a: Variant, b: INT| f(a, Variant::i64(b)));
            engine.register_fn($name, move |a: INT, b: Variant| f(Variant::i64(a), b));
            engine.register_fn($name, move |a: Variant, b: FLOAT| f(a, Variant::f64(b)));
            engine.register_fn($name, move |a: FLOAT, b: Variant| f(Variant::f64(a), b));
            engine.register_fn($name, move |a: Variant, b: ImmutableString| {
                f(a, Variant::str(b))
            });
        }};
    }
    macro_rules! str_lhs {
        ($name:expr, $f:expr) => {{
            let f: fn(Variant, Variant) -> RhaiResult<_> = $f;
            engine.register_fn($name, move |a: ImmutableString, b: Variant| {
                f(Variant::str(a), b)
            });
        }};
    }

    for (name, op) in [
        ("+", Op::Add),
        ("-", Op::Sub),
        ("*", Op::Mul),
        ("/", Op::Div),
    ] {
        let f = move |a: Variant, b: Variant| -> RhaiResult<Dynamic> { Ok(arith(op, a, b)?) };
        engine.register_fn(name, move |a: Variant, b: Variant| f(a, b));
        engine.register_fn(name, move |a: Variant, b: INT| f(a, Variant::i64(b)));
        engine.register_fn(name, move |a: INT, b: Variant| f(Variant::i64(a), b));
        engine.register_fn(name, move |a: Variant, b: FLOAT| f(a, Variant::f64(b)));
        engine.register_fn(name, move |a: FLOAT, b: Variant| f(Variant::f64(a), b));
        engine.register_fn(name, move |a: Variant, b: ImmutableString| {
            f(a, Variant::str(b))
        });
        if op != Op::Add {
            engine.register_fn(name, move |a: ImmutableString, b: Variant| {
                f(Variant::str(a), b)
            });
        }
    }
    engine.register_fn("+", |a: ImmutableString, b: Variant| format!("{a}{b}"));

    binary!("==", |a, b| Ok(equals(a, b)?));
    binary!("!=", |a, b| Ok(!equals(a, b)?));
    str_lhs!("==", |a, b| Ok(equals(a, b)?));
    str_lhs!("!=", |a, b| Ok(!equals(a, b)?));
    binary!("<", |a, b| Ok(compare(a, b)?.is_lt()));
    binary!("<=", |a, b| Ok(compare(a, b)?.is_le()));
    binary!(">", |a, b| Ok(compare(a, b)?.is_gt()));
    binary!(">=", |a, b| Ok(compare(a, b)?.is_ge()));
    str_lhs!("<", |a, b| Ok(compare(a, b)?.is_lt()));
    str_lhs!("<=", |a, b| Ok(compare(a, b)?.is_le()));
    str_lhs!(">", |a, b| Ok(compare(a, b)?.is_gt()));
    str_lhs!(">=", |a, b| Ok(compare(a, b)?.is_ge()));
    binary!("contains", |a, b| Ok(contains(a, b)?));

    for name in PROPERTIES {
        engine.register_get(name, move |v: &mut Variant| -> RhaiResult<Dynamic> {
            Ok(property(v, name)?)
        });
    }
}

const PROPERTIES: [&str; 22] = [
    "kind",
    "value",
    "unit",
    "base",
    "start",
    "end",
    "selected",
    "variants",
    "currency",
    "amount",
    "minor",
    "precision",
    "year",
    "month",
    "day",
    "hour",
    "minute",
    "second",
    "ns",
    "secs",
    "min",
    "max",
];

impl From<Error> for Box<EvalAltResult> {
    fn from(e: Error) -> Self {
        EvalAltResult::ErrorRuntime(e.to_string().into(), rhai::Position::NONE).into()
    }
}

fn script_error(e: impl Display) -> Error {
    Error::Script(e.to_string())
}

impl Variant {
    /// Convert to a rhai value, see the [module docs](crate::script) for the mapping.
    pub fn to_dynamic(&self) -> Result<Dynamic, Error> {
        Ok(match self {
            Variant::Empty => Dynamic::UNIT,
            Variant::Bool(b) => (*b).into(),
            Variant::Str(s) => s.clone().into(),
            Variant::StrList(l) => {
                Dynamic::from_array(l.iter().map(|s| s.clone().into()).collect())
            }
            Variant::Number(n) => number_to_dynamic(*n)?,
            Variant::Binary(b) => Dynamic::from_blob(b.clone()),
            Variant::List(l) => Dynamic::from_array(
                l.iter()
                    .map(Variant::to_dynamic)
                    .collect::<Result<_, _>>()?,
            ),
            Variant::Map(m) => {
                let mut out = rhai::Map::new();
                for (k, v) in &m.0 {
                    let Variant::Str(k) = k else {
                        return Err(Error::Script(format!("map key {k} is not a string")));
                    };
                    out.insert(k.as_str().into(), v.to_dynamic()?);
                }
                Dynamic::from_map(out)
            }
            rich => Dynamic::from(rich.clone()),
        })
    }

    /// Convert a rhai value back, see the [module docs](crate::script) for the mapping.
    pub fn try_from_dynamic(d: Dynamic) -> Result<Variant, Error> {
        if d.is_unit() {
            return Ok(Variant::Empty);
        }
        if let Ok(b) = d.as_bool() {
            return Ok(Variant::Bool(b));
        }
        if let Ok(i) = d.as_int() {
            return Ok(Variant::i64(i));
        }
        if let Ok(f) = d.as_float() {
            return Ok(Variant::f64(f));
        }
        if let Ok(c) = d.as_char() {
            return Ok(Variant::Str(c.to_string()));
        }
        if d.is_string() {
            return d.into_string().map(Variant::Str).map_err(script_error);
        }
        if d.is_array() {
            let array: Array = d.into_array().map_err(script_error)?;
            return array
                .into_iter()
                .map(Variant::try_from_dynamic)
                .collect::<Result<_, _>>()
                .map(Variant::List);
        }
        if d.is_blob() {
            return d.into_blob().map(Variant::Binary).map_err(script_error);
        }
        if d.is_map() {
            let map = d.cast::<rhai::Map>();
            let mut out = Map::new();
            for (k, v) in map {
                out.0
                    .insert(Variant::Str(k.into()), Variant::try_from_dynamic(v)?);
            }
            return Ok(Variant::Map(out));
        }
        let type_name = d.type_name();
        d.try_cast::<Variant>()
            .ok_or_else(|| Error::Script(format!("cannot convert {type_name} to Variant")))
    }
}

fn number_to_dynamic(n: Number) -> Result<Dynamic, Error> {
    if n.is_float() {
        Ok(Dynamic::from_float(n.to_f64_lossy()))
    } else {
        n.as_i64().map(Dynamic::from_int)
    }
}

fn kind(v: &Variant) -> String {
    VariantTyDiscriminants::from(&v.ty()).as_ref().to_string()
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
        })
    }
}

/// Parse string operand as the type of the other operand.
fn coerce(a: Variant, b: Variant) -> Result<(Variant, Variant), Error> {
    match (a, b) {
        (Variant::Str(s), like) if !matches!(like, Variant::Str(_)) => {
            Ok((parse_like(&s, &like)?, like))
        }
        (like, Variant::Str(s)) if !matches!(like, Variant::Str(_)) => {
            let b = parse_like(&s, &like)?;
            Ok((like, b))
        }
        pair => Ok(pair),
    }
}

fn parse_like(s: &str, like: &Variant) -> Result<Variant, Error> {
    let ty = like.ty();
    Variant::try_from_str(s, &ty).or_else(|e| match (&ty, e) {
        // e.g. "100 Ω" for an integer kΩ value
        (VariantTy::SI { unit, .. }, Error::Lossy { .. } | Error::OutOfRange { .. }) => {
            Variant::try_from_str(s, &VariantTy::si(NumberTy::F64, unit.clone()))
        }
        (_, e) => Err(e),
    })
}

/// Rescale SI value, keeping number type if possible, falling back to F64 otherwise.
fn rescale_to(
    value: Number,
    from: &si_dynamic::Unit,
    to: &si_dynamic::Unit,
    ty: NumberTy,
) -> Result<Number, Error> {
    rescale(value, from, to, ty, false).or_else(|e| match e {
        Error::Lossy { .. } | Error::OutOfRange { .. } => {
            rescale(value, from, to, NumberTy::F64, false)
        }
        e => Err(e),
    })
}

fn unsupported(op: impl Display, a: &Variant, b: &Variant) -> Error {
    Error::Script(format!(
        "operator {op} is not supported for {} and {}",
        kind(a),
        kind(b)
    ))
}

fn arith(op: Op, a: Variant, b: Variant) -> Result<Dynamic, Error> {
    use Variant as V;
    let (a, b) = coerce(a, b)?;
    let r = match (a, b) {
        (
            V::SI {
                value: va,
                unit: ua,
            },
            V::SI {
                value: vb,
                unit: ub,
            },
        ) => {
            let vb = rescale_to(vb, &ub, &ua, va.ty())?;
            match op {
                Op::Add | Op::Sub => V::SI {
                    value: num_arith(op, va, vb)?,
                    unit: ua,
                },
                Op::Div => return Ok(Dynamic::from_float(va.to_f64_lossy() / vb.to_f64_lossy())),
                Op::Mul => {
                    return Err(unsupported(
                        op,
                        &V::SI {
                            value: va,
                            unit: ua,
                        },
                        &V::SI {
                            value: vb,
                            unit: ub,
                        },
                    ));
                }
            }
        }
        (V::SI { value, unit }, V::Number(n)) if matches!(op, Op::Mul | Op::Div) => V::SI {
            value: num_arith(op, value, n)?,
            unit,
        },
        (V::Number(n), V::SI { value, unit }) if op == Op::Mul => V::SI {
            value: num_arith(op, value, n)?,
            unit,
        },
        (
            V::Money {
                currency: ca,
                precision: pa,
                value: a,
            },
            V::Money {
                currency: cb,
                precision: pb,
                value: b,
            },
        ) => {
            if ca != cb {
                return Err(Error::CurrencyMismatch {
                    expected: ca.to_string(),
                    found: cb.to_string(),
                });
            }
            let p = pa.max(pb);
            let (a, b) = (scale_money(a, pa, p, &ca)?, scale_money(b, pb, p, &ca)?);
            let value = match op {
                Op::Add => a.checked_add(b),
                Op::Sub => a.checked_sub(b),
                Op::Div => return Ok(Dynamic::from_float(a as f64 / b as f64)),
                Op::Mul => {
                    return Err(Error::Script(
                        "operator * is not supported for Money and Money".into(),
                    ));
                }
            }
            .ok_or_else(|| money_out_of_range(format!("{a} {op} {b}"), &ca, p))?;
            V::Money {
                currency: ca,
                precision: p,
                value,
            }
        }
        (
            V::Money {
                currency,
                precision,
                value,
            },
            V::Number(n),
        ) if matches!(op, Op::Mul | Op::Div) => V::Money {
            value: money_scale(op, value, n, &currency, precision)?,
            currency,
            precision,
        },
        (
            V::Number(n),
            V::Money {
                currency,
                precision,
                value,
            },
        ) if op == Op::Mul => V::Money {
            value: money_scale(op, value, n, &currency, precision)?,
            currency,
            precision,
        },
        (V::Instant(a), V::Instant(b)) if matches!(op, Op::Add | Op::Sub) => {
            let r = if op == Op::Add {
                a.0.checked_add(b.0)
            } else {
                a.0.checked_sub(b.0)
            };
            V::Instant(crate::NanoSeconds(r.ok_or_else(|| Error::OutOfRange {
                value: format!("{} {op} {}", a.0, b.0),
                to: Box::new(VariantTy::Instant),
            })?))
        }
        (a, b) => return Err(unsupported(op, &a, &b)),
    };
    Ok(Dynamic::from(r))
}

fn int128(n: Number) -> Result<i128, Error> {
    match n {
        Number::U128(u) => i128::try_from(u).map_err(|_| Error::OutOfRange {
            value: u.to_string(),
            to: Box::new(VariantTy::Number(NumberTy::I128)),
        }),
        n => n.as_i128(),
    }
}

/// Integer op integer keeps the type of `a` (error on overflow), division with a remainder gives
/// F64. Float arithmetic gives F32 if `a` is F32 and `b` is not F64, F64 otherwise.
fn num_arith(op: Op, a: Number, b: Number) -> Result<Number, Error> {
    if a.is_integer() && b.is_integer() {
        let (x, y) = (int128(a)?, int128(b)?);
        let r = match op {
            Op::Add => x.checked_add(y),
            Op::Sub => x.checked_sub(y),
            Op::Mul => x.checked_mul(y),
            Op::Div => {
                if y == 0 {
                    return Err(Error::Script("division by zero".into()));
                }
                if x % y != 0 {
                    return Ok(Number::F64(F64(x as f64 / y as f64)));
                }
                x.checked_div(y)
            }
        };
        let out_of_range = || Error::OutOfRange {
            value: format!("{a} {op} {b}"),
            to: Box::new(VariantTy::Number(a.ty())),
        };
        match r {
            Some(r) => Number::I128(r)
                .convert_to(a.ty())
                .map_err(|_| out_of_range()),
            None => Err(out_of_range()),
        }
    } else {
        let (x, y) = (a.to_f64_lossy(), b.to_f64_lossy());
        let r = match op {
            Op::Add => x + y,
            Op::Sub => x - y,
            Op::Mul => x * y,
            Op::Div => x / y,
        };
        if a.ty() == NumberTy::F32 && b.ty() != NumberTy::F64 {
            Ok(Number::F32(F32(r as f32)))
        } else {
            Ok(Number::F64(F64(r)))
        }
    }
}

fn money_out_of_range(value: String, currency: &str, precision: u8) -> Error {
    Error::OutOfRange {
        value,
        to: Box::new(VariantTy::money(currency, precision)),
    }
}

fn scale_money(value: i64, from: u8, to: u8, currency: &str) -> Result<i64, Error> {
    10i64
        .checked_pow((to - from) as u32)
        .and_then(|m| value.checked_mul(m))
        .ok_or_else(|| money_out_of_range(value.to_string(), currency, to))
}

/// Multiply or divide money by a number, rounding half away from zero to the minor unit.
fn money_scale(op: Op, value: i64, n: Number, currency: &str, precision: u8) -> Result<i64, Error> {
    let err = || money_out_of_range(format!("{value} {op} {n}"), currency, precision);
    let r = if n.is_integer() {
        let (v, n) = (value as i128, int128(n)?);
        match op {
            Op::Mul => v.checked_mul(n),
            _ if n == 0 => return Err(Error::Script("division by zero".into())),
            _ => {
                let (q, r) = (v / n, v % n);
                Some(if 2 * r.abs() >= n.abs() {
                    q + (v.signum() * n.signum())
                } else {
                    q
                })
            }
        }
    } else {
        let f = n.to_f64_lossy();
        let r = if op == Op::Mul {
            value as f64 * f
        } else {
            value as f64 / f
        };
        if !r.is_finite() {
            return Err(err());
        }
        Some(r.round() as i128)
    };
    r.and_then(|r| i64::try_from(r).ok()).ok_or_else(err)
}

/// `None` if values of this kind are not ordered.
fn order(a: &Variant, b: &Variant) -> Option<Result<Ordering, Error>> {
    use Variant as V;
    Some(match (a, b) {
        (
            V::SI {
                value: va,
                unit: ua,
            },
            V::SI {
                value: vb,
                unit: ub,
            },
        ) => rescale_to(*vb, ub, ua, va.ty()).map(|vb| va.cmp_value(&vb)),
        (V::Money { currency: ca, .. }, V::Money { currency: cb, .. }) => {
            if ca == cb {
                Ok(a.sort_cmp(b))
            } else {
                Err(Error::CurrencyMismatch {
                    expected: ca.to_string(),
                    found: cb.to_string(),
                })
            }
        }
        (V::Date(x), V::Date(y)) => Ok(x.cmp(y)),
        (V::Time(x), V::Time(y)) => Ok(x.cmp(y)),
        (V::DateTime(x), V::DateTime(y)) => Ok(x.cmp(y)),
        (V::Instant(x), V::Instant(y)) => Ok(x.cmp(y)),
        (
            V::Enum {
                name: na,
                selected: sa,
                variants,
            },
            V::Enum {
                name: nb,
                selected: sb,
                ..
            },
        ) if na == nb => {
            let idx = |s: &String| variants.iter().position(|v| v == s);
            Ok(idx(sa).cmp(&idx(sb)))
        }
        _ => return None,
    })
}

fn compare(a: Variant, b: Variant) -> Result<Ordering, Error> {
    let (a, b) = coerce(a, b)?;
    order(&a, &b).unwrap_or_else(|| Err(unsupported("comparison", &a, &b)))
}

fn equals(a: Variant, b: Variant) -> Result<bool, Error> {
    let (a, b) = coerce(a, b)?;
    match order(&a, &b) {
        Some(o) => Ok(o?.is_eq()),
        None if a.ty() == b.ty() => Ok(a == b),
        None => Err(unsupported("==", &a, &b)),
    }
}

fn contains(container: Variant, item: Variant) -> Result<bool, Error> {
    use Variant as V;
    match &container {
        V::SIRange {
            start,
            start_inclusive,
            end,
            end_inclusive,
            unit,
        } => {
            let si = |value: &Number| V::SI {
                value: *value,
                unit: unit.clone(),
            };
            let item = match item {
                V::Str(s) => parse_like(&s, &si(start))?,
                item => item,
            };
            let lo = compare(item.clone(), si(start))?;
            let hi = compare(item, si(end))?;
            Ok((lo.is_gt() || (*start_inclusive && lo.is_eq()))
                && (hi.is_lt() || (*end_inclusive && hi.is_eq())))
        }
        V::MultiSelectionEnum { selected, .. } => match item {
            V::Str(s) => Ok(selected.iter().any(|v| v.eq_ignore_ascii_case(&s))),
            item => Err(unsupported("in", &item, &container)),
        },
        _ => Err(unsupported("in", &item, &container)),
    }
}

fn property(v: &Variant, name: &str) -> Result<Dynamic, Error> {
    use Variant as V;
    let strings = |l: &[String]| Dynamic::from_array(l.iter().map(|s| s.clone().into()).collect());
    Ok(match (name, v) {
        ("kind", v) => kind(v).into(),
        ("value", V::SI { value, .. }) => number_to_dynamic(*value)?,
        ("unit", V::SI { unit, .. } | V::SIRange { unit, .. }) => unit.to_string().into(),
        ("base", V::SI { value, unit }) => Dynamic::from_float(
            shift_number(*value, unit_exp10(unit), NumberTy::F64, false)?.to_f64_lossy(),
        ),
        ("start", V::SIRange { start, unit, .. }) => Dynamic::from(V::SI {
            value: *start,
            unit: unit.clone(),
        }),
        ("end", V::SIRange { end, unit, .. }) => Dynamic::from(V::SI {
            value: *end,
            unit: unit.clone(),
        }),
        ("selected", V::Enum { selected, .. }) => selected.clone().into(),
        ("selected", V::MultiSelectionEnum { selected, .. }) => strings(selected),
        ("variants", V::Enum { variants, .. } | V::MultiSelectionEnum { variants, .. }) => {
            strings(variants)
        }
        ("currency", V::Money { currency, .. }) => currency.to_string().into(),
        (
            "amount",
            V::Money {
                precision, value, ..
            },
        ) => Dynamic::from_float(*value as f64 / 10f64.powi(*precision as i32)),
        ("minor", V::Money { value, .. }) => Dynamic::from_int(*value),
        ("precision", V::Money { precision, .. }) => Dynamic::from_int(*precision as INT),
        ("year", V::Date(d)) => Dynamic::from_int(d.year() as INT),
        ("year", V::DateTime(d)) => Dynamic::from_int(d.year() as INT),
        ("month", V::Date(d)) => Dynamic::from_int(d.month() as INT),
        ("month", V::DateTime(d)) => Dynamic::from_int(d.month() as INT),
        ("day", V::Date(d)) => Dynamic::from_int(d.day() as INT),
        ("day", V::DateTime(d)) => Dynamic::from_int(d.day() as INT),
        ("hour", V::Time(t)) => Dynamic::from_int(t.hour() as INT),
        ("hour", V::DateTime(t)) => Dynamic::from_int(t.hour() as INT),
        ("minute", V::Time(t)) => Dynamic::from_int(t.minute() as INT),
        ("minute", V::DateTime(t)) => Dynamic::from_int(t.minute() as INT),
        ("second", V::Time(t)) => Dynamic::from_int(t.second() as INT),
        ("second", V::DateTime(t)) => Dynamic::from_int(t.second() as INT),
        ("ns", V::Instant(ns)) => Dynamic::from_int(ns.0),
        ("secs", V::Instant(ns)) => Dynamic::from_float(ns.0 as f64 / 1e9),
        ("min", V::Tolerance { min, .. }) => number_to_dynamic(*min)?,
        ("max", V::Tolerance { max, .. }) => number_to_dynamic(*max)?,
        _ => {
            return Err(Error::Script(format!(
                "{} has no property {name:?}",
                kind(v)
            )));
        }
    })
}
