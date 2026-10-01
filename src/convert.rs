use crate::decimal::{format_decimal, parse_decimal_scaled};
use crate::parse::match_enum_variant;
use crate::si::{base_matches, rescale, shift_number, unit_exp10};
use crate::{Error, F32, F64, Map, NanoSeconds, Number, NumberTy, Variant, VariantTy};
use chrono::{DateTime, FixedOffset, NaiveDate, NaiveTime, Utc};
use indexmap::IndexMap;
use si_dynamic::{BaseUnit, Unit};
use std::cmp::Ordering;

impl Variant {
    /// Strict conversion. Never silently loses information: fractional floats to integers,
    /// out of range values, excess decimal digits for Money, etc. result in an error.
    ///
    /// Supported conversions:
    /// * anything -> `Str` (via `Display`), anything -> `Empty`;
    /// * `Str` -> anything (via [`Variant::try_from_str`]);
    /// * `Number` <-> `Number`, `Bool` (0 / 1), `SI`, `Instant` (ns), `Money`;
    /// * `SI` -> `SI` with a compatible unit (prefix rescaling), `SI` -> `Number` (value in its unit);
    /// * `SIRange` -> `SIRange` with the same inclusivity and compatible unit;
    /// * `Money` -> `Money` with the same currency (precision change);
    /// * `DateTime` -> `Date` / `Time`, `Date` -> `DateTime` (midnight UTC);
    /// * `StrList` <-> `List`;
    /// * `Enum` / `MultiSelectionEnum` -> `Enum` / `MultiSelectionEnum` by variant names;
    /// * `Tolerance` -> `Tolerance` with a different number type.
    ///
    /// `Empty` converts only to `Empty` and `Str` (empty string), otherwise [`Error::Empty`].
    pub fn convert_to(self, to: &VariantTy) -> Result<Variant, Error> {
        self.convert(to, false)
    }

    /// Same as [`Variant::convert_to`], but numbers are rounded and saturated instead of failing.
    pub fn convert_lossy(self, to: &VariantTy) -> Result<Variant, Error> {
        self.convert(to, true)
    }

    fn convert(self, to: &VariantTy, lossy: bool) -> Result<Variant, Error> {
        let from = self.ty();
        if &from == to {
            return Ok(self);
        }
        let num = |n: Number, ty: NumberTy| {
            if lossy {
                n.convert_lossy(ty)
            } else {
                n.convert_to(ty)
            }
        };
        match (self, to) {
            (_, VariantTy::Empty) => Ok(Variant::Empty),
            (v, VariantTy::Str) => Ok(Variant::Str(v.to_string())),
            (Variant::Empty, _) => Err(Error::Empty),
            (Variant::Str(s), to) => Variant::try_from_str(s, to),

            (Variant::Number(n), VariantTy::Number(ty)) => Ok(Variant::Number(num(n, *ty)?)),
            (Variant::Bool(b), VariantTy::Number(ty)) => {
                Ok(Variant::Number(num(Number::U8(b as u8), *ty)?))
            }
            (Variant::Number(n), VariantTy::Bool) => {
                if n.eq_value(&Number::U8(0)) {
                    Ok(Variant::Bool(false))
                } else if n.eq_value(&Number::U8(1)) || lossy {
                    Ok(Variant::Bool(true))
                } else {
                    Err(Error::Lossy {
                        value: n.to_string(),
                        to: Box::new(VariantTy::Bool),
                    })
                }
            }
            (Variant::Number(n), VariantTy::SI { number_ty, unit }) => Ok(Variant::SI {
                value: num(n, *number_ty)?,
                unit: unit.clone(),
            }),
            (Variant::SI { value, .. }, VariantTy::Number(ty)) => {
                Ok(Variant::Number(num(value, *ty)?))
            }
            (
                Variant::SI {
                    value,
                    unit: from_unit,
                },
                VariantTy::SI { number_ty, unit },
            ) => Ok(Variant::SI {
                value: rescale(value, &from_unit, unit, *number_ty, lossy)?,
                unit: unit.clone(),
            }),
            (
                Variant::SIRange {
                    start,
                    start_inclusive,
                    end,
                    end_inclusive,
                    unit: from_unit,
                },
                VariantTy::SIRange {
                    number_ty,
                    start_inclusive: to_start_inclusive,
                    end_inclusive: to_end_inclusive,
                    unit,
                },
            ) if start_inclusive == *to_start_inclusive && end_inclusive == *to_end_inclusive => {
                Ok(Variant::SIRange {
                    start: rescale(start, &from_unit, unit, *number_ty, lossy)?,
                    start_inclusive,
                    end: rescale(end, &from_unit, unit, *number_ty, lossy)?,
                    end_inclusive,
                    unit: unit.clone(),
                })
            }

            (Variant::Instant(ns), VariantTy::Number(ty)) => {
                Ok(Variant::Number(num(Number::I64(ns.0), *ty)?))
            }
            (Variant::Number(n), VariantTy::Instant) => match num(n, NumberTy::I64)? {
                Number::I64(ns) => Ok(Variant::Instant(NanoSeconds(ns))),
                _ => Err(Error::Internal),
            },

            (
                Variant::Number(n),
                VariantTy::Money {
                    currency,
                    precision,
                },
            ) => Ok(Variant::Money {
                currency: currency.clone(),
                precision: *precision,
                value: to_money(&n.to_string(), *precision, lossy, to)?,
            }),
            (
                Variant::Money {
                    currency: from_currency,
                    precision: from_precision,
                    value,
                },
                VariantTy::Money {
                    currency,
                    precision,
                },
            ) => {
                if &from_currency != currency {
                    return Err(Error::CurrencyMismatch {
                        expected: currency.to_string(),
                        found: from_currency.to_string(),
                    });
                }
                let amount = format_decimal(value as i128, from_precision as u32, false);
                Ok(Variant::Money {
                    currency: currency.clone(),
                    precision: *precision,
                    value: to_money(&amount, *precision, lossy, to)?,
                })
            }
            (
                Variant::Money {
                    precision, value, ..
                },
                VariantTy::Number(ty),
            ) => Ok(Variant::Number(shift_number(
                Number::I64(value),
                -(precision as i32),
                *ty,
                lossy,
            )?)),

            (Variant::DateTime(dt), VariantTy::Date) => Ok(Variant::Date(dt.date_naive())),
            (Variant::DateTime(dt), VariantTy::Time) => Ok(Variant::Time(dt.time())),
            (Variant::Date(d), VariantTy::DateTime) => Ok(Variant::DateTime(
                d.and_time(NaiveTime::MIN).and_utc().fixed_offset(),
            )),

            (Variant::StrList(list), VariantTy::List) => {
                Ok(Variant::List(list.into_iter().map(Variant::Str).collect()))
            }
            (Variant::List(list), VariantTy::StrList) => Ok(Variant::StrList(
                list.iter().map(|v| v.to_string()).collect(),
            )),

            (Variant::Enum { selected, .. }, VariantTy::Enum { name, variants }) => {
                Ok(Variant::Enum {
                    name: name.clone(),
                    selected: match_enum_variant(&selected, name, variants)?,
                    variants: variants.clone(),
                })
            }
            (Variant::Enum { selected, .. }, VariantTy::MultiSelectionEnum { name, variants }) => {
                Ok(Variant::MultiSelectionEnum {
                    name: name.clone(),
                    selected: vec![match_enum_variant(&selected, name, variants)?],
                    variants: variants.clone(),
                })
            }
            (
                Variant::MultiSelectionEnum { selected, .. },
                VariantTy::MultiSelectionEnum { name, variants },
            ) => Ok(Variant::MultiSelectionEnum {
                name: name.clone(),
                selected: selected
                    .iter()
                    .map(|s| match_enum_variant(s, name, variants))
                    .collect::<Result<_, _>>()?,
                variants: variants.clone(),
            }),
            (Variant::MultiSelectionEnum { selected, .. }, VariantTy::Enum { name, variants })
                if selected.len() == 1 =>
            {
                Ok(Variant::Enum {
                    name: name.clone(),
                    selected: match_enum_variant(&selected[0], name, variants)?,
                    variants: variants.clone(),
                })
            }

            (
                Variant::Tolerance {
                    min,
                    min_percent,
                    max,
                    max_percent,
                },
                VariantTy::Tolerance(ty),
            ) => Ok(Variant::Tolerance {
                min: num(min, *ty)?,
                min_percent,
                max: num(max, *ty)?,
                max_percent,
            }),

            (_, to) => Err(Error::cannot_convert(&from, to)),
        }
    }

    /// Ordering suitable for sorting table columns: numbers are compared by value regardless of
    /// their type, SI values with compatible units and Money with the same currency are compared
    /// by magnitude. Everything else falls back to the structural `Ord`.
    pub fn sort_cmp(&self, other: &Variant) -> Ordering {
        match (self, other) {
            (Variant::Number(a), Variant::Number(b)) => a.cmp_value(b),
            (Variant::SI { value: a, unit: ua }, Variant::SI { value: b, unit: ub })
                if ua.exp == ub.exp && base_matches(&ua.base, &ub.base) =>
            {
                let a = a.to_f64_lossy() * 10f64.powi(unit_exp10(ua));
                let b = b.to_f64_lossy() * 10f64.powi(unit_exp10(ub));
                a.total_cmp(&b)
            }
            (
                Variant::Money {
                    currency: ca,
                    precision: pa,
                    value: a,
                },
                Variant::Money {
                    currency: cb,
                    precision: pb,
                    value: b,
                },
            ) if ca == cb => {
                let a = (*a as i128).checked_mul(10i128.pow(*pb as u32 % 39));
                let b = (*b as i128).checked_mul(10i128.pow(*pa as u32 % 39));
                match (a, b) {
                    (Some(a), Some(b)) => a.cmp(&b),
                    _ => self.cmp(other),
                }
            }
            _ => self.cmp(other),
        }
    }

    fn convert_ref(&self, to: &VariantTy) -> Result<Variant, Error> {
        self.clone().convert_to(to)
    }

    /// `Str` and `Empty` (as "") only.
    pub fn as_str(&self) -> Result<&str, Error> {
        match self {
            Variant::Empty => Ok(""),
            Variant::Str(s) => Ok(s.as_str()),
            o => Err(Error::cannot_convert(&o.ty(), &VariantTy::Str)),
        }
    }

    /// `Str` and `Empty` (as "") only, use `to_string()` to format any value.
    pub fn as_string(&self) -> Result<String, Error> {
        self.as_str().map(str::to_owned)
    }

    /// `Str` only, `Empty` and "" result in [`Error::Empty`].
    pub fn as_non_empty_str(&self) -> Result<&str, Error> {
        match self.as_str()? {
            "" => Err(Error::Empty),
            s => Ok(s),
        }
    }

    pub fn as_non_empty_string(&self) -> Result<String, Error> {
        self.as_non_empty_str().map(str::to_owned)
    }

    pub fn as_bool(&self) -> Result<bool, Error> {
        match self {
            Variant::Bool(b) => Ok(*b),
            o => match o.convert_ref(&VariantTy::Bool)? {
                Variant::Bool(b) => Ok(b),
                _ => Err(Error::Internal),
            },
        }
    }

    /// Numeric value of `Number`, `SI` (in its unit), `Bool`, `Instant` (ns) or a string
    /// containing an integer or a float.
    pub fn as_number(&self) -> Result<Number, Error> {
        match self {
            Variant::Number(n) | Variant::SI { value: n, .. } => Ok(*n),
            Variant::Bool(b) => Ok(Number::U8(*b as u8)),
            Variant::Instant(ns) => Ok(Number::I64(ns.0)),
            Variant::Empty => Err(Error::Empty),
            Variant::Str(s) => Number::try_from_str(s, NumberTy::I64)
                .or_else(|_| Number::try_from_str(s, NumberTy::F64)),
            o => Err(Error::cannot_convert(&o.ty(), &VariantTy::f64())),
        }
    }

    pub fn as_date(&self) -> Result<NaiveDate, Error> {
        match self {
            Variant::Date(d) => Ok(*d),
            o => match o.convert_ref(&VariantTy::Date)? {
                Variant::Date(d) => Ok(d),
                _ => Err(Error::Internal),
            },
        }
    }

    pub fn as_time(&self) -> Result<NaiveTime, Error> {
        match self {
            Variant::Time(t) => Ok(*t),
            o => match o.convert_ref(&VariantTy::Time)? {
                Variant::Time(t) => Ok(t),
                _ => Err(Error::Internal),
            },
        }
    }

    pub fn as_date_time(&self) -> Result<DateTime<FixedOffset>, Error> {
        match self {
            Variant::DateTime(dt) => Ok(*dt),
            o => match o.convert_ref(&VariantTy::DateTime)? {
                Variant::DateTime(dt) => Ok(dt),
                _ => Err(Error::Internal),
            },
        }
    }

    pub fn as_instant(&self) -> Result<NanoSeconds, Error> {
        match self {
            Variant::Instant(ns) => Ok(*ns),
            o => match o.convert_ref(&VariantTy::Instant)? {
                Variant::Instant(ns) => Ok(ns),
                _ => Err(Error::Internal),
            },
        }
    }

    pub fn as_bytes(&self) -> Result<&[u8], Error> {
        match self {
            Variant::Binary(b) => Ok(b),
            o => Err(Error::cannot_convert(&o.ty(), &VariantTy::Binary)),
        }
    }

    pub fn as_list(&self) -> Result<&[Variant], Error> {
        match self {
            Variant::List(l) => Ok(l),
            o => Err(Error::cannot_convert(&o.ty(), &VariantTy::List)),
        }
    }

    pub fn as_str_list(&self) -> Result<&[String], Error> {
        match self {
            Variant::StrList(l) => Ok(l),
            o => Err(Error::cannot_convert(&o.ty(), &VariantTy::StrList)),
        }
    }

    pub fn as_map(&self) -> Result<&Map, Error> {
        match self {
            Variant::Map(m) => Ok(m),
            o => Err(Error::cannot_convert(&o.ty(), &VariantTy::Map)),
        }
    }

    /// Selected variant name of an `Enum`.
    pub fn as_enum_selected(&self) -> Result<&str, Error> {
        match self {
            Variant::Enum { selected, .. } => Ok(selected),
            Variant::Empty => Err(Error::Empty),
            o => Err(Error::cannot_convert(
                &o.ty(),
                &VariantTy::Enum {
                    name: "".into(),
                    variants: Vec::new().into(),
                },
            )),
        }
    }

    /// Value of an SI quantity converted to the base unit without prefix (e.g. 4.7 kΩ -> 4700).
    ///
    /// Integer values that cannot be represented after scaling are returned as `F64`.
    /// Non-SI values are converted first (e.g. strings like "4.7k" are parsed).
    pub fn as_base_unit(&self, base_unit: BaseUnit) -> Result<Number, Error> {
        match self {
            Variant::SI { value, unit } => {
                let base = Unit::base(base_unit);
                if unit.exp != 1 || !base_matches(&unit.base, &base.base) {
                    return Err(Error::UnitMismatch {
                        expected: Box::new(base),
                        found: Box::new(unit.clone()),
                    });
                }
                let shift = unit_exp10(unit);
                if shift == 0 {
                    return Ok(*value);
                }
                shift_number(*value, shift, value.ty(), false)
                    .or_else(|_| shift_number(*value, shift, NumberTy::F64, false))
            }
            o => match o.convert_ref(&VariantTy::SI {
                number_ty: NumberTy::F64,
                unit: Unit::base(base_unit.clone()),
            })? {
                Variant::SI { value, .. } => Ok(value),
                _ => Err(Error::Internal),
            },
        }
    }
}

fn to_money(amount: &str, precision: u8, lossy: bool, ty: &VariantTy) -> Result<i64, Error> {
    let scaled = parse_decimal_scaled(amount, precision as i32).ok_or_else(|| {
        if amount.contains(['i', 'N']) {
            Error::Lossy {
                value: amount.to_string(),
                to: Box::new(ty.clone()),
            }
        } else {
            Error::OutOfRange {
                value: amount.to_string(),
                to: Box::new(ty.clone()),
            }
        }
    })?;
    if !scaled.exact && !lossy {
        return Err(Error::Lossy {
            value: amount.to_string(),
            to: Box::new(ty.clone()),
        });
    }
    match i64::try_from(scaled.value) {
        Ok(v) => Ok(v),
        Err(_) if lossy => Ok(if scaled.value < 0 { i64::MIN } else { i64::MAX }),
        Err(_) => Err(Error::OutOfRange {
            value: amount.to_string(),
            to: Box::new(ty.clone()),
        }),
    }
}

macro_rules! variant_number_accessors {
    ($($name:ident, $ty:ty, $variant:ident);* $(;)?) => {
        impl Variant {
            $(
            #[doc = concat!(
                "Strictly convert to `", stringify!($ty), "`. Works for `Number`, `SI` ",
                "(value in its unit), `Bool`, `Instant`, `Money` and numeric strings."
            )]
            pub fn $name(&self) -> Result<$ty, Error> {
                match self {
                    Variant::Number(n) | Variant::SI { value: n, .. } => n.$name(),
                    o => match o.convert_ref(&VariantTy::Number(NumberTy::$variant))? {
                        Variant::Number(n) => n.$name(),
                        _ => Err(Error::Internal),
                    },
                }
            }
            )*
        }
        $(
        impl From<$ty> for Variant {
            fn from(v: $ty) -> Self {
                Variant::Number(Number::from(v))
            }
        }
        impl TryFrom<&Variant> for $ty {
            type Error = Error;
            fn try_from(v: &Variant) -> Result<Self, Error> {
                v.$name()
            }
        }
        impl TryFrom<Variant> for $ty {
            type Error = Error;
            fn try_from(v: Variant) -> Result<Self, Error> {
                v.$name()
            }
        }
        )*
    };
}

variant_number_accessors!(
    as_i8, i8, I8;
    as_i16, i16, I16;
    as_i32, i32, I32;
    as_i64, i64, I64;
    as_i128, i128, I128;
    as_u8, u8, U8;
    as_u16, u16, U16;
    as_u32, u32, U32;
    as_u64, u64, U64;
    as_u128, u128, U128;
    as_f32, f32, F32;
    as_f64, f64, F64;
);

macro_rules! from_into_variant {
    ($($ty:ty => |$v:ident| $e:expr),* $(,)?) => {
        $(
        impl From<$ty> for Variant {
            fn from($v: $ty) -> Self {
                $e
            }
        }
        )*
    };
}

from_into_variant!(
    bool => |v| Variant::Bool(v),
    String => |v| Variant::Str(v),
    &str => |v| Variant::Str(v.to_string()),
    &String => |v| Variant::Str(v.clone()),
    Vec<String> => |v| Variant::StrList(v),
    Number => |v| Variant::Number(v),
    F32 => |v| Variant::Number(Number::F32(v)),
    F64 => |v| Variant::Number(Number::F64(v)),
    Vec<u8> => |v| Variant::Binary(v),
    &[u8] => |v| Variant::Binary(v.to_vec()),
    Vec<Variant> => |v| Variant::List(v),
    Map => |v| Variant::Map(v),
    IndexMap<Variant, Variant> => |v| Variant::Map(Map(v)),
    NaiveDate => |v| Variant::Date(v),
    NaiveTime => |v| Variant::Time(v),
    DateTime<FixedOffset> => |v| Variant::DateTime(v),
    DateTime<Utc> => |v| Variant::DateTime(v.fixed_offset()),
    NanoSeconds => |v| Variant::Instant(v),
);

/// `None` becomes `Variant::Empty`.
impl<T: Into<Variant>> From<Option<T>> for Variant {
    fn from(v: Option<T>) -> Self {
        v.map(Into::into).unwrap_or(Variant::Empty)
    }
}

macro_rules! try_from_variant {
    ($($ty:ty => $accessor:ident),* $(,)?) => {
        $(
        impl TryFrom<&Variant> for $ty {
            type Error = Error;
            fn try_from(v: &Variant) -> Result<Self, Error> {
                v.$accessor()
            }
        }
        impl TryFrom<Variant> for $ty {
            type Error = Error;
            fn try_from(v: Variant) -> Result<Self, Error> {
                v.$accessor()
            }
        }
        )*
    };
}

try_from_variant!(
    bool => as_bool,
    Number => as_number,
    NaiveDate => as_date,
    NaiveTime => as_time,
    DateTime<FixedOffset> => as_date_time,
    NanoSeconds => as_instant,
);

impl TryFrom<Variant> for String {
    type Error = Error;
    fn try_from(v: Variant) -> Result<Self, Error> {
        match v {
            Variant::Str(s) => Ok(s),
            o => o.as_string(),
        }
    }
}

impl TryFrom<Variant> for Vec<u8> {
    type Error = Error;
    fn try_from(v: Variant) -> Result<Self, Error> {
        match v {
            Variant::Binary(b) => Ok(b),
            o => Err(Error::cannot_convert(&o.ty(), &VariantTy::Binary)),
        }
    }
}

impl TryFrom<Variant> for Vec<String> {
    type Error = Error;
    fn try_from(v: Variant) -> Result<Self, Error> {
        match v {
            Variant::StrList(l) => Ok(l),
            o => match o.convert_to(&VariantTy::StrList)? {
                Variant::StrList(l) => Ok(l),
                _ => Err(Error::Internal),
            },
        }
    }
}

impl TryFrom<Variant> for Vec<Variant> {
    type Error = Error;
    fn try_from(v: Variant) -> Result<Self, Error> {
        match v {
            Variant::List(l) => Ok(l),
            o => match o.convert_to(&VariantTy::List)? {
                Variant::List(l) => Ok(l),
                _ => Err(Error::Internal),
            },
        }
    }
}
