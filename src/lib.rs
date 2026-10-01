//! Dynamically typed value ([`Variant`]) together with a parameterized type descriptor
//! ([`VariantTy`]), inspired by Qt's `QVariant`.
//!
//! Main operations:
//! * [`Variant::try_from_str`] - parse user input / CSV cells as a given type;
//! * [`Variant::convert_to`] / [`Variant::convert_lossy`] - convert between types;
//! * [`Display`](std::fmt::Display) - human-readable form, which [`Variant::try_from_str`] parses back
//!   (`parse(v.to_string(), v.ty()) == v` for all kinds, see `tests/roundtrip.rs` for the exact
//!   guarantees);
//! * typed accessors (`as_u32`, `as_date_time`, ...) and `From` / `TryFrom` impls.

mod convert;
mod decimal;
mod display;
mod error;
mod literal;
mod map;
mod number;
mod parse;
#[cfg(feature = "rhai")]
pub mod script;
mod si;
pub mod util;

pub use crate::error::Error;
pub use crate::map::Map;
pub use crate::number::{F32, F64, Number, NumberTy};
use chrono::{DateTime, FixedOffset, NaiveDate, NaiveTime};
pub use si_dynamic;
use si_dynamic::Unit;
use std::sync::Arc;
use strum::{AsRefStr, EnumDiscriminants, EnumIter};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Variant {
    #[default]
    Empty,
    Bool(bool),
    Str(String),
    StrList(Vec<String>),
    Number(Number),
    /// Physical quantity, `value` is expressed in `unit` (including its prefix).
    SI {
        value: Number,
        unit: Unit,
    },
    SIRange {
        start: Number,
        start_inclusive: bool,
        end: Number,
        end_inclusive: bool,
        unit: Unit,
    },

    Enum {
        name: Arc<str>,
        selected: String,
        variants: Arc<[String]>,
    },
    MultiSelectionEnum {
        name: Arc<str>,
        selected: Vec<String>,
        variants: Arc<[String]>,
    },

    Binary(Vec<u8>),
    List(Vec<Variant>),
    Map(Map),

    Date(NaiveDate),
    Time(NaiveTime),
    DateTime(DateTime<FixedOffset>),
    /// Point in time in nanoseconds, relative to some epoch (e.g. start of a capture).
    Instant(NanoSeconds),

    /// Fixed point amount: `value / 10^precision` units of `currency`.
    Money {
        currency: Arc<str>,
        precision: u8,
        value: i64,
    },

    /// Signed deviations from a nominal value, e.g. `±5%` is `min: -5, max: 5`, both in percent.
    Tolerance {
        min: Number,
        min_percent: bool,
        max: Number,
        max_percent: bool,
    },
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, EnumDiscriminants)]
#[strum_discriminants(derive(EnumIter, AsRefStr))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum VariantTy {
    Empty,
    Bool,
    #[default]
    Str,
    StrList,
    Number(NumberTy),
    SI {
        number_ty: NumberTy,
        unit: Unit,
    },
    SIRange {
        number_ty: NumberTy,
        start_inclusive: bool,
        end_inclusive: bool,
        unit: Unit,
    },

    Enum {
        name: Arc<str>,
        variants: Arc<[String]>,
    },
    MultiSelectionEnum {
        name: Arc<str>,
        variants: Arc<[String]>,
    },

    Binary,
    List,
    Map,

    Date,
    Time,
    DateTime,
    Instant,

    Money {
        currency: Arc<str>,
        precision: u8,
    },

    Tolerance(NumberTy),
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NanoSeconds(pub i64);

impl From<&Variant> for VariantTy {
    fn from(value: &Variant) -> Self {
        match value {
            Variant::Empty => VariantTy::Empty,
            Variant::Bool(_) => VariantTy::Bool,
            Variant::Str(_) => VariantTy::Str,
            Variant::StrList(_) => VariantTy::StrList,
            Variant::Number(n) => VariantTy::Number(n.into()),
            Variant::SI { value, unit } => VariantTy::SI {
                number_ty: value.into(),
                unit: unit.clone(),
            },
            Variant::SIRange {
                start,
                start_inclusive,
                end_inclusive,
                unit,
                ..
            } => VariantTy::SIRange {
                number_ty: start.into(),
                start_inclusive: *start_inclusive,
                end_inclusive: *end_inclusive,
                unit: unit.clone(),
            },

            Variant::Enum { name, variants, .. } => VariantTy::Enum {
                name: name.clone(),
                variants: variants.clone(),
            },
            Variant::MultiSelectionEnum { name, variants, .. } => VariantTy::MultiSelectionEnum {
                name: name.clone(),
                variants: variants.clone(),
            },

            Variant::Binary(_) => VariantTy::Binary,
            Variant::List(_) => VariantTy::List,
            Variant::Map(_) => VariantTy::Map,

            Variant::Date(_) => VariantTy::Date,
            Variant::Time(_) => VariantTy::Time,
            Variant::DateTime(_) => VariantTy::DateTime,
            Variant::Instant(_) => VariantTy::Instant,

            Variant::Money {
                currency,
                precision,
                ..
            } => VariantTy::Money {
                currency: currency.clone(),
                precision: *precision,
            },

            Variant::Tolerance { min, .. } => VariantTy::Tolerance(min.into()),
        }
    }
}

impl From<Variant> for VariantTy {
    fn from(value: Variant) -> Self {
        VariantTy::from(&value)
    }
}

impl Variant {
    /// Type of this value.
    pub fn ty(&self) -> VariantTy {
        self.into()
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Variant::Empty => true,
            Variant::Str(s) => s.is_empty(),
            Variant::StrList(l) => l.is_empty(),
            Variant::MultiSelectionEnum { selected, .. } => selected.is_empty(),
            Variant::Binary(b) => b.is_empty(),
            Variant::List(l) => l.is_empty(),
            Variant::Map(m) => m.0.is_empty(),
            _ => false,
        }
    }

    /// Default value of a type, `VariantTy::from(&Variant::default_of(ty)) == ty` always holds.
    pub fn default_of(ty: &VariantTy) -> Variant {
        match ty {
            VariantTy::Empty => Variant::Empty,
            VariantTy::Bool => Variant::Bool(false),
            VariantTy::Str => Variant::Str(String::new()),
            VariantTy::StrList => Variant::StrList(Vec::new()),
            VariantTy::Number(ty) => Variant::Number(Number::default_of(*ty)),
            VariantTy::SI { number_ty, unit } => Variant::SI {
                value: Number::default_of(*number_ty),
                unit: unit.clone(),
            },
            VariantTy::SIRange {
                number_ty,
                start_inclusive,
                end_inclusive,
                unit,
            } => Variant::SIRange {
                start: Number::default_of(*number_ty),
                start_inclusive: *start_inclusive,
                end: Number::default_of(*number_ty),
                end_inclusive: *end_inclusive,
                unit: unit.clone(),
            },
            VariantTy::Enum { name, variants } => Variant::Enum {
                name: name.clone(),
                selected: variants.first().cloned().unwrap_or_default(),
                variants: variants.clone(),
            },
            VariantTy::MultiSelectionEnum { name, variants } => Variant::MultiSelectionEnum {
                name: name.clone(),
                selected: Vec::new(),
                variants: variants.clone(),
            },
            VariantTy::Binary => Variant::Binary(Vec::new()),
            VariantTy::List => Variant::List(Vec::new()),
            VariantTy::Map => Variant::Map(Map::default()),
            VariantTy::Date => Variant::Date(NaiveDate::default()),
            VariantTy::Time => Variant::Time(NaiveTime::default()),
            VariantTy::DateTime => Variant::DateTime(DateTime::<FixedOffset>::default()),
            VariantTy::Instant => Variant::Instant(NanoSeconds(0)),
            VariantTy::Money {
                currency,
                precision,
            } => Variant::Money {
                currency: currency.clone(),
                precision: *precision,
                value: 0,
            },
            VariantTy::Tolerance(ty) => Variant::Tolerance {
                min: Number::default_of(*ty),
                min_percent: false,
                max: Number::default_of(*ty),
                max_percent: false,
            },
        }
    }

    pub fn str<S: AsRef<str>>(s: S) -> Variant {
        Variant::Str(s.as_ref().to_string())
    }

    pub fn i32(x: i32) -> Variant {
        Variant::Number(Number::I32(x))
    }

    pub fn i64(x: i64) -> Variant {
        Variant::Number(Number::I64(x))
    }

    pub fn u32(x: u32) -> Variant {
        Variant::Number(Number::U32(x))
    }

    pub fn u64(x: u64) -> Variant {
        Variant::Number(Number::U64(x))
    }

    pub fn f32(x: f32) -> Variant {
        Variant::Number(Number::F32(F32(x)))
    }

    pub fn f64(x: f64) -> Variant {
        Variant::Number(Number::F64(F64(x)))
    }

    pub fn si(value: impl Into<Number>, unit: Unit) -> Variant {
        Variant::SI {
            value: value.into(),
            unit,
        }
    }

    pub fn enumeration(
        name: impl Into<Arc<str>>,
        selected: impl Into<String>,
        variants: impl Into<Arc<[String]>>,
    ) -> Variant {
        Variant::Enum {
            name: name.into(),
            selected: selected.into(),
            variants: variants.into(),
        }
    }

    pub fn money(currency: impl Into<Arc<str>>, precision: u8, value: i64) -> Variant {
        Variant::Money {
            currency: currency.into(),
            precision,
            value,
        }
    }
}

impl VariantTy {
    pub const fn i8() -> Self {
        VariantTy::Number(NumberTy::I8)
    }

    pub const fn i16() -> Self {
        VariantTy::Number(NumberTy::I16)
    }

    pub const fn i32() -> Self {
        VariantTy::Number(NumberTy::I32)
    }

    pub const fn i64() -> Self {
        VariantTy::Number(NumberTy::I64)
    }

    pub const fn u8() -> Self {
        VariantTy::Number(NumberTy::U8)
    }

    pub const fn u16() -> Self {
        VariantTy::Number(NumberTy::U16)
    }

    pub const fn u32() -> Self {
        VariantTy::Number(NumberTy::U32)
    }

    pub const fn u64() -> Self {
        VariantTy::Number(NumberTy::U64)
    }

    pub const fn f32() -> Self {
        VariantTy::Number(NumberTy::F32)
    }

    pub const fn f64() -> Self {
        VariantTy::Number(NumberTy::F64)
    }

    pub fn si(number_ty: NumberTy, unit: Unit) -> Self {
        VariantTy::SI { number_ty, unit }
    }

    pub fn enumeration(name: impl Into<Arc<str>>, variants: impl Into<Arc<[String]>>) -> Self {
        VariantTy::Enum {
            name: name.into(),
            variants: variants.into(),
        }
    }

    pub fn money(currency: impl Into<Arc<str>>, precision: u8) -> Self {
        VariantTy::Money {
            currency: currency.into(),
            precision,
        }
    }

    /// Default value of this type, see [`Variant::default_of`].
    pub fn default_value(&self) -> Variant {
        Variant::default_of(self)
    }
}
