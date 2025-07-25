mod number;
pub mod util;

pub use crate::number::{F32, F64, Number, NumberTy};
use chrono::{DateTime, FixedOffset, NaiveDate, NaiveTime};
use indexmap::IndexMap;
pub use si_dynamic;
use si_dynamic::{BaseUnit, OhmF32, Quantity, Unit};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use strum::{AsRefStr, EnumDiscriminants, EnumIter};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(
    feature = "rkyv",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
#[cfg_attr(feature = "rkyv", archive(check_bytes))]
#[cfg_attr(feature = "rkyv", archive_attr(derive(Debug)))]
pub enum Variant {
    #[default]
    Empty,
    Bool(bool),
    Str(String),
    StrList(Vec<String>),
    Number(Number),
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
        name: Arc<String>,
        selected: String,
        variants: Arc<Vec<String>>,
    },
    MultiSelectionEnum {
        name: Arc<String>,
        selected: Vec<String>,
        variants: Arc<Vec<String>>,
    },

    Binary(Vec<u8>),
    List(Vec<Variant>),
    Map(Map),

    Date(NaiveDate),
    Time(NaiveTime),
    DateTime(DateTime<FixedOffset>),
    Instant(NanoSeconds),

    Money {
        currency: Arc<String>,
        precision: u8,
        value: u64,
    },

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
#[cfg_attr(
    feature = "rkyv",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
#[cfg_attr(feature = "rkyv", archive(check_bytes))]
#[cfg_attr(feature = "rkyv", archive_attr(derive(Debug)))]
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
        name: Arc<String>,
        variants: Arc<Vec<String>>,
    },
    MultiSelectionEnum {
        name: Arc<String>,
        variants: Arc<Vec<String>>,
    },

    Binary,
    List,
    Map,

    Date,
    Time,
    DateTime,
    Instant,

    Money {
        currency: Arc<String>,
        precision: u8,
    },

    Tolerance(NumberTy),
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Map(pub IndexMap<Variant, Variant>);
impl Eq for Map {}
impl PartialEq for Map {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}
impl PartialOrd for Map {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Map {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.iter().cmp(other.0.iter())
    }
}
impl Hash for Map {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.iter().for_each(|x| x.hash(state));
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NanoSeconds(pub i64);

// #[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
// #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
// pub enum TimeUnit {
//     Auto,
//     Seconds,
//     Milliseconds,
//     Microseconds,
//     Nanoseconds,
// }

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
                end: _,
                end_inclusive,
                unit,
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

#[derive(Debug)]
pub enum Error {
    ParseIntError(std::num::ParseIntError),
    ParseFloatError(num_traits::ParseFloatError),
    Verbatim(String),
    WrongEnumVariantName,
    ParseBoolError,
    Unimplemented,
    CannotConvert(Box<VariantTy>, Box<VariantTy>),
    Internal,
    Chrono(chrono::ParseError),
    Empty,
}

macro_rules! as_number {
    ($name: ident, $ty:ty, $variant:ident) => {
        pub fn $name(&self) -> Result<$ty, Error> {
            if let Variant::Number(Number::$variant(x)) = self {
                return Ok(*x);
            }
            if let Variant::Number(Number::$variant(x)) = self
                .clone()
                .convert_to(&VariantTy::Number(NumberTy::$variant))?
            {
                Ok(x)
            } else {
                Err(Error::Internal)
            }
        }
    };
}

impl Variant {
    pub fn try_from_str<S: AsRef<str>>(value: S, to: &VariantTy) -> Result<Self, Error> {
        match to {
            VariantTy::Empty => Ok(Variant::Empty),
            VariantTy::Bool => {
                let b = value.as_ref().to_lowercase();
                if b == "true" {
                    Ok(Variant::Bool(true))
                } else if b == "false" {
                    Ok(Variant::Bool(false))
                } else {
                    Err(Error::ParseBoolError)
                }
            }
            VariantTy::Str => Ok(Variant::Str(value.as_ref().to_owned())),
            VariantTy::StrList => Ok(Variant::StrList(
                value
                    .as_ref()
                    .split(&[',', ';', '\t'])
                    .map(|s| s.trim().to_string())
                    .filter(|s| !s.is_empty())
                    .collect(),
            )),

            VariantTy::Number(number_ty) => {
                Ok(Variant::Number(Number::try_from_str(value, *number_ty)?))
            }
            VariantTy::SI { number_ty: _, unit } => {
                if unit.base == BaseUnit::Ohm {
                    let quantity = OhmF32::parse(value)
                        .map_err(|e| Error::Verbatim(format!("SI Ohm parse failed {:?}", e)))?;
                    Ok(Variant::SI {
                        value: Number::F32(quantity.0.into()), // TODO: SI: do not assume f32
                        unit: unit.clone(),
                    })
                } else {
                    let quantity = Quantity::parse(value)
                        .map_err(|e| Error::Verbatim(format!("SI value parse failed: {}", e)))?;
                    // let value = Number::try_from_str(quantity.number, *number_ty)?; // TODO: SI: do not assume f32
                    let value = Number::F32(
                        quantity
                            .as_f32()
                            .map_err(|e| Error::Verbatim(format!("SI parse float error: {e}")))?
                            .into(),
                    );
                    if quantity.unit.base == unit.base {
                        Ok(Variant::SI {
                            value,
                            unit: unit.clone(),
                        })
                    } else if quantity.unit.base.name().eq_ignore_ascii_case("vdc")
                        || quantity.unit.base.name().eq_ignore_ascii_case("vac")
                    {
                        Ok(Variant::SI {
                            value,
                            unit: Unit::base(BaseUnit::Volt),
                        })
                    } else {
                        Err(Error::Verbatim(format!(
                            "Wrong SI unit, expected: {unit}, got: {}",
                            quantity.unit
                        )))
                    }
                }
            }
            VariantTy::SIRange { .. } => Err(Error::Unimplemented),

            VariantTy::Enum { .. } => Err(Error::Unimplemented),
            VariantTy::MultiSelectionEnum { .. } => Err(Error::Unimplemented),

            VariantTy::Binary => Err(Error::Unimplemented),
            VariantTy::List => Err(Error::Unimplemented),
            VariantTy::Map => Err(Error::Unimplemented),

            VariantTy::Date => Err(Error::Unimplemented),
            VariantTy::Time => Err(Error::Unimplemented),
            VariantTy::DateTime => {
                let dt = DateTime::parse_from_rfc3339(value.as_ref()).map_err(Error::Chrono)?;
                Ok(Variant::DateTime(dt))
            }
            VariantTy::Instant => {
                let value = value.as_ref().trim();
                let (value, mul) = if let Some(value) = value.strip_suffix("s") {
                    (value, 1e9)
                } else if let Some(value) = value.strip_suffix("ms") {
                    (value, 1e6)
                } else if let Some(value) = value.strip_suffix("us") {
                    (value, 1e3)
                } else if let Some(value) = value.strip_suffix("ns") {
                    (value, 1e0)
                } else {
                    (value, 1e0)
                };
                let instant: f64 = parse_int::parse(value).map_err(Error::ParseFloatError)?;
                let instant = instant * mul;
                Ok(Variant::Instant(NanoSeconds(instant as i64)))
            }

            VariantTy::Money { .. } => Err(Error::Unimplemented),

            VariantTy::Tolerance(_) => Err(Error::Unimplemented),
        }
    }

    pub fn from_str<S: AsRef<str>>(value: S, to: &VariantTy) -> Self {
        let value = value.as_ref();
        match Self::try_from_str(value, to) {
            Ok(value) => value,
            Err(_) => Variant::Str(value.to_string()),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Variant::Empty => true,
            Variant::Bool(_) => false,
            Variant::Str(s) => s.is_empty(),
            Variant::StrList(l) => l.is_empty(),

            Variant::Number(_) => false,
            Variant::SI { .. } => false,
            Variant::SIRange { .. } => false,

            Variant::Enum { .. } => false,
            Variant::MultiSelectionEnum { .. } => false,

            Variant::Binary(b) => b.is_empty(),
            Variant::List(l) => l.is_empty(),
            Variant::Map(m) => m.0.is_empty(),

            Variant::Date(_) => false,
            Variant::Time(_) => false,
            Variant::DateTime(_) => false,
            Variant::Instant { .. } => false,

            Variant::Money { .. } => false,

            Variant::Tolerance { .. } => false,
        }
    }

    pub fn convert_to(self, to: &VariantTy) -> Result<Variant, Error> {
        if &VariantTy::from(&self) == to {
            return Ok(self);
        }
        // all non-str cases
        match to {
            VariantTy::Empty => return Ok(Variant::Empty),
            VariantTy::Number(number_ty) => match self {
                Variant::Number(n) => return Ok(Variant::Number(n.convert_to(*number_ty)?)),
                Variant::SI { value, .. } => {
                    return Ok(Variant::Number(value.convert_to(*number_ty)?));
                }
                _ => {}
            },
            _ => {}
        }
        if let Variant::Str(s) = self {
            Ok(Variant::try_from_str(s, to)?)
        } else {
            Err(Error::CannotConvert(
                Box::new(VariantTy::from(&self)),
                Box::new(to.clone()),
            ))
        }
    }

    pub fn as_str(&self) -> Result<&str, Error> {
        match self {
            Variant::Empty => Ok(""),
            Variant::Str(s) => Ok(s.as_str()),
            _ => Err(Error::Unimplemented),
        }
    }

    pub fn as_string(&self) -> Result<String, Error> {
        match self {
            Variant::Empty => Ok(String::new()),
            Variant::Str(s) => Ok(s.clone()),
            _ => Err(Error::Unimplemented),
        }
    }

    pub fn as_non_empty_str(&self) -> Result<&str, Error> {
        match self {
            Variant::Str(s) => {
                if s.is_empty() {
                    Err(Error::Empty)
                } else {
                    Ok(s.as_str())
                }
            }
            _ => Err(Error::Unimplemented),
        }
    }

    pub fn as_non_empty_string(&self) -> Result<String, Error> {
        match self {
            Variant::Str(s) => {
                if s.is_empty() {
                    Err(Error::Empty)
                } else {
                    Ok(s.clone())
                }
            }
            _ => Err(Error::Unimplemented),
        }
    }

    as_number!(as_i8, i8, I8);
    as_number!(as_i16, i16, I16);
    as_number!(as_i32, i32, I32);
    as_number!(as_i64, i64, I64);
    as_number!(as_u8, u8, U8);
    as_number!(as_u16, u16, U16);
    as_number!(as_u32, u32, U32);
    as_number!(as_u64, u64, U64);

    pub fn as_date_time(&self) -> Result<DateTime<FixedOffset>, Error> {
        match self {
            Variant::DateTime(dt) => Ok(*dt),
            Variant::Str(s) => {
                let dt = Variant::try_from_str(s, &VariantTy::DateTime)?;
                let Variant::DateTime(dt) = dt else {
                    return Err(Error::Internal);
                };
                Ok(dt)
            }
            o => Err(Error::CannotConvert(
                Box::new(VariantTy::from(o)),
                Box::new(VariantTy::DateTime),
            )),
        }
    }

    pub fn as_instant(&self) -> Result<NanoSeconds, Error> {
        match self {
            Variant::Instant(value) => Ok(*value),
            Variant::Str(s) => {
                let instant = Variant::try_from_str(s, &VariantTy::Instant)?;
                let Variant::Instant(value) = instant else {
                    return Err(Error::Internal);
                };
                Ok(value)
            }
            o => Err(Error::CannotConvert(
                Box::new(VariantTy::from(o)),
                Box::new(VariantTy::Instant),
            )),
        }
    }

    pub fn as_base_unit(&self, base_unit: BaseUnit) -> Result<Number, Error> {
        if let Variant::SI { value, unit } = self {
            if unit.base == base_unit {
                Ok(*value)
            } else {
                Err(Error::Verbatim(format!(
                    "expected unit: {base_unit}, found: {}",
                    unit.base
                )))
            }
        } else {
            match self.clone().convert_to(&VariantTy::SI {
                number_ty: NumberTy::F32,
                unit: Unit::base(base_unit.clone()),
            }) {
                Ok(quantity) => quantity.as_base_unit(base_unit),
                Err(e) => Err(Error::Verbatim(format!(
                    "expected SI value, found {}, convert failed: {e:?}",
                    VariantTy::from(self)
                ))),
            }
        }
    }

    pub fn default_of(ty: &VariantTy) -> Variant {
        match ty {
            VariantTy::Empty => Variant::Empty,
            VariantTy::Str => Variant::Str(String::new()),
            VariantTy::Bool => Variant::Bool(false),
            VariantTy::StrList => Variant::StrList(Vec::new()),
            VariantTy::Number(ty) => Variant::Number(Number::default_of(*ty)),
            VariantTy::Binary => Variant::Binary(Vec::new()),
            VariantTy::List => Variant::List(Vec::new()),
            VariantTy::Instant => Variant::Empty,
            VariantTy::SI { number_ty, unit } => Variant::SI {
                value: Number::default_of(*number_ty),
                unit: unit.clone(),
            },
            VariantTy::SIRange { .. } => Variant::Empty,
            VariantTy::Enum { .. } => Variant::Empty,
            VariantTy::MultiSelectionEnum { .. } => Variant::Empty,
            VariantTy::Map => Variant::Map(Map(IndexMap::new())),
            VariantTy::Date => Variant::Empty,
            VariantTy::Time => Variant::Empty,
            VariantTy::DateTime => Variant::Empty,
            VariantTy::Money { .. } => Variant::Empty,
            VariantTy::Tolerance(_) => Variant::Empty,
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

    pub fn u64(x: u64) -> Variant {
        Variant::Number(Number::U64(x))
    }
}

impl VariantTy {
    pub const fn i32() -> Self {
        VariantTy::Number(NumberTy::I32)
    }

    pub const fn u32() -> Self {
        VariantTy::Number(NumberTy::U32)
    }

    pub const fn i64() -> Self {
        VariantTy::Number(NumberTy::I64)
    }

    pub const fn u64() -> Self {
        VariantTy::Number(NumberTy::U64)
    }
}

impl Display for VariantTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VariantTy::Empty => write!(f, "Empty"),
            VariantTy::Bool => write!(f, "Bool"),
            VariantTy::Str => write!(f, "String"),
            VariantTy::StrList => write!(f, "List<String>"),
            VariantTy::Number(ty) => write!(f, "{}", ty.as_ref()),
            VariantTy::Enum { name, variants: _ } => write!(f, "enum {name}",),
            VariantTy::MultiSelectionEnum { name, variants: _ } => {
                write!(f, "multi-selection {name}",)
            }
            VariantTy::Binary => write!(f, "Binary"),
            VariantTy::List => write!(f, "List<Variant>"),
            VariantTy::Map => write!(f, "Map<Variant, Variant>"),
            VariantTy::Instant => write!(f, "Instant [ns]"),
            VariantTy::SI { number_ty, unit } => write!(f, "{} [{unit}]", number_ty.as_ref()),
            VariantTy::SIRange {
                number_ty,
                start_inclusive,
                end_inclusive,
                unit,
            } => {
                let start = if *start_inclusive { '[' } else { '(' };
                let end = if *end_inclusive { ']' } else { ')' };
                write!(f, "{start}{}{end} [{unit}]", number_ty.as_ref())
            }
            VariantTy::Date => write!(f, "NaiveDate"),
            VariantTy::Time => write!(f, "NaiveTime"),
            VariantTy::DateTime => write!(f, "DateTime<RFC3339>"),
            VariantTy::Money {
                currency,
                precision,
            } => write!(f, "Money({currency}, {precision})"),
            VariantTy::Tolerance(n) => write!(f, "Tolerance({})", n.as_ref()),
        }
    }
}

impl Display for Variant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Variant::Empty => write!(f, "Empty"),
            Variant::Bool(b) => write!(f, "{b}"),
            Variant::Str(s) => write!(f, "{s}"),
            Variant::StrList(list) => {
                let mut iter = list.iter().peekable();
                while let Some(s) = iter.next() {
                    if s.is_empty() {
                        // write!(f, "''")?;
                        write!(f, "⛶")?;
                    } else {
                        write!(f, "{s}")?;
                    }
                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }
                Ok(())
            }
            Variant::Number(n) => write!(f, "{n}"),
            Variant::Binary(b) => write!(f, "Binary[{}B]", b.len()),

            Variant::List(list) => {
                let mut iter = list.iter().peekable();
                while let Some(s) = iter.next() {
                    write!(f, "{s}")?;
                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }
                Ok(())
            }
            Variant::Instant(instant) => write!(f, "{} ns", instant.0),
            Variant::SI { value, unit } => write!(f, "{value} {unit}"),
            Variant::SIRange {
                start,
                start_inclusive,
                end,
                end_inclusive,
                unit,
            } => {
                let start_sym = if *start_inclusive { '[' } else { '(' };
                let end_sym = if *end_inclusive { ']' } else { ')' };
                write!(f, "{start_sym}{start}, {end}{end_sym} [{unit}]")
            }
            Variant::Enum { name, selected, .. } => write!(f, "{name}::{selected}"),
            Variant::MultiSelectionEnum { name, selected, .. } => write!(f, "{name}::{selected:?}"),
            Variant::Map(m) => write!(f, "{:?}", m.0),
            Variant::Date(date) => write!(f, "{date}"),
            Variant::Time(time) => write!(f, "{time}"),
            Variant::DateTime(dt) => write!(f, "{dt}"),
            Variant::Money {
                currency,
                precision,
                value,
            } => {
                let value = *value as f32 / 10u32.pow(*precision as u32) as f32;
                write!(f, "{value} {currency}")
            }
            Variant::Tolerance {
                min,
                min_percent,
                max,
                max_percent,
            } => {
                write!(f, "{min}")?;
                if *min_percent {
                    write!(f, "%")?;
                }
                write!(f, ", {max}")?;
                if *max_percent {
                    write!(f, "%")?;
                }
                Ok(())
            }
        }
    }
}

#[cfg(feature = "rkyv")]
impl Display for ArchivedVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ArchivedVariant::Empty => write!(f, "Empty"),
            ArchivedVariant::Bool(b) => write!(f, "{b}"),
            ArchivedVariant::Str(s) => write!(f, "{s}"),
            ArchivedVariant::I32(x) => write!(f, "{x}"),
            ArchivedVariant::I64(x) => write!(f, "{x}"),
            ArchivedVariant::U32(x) => write!(f, "{x}"),
            ArchivedVariant::U64(x) => write!(f, "{x}"),
            ArchivedVariant::StrList(list) => {
                let mut iter = list.iter().peekable();
                while let Some(s) = iter.next() {
                    if s.is_empty() {
                        write!(f, "⛶")?;
                    } else {
                        write!(f, "{s}")?;
                    }
                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }
                Ok(())
            }
            ArchivedVariant::Enum {
                enum_uid,
                discriminant,
            } => match uid_to_variant_name(*enum_uid, *discriminant) {
                Some(variant_name) => write!(f, "{variant_name}"),
                None => write!(f, "Unknown enum {}", enum_uid),
            },
            ArchivedVariant::Binary(b) => write!(f, "Binary[{}B]", b.len()),

            #[cfg(not(feature = "rkyv"))]
            ArchivedVariant::List(list) => {
                let mut iter = list.iter().peekable();
                while let Some(s) = iter.next() {
                    write!(f, "{s}")?;
                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }
                Ok(())
            }
        }
    }
}
