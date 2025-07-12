use chrono::{DateTime, FixedOffset};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::num::{ParseFloatError, ParseIntError};
use std::sync::OnceLock;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(
    feature = "rkyv",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
#[cfg_attr(feature = "rkyv", archive(check_bytes))]
#[cfg_attr(feature = "rkyv", archive_attr(derive(Debug)))]
// #[strum_discriminants(derive(Serialize, Deserialize))]
// #[strum_discriminants(name(Ty))]
pub enum Variant {
    #[default]
    Empty,
    Bool(bool),
    Str(String),
    StrList(Vec<String>),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Enum {
        enum_uid: u32,
        discriminant: u32,
    },
    Binary(Vec<u8>),

    #[cfg(not(feature = "rkyv"))]
    List(Vec<Variant>),
    // MultiSelectionEnum (temp coefficient?)
    // Tolerance
    // SI
    // SI range (e.g. temperature range)
    // F64,
    // Currency,
    // Date,
    DateTime(DateTime<FixedOffset>),
    Instant(NanoSeconds),
    // MfgPn or DynEnum?
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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
    Str,
    StrList,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Enum {
        enum_uid: u32,
    },
    Binary,

    #[cfg(not(feature = "rkyv"))]
    List,
    DateTime,
    Instant {
        unit: TimeUnit,
    },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NanoSeconds(pub i64);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum TimeUnit {
    Auto,
    Seconds,
    Milliseconds,
    Microseconds,
    Nanoseconds,
}

static KNOWN_ENUMS: OnceLock<HashMap<u32, EnumDefinition>> = OnceLock::new();

#[derive(Debug)]
pub struct EnumDefinition {
    pub name: String,
    pub values: Vec<(u32, String)>,
}

impl From<&Variant> for VariantTy {
    fn from(value: &Variant) -> Self {
        match value {
            Variant::Empty => VariantTy::Empty,
            Variant::Bool(_) => VariantTy::Bool,
            Variant::Str(_) => VariantTy::Str,
            Variant::StrList(_) => VariantTy::StrList,
            Variant::I32(_) => VariantTy::I32,
            Variant::I64(_) => VariantTy::I64,
            Variant::U8(_) => VariantTy::U8,
            Variant::U16(_) => VariantTy::U16,
            Variant::U32(_) => VariantTy::U32,
            Variant::U64(_) => VariantTy::U64,
            Variant::Enum { enum_uid, .. } => VariantTy::Enum {
                enum_uid: *enum_uid,
            },
            Variant::Binary(_) => VariantTy::Binary,

            #[cfg(not(feature = "rkyv"))]
            Variant::List(_) => VariantTy::List,
            Variant::DateTime(_) => VariantTy::DateTime,
            Variant::Instant(_) => VariantTy::Instant {
                unit: TimeUnit::Nanoseconds,
            },
        }
    }
}

#[derive(Debug)]
pub enum Error {
    ParseIntError(ParseIntError),
    ParseFloatError(ParseFloatError),
    WrongEnumVariantName,
    ParseBoolError,
    Unimplemented,
    CannotConvert(VariantTy, VariantTy),
    Internal,
    Chrono(chrono::ParseError),
    Empty,
}

impl Variant {
    pub fn try_from_str<S: AsRef<str>>(value: S, to: VariantTy) -> Result<Self, Error> {
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
            VariantTy::I32 => Ok(Variant::I32(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            VariantTy::I64 => Ok(Variant::I64(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            VariantTy::U8 => Ok(Variant::U8(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            VariantTy::U16 => Ok(Variant::U16(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            VariantTy::U32 => Ok(Variant::U32(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            VariantTy::U64 => Ok(Variant::U64(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            VariantTy::StrList => Ok(Variant::StrList(
                value
                    .as_ref()
                    .split(&[',', ';'])
                    .map(|s| s.trim().to_string())
                    .filter(|s| !s.is_empty())
                    .collect(),
            )),
            VariantTy::Enum { enum_uid } => variant_name_to_discriminant(enum_uid, value)
                .map(|d| Variant::Enum {
                    enum_uid,
                    discriminant: d,
                })
                .ok_or(Error::WrongEnumVariantName),
            VariantTy::Binary => Err(Error::Unimplemented),

            #[cfg(not(feature = "rkyv"))]
            VariantTy::List => Err(Error::Unimplemented),
            VariantTy::DateTime => {
                let dt = DateTime::parse_from_rfc3339(value.as_ref()).map_err(Error::Chrono)?;
                Ok(Variant::DateTime(dt))
            }
            VariantTy::Instant { unit } => {
                // TODO: handle unit override suffix
                let instant: f64 = value.as_ref().parse().map_err(Error::ParseFloatError)?;
                let mul = match unit {
                    TimeUnit::Seconds => 1e9,
                    TimeUnit::Milliseconds => 1e6,
                    TimeUnit::Microseconds => 1e3,
                    TimeUnit::Nanoseconds => 1e0,
                    // TODO: handle unit suffix: [s], s, ms, etc
                    TimeUnit::Auto => 1e0,
                };
                let instant = instant * mul;
                Ok(Variant::Instant(NanoSeconds(instant as i64)))
            }
        }
    }

    pub fn from_str<S: AsRef<str>>(value: S, to: VariantTy) -> Self {
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
            Variant::I32(_) => false,
            Variant::I64(_) => false,
            Variant::U8(_) => false,
            Variant::U16(_) => false,
            Variant::U32(_) => false,
            Variant::U64(_) => false,
            Variant::Enum { .. } => false,
            Variant::Binary(b) => b.is_empty(),

            #[cfg(not(feature = "rkyv"))]
            Variant::List(l) => l.is_empty(),
            Variant::DateTime(_) => false,
            Variant::Instant(_) => false,
        }
    }

    pub fn convert_to(self, ty: VariantTy) -> Result<Variant, Error> {
        if VariantTy::from(&self) == ty {
            return Ok(self);
        }
        match ty {
            VariantTy::Empty => Ok(Variant::Empty),
            VariantTy::Str => match self {
                Variant::Str(s) => Ok(Variant::Str(s)),
                o => Ok(Variant::Str(format!("{}", o))),
            },
            VariantTy::Bool => match self {
                Variant::Bool(b) => Ok(Variant::Bool(b)),
                Variant::Str(s) => Variant::try_from_str(s, ty),
                o => Err(Error::CannotConvert(VariantTy::from(&o), ty)),
            },
            VariantTy::StrList => match self {
                Variant::StrList(l) => Ok(Variant::StrList(l)),
                Variant::Str(s) => Variant::try_from_str(s, ty),
                o => Err(Error::CannotConvert(VariantTy::from(&o), ty)),
            },
            VariantTy::I32 => match self {
                Variant::I32(x) => Ok(Variant::I32(x)),
                Variant::Str(s) => Variant::try_from_str(s, ty),
                o => Err(Error::CannotConvert(VariantTy::from(&o), ty)),
            },
            VariantTy::I64 => match self {
                Variant::I64(x) => Ok(Variant::I64(x)),
                Variant::Str(s) => Variant::try_from_str(s, ty),
                o => Err(Error::CannotConvert(VariantTy::from(&o), ty)),
            },
            VariantTy::U8 => match self {
                Variant::U8(x) => Ok(Variant::U8(x)),
                Variant::Str(s) => Variant::try_from_str(s, ty),
                o => Err(Error::CannotConvert(VariantTy::from(&o), ty)),
            },
            VariantTy::U16 => match self {
                Variant::U16(x) => Ok(Variant::U16(x)),
                Variant::Str(s) => Variant::try_from_str(s, ty),
                o => Err(Error::CannotConvert(VariantTy::from(&o), ty)),
            },
            VariantTy::U32 => match self {
                Variant::U32(x) => Ok(Variant::U32(x)),
                Variant::Str(s) => Variant::try_from_str(s, ty),
                o => Err(Error::CannotConvert(VariantTy::from(&o), ty)),
            },
            VariantTy::U64 => match self {
                Variant::U64(x) => Ok(Variant::U64(x)),
                Variant::Str(s) => Variant::try_from_str(s, ty),
                o => Err(Error::CannotConvert(VariantTy::from(&o), ty)),
            },
            VariantTy::Enum { enum_uid: to_uid } => match self {
                Variant::Enum {
                    enum_uid,
                    discriminant,
                } => {
                    if enum_uid == to_uid {
                        Ok(Variant::Enum {
                            enum_uid,
                            discriminant,
                        })
                    } else {
                        Err(Error::CannotConvert(
                            VariantTy::Enum { enum_uid },
                            VariantTy::Enum { enum_uid: to_uid },
                        ))
                    }
                }
                Variant::Str(s) => Variant::try_from_str(s, ty),
                o => Err(Error::CannotConvert(VariantTy::from(&o), ty)),
            },
            VariantTy::Binary => Err(Error::Unimplemented),

            #[cfg(not(feature = "rkyv"))]
            VariantTy::List => Err(Error::Unimplemented),
            VariantTy::DateTime => match self {
                Variant::DateTime(dt) => Ok(Variant::DateTime(dt)),
                Variant::Str(s) => Ok(Variant::try_from_str(s, VariantTy::DateTime)?),
                o => Err(Error::CannotConvert(VariantTy::from(&o), ty)),
            },
            VariantTy::Instant { .. } => match self {
                Variant::Instant(i) => Ok(Variant::Instant(i)),
                Variant::Str(s) => Ok(Variant::try_from_str(
                    s,
                    VariantTy::Instant {
                        unit: TimeUnit::Seconds,
                    },
                )?),
                o => Err(Error::CannotConvert(VariantTy::from(&o), ty)),
            },
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

    pub fn as_u8(&self) -> Result<u8, Error> {
        match self {
            Variant::U8(x) => Ok(*x),
            Variant::Str(s) => {
                let v_u8 = Variant::try_from_str(s, VariantTy::U8)?;
                let Variant::U8(x) = v_u8 else {
                    return Err(Error::Internal);
                };
                Ok(x)
            }
            o => Err(Error::CannotConvert(VariantTy::from(o), VariantTy::U8)),
        }
    }

    pub fn as_date_time(&self) -> Result<DateTime<FixedOffset>, Error> {
        match self {
            Variant::DateTime(dt) => Ok(*dt),
            Variant::Str(s) => {
                let dt = Variant::try_from_str(s, VariantTy::DateTime)?;
                let Variant::DateTime(dt) = dt else {
                    return Err(Error::Internal);
                };
                Ok(dt)
            }
            o => Err(Error::CannotConvert(
                VariantTy::from(o),
                VariantTy::DateTime,
            )),
        }
    }

    pub fn as_instant(&self) -> Result<NanoSeconds, Error> {
        match self {
            Variant::Instant(instant) => Ok(*instant),
            Variant::Str(s) => {
                let instant = Variant::try_from_str(
                    s,
                    VariantTy::Instant {
                        unit: TimeUnit::Seconds,
                    },
                )?;
                let Variant::Instant(instant) = instant else {
                    return Err(Error::Internal);
                };
                Ok(instant)
            }
            o => Err(Error::CannotConvert(
                VariantTy::from(o),
                VariantTy::Instant {
                    unit: TimeUnit::Seconds,
                },
            )),
        }
    }

    pub fn default_of(ty: VariantTy) -> Variant {
        match ty {
            VariantTy::Empty => Variant::Empty,
            VariantTy::Str => Variant::Str(String::new()),
            VariantTy::Bool => Variant::Bool(false),
            VariantTy::StrList => Variant::StrList(Vec::new()),
            VariantTy::I32 => Variant::I32(0),
            VariantTy::I64 => Variant::I64(0),
            VariantTy::U8 => Variant::U8(0),
            VariantTy::U16 => Variant::U16(0),
            VariantTy::U32 => Variant::U32(0),
            VariantTy::U64 => Variant::U64(0),
            VariantTy::Enum { enum_uid } => Variant::Enum {
                enum_uid,
                discriminant: 0,
            },
            VariantTy::Binary => Variant::Binary(Vec::new()),
            VariantTy::List => Variant::List(Vec::new()),
            VariantTy::DateTime => Variant::Empty, // TODO: Change to Option?
            VariantTy::Instant { .. } => Variant::Empty,
        }
    }

    pub fn str<S: AsRef<str>>(s: S) -> Variant {
        Variant::Str(s.as_ref().to_string())
    }
}

impl TryInto<u32> for &Variant {
    type Error = ();

    fn try_into(self) -> Result<u32, Self::Error> {
        match self {
            Variant::U32(x) => Ok(*x),
            Variant::I64(_x) => todo!(),
            Variant::U64(_x) => todo!(),
            _ => Err(()),
        }
    }
}

impl TryInto<Vec<String>> for &Variant {
    type Error = ();

    fn try_into(self) -> Result<Vec<String>, Self::Error> {
        match self {
            Variant::Str(s) => Ok(s
                .split([',', ';'])
                .filter(|s| !s.is_empty())
                .map(|s| s.trim().to_owned())
                .collect()),
            Variant::StrList(list) => Ok(list.clone()),
            _ => Err(()),
        }
    }
}

impl Display for VariantTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VariantTy::Empty => write!(f, "Empty"),
            VariantTy::Bool => write!(f, "Bool"),
            VariantTy::Str => write!(f, "String"),
            VariantTy::StrList => write!(f, "List<String>"),
            VariantTy::I32 => write!(f, "i32"),
            VariantTy::I64 => write!(f, "i64"),
            VariantTy::U8 => write!(f, "u8"),
            VariantTy::U16 => write!(f, "u16"),
            VariantTy::U32 => write!(f, "u32"),
            VariantTy::U64 => write!(f, "u64"),
            VariantTy::Enum { enum_uid } => write!(
                f,
                "enum {}",
                uid_to_enum_name(*enum_uid).unwrap_or("Undefined")
            ),
            VariantTy::Binary => write!(f, "Binary"),

            #[cfg(not(feature = "rkyv"))]
            VariantTy::List => write!(f, "List<Value>"),
            VariantTy::DateTime => write!(f, "DateTime<RFC3339>"),
            VariantTy::Instant { unit } => write!(f, "Instant [{unit}]"),
        }
    }
}

impl Display for TimeUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let unit = match self {
            TimeUnit::Auto => "auto",
            TimeUnit::Seconds => "s",
            TimeUnit::Milliseconds => "ms",
            TimeUnit::Microseconds => "us",
            TimeUnit::Nanoseconds => "ns",
        };
        write!(f, "{unit}")
    }
}

impl Display for Variant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Variant::Empty => write!(f, "Empty"),
            Variant::Bool(b) => write!(f, "{b}"),
            Variant::Str(s) => write!(f, "{s}"),
            Variant::I32(x) => write!(f, "{x}"),
            Variant::I64(x) => write!(f, "{x}"),
            Variant::U8(x) => write!(f, "{x}"),
            Variant::U16(x) => write!(f, "{x}"),
            Variant::U32(x) => write!(f, "{x}"),
            Variant::U64(x) => write!(f, "{x}"),
            Variant::StrList(list) => {
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
            Variant::Enum {
                enum_uid,
                discriminant,
            } => match uid_to_variant_name(*enum_uid, *discriminant) {
                Some(variant_name) => write!(f, "{variant_name}"),
                None => write!(f, "Unknown enum {}", enum_uid),
            },
            Variant::Binary(b) => write!(f, "Binary[{}B]", b.len()),

            #[cfg(not(feature = "rkyv"))]
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
            Variant::DateTime(dt) => write!(f, "{dt}"),
            Variant::Instant(instant) => write!(f, "{}ns", instant.0),
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

pub fn register_enums(defs: HashMap<u32, EnumDefinition>) {
    KNOWN_ENUMS.set(defs).unwrap();
}

pub fn name_to_uid<S: AsRef<str>>(name: S) -> Option<u32> {
    let known_enums = KNOWN_ENUMS.get()?;
    let name = name.as_ref();
    known_enums
        .iter()
        .find(|&(_, def)| def.name == name)
        .map(|(id, _)| *id)
}

pub fn uid_to_full_name(enum_uid: u32, discriminant: u32) -> Option<(&'static str, &'static str)> {
    let known_enums = KNOWN_ENUMS.get()?;
    known_enums.get(&enum_uid).and_then(|def| {
        def.values
            .iter()
            .find(|(id, _)| *id == discriminant)
            .map(|(_, variant_name)| (def.name.as_str(), variant_name.as_str()))
    })
}

pub fn uid_to_variant_name(enum_uid: u32, discriminant: u32) -> Option<&'static str> {
    let known_enums = KNOWN_ENUMS.get()?;
    known_enums.get(&enum_uid).and_then(|def| {
        def.values
            .iter()
            .find(|(id, _)| *id == discriminant)
            .map(|(_, variant_name)| variant_name.as_str())
    })
}

pub fn variant_name_to_discriminant<S: AsRef<str>>(enum_uid: u32, variant_name: S) -> Option<u32> {
    let known_enums = KNOWN_ENUMS.get()?;
    known_enums.get(&enum_uid).and_then(|def| {
        def.values
            .iter()
            .find(|(_, n)| n == variant_name.as_ref())
            .map(|(discriminant, _)| *discriminant)
    })
}

pub fn uid_to_enum_name(enum_uid: u32) -> Option<&'static str> {
    let known_enums = KNOWN_ENUMS.get()?;
    known_enums.get(&enum_uid).map(|def| def.name.as_str())
}

pub fn variant_names(enum_uid: u32) -> Option<&'static [(u32, String)]> {
    let known_enums = KNOWN_ENUMS.get()?;
    known_enums.get(&enum_uid).map(|def| &def.values[..])
}
