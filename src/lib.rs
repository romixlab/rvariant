use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::sync::OnceLock;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    Empty,
    Bool(bool),
    Str(String),
    StrList(Vec<String>),
    I32(i32),
    I64(i64),
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
    // DateTime,
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
    U32,
    U64,
    Enum {
        enum_uid: u32,
    },
    Binary,

    #[cfg(not(feature = "rkyv"))]
    List,
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
            Variant::U32(_) => VariantTy::U32,
            Variant::U64(_) => VariantTy::U64,
            Variant::Enum { enum_uid, .. } => VariantTy::Enum {
                enum_uid: *enum_uid,
            },
            Variant::Binary(_) => VariantTy::Binary,

            #[cfg(not(feature = "rkyv"))]
            Variant::List(_) => VariantTy::List,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    ParseIntError(std::num::ParseIntError),
    WrongEnumVariantName,
    ParseBoolError,
    Unimplemented,
    CannotConvert(VariantTy, VariantTy),
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
            VariantTy::I32 => {
                let x: Result<i32, _> = value.as_ref().parse();
                match x {
                    Ok(x) => Ok(Variant::I32(x)),
                    Err(e) => Err(Error::ParseIntError(e)),
                }
            }
            VariantTy::I64 => {
                let x: Result<i64, _> = value.as_ref().parse();
                match x {
                    Ok(x) => Ok(Variant::I64(x)),
                    Err(e) => Err(Error::ParseIntError(e)),
                }
            }
            VariantTy::U32 => {
                let x: Result<u32, _> = value.as_ref().parse();
                match x {
                    Ok(x) => Ok(Variant::U32(x)),
                    Err(e) => Err(Error::ParseIntError(e)),
                }
            }
            VariantTy::U64 => {
                let x: Result<u64, _> = value.as_ref().parse();
                match x {
                    Ok(x) => Ok(Variant::U64(x)),
                    Err(e) => Err(Error::ParseIntError(e)),
                }
            }
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
            Variant::U32(_) => false,
            Variant::U64(_) => false,
            Variant::Enum { .. } => false,
            Variant::Binary(b) => b.is_empty(),

            #[cfg(not(feature = "rkyv"))]
            Variant::List(l) => l.is_empty(),
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
        }
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
        }
    }
}

impl Display for Variant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Variant::Empty => write!(f, "Empty"),
            Variant::Bool(b) => write!(f, "{b}"),
            Variant::Str(s) => write!(f, "{s}"),
            Variant::I64(x) => write!(f, "{x}"),
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
            Variant::I32(x) => write!(f, "{x}"),
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
