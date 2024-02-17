use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, PartialEq, Eq, Default, Hash)]
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
    // For cells that logically cannot have values, e.g. different entity without particular property
    Never,
    #[default]
    Empty,
    Bool(bool),
    Str(String),
    StrList(Vec<String>),
    I32(i32),
    I64(i64),
    U32(u32),
    U64(u64),
    // Enum {
    //     enum_uid: u32,
    //     discriminant: u32,
    // },
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
    Never,
    Empty,
    Bool,
    Str,
    StrList,
    I32,
    I64,
    U32,
    U64,
    // Enum { enum_uid: u32 },
    Binary,

    #[cfg(not(feature = "rkyv"))]
    List,
}

impl From<&Variant> for VariantTy {
    fn from(value: &Variant) -> Self {
        match value {
            Variant::Never => VariantTy::Never,
            Variant::Empty => VariantTy::Empty,
            Variant::Bool(_) => VariantTy::Bool,
            Variant::Str(_) => VariantTy::Str,
            Variant::StrList(_) => VariantTy::StrList,
            Variant::I32(_) => VariantTy::I32,
            Variant::I64(_) => VariantTy::I64,
            Variant::U32(_) => VariantTy::U32,
            Variant::U64(_) => VariantTy::U64,
            // Value::Enum { enum_uid, .. } => Ty::Enum {
            //     enum_uid: *enum_uid,
            // },
            Variant::Binary(_) => VariantTy::Binary,

            #[cfg(not(feature = "rkyv"))]
            Variant::List(_) => VariantTy::List,
        }
    }
}

pub enum Error {
    ParseIntError(std::num::ParseIntError),
    NeverCast,
    WrongEnumVariantName,
    ParseBoolError,
    Unimplemented,
}

impl Variant {
    pub fn try_from_str<S: AsRef<str>>(value: S, to: VariantTy) -> Result<Self, Error> {
        match to {
            VariantTy::Never => Err(Error::NeverCast),
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
            // Ty::Enum { enum_uid } => enum_value::variant_name_to_discriminant(enum_uid, value)
            //     .map(|d| Value::Enum {
            //         enum_uid,
            //         discriminant: d,
            //     })
            //     .ok_or(Error::WrongEnumVariantName),
            VariantTy::Binary => Err(Error::Unimplemented),

            #[cfg(not(feature = "rkyv"))]
            VariantTy::List => Err(Error::Unimplemented),
        }
    }

    pub fn from_str<S: AsRef<str>>(value: S, to: VariantTy) -> Self {
        let value = value.as_ref();
        if value.is_empty() {
            Variant::Empty
        } else {
            match Self::try_from_str(value, to) {
                Ok(value) => value,
                Err(_) => Variant::Str(value.to_string()),
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Variant::Never => true,
            Variant::Empty => true,
            Variant::Bool(_) => false,
            Variant::Str(s) => s.is_empty(),
            Variant::StrList(l) => l.is_empty(),
            Variant::I32(_) => false,
            Variant::I64(_) => false,
            Variant::U32(_) => false,
            Variant::U64(_) => false,
            // Value::Enum { .. } => false,
            Variant::Binary(b) => b.is_empty(),

            #[cfg(not(feature = "rkyv"))]
            Variant::List(l) => l.is_empty(),
        }
    }

    //
    // pub fn try_into(&self, ty: Ty) -> Option<Value> {
    //     match ty {
    //         Ty::Str => match self {
    //             Value::Str(s) => Some(Value::Str(s.clone())),
    //             other => Some(Value::Str(format!("{}", other)))
    //         }
    //         _ => unimplemented!()
    //     }
    // }
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
            VariantTy::Never => write!(f, "Never"),
            VariantTy::Empty => write!(f, "Empty"),
            VariantTy::Bool => write!(f, "Bool"),
            VariantTy::Str => write!(f, "String"),
            VariantTy::StrList => write!(f, "List<String>"),
            VariantTy::I32 => write!(f, "i32"),
            VariantTy::I64 => write!(f, "i64"),
            VariantTy::U32 => write!(f, "u32"),
            VariantTy::U64 => write!(f, "u64"),
            // Ty::Enum { enum_uid } => write!(
            //     f,
            //     "enum {}",
            //     enum_value::uid_to_enum_name(*enum_uid).unwrap_or("Undefined")
            // ),
            VariantTy::Binary => write!(f, "Binary"),

            #[cfg(not(feature = "rkyv"))]
            VariantTy::List => write!(f, "List<Value>"),
        }
    }
}

impl Display for Variant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Variant::Never => write!(f, "Never"),
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
            // Value::Enum {
            //     enum_uid,
            //     discriminant,
            // } => match enum_value::uid_to_variant_name(*enum_uid, *discriminant) {
            //     Some(variant_name) => write!(f, "{variant_name}"),
            //     None => write!(f, "Unknown enum {}", enum_uid),
            // },
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
