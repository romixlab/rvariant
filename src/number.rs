use crate::Error;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use strum::{AsRefStr, EnumDiscriminants, EnumIter};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, EnumDiscriminants)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[strum_discriminants(name(NumberTy))]
#[strum_discriminants(derive(PartialOrd, Ord, Hash, EnumIter, AsRefStr))]
#[strum_discriminants(cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize)))]
pub enum Number {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),

    F32(F32),
    F64(F64),
}

macro_rules! float_ty {
    ($ty_name:ident, $base_ty:ident) => {
        #[derive(Copy, Clone, Debug)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub struct $ty_name(pub $base_ty);
        impl From<$base_ty> for $ty_name {
            fn from(v: $base_ty) -> Self {
                $ty_name(v)
            }
        }
        impl PartialEq for $ty_name {
            fn eq(&self, other: &Self) -> bool {
                self.cmp(other).is_eq()
            }
        }
        impl Eq for $ty_name {}
        impl Hash for $ty_name {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.0.to_bits().hash(state);
            }
        }
        impl PartialOrd for $ty_name {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }
        impl Ord for $ty_name {
            fn cmp(&self, other: &Self) -> Ordering {
                self.0.total_cmp(&other.0)
            }
        }
    };
}
float_ty!(F32, f32);
float_ty!(F64, f64);

impl Number {
    pub fn try_from_str<S: AsRef<str>>(value: S, to: NumberTy) -> Result<Self, Error> {
        match to {
            NumberTy::I8 => Ok(Number::I8(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            NumberTy::I16 => Ok(Number::I16(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            NumberTy::I32 => Ok(Number::I32(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            NumberTy::I64 => Ok(Number::I64(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            NumberTy::I128 => Ok(Number::I128(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            NumberTy::U8 => Ok(Number::U8(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            NumberTy::U16 => Ok(Number::U16(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            NumberTy::U32 => Ok(Number::U32(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            NumberTy::U64 => Ok(Number::U64(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            NumberTy::U128 => Ok(Number::U128(
                parse_int::parse(value.as_ref()).map_err(Error::ParseIntError)?,
            )),
            NumberTy::F32 => Ok(Number::F32(F32(
                parse_int::parse(value.as_ref()).map_err(Error::ParseFloatError)?
            ))),
            NumberTy::F64 => Ok(Number::F64(F64(
                parse_int::parse(value.as_ref()).map_err(Error::ParseFloatError)?
            ))),
        }
    }

    pub fn convert_to(self, to: NumberTy) -> Result<Number, Error> {
        if NumberTy::from(&self) == to {
            return Ok(self);
        }
        match to {
            NumberTy::F32 => {
                if let Number::F64(v) = self {
                    return Ok(Number::F32(F32(v.0 as f32)));
                }
            }
            NumberTy::F64 => {
                if let Number::F32(v) = self {
                    return Ok(Number::F64(F64(v.0 as f64)));
                }
            }
            _ => {}
        }
        let int_str = self.to_int_str();
        Self::try_from_str(&int_str, to)
    }

    fn to_int_str(self) -> String {
        match self {
            Number::I8(v) => v.to_string(),
            Number::I16(v) => v.to_string(),
            Number::I32(v) => v.to_string(),
            Number::I64(v) => v.to_string(),
            Number::I128(v) => v.to_string(),
            Number::U8(v) => v.to_string(),
            Number::U16(v) => v.to_string(),
            Number::U32(v) => v.to_string(),
            Number::U64(v) => v.to_string(),
            Number::U128(v) => v.to_string(),
            Number::F32(v) => (v.0 as i128).to_string(),
            Number::F64(v) => (v.0 as i128).to_string(),
        }
    }

    pub(crate) fn default_of(ty: NumberTy) -> Number {
        match ty {
            NumberTy::I8 => Number::I8(0),
            NumberTy::I16 => Number::I16(0),
            NumberTy::I32 => Number::I32(0),
            NumberTy::I64 => Number::I64(0),
            NumberTy::I128 => Number::I128(0),
            NumberTy::U8 => Number::U8(0),
            NumberTy::U16 => Number::U16(0),
            NumberTy::U32 => Number::U32(0),
            NumberTy::U64 => Number::U64(0),
            NumberTy::U128 => Number::U128(0),
            NumberTy::F32 => Number::F32(F32(0.0)),
            NumberTy::F64 => Number::F64(F64(0.0)),
        }
    }

    pub fn as_f32(&self) -> Result<f32, Error> {
        if let Number::F32(x) = self {
            return Ok(x.0);
        }
        if let Number::F32(x) = self.convert_to(NumberTy::F32)? {
            Ok(x.0)
        } else {
            Err(Error::Internal)
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::I8(x) => write!(f, "{x}"),
            Number::I16(x) => write!(f, "{x}"),
            Number::I32(x) => write!(f, "{x}"),
            Number::I64(x) => write!(f, "{x}"),
            Number::I128(x) => write!(f, "{x}"),
            Number::U8(x) => write!(f, "{x}"),
            Number::U16(x) => write!(f, "{x}"),
            Number::U32(x) => write!(f, "{x}"),
            Number::U64(x) => write!(f, "{x}"),
            Number::U128(x) => write!(f, "{x}"),
            Number::F32(x) => write!(f, "{}", x.0),
            Number::F64(x) => write!(f, "{}", x.0),
        }
    }
}

// macro_rules! as_ty {
//     () => {};
// }
