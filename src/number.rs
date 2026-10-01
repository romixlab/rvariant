use crate::{Error, VariantTy};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use strum::{AsRefStr, EnumDiscriminants, EnumIter, EnumString};

/// Numeric value with an explicit underlying type.
///
/// `PartialEq`, `Eq`, `Hash` and `Ord` are *structural*: `I32(1) != I64(1)` and values of different
/// types are ordered by type first. Use [`Number::cmp_value`] / [`Number::eq_value`] to compare by
/// numeric value.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, EnumDiscriminants)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[strum_discriminants(name(NumberTy))]
#[strum_discriminants(derive(
    PartialOrd,
    Ord,
    Hash,
    EnumIter,
    AsRefStr,
    EnumString,
    strum::Display
))]
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
        /// Float wrapper with total ordering (`total_cmp`), making it usable as a map key.
        /// Note that `NaN == NaN` and `-0.0 != 0.0` under this ordering.
        #[derive(Copy, Clone, Debug, Default)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub struct $ty_name(pub $base_ty);
        impl From<$base_ty> for $ty_name {
            fn from(v: $base_ty) -> Self {
                $ty_name(v)
            }
        }
        impl From<$ty_name> for $base_ty {
            fn from(v: $ty_name) -> Self {
                v.0
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
        impl Display for $ty_name {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                Display::fmt(&self.0, f)
            }
        }
    };
}
float_ty!(F32, f32);
float_ty!(F64, f64);

/// Widened representation used for conversions and comparisons.
#[derive(Copy, Clone, Debug)]
pub(crate) enum Wide {
    I(i128),
    U(u128),
    F(f64),
}

impl Wide {
    fn as_f64(self) -> f64 {
        match self {
            Wide::I(v) => v as f64,
            Wide::U(v) => v as f64,
            Wide::F(v) => v,
        }
    }
}

/// Exact integer value, split by sign so that the whole i128 + u128 range is covered.
#[derive(Copy, Clone, Debug)]
pub(crate) enum IntVal {
    Neg(i128),
    Pos(u128),
}

impl IntVal {
    fn from_i128(v: i128) -> Self {
        if v < 0 {
            IntVal::Neg(v)
        } else {
            IntVal::Pos(v as u128)
        }
    }
}

impl Display for IntVal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IntVal::Neg(v) => write!(f, "{v}"),
            IntVal::Pos(v) => write!(f, "{v}"),
        }
    }
}

impl NumberTy {
    pub fn is_float(&self) -> bool {
        matches!(self, NumberTy::F32 | NumberTy::F64)
    }

    pub fn is_integer(&self) -> bool {
        !self.is_float()
    }

    pub fn is_signed(&self) -> bool {
        !matches!(
            self,
            NumberTy::U8 | NumberTy::U16 | NumberTy::U32 | NumberTy::U64 | NumberTy::U128
        )
    }

    /// (min, max) for integer types.
    fn int_range(&self) -> (i128, u128) {
        match self {
            NumberTy::I8 => (i8::MIN as i128, i8::MAX as u128),
            NumberTy::I16 => (i16::MIN as i128, i16::MAX as u128),
            NumberTy::I32 => (i32::MIN as i128, i32::MAX as u128),
            NumberTy::I64 => (i64::MIN as i128, i64::MAX as u128),
            NumberTy::I128 => (i128::MIN, i128::MAX as u128),
            NumberTy::U8 => (0, u8::MAX as u128),
            NumberTy::U16 => (0, u16::MAX as u128),
            NumberTy::U32 => (0, u32::MAX as u128),
            NumberTy::U64 => (0, u64::MAX as u128),
            NumberTy::U128 => (0, u128::MAX),
            NumberTy::F32 | NumberTy::F64 => (i128::MIN, u128::MAX),
        }
    }
}

fn out_of_range(value: impl Display, to: NumberTy) -> Error {
    Error::OutOfRange {
        value: value.to_string(),
        to: Box::new(VariantTy::Number(to)),
    }
}

fn lossy(value: impl Display, to: NumberTy) -> Error {
    Error::Lossy {
        value: value.to_string(),
        to: Box::new(VariantTy::Number(to)),
    }
}

/// Normalize user input: trim, drop digit separators and replace unicode minus.
pub(crate) fn clean_number_str(s: &str) -> String {
    s.trim()
        .chars()
        .filter(|c| *c != '_')
        .map(|c| if c == '\u{2212}' { '-' } else { c })
        .collect()
}

/// Parse integer literal with optional sign and 0x / 0o / 0b prefix.
pub(crate) fn parse_int_literal(s: &str) -> Option<IntVal> {
    let (neg, rest) = if let Some(r) = s.strip_prefix('-') {
        (true, r)
    } else if let Some(r) = s.strip_prefix('+') {
        (false, r)
    } else {
        (false, s)
    };
    let (radix, digits) = match rest.get(..2) {
        Some("0x") | Some("0X") => (16, &rest[2..]),
        Some("0o") | Some("0O") => (8, &rest[2..]),
        Some("0b") | Some("0B") => (2, &rest[2..]),
        _ => (10, rest),
    };
    // from_str_radix accepts a leading '+', which we already consumed
    if digits.is_empty() || digits.starts_with(['+', '-']) {
        return None;
    }
    let magnitude = u128::from_str_radix(digits, radix).ok()?;
    if neg {
        if magnitude == 0 {
            Some(IntVal::Pos(0))
        } else if magnitude <= i128::MAX as u128 + 1 {
            Some(IntVal::Neg((magnitude as i128).wrapping_neg()))
        } else {
            None
        }
    } else {
        Some(IntVal::Pos(magnitude))
    }
}

impl Number {
    pub fn ty(&self) -> NumberTy {
        self.into()
    }

    pub fn is_float(&self) -> bool {
        self.ty().is_float()
    }

    pub fn is_integer(&self) -> bool {
        self.ty().is_integer()
    }

    pub(crate) fn wide(self) -> Wide {
        match self {
            Number::I8(v) => Wide::I(v as i128),
            Number::I16(v) => Wide::I(v as i128),
            Number::I32(v) => Wide::I(v as i128),
            Number::I64(v) => Wide::I(v as i128),
            Number::I128(v) => Wide::I(v),
            Number::U8(v) => Wide::U(v as u128),
            Number::U16(v) => Wide::U(v as u128),
            Number::U32(v) => Wide::U(v as u128),
            Number::U64(v) => Wide::U(v as u128),
            Number::U128(v) => Wide::U(v),
            Number::F32(v) => Wide::F(v.0 as f64),
            Number::F64(v) => Wide::F(v.0),
        }
    }

    /// Parse a number of the requested type.
    ///
    /// Integers accept an optional sign, `0x` / `0o` / `0b` prefixes and `_` separators.
    /// Float syntax (`5.0`, `1e3`) is accepted for integer types as long as the value is integral.
    /// Out of range values are rejected (never wrapped or saturated).
    pub fn try_from_str<S: AsRef<str>>(value: S, to: NumberTy) -> Result<Self, Error> {
        let input = value.as_ref();
        let s = clean_number_str(input);
        let ty = VariantTy::Number(to);
        if s.is_empty() {
            return Err(Error::Empty);
        }
        match to {
            NumberTy::F32 => s
                .parse::<f32>()
                .map(|v| Number::F32(F32(v)))
                .map_err(|e| Error::parse(input, &ty, e.to_string())),
            NumberTy::F64 => s
                .parse::<f64>()
                .map(|v| Number::F64(F64(v)))
                .map_err(|e| Error::parse(input, &ty, e.to_string())),
            _ => {
                if let Some(v) = parse_int_literal(&s) {
                    Self::from_int(v, to, false)
                } else if let Ok(f) = s.parse::<f64>() {
                    Number::F64(F64(f)).convert_to(to)
                } else {
                    Err(Error::parse(input, &ty, "invalid integer"))
                }
            }
        }
    }

    fn from_int(v: IntVal, to: NumberTy, saturate: bool) -> Result<Number, Error> {
        let (min, max) = to.int_range();
        let v = match v {
            IntVal::Neg(i) if i < min => {
                if saturate {
                    IntVal::from_i128(min)
                } else {
                    return Err(out_of_range(v, to));
                }
            }
            IntVal::Pos(u) if u > max => {
                if saturate {
                    IntVal::Pos(max)
                } else {
                    return Err(out_of_range(v, to));
                }
            }
            v => v,
        };
        let i = match v {
            IntVal::Neg(i) => i,
            IntVal::Pos(u) => u as i128, // in range for all types except U128, handled below
        };
        Ok(match to {
            NumberTy::I8 => Number::I8(i as i8),
            NumberTy::I16 => Number::I16(i as i16),
            NumberTy::I32 => Number::I32(i as i32),
            NumberTy::I64 => Number::I64(i as i64),
            NumberTy::I128 => Number::I128(i),
            NumberTy::U8 => Number::U8(i as u8),
            NumberTy::U16 => Number::U16(i as u16),
            NumberTy::U32 => Number::U32(i as u32),
            NumberTy::U64 => Number::U64(i as u64),
            NumberTy::U128 => Number::U128(match v {
                IntVal::Pos(u) => u,
                IntVal::Neg(_) => 0,
            }),
            NumberTy::F32 => Number::F32(F32(match v {
                IntVal::Neg(i) => i as f32,
                IntVal::Pos(u) => u as f32,
            })),
            NumberTy::F64 => Number::F64(F64(match v {
                IntVal::Neg(i) => i as f64,
                IntVal::Pos(u) => u as f64,
            })),
        })
    }

    /// Strict conversion:
    /// * integer -> integer: must be in range;
    /// * float -> integer: must be finite, integral and in range;
    /// * anything -> float: rounds to nearest, but finite values must not overflow to infinity.
    pub fn convert_to(self, to: NumberTy) -> Result<Number, Error> {
        self.convert(to, false)
    }

    /// Lossy conversion: floats are rounded to nearest integer, out of range values are saturated,
    /// float overflow results in infinity. NaN -> integer is still an error.
    pub fn convert_lossy(self, to: NumberTy) -> Result<Number, Error> {
        self.convert(to, true)
    }

    fn convert(self, to: NumberTy, is_lossy: bool) -> Result<Number, Error> {
        if self.ty() == to {
            return Ok(self);
        }
        let w = self.wide();
        match to {
            NumberTy::F32 => {
                let r = match w {
                    Wide::I(v) => v as f32,
                    Wide::U(v) => v as f32,
                    Wide::F(v) => v as f32,
                };
                if r.is_infinite() && w.as_f64().is_finite() && !is_lossy {
                    return Err(out_of_range(self, to));
                }
                Ok(Number::F32(F32(r)))
            }
            NumberTy::F64 => Ok(Number::F64(F64(w.as_f64()))),
            _ => {
                let v = match w {
                    Wide::I(i) => IntVal::from_i128(i),
                    Wide::U(u) => IntVal::Pos(u),
                    Wide::F(f) => {
                        if f.is_nan() {
                            return Err(lossy(self, to));
                        }
                        let r = if is_lossy { f.round() } else { f };
                        if r.fract() != 0.0 {
                            return Err(lossy(self, to));
                        }
                        const I128_MIN: f64 = -170141183460469231731687303715884105728.0; // -2^127
                        const U128_LIMIT: f64 = 340282366920938463463374607431768211456.0; // 2^128
                        if r < 0.0 {
                            if r < I128_MIN {
                                if is_lossy {
                                    IntVal::Neg(i128::MIN)
                                } else {
                                    return Err(out_of_range(self, to));
                                }
                            } else {
                                IntVal::Neg(r as i128)
                            }
                        } else if r >= U128_LIMIT {
                            if is_lossy {
                                IntVal::Pos(u128::MAX)
                            } else {
                                return Err(out_of_range(self, to));
                            }
                        } else {
                            IntVal::Pos(r as u128)
                        }
                    }
                };
                match Self::from_int(v, to, is_lossy) {
                    Ok(n) => Ok(n),
                    Err(Error::OutOfRange { .. }) => Err(out_of_range(self, to)),
                    Err(e) => Err(e),
                }
            }
        }
    }

    /// Value as f64, possibly losing precision for big integers.
    pub fn to_f64_lossy(&self) -> f64 {
        self.wide().as_f64()
    }

    /// Compare by numeric value, regardless of the underlying type.
    /// Floats use `total_cmp` semantics (NaN is ordered after +inf).
    pub fn cmp_value(&self, other: &Number) -> Ordering {
        match (self.wide(), other.wide()) {
            (Wide::I(a), Wide::I(b)) => a.cmp(&b),
            (Wide::U(a), Wide::U(b)) => a.cmp(&b),
            (Wide::I(a), Wide::U(b)) => {
                if a < 0 {
                    Ordering::Less
                } else {
                    (a as u128).cmp(&b)
                }
            }
            (Wide::U(a), Wide::I(b)) => {
                if b < 0 {
                    Ordering::Greater
                } else {
                    a.cmp(&(b as u128))
                }
            }
            (a, b) => a.as_f64().total_cmp(&b.as_f64()),
        }
    }

    /// Equality by numeric value, `I32(1).eq_value(&F64(1.0)) == true`.
    pub fn eq_value(&self, other: &Number) -> bool {
        self.cmp_value(other).is_eq()
    }

    pub fn default_of(ty: NumberTy) -> Number {
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
}

macro_rules! number_accessors {
    ($($name:ident, $ty:ty, $variant:ident, $inner:tt);* $(;)?) => {
        impl Number {
            $(
            #[doc = concat!("Strictly convert to `", stringify!($ty), "`, see [`Number::convert_to`].")]
            pub fn $name(&self) -> Result<$ty, Error> {
                match self.convert_to(NumberTy::$variant)? {
                    Number::$variant(x) => Ok(number_accessors!(@unwrap x $inner)),
                    _ => Err(Error::Internal),
                }
            }
            )*
        }
        $(
        impl From<$ty> for Number {
            fn from(v: $ty) -> Self {
                Number::$variant(number_accessors!(@wrap v $inner $variant))
            }
        }
        impl TryFrom<Number> for $ty {
            type Error = Error;
            fn try_from(n: Number) -> Result<Self, Error> {
                n.$name()
            }
        }
        )*
    };
    (@unwrap $x:ident int) => { $x };
    (@unwrap $x:ident float) => { $x.0 };
    (@wrap $v:ident int $variant:ident) => { $v };
    (@wrap $v:ident float $variant:ident) => { $variant($v) };
}

number_accessors!(
    as_i8, i8, I8, int;
    as_i16, i16, I16, int;
    as_i32, i32, I32, int;
    as_i64, i64, I64, int;
    as_i128, i128, I128, int;
    as_u8, u8, U8, int;
    as_u16, u16, U16, int;
    as_u32, u32, U32, int;
    as_u64, u64, U64, int;
    as_u128, u128, U128, int;
    as_f32, f32, F32, float;
    as_f64, f64, F64, float;
);

impl From<F32> for Number {
    fn from(v: F32) -> Self {
        Number::F32(v)
    }
}

impl From<F64> for Number {
    fn from(v: F64) -> Self {
        Number::F64(v)
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
