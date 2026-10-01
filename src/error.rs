use crate::VariantTy;
use si_dynamic::Unit;

/// Errors produced by parsing, conversion and typed accessors.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
#[non_exhaustive]
pub enum Error {
    /// Input string cannot be parsed as the requested type.
    #[error("cannot parse {input:?} as {expected}: {reason}")]
    Parse {
        input: String,
        expected: Box<VariantTy>,
        reason: String,
    },
    /// There is no conversion between these two types.
    #[error("cannot convert {0} to {1}")]
    CannotConvert(Box<VariantTy>, Box<VariantTy>),
    /// Value does not fit into the target type.
    #[error("{value} is out of range for {to}")]
    OutOfRange { value: String, to: Box<VariantTy> },
    /// Conversion would lose information (fractional part, NaN, excess decimal digits).
    /// Use `convert_lossy` if this is acceptable.
    #[error("{value} cannot be represented as {to} without loss")]
    Lossy { value: String, to: Box<VariantTy> },
    /// Physical units are not compatible.
    #[error("unit mismatch: expected {expected}, found {found}")]
    UnitMismatch {
        expected: Box<Unit>,
        found: Box<Unit>,
    },
    /// Money values with different currencies cannot be converted.
    #[error("currency mismatch: expected {expected:?}, found {found:?}")]
    CurrencyMismatch { expected: String, found: String },
    /// String does not name any of the enum variants.
    #[error("{value:?} is not a variant of enum {name}")]
    WrongEnumVariantName { name: String, value: String },
    /// Value (or input string) is empty.
    #[error("value is empty")]
    Empty,
    /// Script compilation or evaluation failed.
    #[cfg(feature = "rhai")]
    #[error("{0}")]
    Script(String),
    #[error("internal error")]
    Internal,
}

impl Error {
    pub(crate) fn parse(input: &str, expected: &VariantTy, reason: impl Into<String>) -> Self {
        Error::Parse {
            input: input.to_string(),
            expected: Box::new(expected.clone()),
            reason: reason.into(),
        }
    }

    pub(crate) fn cannot_convert(from: &VariantTy, to: &VariantTy) -> Self {
        Error::CannotConvert(Box::new(from.clone()), Box::new(to.clone()))
    }
}
