//! SI quantity handling.
//!
//! Convention: in `Variant::SI { value, unit }` the `value` is expressed in `unit`, *including* its
//! prefix and exponent. E.g. `SI { value: 4.7, unit: kΩ }` is 4700 Ω.

use crate::{Error, Number, NumberTy, VariantTy};
use si_dynamic::{BaseUnit, OhmF32, Quantity, Unit, UnitExpr};

fn named_alias(word: &str) -> Option<BaseUnit> {
    match word.to_ascii_lowercase().as_str() {
        "vdc" | "vac" | "vrms" => Some(BaseUnit::Volt),
        "ohm" | "ohms" | "r" => Some(BaseUnit::Ohm),
        "adc" | "aac" | "arms" => Some(BaseUnit::Ampere),
        _ => None,
    }
}

/// `unit` with the unknown words rvariant knows (`Vdc`, `Ohms`) replaced by their unit.
pub(crate) fn with_aliases(unit: &UnitExpr) -> UnitExpr {
    match unit {
        UnitExpr::Unit(u) => match &u.base {
            BaseUnit::Unknown(word) => match named_alias(word) {
                Some(base) => UnitExpr::Unit(Unit { base, ..u.clone() }),
                None => unit.clone(),
            },
            _ => unit.clone(),
        },
        UnitExpr::Mul(a, b) => with_aliases(a) * with_aliases(b),
        UnitExpr::Div(a, b) => with_aliases(a) / with_aliases(b),
        UnitExpr::Pow(a, n) => with_aliases(a).pow(*n),
    }
}

/// The power of ten that turns a value in `from` into one in `to`, when that is all a conversion takes:
/// the same unit up to prefixes (`mΩ` and `kΩ`, `kg` and `g`), or units of one dimension whose scales are powers of
/// ten (`V/mA` and `kΩ`). `None` otherwise: other dimensions, unknown words, °C against K (an offset), a `Factor`.
pub(crate) fn shift_between(from: &UnitExpr, to: &UnitExpr) -> Option<i32> {
    let (from, to) = (with_aliases(from), with_aliases(to));
    if from.same_but_prefixes(&to) {
        return Some(from.prefix_exp10() - to.prefix_exp10());
    }
    if from.dim()? != to.dim()? {
        return None;
    }
    Some(from.exp10()? - to.exp10()?)
}

fn mismatch(expected: &UnitExpr, found: &UnitExpr) -> Error {
    Error::UnitMismatch {
        expected: Box::new(expected.clone()),
        found: Box::new(found.clone()),
    }
}

/// Shift decimal point of a number by `exp10` and convert to `to` (strict or lossy).
pub(crate) fn shift_number(
    value: Number,
    exp10: i32,
    to: NumberTy,
    lossy: bool,
) -> Result<Number, Error> {
    if exp10 == 0 || (value.is_float() && !value.to_f64_lossy().is_finite()) {
        return if lossy {
            value.convert_lossy(to)
        } else {
            value.convert_to(to)
        };
    }
    shift_str(&value.to_string(), exp10, to, lossy)
}

/// Parse decimal string, shift it by `exp10` and produce a number of type `to`.
/// Shifting is done textually to avoid double rounding.
fn shift_str(number: &str, exp10: i32, to: NumberTy, lossy: bool) -> Result<Number, Error> {
    let number = number.trim().replace('\u{2212}', "-");
    let (mantissa, exp) = match number.split_once(['e', 'E']) {
        Some((m, e)) => (
            m.to_string(),
            e.parse::<i32>()
                .map_err(|_| Error::parse(&number, &VariantTy::Number(to), "bad exponent"))?,
        ),
        None => (number.clone(), 0),
    };
    let shifted = format!("{mantissa}e{}", exp + exp10);
    match Number::try_from_str(&shifted, to) {
        Ok(n) => Ok(n),
        Err(e @ (Error::Lossy { .. } | Error::OutOfRange { .. })) if lossy => {
            let f: f64 = shifted.parse().map_err(|_| e.clone())?;
            Number::F64(f.into()).convert_lossy(to)
        }
        Err(Error::Lossy { .. }) => Err(Error::Lossy {
            value: number.to_string(),
            to: Box::new(VariantTy::Number(to)),
        }),
        Err(Error::OutOfRange { .. }) => Err(Error::OutOfRange {
            value: number.to_string(),
            to: Box::new(VariantTy::Number(to)),
        }),
        Err(e) => Err(e),
    }
}

/// `value`, given in a unit `10^exp10` times its base unit, in the base unit as `f64`. Shifted as text,
/// so 13.7 kΩ gives exactly 13700 and an `F32` keeps its decimal digits (0.1, not 0.10000000149).
pub(crate) fn base_f64(value: Number, exp10: i32) -> f64 {
    let lossy = value.to_f64_lossy();
    if value.is_float() && !lossy.is_finite() {
        return lossy;
    }
    match shift_str(&value.to_string(), exp10, NumberTy::F64, true) {
        Ok(Number::F64(f)) => f.0,
        _ => lossy * 10f64.powi(exp10),
    }
}

/// `value` in `unit`, as one number in coherent SI through the unit's scale (13.7 kΩ is 13700 Ω, 10 kg is 10 kg,
/// 100 mm² is 0.0001 m², exact through text when the scale is a power of ten); a unit with an offset (°C) and an
/// unknown word stay in their own unit without prefix.
pub(crate) fn si_f64(value: Number, unit: &UnitExpr) -> f64 {
    if let Some(exp10) = unit.exp10() {
        return base_f64(value, exp10);
    }
    let prefixes = unit.prefix_exp10();
    match unit.factor() {
        Some(factor) if !unit.has_offset() => base_f64(value, 0) * factor,
        _ => base_f64(value, prefixes),
    }
}

/// Parse SI quantity and express it in `unit` with `number_ty`.
///
/// Accepts: plain number (interpreted in `unit`), `4.7kΩ`, `4.7 kOhm`, `4k7` (resistance only),
/// `5 mV`, `12Vdc` (only for Volt), `3 km^2`, `10 V/us`, and units of the same dimension (`V/mA` for `kΩ`).
pub(crate) fn parse_si(input: &str, number_ty: NumberTy, unit: &UnitExpr) -> Result<Number, Error> {
    let s = input.trim();
    let ty = VariantTy::SI {
        number_ty,
        unit: unit.clone(),
    };
    if s.is_empty() {
        return Err(Error::Empty);
    }
    match Number::try_from_str(s, number_ty) {
        Ok(n) => return Ok(n),
        Err(e @ (Error::Lossy { .. } | Error::OutOfRange { .. })) => return Err(e),
        Err(_) => {}
    }
    let quantity_err = match Quantity::parse(s) {
        Ok(q) => match shift_between(&q.unit, unit) {
            Some(shift) => return shift_str(&q.number, shift, number_ty, false),
            None => mismatch(unit, &q.unit),
        },
        Err(e) => Error::parse(input, &ty, e.to_string()),
    };
    if let Some(shift) = shift_between(&BaseUnit::Ohm.into(), unit)
        && let Ok(ohm) = OhmF32::parse(s)
    {
        return shift_str(&ohm.0.to_string(), shift, number_ty, false);
    }
    Err(quantity_err)
}

/// Re-express SI value given in `from` unit in `to` unit.
pub(crate) fn rescale(
    value: Number,
    from: &UnitExpr,
    to: &UnitExpr,
    number_ty: NumberTy,
    lossy: bool,
) -> Result<Number, Error> {
    let shift = shift_between(from, to).ok_or_else(|| mismatch(to, from))?;
    shift_number(value, shift, number_ty, lossy)
}

/// Unit formatted for display after a number ("" for unitless).
pub(crate) fn unit_suffix(unit: &UnitExpr) -> String {
    if *unit == UnitExpr::one() {
        String::new()
    } else {
        format!(" {unit}")
    }
}
