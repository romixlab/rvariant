//! SI quantity handling.
//!
//! Convention: in `Variant::SI { value, unit }` the `value` is expressed in `unit`, *including* its
//! prefix and exponent. E.g. `SI { value: 4.7, unit: kΩ }` is 4700 Ω.

use crate::{Error, Number, NumberTy, VariantTy};
use si_dynamic::{BaseUnit, OhmF32, Prefix, Quantity, Unit};

pub(crate) fn prefix_exp10(p: Prefix) -> i32 {
    match p {
        Prefix::Quecto => -30,
        Prefix::Ronto => -27,
        Prefix::Yocto => -24,
        Prefix::Zepto => -21,
        Prefix::Atto => -18,
        Prefix::Femto => -15,
        Prefix::Pico => -12,
        Prefix::Nano => -9,
        Prefix::Micro => -6,
        Prefix::Milli => -3,
        Prefix::Centi => -2,
        Prefix::Deci => -1,
        Prefix::Unit => 0,
        Prefix::Deca => 1,
        Prefix::Hecto => 2,
        Prefix::Kilo => 3,
        Prefix::Mega => 6,
        Prefix::Giga => 9,
        Prefix::Tera => 12,
        Prefix::Peta => 15,
        Prefix::Exa => 18,
        Prefix::Zetta => 21,
        Prefix::Yotta => 24,
        Prefix::Ronna => 27,
        Prefix::Quetta => 30,
    }
}

/// Power of ten that converts a value in `unit` into base units.
pub(crate) fn unit_exp10(unit: &Unit) -> i32 {
    prefix_exp10(unit.prefix) * unit.exp as i32
}

/// Same as [`unit_exp10`], but for units coming out of `si_dynamic` parser, which reports grams as
/// `BaseUnit::Kilogram` with the prefix applied to the gram ("5kg" -> Kilo + Kilogram).
fn parsed_unit_exp10(unit: &Unit) -> i32 {
    let mut e = unit_exp10(unit);
    if unit.base == BaseUnit::Kilogram {
        e -= 3 * unit.exp as i32;
    }
    e
}

fn named_alias(name: &str) -> Option<BaseUnit> {
    match name.to_ascii_lowercase().as_str() {
        "vdc" | "vac" | "vrms" => Some(BaseUnit::Volt),
        "ohm" | "ohms" | "r" => Some(BaseUnit::Ohm),
        "adc" | "aac" | "arms" => Some(BaseUnit::Ampere),
        _ => None,
    }
}

/// Whether a parsed base unit can be interpreted as `expected`.
pub(crate) fn base_matches(found: &BaseUnit, expected: &BaseUnit) -> bool {
    if found == expected {
        return true;
    }
    match (found, expected) {
        (BaseUnit::Named { name: a, .. }, BaseUnit::Named { name: b, .. }) => a == b,
        (BaseUnit::Named { name, .. }, expected) => named_alias(name).as_ref() == Some(expected),
        _ => false,
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

/// Parse SI quantity and express it in `unit` with `number_ty`.
///
/// Accepts: plain number (interpreted in `unit`), `4.7kΩ`, `4.7 kOhm`, `4k7` (resistance only),
/// `5 mV`, `12Vdc` (only for Volt), `3 km^2`.
pub(crate) fn parse_si(input: &str, number_ty: NumberTy, unit: &Unit) -> Result<Number, Error> {
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
    let target_exp10 = unit_exp10(unit);
    let quantity = Quantity::parse(s);
    let quantity_err = match quantity {
        Ok(q) => {
            if q.unit.exp == unit.exp && base_matches(&q.unit.base, &unit.base) {
                let shift = parsed_unit_exp10(&q.unit) - target_exp10;
                return shift_str(&q.number, shift, number_ty, false);
            }
            Error::UnitMismatch {
                expected: Box::new(unit.clone()),
                found: Box::new(q.unit),
            }
        }
        Err(e) => Error::parse(input, &ty, e.to_string()),
    };
    if unit.base == BaseUnit::Ohm
        && unit.exp == 1
        && let Ok(ohm) = OhmF32::parse(s)
    {
        return shift_str(&ohm.0.to_string(), -target_exp10, number_ty, false);
    }
    Err(quantity_err)
}

/// Re-express SI value given in `from` unit in `to` unit.
pub(crate) fn rescale(
    value: Number,
    from: &Unit,
    to: &Unit,
    number_ty: NumberTy,
    lossy: bool,
) -> Result<Number, Error> {
    if from.exp != to.exp || !base_matches(&from.base, &to.base) {
        return Err(Error::UnitMismatch {
            expected: Box::new(to.clone()),
            found: Box::new(from.clone()),
        });
    }
    shift_number(value, unit_exp10(from) - unit_exp10(to), number_ty, lossy)
}

/// Unit formatted for display after a number ("" for unitless).
pub(crate) fn unit_suffix(unit: &Unit) -> String {
    if unit.base == BaseUnit::Unitless {
        String::new()
    } else {
        format!(" {unit}")
    }
}
