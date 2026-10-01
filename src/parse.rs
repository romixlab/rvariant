use crate::decimal::parse_decimal_scaled;
use crate::display::EMPTY_STR_MARKER;
use crate::literal::parse_literal;
use crate::si::parse_si;
use crate::{Error, NanoSeconds, Number, NumberTy, Variant, VariantTy};
use chrono::{DateTime, FixedOffset, NaiveDate, NaiveDateTime, NaiveTime};
use si_dynamic::Unit;

const DATE_FORMATS: &[&str] = &["%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y"];
const TIME_FORMATS: &[&str] = &["%H:%M:%S%.f", "%H:%M"];
const NAIVE_DATE_TIME_FORMATS: &[&str] = &[
    "%Y-%m-%d %H:%M:%S%.f",
    "%Y-%m-%dT%H:%M:%S%.f",
    "%Y-%m-%d %H:%M",
    "%Y-%m-%dT%H:%M",
];
const DATE_TIME_FORMATS: &[&str] = &["%Y-%m-%d %H:%M:%S%.f %:z", "%Y-%m-%d %H:%M:%S%.f%:z"];

impl Variant {
    /// Parse a string as the given type.
    ///
    /// Leading and trailing whitespace is ignored for all types except `Str`.
    /// Anything produced by `Display` is accepted back. See the README for the full list of
    /// accepted formats.
    pub fn try_from_str<S: AsRef<str>>(value: S, to: &VariantTy) -> Result<Self, Error> {
        let input = value.as_ref();
        let s = input.trim();
        match to {
            VariantTy::Empty => Ok(Variant::Empty),
            VariantTy::Str => Ok(Variant::Str(input.to_owned())),
            VariantTy::Bool => {
                non_empty(s)?;
                parse_bool(s).map(Variant::Bool).ok_or_else(|| {
                    Error::parse(input, to, "expected true/false, yes/no, on/off, 1/0")
                })
            }
            VariantTy::StrList => Ok(Variant::StrList(parse_str_list(s))),

            VariantTy::Number(number_ty) => {
                Ok(Variant::Number(Number::try_from_str(s, *number_ty)?))
            }
            VariantTy::SI { number_ty, unit } => Ok(Variant::SI {
                value: parse_si(s, *number_ty, unit)?,
                unit: unit.clone(),
            }),
            VariantTy::SIRange {
                number_ty,
                start_inclusive,
                end_inclusive,
                unit,
            } => parse_si_range(
                input,
                to,
                *number_ty,
                *start_inclusive,
                *end_inclusive,
                unit,
            ),

            VariantTy::Enum { name, variants } => Ok(Variant::Enum {
                name: name.clone(),
                selected: match_enum_variant(s, name, variants)?,
                variants: variants.clone(),
            }),
            VariantTy::MultiSelectionEnum { name, variants } => {
                let mut selected = Vec::new();
                for part in s
                    .split([',', ';', '|'])
                    .map(str::trim)
                    .filter(|p| !p.is_empty())
                {
                    let v = match_enum_variant(part, name, variants)?;
                    if !selected.contains(&v) {
                        selected.push(v);
                    }
                }
                Ok(Variant::MultiSelectionEnum {
                    name: name.clone(),
                    selected,
                    variants: variants.clone(),
                })
            }

            VariantTy::Binary => parse_binary(s)
                .map(Variant::Binary)
                .ok_or_else(|| Error::parse(input, to, "expected hex bytes, e.g. \"0A FF 12\"")),
            VariantTy::List => {
                if s.is_empty() {
                    return Ok(Variant::List(Vec::new()));
                }
                match parse_literal(s) {
                    Ok(v @ Variant::List(_)) => Ok(v),
                    Ok(_) => Err(Error::parse(input, to, "expected [...]")),
                    Err(e) => Err(Error::parse(input, to, e)),
                }
            }
            VariantTy::Map => {
                if s.is_empty() {
                    return Ok(Variant::Map(Default::default()));
                }
                match parse_literal(s) {
                    Ok(v @ Variant::Map(_)) => Ok(v),
                    Ok(_) => Err(Error::parse(input, to, "expected {...}")),
                    Err(e) => Err(Error::parse(input, to, e)),
                }
            }

            VariantTy::Date => {
                non_empty(s)?;
                DATE_FORMATS
                    .iter()
                    .find_map(|fmt| NaiveDate::parse_from_str(s, fmt).ok())
                    .map(Variant::Date)
                    .ok_or_else(|| Error::parse(input, to, "expected YYYY-MM-DD"))
            }
            VariantTy::Time => {
                non_empty(s)?;
                TIME_FORMATS
                    .iter()
                    .find_map(|fmt| NaiveTime::parse_from_str(s, fmt).ok())
                    .map(Variant::Time)
                    .ok_or_else(|| Error::parse(input, to, "expected HH:MM[:SS[.fff]]"))
            }
            VariantTy::DateTime => {
                non_empty(s)?;
                parse_date_time(s)
                    .map(Variant::DateTime)
                    .ok_or_else(|| Error::parse(input, to, "expected RFC 3339 date time"))
            }
            VariantTy::Instant => parse_instant(s)
                .map(|ns| Variant::Instant(NanoSeconds(ns)))
                .map_err(|reason| match reason {
                    Error::Parse { reason, .. } => Error::parse(input, to, reason),
                    e => e,
                }),

            VariantTy::Money {
                currency,
                precision,
            } => Ok(Variant::Money {
                currency: currency.clone(),
                precision: *precision,
                value: parse_money(input, s, currency, *precision, to)?,
            }),

            VariantTy::Tolerance(number_ty) => parse_tolerance(input, s, *number_ty, to),
        }
    }

    /// Like [`Variant::try_from_str`], but falls back to `Variant::Str` if parsing fails,
    /// so that no input is lost.
    pub fn from_str<S: AsRef<str>>(value: S, to: &VariantTy) -> Self {
        let value = value.as_ref();
        match Self::try_from_str(value, to) {
            Ok(value) => value,
            Err(_) => Variant::Str(value.to_string()),
        }
    }
}

fn non_empty(s: &str) -> Result<(), Error> {
    if s.is_empty() {
        Err(Error::Empty)
    } else {
        Ok(())
    }
}

pub(crate) fn parse_bool(s: &str) -> Option<bool> {
    match s.to_lowercase().as_str() {
        "true" | "t" | "yes" | "y" | "on" | "1" | "✓" | "✔" => Some(true),
        "false" | "f" | "no" | "n" | "off" | "0" | "✗" | "✘" => Some(false),
        _ => None,
    }
}

fn parse_str_list(s: &str) -> Vec<String> {
    if s.is_empty() {
        return Vec::new();
    }
    s.split([',', ';', '\t'])
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .map(|s| {
            if s == EMPTY_STR_MARKER {
                String::new()
            } else {
                s.to_string()
            }
        })
        .collect()
}

pub(crate) fn match_enum_variant(
    s: &str,
    name: &str,
    variants: &[String],
) -> Result<String, Error> {
    let s = s.trim();
    let s = s
        .strip_prefix(name)
        .and_then(|rest| rest.strip_prefix("::"))
        .unwrap_or(s)
        .trim();
    if let Some(v) = variants.iter().find(|v| v.as_str() == s) {
        return Ok(v.clone());
    }
    let mut matches = variants.iter().filter(|v| v.eq_ignore_ascii_case(s));
    match (matches.next(), matches.next()) {
        (Some(v), None) => Ok(v.clone()),
        _ => Err(Error::WrongEnumVariantName {
            name: name.to_string(),
            value: s.to_string(),
        }),
    }
}

fn parse_binary(s: &str) -> Option<Vec<u8>> {
    let mut hex = String::with_capacity(s.len());
    for token in s.split([' ', ',', ':', '-', '_', '\t', '\n']) {
        let token = token
            .strip_prefix("0x")
            .or_else(|| token.strip_prefix("0X"))
            .unwrap_or(token);
        hex.push_str(token);
    }
    if !hex.len().is_multiple_of(2) || !hex.is_ascii() {
        return None;
    }
    (0..hex.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).ok())
        .collect()
}

fn parse_date_time(s: &str) -> Option<DateTime<FixedOffset>> {
    if let Ok(dt) = DateTime::parse_from_rfc3339(s) {
        return Some(dt);
    }
    for fmt in DATE_TIME_FORMATS {
        if let Ok(dt) = DateTime::parse_from_str(s, fmt) {
            return Some(dt);
        }
    }
    if let Ok(dt) = DateTime::parse_from_rfc2822(s) {
        return Some(dt);
    }
    // No offset given: assume UTC
    for fmt in NAIVE_DATE_TIME_FORMATS {
        if let Ok(dt) = NaiveDateTime::parse_from_str(s, fmt) {
            return Some(dt.and_utc().fixed_offset());
        }
    }
    DATE_FORMATS
        .iter()
        .find_map(|fmt| NaiveDate::parse_from_str(s, fmt).ok())
        .map(|d| d.and_time(NaiveTime::MIN).and_utc().fixed_offset())
}

/// Split `"1.5 ms"` / `"1.5ms"` into number and unit parts.
fn split_unit(s: &str) -> (&str, &str) {
    let idx = s
        .char_indices()
        .rev()
        .take_while(|(_, c)| c.is_alphabetic())
        .last()
        .map(|(i, _)| i)
        .unwrap_or(s.len());
    (s[..idx].trim(), &s[idx..])
}

fn parse_instant(s: &str) -> Result<i64, Error> {
    let ty = VariantTy::Instant;
    non_empty(s)?;
    let (number, unit) = split_unit(s);
    let scale = match unit {
        "s" => 9,
        "ms" => 6,
        "us" | "µs" | "μs" => 3,
        "ns" | "" => 0,
        other => {
            return Err(Error::parse(
                s,
                &ty,
                format!("unknown time unit {other:?}, expected s, ms, us or ns"),
            ));
        }
    };
    let number = number.replace('_', "");
    let scaled = parse_decimal_scaled(&number, scale)
        .ok_or_else(|| Error::parse(s, &ty, "invalid number"))?;
    if !scaled.exact {
        return Err(Error::Lossy {
            value: s.to_string(),
            to: Box::new(ty),
        });
    }
    i64::try_from(scaled.value).map_err(|_| Error::OutOfRange {
        value: s.to_string(),
        to: Box::new(ty),
    })
}

fn parse_money(
    input: &str,
    s: &str,
    currency: &str,
    precision: u8,
    ty: &VariantTy,
) -> Result<i64, Error> {
    non_empty(s)?;
    let amount = if currency.is_empty() {
        s
    } else if let Some(rest) = s.strip_prefix(currency) {
        rest
    } else if let Some(rest) = s.strip_suffix(currency) {
        rest
    } else {
        s
    };
    let amount: String = amount
        .trim()
        .chars()
        .filter(|c| !matches!(c, ' ' | '_' | '\'' | '\u{a0}' | '\u{202f}'))
        .collect();
    // "1,234.56" -> comma is a thousands separator; "12,50" -> decimal comma; "1,234,567" -> separators
    let amount = if amount.contains('.') || amount.matches(',').count() > 1 {
        amount.replace(',', "")
    } else {
        amount.replace(',', ".")
    };
    let Some(scaled) = parse_decimal_scaled(&amount, precision as i32) else {
        return if amount
            .chars()
            .any(|c| c.is_alphabetic() || "$€£¥₽".contains(c))
        {
            Err(Error::CurrencyMismatch {
                expected: currency.to_string(),
                found: input.trim().to_string(),
            })
        } else {
            Err(Error::parse(input, ty, "invalid amount"))
        };
    };
    if !scaled.exact {
        return Err(Error::Lossy {
            value: input.to_string(),
            to: Box::new(ty.clone()),
        });
    }
    i64::try_from(scaled.value).map_err(|_| Error::OutOfRange {
        value: input.to_string(),
        to: Box::new(ty.clone()),
    })
}

fn parse_si_range(
    input: &str,
    ty: &VariantTy,
    number_ty: NumberTy,
    start_inclusive: bool,
    end_inclusive: bool,
    unit: &Unit,
) -> Result<Variant, Error> {
    let s = input.trim();
    non_empty(s)?;
    let (inner, suffix, inclusive) = match s.chars().next() {
        Some(open @ ('[' | '(')) => {
            let close_idx = s
                .rfind([']', ')'])
                .ok_or_else(|| Error::parse(input, ty, "missing closing bracket"))?;
            let close = s[close_idx..].chars().next();
            (
                &s[1..close_idx],
                s[close_idx + 1..].trim(),
                Some((open == '[', close == Some(']'))),
            )
        }
        _ => (s, "", None),
    };
    if let Some(inclusive) = inclusive
        && inclusive != (start_inclusive, end_inclusive)
    {
        return Err(Error::parse(
            input,
            ty,
            "range bounds inclusivity does not match the type",
        ));
    }
    let parts: Vec<&str> = if inner.contains("..") {
        inner.splitn(2, "..").collect()
    } else if inner.contains(',') {
        inner.splitn(2, ',').collect()
    } else if inner.contains(';') {
        inner.splitn(2, ';').collect()
    } else if inner.contains(" to ") {
        inner.splitn(2, " to ").collect()
    } else {
        inner.splitn(2, '~').collect()
    };
    let [start, end] = parts[..] else {
        return Err(Error::parse(
            input,
            ty,
            "expected two bounds, e.g. [1, 5] V",
        ));
    };
    let is_dot_range = inner.contains("..");
    let parse_bound = |b: &str| {
        let b = b.trim();
        let b = if is_dot_range {
            b.trim_start_matches(['.', '=']).trim()
        } else {
            b
        };
        if !suffix.is_empty() && Number::try_from_str(b, NumberTy::F64).is_ok() {
            parse_si(&format!("{b} {suffix}"), number_ty, unit)
        } else {
            parse_si(b, number_ty, unit)
        }
    };
    let start = parse_bound(start)?;
    let end = parse_bound(end)?;
    if start.cmp_value(&end).is_gt() {
        return Err(Error::parse(input, ty, "range start is greater than end"));
    }
    Ok(Variant::SIRange {
        start,
        start_inclusive,
        end,
        end_inclusive,
        unit: unit.clone(),
    })
}

fn parse_tolerance(
    input: &str,
    s: &str,
    number_ty: NumberTy,
    ty: &VariantTy,
) -> Result<Variant, Error> {
    non_empty(s)?;
    let part = |p: &str| -> Result<(Number, bool, String), Error> {
        let p = p.trim();
        let (num, is_percent) = match p.strip_suffix('%') {
            Some(n) => (n.trim(), true),
            None => (p, false),
        };
        Ok((
            Number::try_from_str(num, number_ty)?,
            is_percent,
            num.to_string(),
        ))
    };
    let symmetric = ["±", "+/-", "+-"]
        .iter()
        .find_map(|prefix| s.strip_prefix(prefix));
    let parts: Vec<&str> = s.split(['/', ',']).collect();
    let ((min, min_percent), (max, max_percent)) = match (symmetric, parts.as_slice()) {
        (Some(rest), _) | (None, &[rest]) => {
            let (max, is_percent, num) = part(rest)?;
            let num = num.trim_start_matches('+');
            let min = if num.starts_with(['-', '\u{2212}']) {
                return Err(Error::parse(input, ty, "expected ±X with non-negative X"));
            } else {
                Number::try_from_str(format!("-{num}"), number_ty)?
            };
            ((min, is_percent), (max, is_percent))
        }
        (None, &[a, b]) => {
            let (a, a_pct, _) = part(a)?;
            let (b, b_pct, _) = part(b)?;
            if a.cmp_value(&b).is_le() {
                ((a, a_pct), (b, b_pct))
            } else {
                ((b, b_pct), (a, a_pct))
            }
        }
        _ => {
            return Err(Error::parse(
                input,
                ty,
                "expected ±X[%] or MIN[%]/MAX[%], e.g. -10%/+20%",
            ));
        }
    };
    Ok(Variant::Tolerance {
        min,
        min_percent,
        max,
        max_percent,
    })
}
