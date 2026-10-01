use crate::decimal::format_decimal;
use crate::literal::{write_list, write_map};
use crate::si::unit_suffix;
use crate::{Number, Variant, VariantTy};
use chrono::SecondsFormat;
use std::fmt::{Display, Formatter, Write};

/// Placeholder used for empty strings in `StrList`.
pub(crate) const EMPTY_STR_MARKER: &str = "⛶";

impl Display for VariantTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VariantTy::Empty => write!(f, "Empty"),
            VariantTy::Bool => write!(f, "Bool"),
            VariantTy::Str => write!(f, "String"),
            VariantTy::StrList => write!(f, "List<String>"),
            VariantTy::Number(ty) => write!(f, "{}", ty.as_ref()),
            VariantTy::Enum { name, .. } => write!(f, "enum {name}"),
            VariantTy::MultiSelectionEnum { name, .. } => write!(f, "multi-selection {name}"),
            VariantTy::Binary => write!(f, "Binary"),
            VariantTy::List => write!(f, "List<Variant>"),
            VariantTy::Map => write!(f, "Map<Variant, Variant>"),
            VariantTy::Instant => write!(f, "Instant"),
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
            VariantTy::Date => write!(f, "Date"),
            VariantTy::Time => write!(f, "Time"),
            VariantTy::DateTime => write!(f, "DateTime"),
            VariantTy::Money {
                currency,
                precision,
            } => write!(f, "Money({currency}, {precision})"),
            VariantTy::Tolerance(n) => write!(f, "Tolerance({})", n.as_ref()),
        }
    }
}

fn percent(is_percent: bool) -> &'static str {
    if is_percent { "%" } else { "" }
}

impl Display for Variant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Variant::Empty => Ok(()),
            Variant::Bool(b) => write!(f, "{b}"),
            Variant::Str(s) => f.write_str(s),
            Variant::StrList(list) => {
                for (idx, s) in list.iter().enumerate() {
                    if idx > 0 {
                        f.write_str(", ")?;
                    }
                    if s.is_empty() {
                        f.write_str(EMPTY_STR_MARKER)?;
                    } else {
                        f.write_str(s)?;
                    }
                }
                Ok(())
            }
            Variant::Number(n) => write!(f, "{n}"),
            Variant::SI { value, unit } => write!(f, "{value}{}", unit_suffix(unit)),
            Variant::SIRange {
                start,
                start_inclusive,
                end,
                end_inclusive,
                unit,
            } => {
                let start_sym = if *start_inclusive { '[' } else { '(' };
                let end_sym = if *end_inclusive { ']' } else { ')' };
                write!(f, "{start_sym}{start}, {end}{end_sym}{}", unit_suffix(unit))
            }
            Variant::Enum { selected, .. } => f.write_str(selected),
            Variant::MultiSelectionEnum { selected, .. } => {
                for (idx, s) in selected.iter().enumerate() {
                    if idx > 0 {
                        f.write_str(", ")?;
                    }
                    f.write_str(s)?;
                }
                Ok(())
            }
            Variant::Binary(b) => {
                for (idx, byte) in b.iter().enumerate() {
                    if idx > 0 {
                        f.write_char(' ')?;
                    }
                    write!(f, "{byte:02X}")?;
                }
                Ok(())
            }
            Variant::List(list) => write_list(list, f),
            Variant::Map(m) => write_map(m, f),
            Variant::Date(date) => write!(f, "{}", date.format("%Y-%m-%d")),
            Variant::Time(time) => write!(f, "{}", time.format("%H:%M:%S%.f")),
            Variant::DateTime(dt) => f.write_str(&dt.to_rfc3339_opts(SecondsFormat::AutoSi, true)),
            Variant::Instant(ns) => {
                let ns = ns.0 as i128;
                let (scale, unit) = match ns.unsigned_abs() {
                    a if a >= 1_000_000_000 => (9, "s"),
                    a if a >= 1_000_000 => (6, "ms"),
                    a if a >= 1_000 => (3, "us"),
                    _ => (0, "ns"),
                };
                write!(f, "{} {unit}", format_decimal(ns, scale, true))
            }
            Variant::Money {
                currency,
                precision,
                value,
            } => {
                let amount = format_decimal(*value as i128, *precision as u32, false);
                if currency.is_empty() {
                    f.write_str(&amount)
                } else {
                    write!(f, "{amount} {currency}")
                }
            }
            Variant::Tolerance {
                min,
                min_percent,
                max,
                max_percent,
            } => {
                let symmetric = min_percent == max_percent
                    && min.to_f64_lossy() == -max.to_f64_lossy()
                    && max.cmp_value(&Number::U8(0)).is_ge();
                if symmetric {
                    write!(f, "±{max}{}", percent(*max_percent))
                } else {
                    let plus = if max.cmp_value(&Number::U8(0)).is_ge() {
                        "+"
                    } else {
                        ""
                    };
                    write!(
                        f,
                        "{min}{}/{plus}{max}{}",
                        percent(*min_percent),
                        percent(*max_percent)
                    )
                }
            }
        }
    }
}
