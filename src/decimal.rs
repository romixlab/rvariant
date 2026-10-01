//! Exact decimal string <-> scaled integer helpers (no floating point involved).

/// Result of parsing a decimal literal as a scaled integer.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) struct Scaled {
    /// `round(value * 10^scale)`, rounding half away from zero.
    pub value: i128,
    /// false if non-zero digits were rounded off.
    pub exact: bool,
}

/// Parse decimal literal like `-12.345`, `1.5e3`, `+.5` and return it multiplied by `10^scale`.
/// Returns None on syntax error or overflow.
pub(crate) fn parse_decimal_scaled(s: &str, scale: i32) -> Option<Scaled> {
    let s = s.trim();
    let (neg, rest) = if let Some(r) = s.strip_prefix(['-', '\u{2212}']) {
        (true, r)
    } else if let Some(r) = s.strip_prefix('+') {
        (false, r)
    } else {
        (false, s)
    };
    let (mantissa, exp) = match rest.split_once(['e', 'E']) {
        Some((m, e)) => (m, e.parse::<i32>().ok()?),
        None => (rest, 0),
    };
    let (int_part, frac_part) = mantissa.split_once('.').unwrap_or((mantissa, ""));
    if int_part.is_empty() && frac_part.is_empty() {
        return None;
    }
    let digits_ok = |d: &str| d.chars().all(|c| c.is_ascii_digit() || c == '_');
    if !digits_ok(int_part) || !digits_ok(frac_part) {
        return None;
    }
    let frac_part = frac_part.replace('_', "");
    let mut digits: String = int_part.replace('_', "");
    digits.push_str(&frac_part);
    let digits = digits.trim_start_matches('0');
    // drop trailing zeros to keep the number of significant digits small
    let trimmed = digits.trim_end_matches('0');
    let trailing_zeros = (digits.len() - trimmed.len()) as i64;
    let shift = exp as i64 + scale as i64 - frac_part.len() as i64 + trailing_zeros;
    if trimmed.is_empty() {
        return Some(Scaled {
            value: 0,
            exact: true,
        });
    }
    let mantissa: i128 = trimmed.parse().ok()?;
    let (value, exact) = if shift >= 0 {
        let m = 10i128.checked_pow(u32::try_from(shift).ok()?)?;
        (mantissa.checked_mul(m)?, true)
    } else {
        let shift = -shift;
        if shift > 38 {
            (0, false) // mantissa < 10^39, so result rounds to zero (or is at most one ulp)
        } else {
            let d = 10i128.pow(shift as u32);
            let q = mantissa / d;
            let r = mantissa % d;
            let q = if r >= d - r { q + 1 } else { q };
            (q, r == 0)
        }
    };
    Some(Scaled {
        value: if neg { -value } else { value },
        exact,
    })
}

/// Format `value / 10^scale` exactly. If `trim` is true, trailing zeros of the fractional part
/// are removed (and the decimal point if nothing is left).
pub(crate) fn format_decimal(value: i128, scale: u32, trim: bool) -> String {
    let sign = if value < 0 { "-" } else { "" };
    let abs = value.unsigned_abs();
    let Some(d) = 10u128.checked_pow(scale) else {
        return format!("{value}e-{scale}");
    };
    let int = abs / d;
    let frac = abs % d;
    if scale == 0 {
        return format!("{sign}{int}");
    }
    let mut frac = format!("{frac:0width$}", width = scale as usize);
    if trim {
        while frac.ends_with('0') {
            frac.pop();
        }
    }
    if frac.is_empty() {
        format!("{sign}{int}")
    } else {
        format!("{sign}{int}.{frac}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(s: &str, scale: i32) -> Option<(i128, bool)> {
        parse_decimal_scaled(s, scale).map(|s| (s.value, s.exact))
    }

    #[test]
    fn parse() {
        assert_eq!(p("12.34", 2), Some((1234, true)));
        assert_eq!(p("-12.345", 2), Some((-1235, false)));
        assert_eq!(p("12.344", 2), Some((1234, false)));
        assert_eq!(p("1.5e3", 0), Some((1500, true)));
        assert_eq!(p("1.5", 6), Some((1_500_000, true)));
        assert_eq!(p(".5", 1), Some((5, true)));
        assert_eq!(p("5.", 1), Some((50, true)));
        assert_eq!(p("0.000", 2), Some((0, true)));
        assert_eq!(p("100", -2), Some((1, true)));
        assert_eq!(p("1_000.5", 1), Some((10005, true)));
        assert_eq!(p("", 1), None);
        assert_eq!(p(".", 1), None);
        assert_eq!(p("1.2.3", 1), None);
        assert_eq!(p("abc", 1), None);
    }

    #[test]
    fn format() {
        assert_eq!(format_decimal(1234, 2, false), "12.34");
        assert_eq!(format_decimal(-50, 2, false), "-0.50");
        assert_eq!(format_decimal(1_500_000, 6, true), "1.5");
        assert_eq!(format_decimal(2_000_000, 6, true), "2");
        assert_eq!(format_decimal(7, 0, true), "7");
    }
}
