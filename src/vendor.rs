//! Vendor text (Digi-Key parametrics, BOM cells, datasheet tables) read as a typed value: [`Variant::parse_vendor`].
//!
//! [`Variant::try_from_str`] is strict and reads what [`Display`](std::fmt::Display) writes. Vendors write the same
//! values in many more ways (`13.7 kOhms`, `4u7`, `0.063W, 1/16W`, `2pF @ 0V, 1MHz`, `-40°C ~ 85°C (TA)`), so
//! `parse_vendor` takes the text apart first: mojibake repaired, lists split, conditions after `@` read, text in
//! parentheses and words after the value set aside as warnings; the value itself is still read strictly against
//! the type (a different dimension or a bare word is an error, never a guess).
//!
//! The input is untrusted: it never panics, and its work is bounded by [`MAX_VENDOR_LEN`].

use crate::parse::{match_enum_variant, parse_tolerance};
use crate::si::{parse_si, with_aliases};
use crate::{Error, Number, NumberTy, Variant, VariantTy};
use si_dynamic::{BaseUnit, Quantity, UnitExpr};
use std::fmt;

/// The longest text [`Variant::parse_vendor`] reads, in bytes; longer text is an error.
pub const MAX_VENDOR_LEN: usize = 512;

/// At most this many words after a value are set aside as [`VendorWarning::Trailing`].
const MAX_TRAILING_WORDS: usize = 6;

/// Items of a list that agree within this relative difference are one value (`0.063W, 1/16W`).
const SAME_VALUE: f64 = 0.01;

/// A value read from vendor text, with what the text said besides it.
#[derive(Clone, Debug, PartialEq)]
pub struct VendorValue {
    /// The value in the requested type (its unit and number type), or, where the text said more than one,
    /// a [`Variant::List`] of them; a single value where a range was asked is a [`Variant::SI`]
    /// ([`VendorWarning::ScalarForRange`]).
    pub value: Variant,
    /// The conditions written after `@`, in order, each a [`Variant::SI`] in the unit as written
    /// (`2pF @ 0V, 1MHz`: `0 V`, `1 MHz`), or a [`Variant::Number`] when it has no unit.
    pub cond: Vec<Variant>,
    /// What was odd about the text but still read, each kind once.
    pub warnings: Vec<VendorWarning>,
}

/// Something in vendor text that was read anyway. [`Display`](fmt::Display) gives the code the shared corpus
/// (`tests/corpus/vendor.toml`) uses: `mojibake`, `note:TJ`, `trailing:Max`, ...
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[non_exhaustive]
pub enum VendorWarning {
    /// UTF-8 read as Latin-1 and repaired (`4.7ÂµF`).
    Mojibake,
    /// Text in parentheses that is no other spelling of the value: `(TJ)`, `(Typ)`, `(8/20µs)`.
    Note(String),
    /// Words after the value: `Max`, `per Contact`, `0603 film`; also list items that are no value.
    Trailing(String),
    /// One value where a range was expected (`150°C (TJ)` for an operating temperature).
    ScalarForRange,
    /// A decimal comma (`4,7k`).
    DecimalComma,
    /// More than one space in a row.
    Spacing,
    /// A small `r` for ohms (`4r7`, `10r`).
    LowercaseR,
    /// `R` after a prefix that already makes it ohms (`10kR`, `4K7R`).
    RedundantR,
    /// The unit as a separate word after resistor notation (`4k7 Ohm`).
    SeparateUnit,
}

impl fmt::Display for VendorWarning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VendorWarning::Mojibake => f.write_str("mojibake"),
            VendorWarning::Note(text) => write!(f, "note:{text}"),
            VendorWarning::Trailing(text) => write!(f, "trailing:{text}"),
            VendorWarning::ScalarForRange => f.write_str("scalar_for_range"),
            VendorWarning::DecimalComma => f.write_str("decimal_comma"),
            VendorWarning::Spacing => f.write_str("spacing"),
            VendorWarning::LowercaseR => f.write_str("lowercase_r"),
            VendorWarning::RedundantR => f.write_str("redundant_r"),
            VendorWarning::SeparateUnit => f.write_str("separate_unit"),
        }
    }
}

impl Variant {
    /// Read vendor text as `ty`, tolerant of how vendors write values, strict about what they mean.
    ///
    /// - `SI` and `Number`: `13.7 kOhms`, `16200.00 Ohm`, `4k7`, `4u7`, `3V3`, `10k` (a prefix alone takes the
    ///   type's unit), `1/16W`, `48VDC`, a list (`7.8µH, 1.6µH` is a [`Variant::List`]; items that agree within
    ///   1 % are one value, the fraction when one is written: `0.063W, 1/16W` is 1/16 W), conditions
    ///   (`18mOhm @ 100kHz`, `2pF @ 0V, 1MHz`), another spelling in parentheses (`0.039" (1.00mm)` is 1 mm,
    ///   `2000V (2kV)` 2 kV).
    /// - `SIRange`: `-40°C ~ 85°C`, a list of ranges, one value with [`VendorWarning::ScalarForRange`].
    /// - `Tolerance`: `±1%`, `-20%, +80%`, `±0.25pF`, `0/ +300ppm/°C` (a bare side takes the other side's unit).
    /// - `Enum`: the first item that names a variant (`C0G, NP0`), the others as trailing text.
    /// - `Str`: the text with mojibake repaired, trimmed. Every other type: [`Variant::try_from_str`].
    ///
    /// Text in parentheses, words after the value and list items that are no value become
    /// [`VendorWarning`]s. A unit of another dimension, a word without a number (`Jumper`), a number without its
    /// unit followed by words (`2000 Hrs` for a time) and text longer than [`MAX_VENDOR_LEN`] are errors.
    pub fn parse_vendor(text: &str, ty: &VariantTy) -> Result<VendorValue, Error> {
        if text.len() > MAX_VENDOR_LEN {
            let head: String = text.chars().take(32).collect();
            return Err(Error::parse(
                &format!("{head}..."),
                ty,
                format!("longer than {MAX_VENDOR_LEN} bytes"),
            ));
        }
        let mut warnings = Vec::new();
        if let VariantTy::Str = ty {
            let s = repair(text, &mut warnings);
            return Ok(VendorValue {
                value: Variant::Str(s.trim().to_string()),
                cond: Vec::new(),
                warnings,
            });
        }
        let s = clean(text, &mut warnings);
        if s.is_empty() {
            return Err(Error::Empty);
        }
        let (value, cond) = match ty {
            VariantTy::SI { .. } | VariantTy::Number(_) => scalars(&s, ty, &mut warnings)?,
            VariantTy::SIRange { .. } => (ranges(&s, ty, &mut warnings)?, Vec::new()),
            VariantTy::Tolerance(number_ty) => {
                (tolerance(&s, *number_ty, ty, &mut warnings)?, Vec::new())
            }
            VariantTy::Enum { name, variants } => {
                (enumeration(&s, name, variants, &mut warnings)?, Vec::new())
            }
            _ => (Variant::try_from_str(&s, ty)?, Vec::new()),
        };
        Ok(VendorValue {
            value,
            cond,
            warnings,
        })
    }
}

fn warn(warnings: &mut Vec<VendorWarning>, w: VendorWarning) {
    if !warnings.contains(&w) {
        warnings.push(w);
    }
}

/// UTF-8 read as Latin-1 (or Windows-1252), for the characters vendor values carry.
const MOJIBAKE: [(&str, &str); 5] = [
    ("Âµ", "µ"),
    ("Â°", "°"),
    ("Â±", "±"),
    ("Î©", "Ω"),
    ("Î¼", "μ"),
];

fn repair(text: &str, warnings: &mut Vec<VendorWarning>) -> String {
    let mut s = text.to_string();
    for (bad, good) in MOJIBAKE {
        if s.contains(bad) {
            s = s.replace(bad, good);
            warn(warnings, VendorWarning::Mojibake);
        }
    }
    s
}

/// Mojibake repaired, trimmed, every run of whitespace one space, `−` as `-`.
fn clean(text: &str, warnings: &mut Vec<VendorWarning>) -> String {
    let s = repair(text, warnings);
    let mut out = String::with_capacity(s.len());
    let mut run = 0;
    for c in s.trim().chars() {
        if c.is_whitespace() {
            run += 1;
            if run == 1 {
                out.push(' ');
            } else if run == 2 {
                warn(warnings, VendorWarning::Spacing);
            }
        } else {
            run = 0;
            out.push(if c == '\u{2212}' { '-' } else { c });
        }
    }
    out
}

/// The items of a list: split at `,` and `;` outside parentheses, but not at a comma between digits (`4,7k`,
/// `10,000`).
fn split_items(s: &str) -> Vec<&str> {
    let bytes = s.as_bytes();
    let mut out = Vec::new();
    let mut depth = 0i32;
    let mut start = 0;
    for (i, c) in s.char_indices() {
        match c {
            '(' => depth += 1,
            ')' => depth -= 1,
            ',' | ';' if depth <= 0 => {
                let between_digits = c == ','
                    && i > 0
                    && bytes[i - 1].is_ascii_digit()
                    && bytes.get(i + 1).is_some_and(u8::is_ascii_digit);
                if !between_digits {
                    out.push(s[start..i].trim());
                    start = i + 1;
                }
            }
            _ => {}
        }
    }
    out.push(s[start..].trim());
    out.retain(|item| !item.is_empty());
    out
}

/// The items, an item without `@` after one with `@` joined to it: `2pF @ 0V, 1MHz` is one item.
fn items_with_conditions(s: &str) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for item in split_items(s) {
        match out.last_mut() {
            Some(prev) if prev.contains('@') && !item.contains('@') => {
                prev.push_str(", ");
                prev.push_str(item);
            }
            _ => out.push(item.to_string()),
        }
    }
    out
}

/// An item taken apart: the text outside parentheses up to `@`, the text in parentheses, the condition text.
struct Item {
    head: String,
    notes: Vec<String>,
    cond: Option<String>,
}

fn split_item(item: &str) -> Item {
    let mut main = String::with_capacity(item.len());
    let mut notes = Vec::new();
    let mut depth = 0usize;
    let mut note = String::new();
    for c in item.chars() {
        match c {
            '(' => {
                if depth > 0 {
                    note.push(c);
                }
                depth += 1;
            }
            ')' if depth > 0 => {
                depth -= 1;
                if depth == 0 {
                    notes.push(note.trim().to_string());
                    note.clear();
                } else {
                    note.push(c);
                }
            }
            _ if depth > 0 => note.push(c),
            _ => main.push(c),
        }
    }
    if depth > 0 {
        // Unbalanced: keep it in the text, where it fails to read
        main.push('(');
        main.push_str(&note);
    }
    notes.retain(|n| !n.is_empty());
    let main = main.split_whitespace().collect::<Vec<_>>().join(" ");
    let (head, cond) = match main.split_once('@') {
        Some((head, cond)) => (head.trim().to_string(), Some(cond.trim().to_string())),
        None => (main, None),
    };
    Item { head, notes, cond }
}

/// The conditions after `@`, each a quantity in its own unit; a piece that is none is trailing text.
fn conditions(text: &str, warnings: &mut Vec<VendorWarning>) -> Vec<Variant> {
    let mut out = Vec::new();
    for piece in text.split(',').map(str::trim).filter(|p| !p.is_empty()) {
        match condition(piece) {
            Some(v) => out.push(v),
            None => warn(warnings, VendorWarning::Trailing(piece.to_string())),
        }
    }
    out
}

fn condition(piece: &str) -> Option<Variant> {
    if let Ok(n) = Number::try_from_str(piece, NumberTy::F64) {
        return Some(Variant::Number(n));
    }
    let q = Quantity::parse(piece).ok()?;
    let unit = with_aliases(&q.unit);
    unit.dim()?;
    let value = Number::try_from_str(&q.number, NumberTy::F64).ok()?;
    Some(Variant::SI { value, unit })
}

/// One value read from an item's head.
struct Scalar {
    value: Number,
    /// Written as a fraction (`1/16W`): exact where its decimal spelling is rounded
    fraction: bool,
    warnings: Vec<VendorWarning>,
}

/// A decimal comma or thousands separators made plain, a dot without digits after it dropped (`12.mm`).
fn plain_number(text: &str, warnings: &mut Vec<VendorWarning>) -> String {
    let chars: Vec<char> = text.chars().collect();
    let digit = |i: usize| chars.get(i).is_some_and(char::is_ascii_digit);
    let commas: Vec<usize> = (1..chars.len())
        .filter(|&i| chars[i] == ',' && digit(i - 1) && digit(i + 1))
        .collect();
    let thousands = !commas.is_empty()
        && !text.contains('.')
        && commas
            .iter()
            .all(|&i| digit(i + 3) && !digit(i + 4) && (1..=3).all(|k| digit(i + k)));
    let mut out = String::with_capacity(text.len());
    for (i, &c) in chars.iter().enumerate() {
        if commas.contains(&i) {
            if !thousands {
                out.push('.');
            }
        } else if c == '.' && i > 0 && digit(i - 1) && !digit(i + 1) {
            // `12.mm`
        } else {
            out.push(c);
        }
    }
    if !commas.is_empty() && !thousands {
        warn(warnings, VendorWarning::DecimalComma);
    }
    out
}

const PREFIX_LETTERS: &str = "pnuµμmkKMG";

/// The type's unit when it is one unit without exponent: its symbol (`Ω`, `F`, `Hz`)
fn simple_symbol(unit: &UnitExpr) -> Option<&str> {
    let unit = unit.as_unit()?;
    (unit.exp == 1 && !matches!(unit.base, BaseUnit::Ratio)).then(|| unit.base.symbol())
}

/// A head that is only a number (`10`, `-4.7`), so dropping the words after it would give the type's unit to it.
fn is_bare_number(head: &str) -> bool {
    Number::try_from_str(head, NumberTy::F64).is_ok()
}

/// The value at the start of `head`: the whole head, or with up to [`MAX_TRAILING_WORDS`] words dropped from its end
/// (as [`VendorWarning::Trailing`]). For `SI` a bare number left after dropping words is no value: `2000 Hrs` is not
/// 2000 s.
fn scalar(head: &str, ty: &VariantTy) -> Option<Scalar> {
    let words: Vec<&str> = head.split(' ').filter(|w| !w.is_empty()).collect();
    let max_drop = MAX_TRAILING_WORDS.min(words.len().saturating_sub(1));
    for drop in 0..=max_drop {
        let n = words.len() - drop;
        let h = words[..n].join(" ");
        if drop > 0 && matches!(ty, VariantTy::SI { .. }) && is_bare_number(&h) {
            continue;
        }
        if let Some(mut s) = scalar_exact(&h, ty) {
            if drop > 0 {
                s.warnings
                    .push(VendorWarning::Trailing(words[n..].join(" ")));
            }
            return Some(s);
        }
    }
    None
}

fn scalar_exact(head: &str, ty: &VariantTy) -> Option<Scalar> {
    let mut warnings = Vec::new();
    let head = plain_number(head, &mut warnings);
    match ty {
        VariantTy::Number(number_ty) => {
            let value = Number::try_from_str(&head, *number_ty).ok()?;
            Some(Scalar {
                value,
                fraction: false,
                warnings,
            })
        }
        VariantTy::SI { number_ty, unit } => {
            let (value, fraction) = si_exact(&head, *number_ty, unit, &mut warnings)?;
            Some(Scalar {
                value,
                fraction,
                warnings,
            })
        }
        _ => None,
    }
}

/// `head` in `unit`: a fraction, resistor notation, a number with a prefix alone, or what [`parse_si`] reads.
fn si_exact(
    head: &str,
    number_ty: NumberTy,
    unit: &UnitExpr,
    warnings: &mut Vec<VendorWarning>,
) -> Option<(Number, bool)> {
    let symbol = simple_symbol(unit);
    if let Some(value) = fraction(head, number_ty, unit) {
        return Some((value, true));
    }
    if let Some(symbol) = symbol {
        if let Some(text) = resistor_notation(head, symbol, warnings) {
            return parse_si(&text, number_ty, unit).ok().map(|v| (v, false));
        }
        if let Some(text) = prefix_alone(head, symbol) {
            return parse_si(&text, number_ty, unit).ok().map(|v| (v, false));
        }
    }
    let value = parse_si(head, number_ty, unit).ok()?;
    if symbol == Some("Ω") {
        ohm_letter_warnings(head, warnings);
    }
    Some((value, false))
}

/// `1/16W`, `1/4 W`, `3/4`: numerator and denominator in digits, then the unit (or none).
fn fraction(head: &str, number_ty: NumberTy, unit: &UnitExpr) -> Option<Number> {
    let (num, rest) = head.split_once('/')?;
    let den_len = rest
        .find(|c: char| !c.is_ascii_digit())
        .unwrap_or(rest.len());
    let (den, unit_text) = rest.split_at(den_len);
    if num.is_empty() || den.is_empty() || !num.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    let (num, den): (f64, f64) = (num.parse().ok()?, den.parse().ok()?);
    if den == 0.0 || !num.is_finite() || !den.is_finite() {
        return None;
    }
    let one = parse_si(&format!("1{unit_text}"), NumberTy::F64, unit).ok()?;
    let value = num / den * one.to_f64_lossy();
    if !value.is_finite() {
        return None;
    }
    // A float column rounds a fraction as any division does; an integer column takes only an exact one
    let value = Number::F64(value.into());
    if number_ty.is_float() {
        value.convert_lossy(number_ty).ok()
    } else {
        value.convert_to(number_ty).ok()
    }
}

/// Resistor notation, a letter in place of the decimal point (`4k7`, `2R2`, `4u7`, `3V3`), as text [`parse_si`]
/// reads (`4.7 kΩ`). The letter is a prefix, the unit's own symbol, or `R` for ohms; a unit may follow.
fn resistor_notation(
    head: &str,
    symbol: &str,
    warnings: &mut Vec<VendorWarning>,
) -> Option<String> {
    let ohm = symbol == "Ω";
    let (sign, rest) = match head.strip_prefix('-') {
        Some(rest) => ("-", rest),
        None => ("", head),
    };
    let int_len = rest.find(|c: char| !c.is_ascii_digit())?;
    if int_len == 0 {
        return None;
    }
    let (int, rest) = rest.split_at(int_len);
    let mut chars = rest.chars();
    let letter = chars.next()?;
    let rest = chars.as_str();
    let frac_len = rest
        .find(|c: char| !c.is_ascii_digit())
        .unwrap_or(rest.len());
    if frac_len == 0 {
        return None;
    }
    let (frac, after) = rest.split_at(frac_len);
    let is_prefix = PREFIX_LETTERS.contains(letter);
    let is_unit = letter.to_string() == symbol || (ohm && (letter == 'R' || letter == 'r'));
    if !is_prefix && !is_unit {
        return None;
    }
    let mut found = Vec::new();
    if letter == 'r' {
        found.push(VendorWarning::LowercaseR);
    }
    let unit_word = after.trim_start();
    if !unit_word.is_empty() {
        let same = unit_word == symbol
            || ohm && ["Ohm", "Ohms", "ohm", "ohms", "R", "r"].contains(&unit_word);
        if !same || is_unit {
            return None;
        }
        if after.starts_with(' ') {
            found.push(VendorWarning::SeparateUnit);
        } else if unit_word == "R" || unit_word == "r" {
            found.push(VendorWarning::RedundantR);
        }
    }
    for w in found {
        warn(warnings, w);
    }
    let prefix = if is_prefix {
        letter.to_string()
    } else {
        String::new()
    };
    Some(format!("{sign}{int}.{frac} {prefix}{symbol}"))
}

/// A number with a prefix and no unit (`10k`, `100n`, `4.7 k`) in the unit's symbol: `10 kΩ`. Not when the letter
/// is the unit itself (`10 m` is ten metres).
fn prefix_alone(head: &str, symbol: &str) -> Option<String> {
    let mut chars = head.chars();
    let letter = chars.next_back()?;
    if !PREFIX_LETTERS.contains(letter) || letter.to_string() == symbol {
        return None;
    }
    let number = chars.as_str().trim_end();
    is_bare_number(number).then(|| format!("{number} {letter}{symbol}"))
}

/// `10r`, `49r` (a small `r`), `10kR` (`R` after a prefix).
fn ohm_letter_warnings(head: &str, warnings: &mut Vec<VendorWarning>) {
    let mut chars = head.chars().rev();
    let last = chars.next();
    if !matches!(last, Some('r' | 'R')) {
        return;
    }
    if last == Some('r') {
        warn(warnings, VendorWarning::LowercaseR);
    }
    if chars.next().is_some_and(|c| PREFIX_LETTERS.contains(c)) {
        warn(warnings, VendorWarning::RedundantR);
    }
}

/// An item's value: its head, else one of its parentheses (`0.039" (1.00mm)`); parentheses that spell the same
/// value are dropped, the others are notes.
fn item_value(item: &Item, ty: &VariantTy) -> Option<(Scalar, Vec<String>)> {
    let mut notes = item.notes.clone();
    let scalar = match scalar(&item.head, ty) {
        Some(s) => {
            let same =
                |n: &String| scalar_exact(n, ty).is_some_and(|alt| close(&alt.value, &s.value));
            notes.retain(|n| !same(n));
            s
        }
        None => {
            let i = notes.iter().position(|n| scalar_exact(n, ty).is_some())?;
            scalar_exact(&notes.remove(i), ty)?
        }
    };
    Some((scalar, notes))
}

fn close(a: &Number, b: &Number) -> bool {
    let (a, b) = (a.to_f64_lossy(), b.to_f64_lossy());
    a == b || (a - b).abs() <= SAME_VALUE * a.abs().max(b.abs())
}

fn wrap(value: Number, ty: &VariantTy) -> Variant {
    match ty {
        VariantTy::SI { unit, .. } => Variant::SI {
            value,
            unit: unit.clone(),
        },
        _ => Variant::Number(value),
    }
}

/// One value, or a list when the values differ by more than [`SAME_VALUE`] (the fraction of equal ones wins).
fn one_or_list(values: Vec<Scalar>, ty: &VariantTy) -> Variant {
    let all_same = values.iter().all(|v| close(&v.value, &values[0].value));
    if all_same && let Some(first) = values.first() {
        let pick = values.iter().find(|v| v.fraction).unwrap_or(first);
        return wrap(pick.value, ty);
    }
    Variant::List(values.into_iter().map(|v| wrap(v.value, ty)).collect())
}

/// The error of the first item's head read strictly, so a unit mismatch says so.
fn failure(s: &str, head: &str, ty: &VariantTy) -> Error {
    match Variant::try_from_str(head, ty) {
        Err(e) => e,
        Ok(_) => Error::parse(s, ty, "no value"),
    }
}

/// `SI` and `Number`: one value or a list, with the conditions they share.
fn scalars(
    s: &str,
    ty: &VariantTy,
    warnings: &mut Vec<VendorWarning>,
) -> Result<(Variant, Vec<Variant>), Error> {
    let items = items_with_conditions(s);
    let many = items.len() > 1;
    let mut values = Vec::new();
    let mut cond: Option<Vec<Variant>> = None;
    let mut first_head = None;
    for text in &items {
        let item = split_item(text);
        first_head.get_or_insert_with(|| item.head.clone());
        let Some((scalar, notes)) = item_value(&item, ty) else {
            if many {
                warn(warnings, VendorWarning::Trailing(text.clone()));
                continue;
            }
            break;
        };
        let mut found = Vec::new();
        let item_cond = item
            .cond
            .as_deref()
            .map(|c| conditions(c, &mut found))
            .unwrap_or_default();
        match &cond {
            Some(c) if *c != item_cond => {
                return Err(Error::parse(s, ty, "list items under different conditions"));
            }
            _ => cond = Some(item_cond),
        }
        for w in found.into_iter().chain(scalar.warnings.iter().cloned()) {
            warn(warnings, w);
        }
        for n in notes {
            warn(warnings, VendorWarning::Note(n));
        }
        values.push(scalar);
    }
    if values.is_empty() {
        return Err(failure(s, first_head.as_deref().unwrap_or(s), ty));
    }
    Ok((one_or_list(values, ty), cond.unwrap_or_default()))
}

/// `SIRange`: `lo ~ hi`, a list of them, or values without `~` ([`VendorWarning::ScalarForRange`]).
fn ranges(s: &str, ty: &VariantTy, warnings: &mut Vec<VendorWarning>) -> Result<Variant, Error> {
    let VariantTy::SIRange {
        number_ty,
        start_inclusive,
        end_inclusive,
        unit,
    } = ty
    else {
        return Err(Error::Internal);
    };
    let scalar_ty = VariantTy::SI {
        number_ty: *number_ty,
        unit: unit.clone(),
    };
    let items = split_items(s);
    let many = items.len() > 1;
    let mut found_ranges = Vec::new();
    let mut found_scalars = Vec::new();
    for text in &items {
        let item = split_item(text);
        let bounds: Vec<&str> = item.head.split('~').map(str::trim).collect();
        let read = match bounds[..] {
            [lo, hi] => match (scalar(lo, &scalar_ty), scalar(hi, &scalar_ty)) {
                (Some(lo), Some(hi)) => Some(Ok((lo, hi))),
                _ => None,
            },
            [_] => item_value(&item, &scalar_ty).map(|(s, notes)| Err((s, notes))),
            _ => None,
        };
        let notes = match read {
            Some(Ok((lo, hi))) => {
                if lo.value.cmp_value(&hi.value).is_gt() {
                    return Err(Error::parse(s, ty, "range start is greater than end"));
                }
                for w in lo.warnings.iter().chain(&hi.warnings) {
                    warn(warnings, w.clone());
                }
                found_ranges.push(Variant::SIRange {
                    start: lo.value,
                    start_inclusive: *start_inclusive,
                    end: hi.value,
                    end_inclusive: *end_inclusive,
                    unit: unit.clone(),
                });
                item.notes
            }
            Some(Err((one, notes))) => {
                for w in &one.warnings {
                    warn(warnings, w.clone());
                }
                found_scalars.push(one);
                notes
            }
            None if many => {
                warn(warnings, VendorWarning::Trailing(text.to_string()));
                continue;
            }
            None => return Err(failure(s, bounds[0], &scalar_ty)),
        };
        if let Some(c) = &item.cond {
            warn(warnings, VendorWarning::Trailing(format!("@ {c}")));
        }
        for n in notes {
            warn(warnings, VendorWarning::Note(n));
        }
    }
    match (found_ranges.len(), found_scalars.is_empty()) {
        (0, false) => {
            warn(warnings, VendorWarning::ScalarForRange);
            Ok(one_or_list(found_scalars, &scalar_ty))
        }
        (1, true) => Ok(found_ranges.remove(0)),
        (_, true) if !found_ranges.is_empty() => Ok(Variant::List(found_ranges)),
        (0, true) => Err(Error::parse(s, ty, "no value")),
        _ => Err(Error::parse(s, ty, "both ranges and single values")),
    }
}

/// `Tolerance`: [`parse_tolerance`] on the text without parentheses; a side without unit takes the other side's
/// (`0/ +300ppm/°C`).
fn tolerance(
    s: &str,
    number_ty: NumberTy,
    ty: &VariantTy,
    warnings: &mut Vec<VendorWarning>,
) -> Result<Variant, Error> {
    let item = split_item(s);
    let head = match &item.cond {
        Some(c) => format!("{} @ {c}", item.head),
        None => item.head.clone(),
    };
    let value = parse_tolerance(s, &head, number_ty, ty)?;
    for n in item.notes {
        warn(warnings, VendorWarning::Note(n));
    }
    Ok(match value {
        Variant::Tolerance {
            min,
            min_unit,
            max,
            max_unit,
        } => {
            let one = UnitExpr::one();
            let (min_unit, max_unit) = match (min_unit == one, max_unit == one) {
                (true, false) => (max_unit.clone(), max_unit),
                (false, true) => (min_unit.clone(), min_unit),
                _ => (min_unit, max_unit),
            };
            Variant::Tolerance {
                min,
                min_unit,
                max,
                max_unit,
            }
        }
        other => other,
    })
}

/// `Enum`: the first item naming a variant; the other items are trailing text.
fn enumeration(
    s: &str,
    name: &std::sync::Arc<str>,
    variants: &std::sync::Arc<[String]>,
    warnings: &mut Vec<VendorWarning>,
) -> Result<Variant, Error> {
    let mut selected = None;
    let mut rest = Vec::new();
    for text in split_items(s) {
        let item = split_item(text);
        match (&selected, match_enum_variant(&item.head, name, variants)) {
            (None, Ok(v)) => {
                selected = Some(v);
                for n in item.notes {
                    warn(warnings, VendorWarning::Note(n));
                }
            }
            _ => rest.push(text),
        }
    }
    let Some(selected) = selected else {
        return Err(match_enum_variant(s, name, variants)
            .err()
            .unwrap_or(Error::Internal));
    };
    for text in rest {
        warn(warnings, VendorWarning::Trailing(text.to_string()));
    }
    Ok(Variant::Enum {
        name: name.clone(),
        selected,
        variants: variants.clone(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn items_split_outside_parentheses_and_digits() {
        assert_eq!(
            split_items("1A (AC), 500mA (DC)"),
            ["1A (AC)", "500mA (DC)"]
        );
        assert_eq!(split_items("4,7k"), ["4,7k"]);
        assert_eq!(split_items("a (b, c); d"), ["a (b, c)", "d"]);
        assert_eq!(
            items_with_conditions("2pF @ 0V, 1MHz, 3pF @ 1V, 1MHz"),
            ["2pF @ 0V, 1MHz", "3pF @ 1V, 1MHz"]
        );
    }

    #[test]
    fn item_parts() {
        let item = split_item("-40°C ~ 85°C (TA)");
        assert_eq!(item.head, "-40°C ~ 85°C");
        assert_eq!(item.notes, ["TA"]);
        let item = split_item("24 (16+8 (x) Dummy) @ 5V");
        assert_eq!(item.head, "24");
        assert_eq!(item.notes, ["16+8 (x) Dummy"]);
        assert_eq!(item.cond.as_deref(), Some("5V"));
        assert_eq!(split_item("5 (open").head, "5 (open");
    }

    #[test]
    fn numbers_made_plain() {
        let mut w = Vec::new();
        assert_eq!(plain_number("10,000", &mut w), "10000");
        assert_eq!(plain_number("12.mm", &mut w), "12mm");
        assert!(w.is_empty());
        assert_eq!(plain_number("4,7k", &mut w), "4.7k");
        assert_eq!(w, [VendorWarning::DecimalComma]);
    }
}
