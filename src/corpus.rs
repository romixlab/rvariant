//! The shared vendor text corpus, `tests/corpus/vendor.toml` (feature `corpus`): vendor and BOM texts with the type
//! they are read as and what they must give. rvariant checks [`Variant::parse_vendor`] against it
//! (`tests/vendor_corpus.rs`); mx3 and better_erc load the same cases for their own tests, so the parsers agree.
//! The file's header documents the fields.

use crate::{Error, NumberTy, Variant, VariantTy, VendorValue};
use serde::Deserialize;
use si_dynamic::{Quantity, UnitExpr};

/// The corpus file as text.
pub const VENDOR_TOML: &str = include_str!("../tests/corpus/vendor.toml");

/// Relative difference within which an expected number matches.
pub const REL_TOLERANCE: f64 = 1e-9;

/// One case of the corpus.
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Case {
    /// The Digi-Key parameter the text came from.
    pub def: Option<String>,
    /// Where a case without `def` came from: `better_erc`, `synthetic`.
    pub source: Option<String>,
    pub text: String,
    /// `si`, `range`, `tolerance`, `number`, `enum`, `str`.
    #[serde(rename = "type")]
    pub kind: String,
    /// The type's unit without prefix (`Ω`, `°C`, `V/s`).
    pub unit: Option<String>,
    /// The variants of an `enum`.
    #[serde(default)]
    pub variants: Vec<String>,
    pub value: Option<f64>,
    pub values: Option<Vec<f64>>,
    pub lo: Option<f64>,
    pub hi: Option<f64>,
    pub ranges: Option<Vec<[f64; 2]>>,
    pub selected: Option<String>,
    pub str: Option<String>,
    /// The conditions, each `(value, unit)` in the unit without prefix.
    #[serde(default)]
    pub cond: Vec<(f64, String)>,
    /// The codes of [`VendorWarning`](crate::VendorWarning), as a set.
    #[serde(default)]
    pub warnings: Vec<String>,
    #[serde(default)]
    pub error: bool,
}

#[derive(Deserialize)]
struct File {
    case: Vec<Case>,
}

/// Every case of [`VENDOR_TOML`].
pub fn vendor_cases() -> Result<Vec<Case>, String> {
    toml::from_str::<File>(VENDOR_TOML)
        .map(|f| f.case)
        .map_err(|e| e.to_string())
}

fn unit(text: &str) -> Result<UnitExpr, String> {
    Quantity::parse(format!("1 {text}"))
        .map(|q| q.unit)
        .map_err(|e| format!("unit {text:?}: {e}"))
}

fn close(got: f64, want: f64) -> bool {
    got == want || (got - want).abs() <= REL_TOLERANCE * got.abs().max(want.abs())
}

fn number(what: &str, got: Option<f64>, want: f64) -> Result<(), String> {
    match got {
        Some(g) if close(g, want) => Ok(()),
        _ => Err(format!("{what}: got {got:?}, want {want}")),
    }
}

fn bounds(what: &str, v: &Variant, lo: f64, hi: f64) -> Result<(), String> {
    match v.bounds_base_f64() {
        Some((l, h)) if close(l, lo) && close(h, hi) => Ok(()),
        got => Err(format!("{what}: got {got:?} from {v:?}, want ({lo}, {hi})")),
    }
}

impl Case {
    /// Where the case came from, for messages: `Resistance` or `better_erc`.
    pub fn origin(&self) -> &str {
        self.def
            .as_deref()
            .or(self.source.as_deref())
            .unwrap_or("?")
    }

    /// The type the text is read as (numbers as `F64`).
    pub fn ty(&self) -> Result<VariantTy, String> {
        let unit = || unit(self.unit.as_deref().ok_or("no unit")?);
        Ok(match self.kind.as_str() {
            "si" => VariantTy::SI {
                number_ty: NumberTy::F64,
                unit: unit()?,
            },
            "range" => VariantTy::SIRange {
                number_ty: NumberTy::F64,
                start_inclusive: true,
                end_inclusive: true,
                unit: unit()?,
            },
            "tolerance" => VariantTy::Tolerance(NumberTy::F64),
            "number" => VariantTy::Number(NumberTy::F64),
            "enum" => VariantTy::enumeration("corpus", self.variants.clone()),
            "str" => VariantTy::Str,
            other => return Err(format!("unknown type {other:?}")),
        })
    }

    /// Whether `got` (what [`Variant::parse_vendor`] gave for [`Case::text`] and [`Case::ty`]) is what the case
    /// expects; the error says what differs.
    pub fn check(&self, got: &Result<VendorValue, Error>) -> Result<(), String> {
        let got = match (got, self.error) {
            (Err(_), true) => return Ok(()),
            (Ok(v), true) => return Err(format!("want an error, got {v:?}")),
            (Err(e), false) => return Err(format!("error: {e}")),
            (Ok(v), false) => v,
        };
        let v = &got.value;
        if let Some(want) = self.value {
            if matches!(v, Variant::List(_)) {
                return Err(format!("want one value, got {v:?}"));
            }
            number("value", v.num_base_f64(), want)?;
        }
        if let Some(want) = &self.values {
            let Variant::List(items) = v else {
                return Err(format!("want a list, got {v:?}"));
            };
            if items.len() != want.len() {
                return Err(format!("want {} values, got {v:?}", want.len()));
            }
            for (item, want) in items.iter().zip(want) {
                number("list item", item.num_base_f64(), *want)?;
            }
        }
        if let (Some(lo), Some(hi)) = (self.lo, self.hi) {
            bounds("bounds", v, lo, hi)?;
        }
        if let Some(want) = &self.ranges {
            let Variant::List(items) = v else {
                return Err(format!("want a list of ranges, got {v:?}"));
            };
            if items.len() != want.len() {
                return Err(format!("want {} ranges, got {v:?}", want.len()));
            }
            for (item, [lo, hi]) in items.iter().zip(want) {
                bounds("range", item, *lo, *hi)?;
            }
        }
        if let Some(want) = &self.selected {
            match v {
                Variant::Enum { selected, .. } if selected == want => {}
                _ => return Err(format!("want {want:?}, got {v:?}")),
            }
        }
        if let Some(want) = &self.str {
            match v {
                Variant::Str(s) if s == want => {}
                _ => return Err(format!("want {want:?}, got {v:?}")),
            }
        }
        if got.cond.len() != self.cond.len() {
            return Err(format!(
                "want conditions {:?}, got {:?}",
                self.cond, got.cond
            ));
        }
        for (c, (want, want_unit)) in got.cond.iter().zip(&self.cond) {
            number("condition", c.num_base_f64(), *want)?;
            let unit = match c {
                Variant::SI { unit, .. } => unit.without_prefixes().to_string(),
                _ => String::new(),
            };
            if unit != *want_unit {
                return Err(format!("condition unit: got {unit:?}, want {want_unit:?}"));
            }
        }
        let mut have: Vec<String> = got.warnings.iter().map(|w| w.to_string()).collect();
        let mut want = self.warnings.clone();
        have.sort();
        have.dedup();
        want.sort();
        want.dedup();
        if have != want {
            return Err(format!("warnings: got {have:?}, want {want:?}"));
        }
        Ok(())
    }
}
