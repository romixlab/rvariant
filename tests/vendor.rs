//! `Variant::parse_vendor` beyond the shared corpus (`tests/vendor_corpus.rs`): limits, number types, and input
//! that must be rejected without a panic.

use rvariant::si_dynamic::{BaseUnit, Prefix, Unit};
use rvariant::{MAX_VENDOR_LEN, NumberTy, Variant, VariantTy, VendorWarning};

fn types() -> Vec<VariantTy> {
    vec![
        VariantTy::si(NumberTy::F64, BaseUnit::Ohm),
        VariantTy::si(NumberTy::F32, Unit::new(Prefix::Kilo, BaseUnit::Ohm, 1)),
        VariantTy::si(NumberTy::I32, BaseUnit::Volt),
        VariantTy::si(NumberTy::F64, BaseUnit::Meter),
        VariantTy::SIRange {
            number_ty: NumberTy::F64,
            start_inclusive: true,
            end_inclusive: true,
            unit: BaseUnit::DegreeCelsius.into(),
        },
        VariantTy::Tolerance(NumberTy::F64),
        VariantTy::Number(NumberTy::U8),
        VariantTy::Number(NumberTy::F64),
        VariantTy::enumeration("Dielectric", vec!["C0G".to_string(), "X7R".to_string()]),
        VariantTy::Str,
        VariantTy::Bool,
        VariantTy::Date,
        VariantTy::List,
    ]
}

#[test]
fn text_longer_than_the_limit_is_an_error() {
    let ohm = VariantTy::si(NumberTy::F64, BaseUnit::Ohm);
    let long = format!("{}1k", " ".repeat(MAX_VENDOR_LEN));
    assert!(Variant::parse_vendor(&long, &ohm).is_err());
    let at_limit = format!("{:>width$}", "1k", width = MAX_VENDOR_LEN);
    assert!(Variant::parse_vendor(&at_limit, &ohm).is_ok());
}

#[test]
fn number_types_of_the_column_are_kept() {
    let kohm = VariantTy::si(NumberTy::F32, Unit::new(Prefix::Kilo, BaseUnit::Ohm, 1));
    let v = Variant::parse_vendor("13.7 kOhms", &kohm).unwrap();
    assert_eq!(
        v.value,
        Variant::si(13.7f32, Unit::new(Prefix::Kilo, BaseUnit::Ohm, 1))
    );
    // A fraction is rounded to the column's float type; an integer column can't hold 1/3
    let w = VariantTy::si(NumberTy::F32, BaseUnit::Watt);
    assert!(Variant::parse_vendor("0.333W, 1/3W", &w).is_ok());
    let volt_i32 = VariantTy::si(NumberTy::I32, BaseUnit::Volt);
    assert!(Variant::parse_vendor("1/3V", &volt_i32).is_err());
    assert!(Variant::parse_vendor("4.5V", &volt_i32).is_err());
    assert_eq!(
        Variant::parse_vendor("48VDC", &volt_i32).unwrap().value,
        Variant::si(48i32, BaseUnit::Volt)
    );
}

#[test]
fn conditions_without_unit_are_numbers() {
    let ohm = VariantTy::si(NumberTy::F64, BaseUnit::Ohm);
    let v = Variant::parse_vendor("10 Ohm @ 5", &ohm).unwrap();
    assert_eq!(v.cond, [Variant::f64(5.0)]);
    let v = Variant::parse_vendor("10 Ohm @ 5V, lots", &ohm).unwrap();
    assert_eq!(v.cond.len(), 1);
    assert_eq!(v.warnings, [VendorWarning::Trailing("lots".into())]);
}

#[test]
fn list_items_under_different_conditions_are_an_error() {
    let f = VariantTy::si(NumberTy::F64, BaseUnit::Farad);
    assert!(Variant::parse_vendor("1pF @ 1MHz, 2pF @ 10MHz", &f).is_err());
}

#[test]
fn odd_input_is_rejected_without_panic() {
    let texts = [
        "",
        " ",
        ",",
        ",,,",
        ";",
        "@",
        "@@",
        "(",
        ")",
        "((((",
        "))))",
        "(()",
        "~",
        "~~",
        "1~",
        "~1",
        "/",
        "1/0W",
        "0/0",
        "1/",
        "/1",
        "99999999999999999999999999999/1W",
        "1/99999999999999999999999999999",
        "1e999 Ohm",
        "-1e999",
        "1e-999 F",
        "±",
        "±%",
        "+-",
        "-",
        "--1",
        "1..2",
        "1,",
        ",1",
        "1,2,3,4",
        "1,234,56",
        "4,7,k",
        "Â",
        "Âµ",
        "Î",
        "Î©Î©",
        "µ",
        "kΩ",
        "k",
        "R",
        "r",
        "0x1F",
        "1 2 3 4 5 6 7 8 9 10 11 12",
        "4k7k7",
        "4k",
        "k7",
        "-k7",
        "4.7.7k",
        "12.",
        ".5",
        "-.5V",
        "1 @ 2 @ 3",
        "(1) (2) (3)",
        "1 (2 (3 (4)))",
        "1A (",
        "1A )",
        "\u{0}",
        "\u{feff}1V",
        "1\u{a0}V",
        "1\u{2009}kΩ",
        "−5 V",
        "1\tV",
        "1\nV",
        "🙂",
        "1🙂V",
        "C0G, ",
        ", X7R",
        "[1, 2]",
        "{\"a\": 1}",
        "true",
        "2024-13-45",
        "0b2",
        "1/1/1/1/1",
    ];
    let mut long_list = "1V, ".repeat(MAX_VENDOR_LEN / 4);
    long_list.truncate(MAX_VENDOR_LEN);
    let nested = "(".repeat(MAX_VENDOR_LEN);
    for ty in types() {
        for text in texts
            .iter()
            .copied()
            .chain([long_list.as_str(), nested.as_str()])
        {
            let _ = Variant::parse_vendor(text, &ty);
        }
    }
}

#[test]
fn every_corpus_text_reads_under_every_type_without_panic() {
    let corpus = include_str!("corpus/vendor.toml");
    let texts = corpus
        .lines()
        .filter_map(|l| l.strip_prefix("text = \""))
        .filter_map(|l| l.strip_suffix('"'));
    let texts: Vec<&str> = texts.collect();
    assert!(texts.len() > 300);
    for ty in types() {
        for text in &texts {
            let _ = Variant::parse_vendor(text, &ty);
        }
    }
}
