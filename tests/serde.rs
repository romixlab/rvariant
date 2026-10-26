//! Golden JSON for every `Variant` and `VariantTy` kind (SER-2). mx3 stores both as JSON (parameter
//! facts, definition types): a change here breaks stored data and needs a CHANGELOG entry with the way
//! to migrate.
#![cfg(feature = "serde")]

use chrono::{NaiveDate, NaiveTime};
use rvariant::si_dynamic::{BaseUnit, Prefix, Unit, UnitExpr};
use rvariant::{
    Map, NanoSeconds, Number, NumberTy, Variant, VariantSeed, VariantTy, VariantTyDiscriminants,
};
use serde::de::DeserializeSeed;
use std::collections::BTreeSet;
use strum::IntoEnumIterator;

fn unit(prefix: Prefix, base: BaseUnit) -> UnitExpr {
    Unit::new(prefix, base, 1).into()
}

fn colors() -> VariantTy {
    VariantTy::enumeration(
        "Color",
        vec!["Red".to_string(), "Green".to_string(), "Blue".to_string()],
    )
}

fn color_set() -> VariantTy {
    VariantTy::MultiSelectionEnum {
        name: "Colors".into(),
        variants: vec!["Red".to_string(), "Green".to_string(), "Blue".to_string()].into(),
    }
}

/// One value of every kind with its stored JSON.
fn golden_values() -> Vec<(Variant, &'static str)> {
    let k_ohm = unit(Prefix::Kilo, BaseUnit::Ohm);
    let map: Map = [
        (Variant::i32(1), Variant::str("a")),
        (Variant::str("k"), Variant::Bool(true)),
    ]
    .into_iter()
    .collect();
    vec![
        (Variant::Empty, r#""Empty""#),
        (Variant::Bool(true), r#"{"Bool":true}"#),
        (Variant::str("13k7 \"x\""), r#"{"Str":"13k7 \"x\""}"#),
        (
            Variant::StrList(vec!["a".to_string(), "b".to_string()]),
            r#"{"StrList":["a","b"]}"#,
        ),
        (Variant::Number(Number::I8(-8)), r#"{"Number":{"I8":-8}}"#),
        (
            Variant::Number(Number::I16(-16)),
            r#"{"Number":{"I16":-16}}"#,
        ),
        (Variant::i32(-32), r#"{"Number":{"I32":-32}}"#),
        (Variant::i64(-64), r#"{"Number":{"I64":-64}}"#),
        (
            Variant::Number(Number::I128(-(1 << 100))),
            r#"{"Number":{"I128":-1267650600228229401496703205376}}"#,
        ),
        (Variant::Number(Number::U8(8)), r#"{"Number":{"U8":8}}"#),
        (Variant::Number(Number::U16(16)), r#"{"Number":{"U16":16}}"#),
        (Variant::u32(32), r#"{"Number":{"U32":32}}"#),
        (
            Variant::u64(u64::MAX),
            r#"{"Number":{"U64":18446744073709551615}}"#,
        ),
        (
            Variant::Number(Number::U128(1 << 100)),
            r#"{"Number":{"U128":1267650600228229401496703205376}}"#,
        ),
        (Variant::f32(0.5), r#"{"Number":{"F32":0.5}}"#),
        (Variant::f64(13.7), r#"{"Number":{"F64":13.7}}"#),
        (
            Variant::si(13.7f64, k_ohm.clone()),
            r#"{"SI":{"value":{"F64":13.7},"unit":{"Unit":{"prefix":"Kilo","base":"Ohm","exp":1}}}}"#,
        ),
        (
            Variant::SIRange {
                start: Number::F64((-55.0).into()),
                start_inclusive: true,
                end: Number::F64(125.0.into()),
                end_inclusive: false,
                unit: unit(Prefix::Unit, BaseUnit::DegreeCelsius),
            },
            r#"{"SIRange":{"start":{"F64":-55.0},"start_inclusive":true,"end":{"F64":125.0},"end_inclusive":false,"unit":{"Unit":{"prefix":"Unit","base":"DegreeCelsius","exp":1}}}}"#,
        ),
        (
            Variant::Enum {
                name: "Color".into(),
                selected: "Green".to_string(),
                variants: vec!["Red".to_string(), "Green".to_string(), "Blue".to_string()].into(),
            },
            r#"{"Enum":{"selected":"Green"}}"#,
        ),
        (
            Variant::MultiSelectionEnum {
                name: "Colors".into(),
                selected: vec!["Red".to_string(), "Blue".to_string()],
                variants: vec!["Red".to_string(), "Green".to_string(), "Blue".to_string()].into(),
            },
            r#"{"MultiSelectionEnum":{"selected":["Red","Blue"]}}"#,
        ),
        (
            Variant::Binary(vec![0, 0xAA, 0xFF]),
            r#"{"Binary":[0,170,255]}"#,
        ),
        (
            Variant::List(vec![Variant::i64(1), Variant::str("a"), Variant::Empty]),
            r#"{"List":[{"Number":{"I64":1}},{"Str":"a"},"Empty"]}"#,
        ),
        (
            Variant::Map(map),
            r#"{"Map":[[{"Number":{"I32":1}},{"Str":"a"}],[{"Str":"k"},{"Bool":true}]]}"#,
        ),
        (
            Variant::Date(NaiveDate::from_ymd_opt(2026, 10, 26).unwrap()),
            r#"{"Date":"2026-10-26"}"#,
        ),
        (
            Variant::Time(NaiveTime::from_hms_milli_opt(9, 24, 5, 250).unwrap()),
            r#"{"Time":"09:24:05.250"}"#,
        ),
        (
            Variant::DateTime("2026-10-26T09:24:05+01:00".parse().unwrap()),
            r#"{"DateTime":"2026-10-26T09:24:05+01:00"}"#,
        ),
        (
            Variant::Instant(NanoSeconds(1_500_000)),
            r#"{"Instant":1500000}"#,
        ),
        (
            Variant::money("EUR", 2, 1234),
            r#"{"Money":{"currency":"EUR","precision":2,"value":1234}}"#,
        ),
        (
            Variant::Tolerance {
                min: Number::F64((-5.0).into()),
                min_percent: true,
                max: Number::F64(5.0.into()),
                max_percent: true,
            },
            r#"{"Tolerance":{"min":{"F64":-5.0},"min_percent":true,"max":{"F64":5.0},"max_percent":true}}"#,
        ),
    ]
}

/// One type of every kind with its stored JSON (the `ty` column of mx3's parameter definitions).
fn golden_types() -> Vec<(VariantTy, &'static str)> {
    vec![
        (VariantTy::Empty, r#""Empty""#),
        (VariantTy::Bool, r#""Bool""#),
        (VariantTy::Str, r#""Str""#),
        (VariantTy::StrList, r#""StrList""#),
        (VariantTy::u16(), r#"{"Number":"U16"}"#),
        (
            VariantTy::si(NumberTy::F64, unit(Prefix::Kilo, BaseUnit::Ohm)),
            r#"{"SI":{"number_ty":"F64","unit":{"Unit":{"prefix":"Kilo","base":"Ohm","exp":1}}}}"#,
        ),
        (
            VariantTy::SIRange {
                number_ty: NumberTy::F32,
                start_inclusive: true,
                end_inclusive: true,
                unit: unit(Prefix::Unit, BaseUnit::DegreeCelsius),
            },
            r#"{"SIRange":{"number_ty":"F32","start_inclusive":true,"end_inclusive":true,"unit":{"Unit":{"prefix":"Unit","base":"DegreeCelsius","exp":1}}}}"#,
        ),
        (
            colors(),
            r#"{"Enum":{"name":"Color","variants":["Red","Green","Blue"]}}"#,
        ),
        (
            color_set(),
            r#"{"MultiSelectionEnum":{"name":"Colors","variants":["Red","Green","Blue"]}}"#,
        ),
        (VariantTy::Binary, r#""Binary""#),
        (VariantTy::List, r#""List""#),
        (VariantTy::Map, r#""Map""#),
        (VariantTy::Date, r#""Date""#),
        (VariantTy::Time, r#""Time""#),
        (VariantTy::DateTime, r#""DateTime""#),
        (VariantTy::Instant, r#""Instant""#),
        (
            VariantTy::money("EUR", 2),
            r#"{"Money":{"currency":"EUR","precision":2}}"#,
        ),
        (
            VariantTy::Tolerance(NumberTy::F64),
            r#"{"Tolerance":"F64"}"#,
        ),
    ]
}

fn kinds<'a>(types: impl Iterator<Item = &'a VariantTy>) -> BTreeSet<String> {
    types
        .map(|ty| VariantTyDiscriminants::from(ty).as_ref().to_string())
        .collect()
}

fn all_kinds() -> BTreeSet<String> {
    VariantTyDiscriminants::iter()
        .map(|k| k.as_ref().to_string())
        .collect()
}

#[test]
fn every_kind_has_golden_json() {
    let values: Vec<VariantTy> = golden_values().iter().map(|(v, _)| v.ty()).collect();
    assert_eq!(kinds(values.iter()), all_kinds());
    let types = golden_types();
    assert_eq!(kinds(types.iter().map(|(ty, _)| ty)), all_kinds());
}

#[test]
fn variant_json_is_unchanged() {
    for (v, json) in golden_values() {
        assert_eq!(serde_json::to_string(&v).unwrap(), json, "{v:?}");
        let back: Variant = serde_json::from_str(json).unwrap();
        assert_eq!(back.restore(&v.ty()), Ok(v.clone()), "{json}");
        let mut de = serde_json::Deserializer::from_str(json);
        assert_eq!(VariantSeed(&v.ty()).deserialize(&mut de).unwrap(), v);
    }
}

#[test]
fn variant_ty_json_is_unchanged() {
    for (ty, json) in golden_types() {
        assert_eq!(serde_json::to_string(&ty).unwrap(), json, "{ty:?}");
        assert_eq!(
            serde_json::from_str::<VariantTy>(json).unwrap(),
            ty,
            "{json}"
        );
    }
}

#[test]
fn enum_values_store_only_the_selection() {
    let v = Variant::try_from_str("green", &colors()).unwrap();
    let json = serde_json::to_string(&v).unwrap();
    assert_eq!(json, r#"{"Enum":{"selected":"Green"}}"#);

    let loaded: Variant = serde_json::from_str(&json).unwrap();
    assert_eq!(
        loaded,
        Variant::Enum {
            name: "".into(),
            selected: "Green".to_string(),
            variants: Vec::<String>::new().into(),
        }
    );
    assert_eq!(loaded.to_string(), "Green");
    assert_eq!(loaded.restore(&colors()), Ok(v));
}

#[test]
fn enum_values_load_with_the_current_variants() {
    // A variant added to the type after the value was stored shows up on load.
    let json = r#"{"Enum":{"selected":"Green"}}"#;
    let grown = VariantTy::enumeration(
        "Color",
        vec!["Red".to_string(), "Green".to_string(), "Violet".to_string()],
    );
    let mut de = serde_json::Deserializer::from_str(json);
    let v = VariantSeed(&grown).deserialize(&mut de).unwrap();
    assert_eq!(v.ty(), grown);

    // A selection the type no longer has is an error, not a silent value.
    let json = r#"{"Enum":{"selected":"Blue"}}"#;
    let mut de = serde_json::Deserializer::from_str(json);
    let e = VariantSeed(&grown).deserialize(&mut de).unwrap_err();
    assert!(e.to_string().contains("Blue"), "{e}");

    let json = r#"{"MultiSelectionEnum":{"selected":["blue","Red"]}}"#;
    let mut de = serde_json::Deserializer::from_str(json);
    let v = VariantSeed(&color_set()).deserialize(&mut de).unwrap();
    assert_eq!(
        v,
        Variant::MultiSelectionEnum {
            name: "Colors".into(),
            selected: vec!["Blue".to_string(), "Red".to_string()],
            variants: vec!["Red".to_string(), "Green".to_string(), "Blue".to_string()].into(),
        }
    );
}

#[test]
fn enum_values_in_the_0_3_format_still_load() {
    // 0.3 stored the name and the variants in every value; they are ignored on load now.
    let json = r#"{"Enum":{"name":"Old","selected":"Green","variants":["Green"]}}"#;
    let mut de = serde_json::Deserializer::from_str(json);
    let v = VariantSeed(&colors()).deserialize(&mut de).unwrap();
    assert_eq!(v, Variant::try_from_str("Green", &colors()).unwrap());
    let json =
        r#"{"MultiSelectionEnum":{"name":"Old","selected":["Red"],"variants":["Red","Blue"]}}"#;
    let mut de = serde_json::Deserializer::from_str(json);
    let v = VariantSeed(&color_set()).deserialize(&mut de).unwrap();
    assert_eq!(v, Variant::try_from_str("Red", &color_set()).unwrap());
}

#[test]
fn restore_keeps_values_of_other_types() {
    // A stored value is never reinterpreted by a later change of its definition's type.
    let v = Variant::si(13.7f64, unit(Prefix::Kilo, BaseUnit::Ohm));
    assert_eq!(v.clone().restore(&VariantTy::u32()), Ok(v));
    let loaded: Variant = serde_json::from_str(r#"{"Enum":{"selected":"Green"}}"#).unwrap();
    assert_eq!(loaded.clone().restore(&VariantTy::Str), Ok(loaded));
}
