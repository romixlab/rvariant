//! `Variant::try_from_str(v.to_string(), &v.ty()) == v` and `default_of` invariants.
//!
//! Exceptions (documented):
//! * `StrList` elements containing `,` `;` or tab, or with surrounding whitespace;
//! * `List` / `Map` elements of kinds other than Empty/Bool/Number/Str/List/Map come back as `Str`,
//!   and number types are normalized to I64 / F64;
//! * `Str` is always returned verbatim.
use chrono::{NaiveDate, NaiveTime, TimeZone, Utc};
use rvariant::si_dynamic::{BaseUnit, Prefix, Unit};
use rvariant::*;

fn samples() -> Vec<Variant> {
    let kohm = Unit {
        prefix: Prefix::Kilo,
        base: BaseUnit::Ohm,
        exp: 1,
    };
    let uf = Unit {
        prefix: Prefix::Micro,
        base: BaseUnit::Farad,
        exp: 1,
    };
    let km2 = Unit {
        prefix: Prefix::Kilo,
        base: BaseUnit::Meter,
        exp: 2,
    };
    let variants: Vec<String> = vec!["Red".into(), "Green".into(), "Blue".into()];
    vec![
        Variant::Empty,
        Variant::Bool(true),
        Variant::Bool(false),
        Variant::str("hello, world"),
        Variant::str(""),
        Variant::StrList(vec!["a".into(), "".into(), "b c".into()]),
        Variant::StrList(vec![]),
        Variant::Number(Number::I8(-128)),
        Variant::Number(Number::I16(i16::MAX)),
        Variant::i32(-42),
        Variant::i64(i64::MIN),
        Variant::Number(Number::I128(i128::MIN)),
        Variant::Number(Number::U8(255)),
        Variant::Number(Number::U16(0)),
        Variant::u32(u32::MAX),
        Variant::u64(u64::MAX),
        Variant::Number(Number::U128(u128::MAX)),
        Variant::f32(0.1),
        Variant::f32(-1e-30),
        Variant::f32(f32::MAX),
        Variant::f64(0.1 + 0.2),
        Variant::f64(f64::MIN_POSITIVE),
        Variant::f64(f64::INFINITY),
        Variant::f64(f64::NEG_INFINITY),
        Variant::si(F32(4.7), kohm.clone()),
        Variant::si(F64(0.1), uf.clone()),
        Variant::si(-3i32, Unit::base(BaseUnit::Volt)),
        Variant::si(F32(25.0), Unit::base(BaseUnit::DegreeCelsius)),
        Variant::si(F64(2.5), km2),
        Variant::si(F32(3.0), Unit::unit()),
        Variant::SIRange {
            start: Number::F32((-40.0).into()),
            start_inclusive: true,
            end: Number::F32(85.0.into()),
            end_inclusive: false,
            unit: Unit::base(BaseUnit::DegreeCelsius),
        },
        Variant::SIRange {
            start: Number::I32(1),
            start_inclusive: false,
            end: Number::I32(10),
            end_inclusive: true,
            unit: uf,
        },
        Variant::enumeration("Color", "Green", variants.clone()),
        Variant::MultiSelectionEnum {
            name: "Color".into(),
            selected: vec!["Red".into(), "Blue".into()],
            variants: variants.clone().into(),
        },
        Variant::MultiSelectionEnum {
            name: "Color".into(),
            selected: vec![],
            variants: variants.into(),
        },
        Variant::Binary(vec![0, 1, 0xfe, 0xff]),
        Variant::Binary(vec![]),
        Variant::List(vec![
            Variant::i64(1),
            Variant::f64(2.0),
            Variant::str("x\ny \"z\" \\"),
            Variant::Bool(false),
            Variant::Empty,
            Variant::List(vec![]),
        ]),
        Variant::Map(
            [
                (Variant::str("a"), Variant::i64(1)),
                (Variant::i64(2), Variant::List(vec![Variant::f64(0.5)])),
            ]
            .into_iter()
            .collect(),
        ),
        Variant::Date(NaiveDate::from_ymd_opt(2024, 12, 31).unwrap()),
        Variant::Time(NaiveTime::from_hms_micro_opt(23, 59, 1, 123456).unwrap()),
        Variant::Time(NaiveTime::from_hms_opt(0, 0, 0).unwrap()),
        Variant::DateTime(Utc.with_ymd_and_hms(2024, 1, 2, 3, 4, 5).unwrap().into()),
        Variant::DateTime(
            chrono::DateTime::parse_from_rfc3339("2024-01-02T03:04:05.123+05:30").unwrap(),
        ),
        Variant::Instant(NanoSeconds(0)),
        Variant::Instant(NanoSeconds(999)),
        Variant::Instant(NanoSeconds(1_500)),
        Variant::Instant(NanoSeconds(-2_000_001)),
        Variant::Instant(NanoSeconds(i64::MAX)),
        Variant::Instant(NanoSeconds(i64::MIN)),
        Variant::money("EUR", 2, 123456789012),
        Variant::money("$", 4, -1),
        Variant::money("", 0, i64::MAX),
        Variant::money("BTC", 18, i64::MIN),
        Variant::Tolerance {
            min: Number::F32((-5.0).into()),
            min_percent: true,
            max: Number::F32(5.0.into()),
            max_percent: true,
        },
        Variant::Tolerance {
            min: Number::I32(-10),
            min_percent: true,
            max: Number::I32(20),
            max_percent: true,
        },
        Variant::Tolerance {
            min: Number::F64((-0.1).into()),
            min_percent: false,
            max: Number::F64(0.2.into()),
            max_percent: true,
        },
    ]
}

#[test]
fn display_parse_roundtrip() {
    for v in samples() {
        let ty = v.ty();
        let s = v.to_string();
        if v == Variant::Empty {
            assert_eq!(s, "");
            continue;
        }
        let back = Variant::try_from_str(&s, &ty);
        assert_eq!(back.as_ref(), Ok(&v), "{v:?} displayed as {s:?}");
    }
}

#[test]
fn convert_to_str_and_back() {
    for v in samples() {
        if v == Variant::Empty {
            continue;
        }
        let ty = v.ty();
        let s = v.clone().convert_to(&VariantTy::Str).unwrap();
        assert_eq!(s.convert_to(&ty), Ok(v));
    }
}

#[test]
fn default_of_has_requested_type() {
    for v in samples() {
        let ty = v.ty();
        assert_eq!(Variant::default_of(&ty).ty(), ty);
    }
}

#[cfg(feature = "serde")]
#[test]
fn serde_json_roundtrip() {
    for v in samples() {
        // JSON cannot represent infinity
        if let Variant::Number(Number::F64(f)) = &v
            && !f.0.is_finite()
        {
            continue;
        }
        let json = serde_json::to_string(&v).unwrap();
        let back: Variant = serde_json::from_str(&json).unwrap();
        assert_eq!(back, v, "{json}");
        let ty = v.ty();
        let json = serde_json::to_string(&ty).unwrap();
        assert_eq!(serde_json::from_str::<VariantTy>(&json).unwrap(), ty);
    }
}
