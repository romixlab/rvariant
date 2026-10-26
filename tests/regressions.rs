//! Regression tests for bugs found during review.
use rvariant::si_dynamic::{BaseUnit, Prefix, Unit, UnitExpr};
use rvariant::*;

fn volt() -> UnitExpr {
    BaseUnit::Volt.into()
}

fn si_ty(number_ty: NumberTy, unit: impl Into<UnitExpr>) -> VariantTy {
    VariantTy::si(number_ty, unit)
}

#[test]
fn negative_to_unsigned_is_rejected_not_wrapped() {
    assert!(matches!(
        Variant::try_from_str("-1", &VariantTy::u32()),
        Err(Error::OutOfRange { .. })
    ));
    assert!(matches!(
        Variant::try_from_str("-5", &VariantTy::u8()),
        Err(Error::OutOfRange { .. })
    ));
    assert!(Variant::i32(-1).convert_to(&VariantTy::u32()).is_err());
    assert!(matches!(
        Variant::try_from_str("256", &VariantTy::u8()),
        Err(Error::OutOfRange { .. })
    ));
}

#[test]
fn integer_literals() {
    let p = |s: &str, ty: NumberTy| Number::try_from_str(s, ty);
    assert_eq!(p("0x10", NumberTy::U8), Ok(Number::U8(16)));
    assert_eq!(p("-0x10", NumberTy::I8), Ok(Number::I8(-16)));
    assert_eq!(p("0b101", NumberTy::U8), Ok(Number::U8(5)));
    assert_eq!(p("1_000", NumberTy::U16), Ok(Number::U16(1000)));
    assert_eq!(p(" +7 ", NumberTy::I32), Ok(Number::I32(7)));
    assert_eq!(p("-128", NumberTy::I8), Ok(Number::I8(-128)));
    assert_eq!(p("5.0", NumberTy::U8), Ok(Number::U8(5)));
    assert_eq!(p("1e3", NumberTy::U16), Ok(Number::U16(1000)));
    assert!(matches!(p("5.5", NumberTy::U8), Err(Error::Lossy { .. })));
    assert!(matches!(p("", NumberTy::U8), Err(Error::Empty)));
    assert!(matches!(p("abc", NumberTy::U8), Err(Error::Parse { .. })));
    assert!(matches!(p("0x", NumberTy::U8), Err(Error::Parse { .. })));
    assert_eq!(
        p("-170141183460469231731687303715884105728", NumberTy::I128),
        Ok(Number::I128(i128::MIN))
    );
    assert_eq!(
        p("340282366920938463463374607431768211455", NumberTy::U128),
        Ok(Number::U128(u128::MAX))
    );
}

#[test]
fn instant_suffixes() {
    let p = |s: &str| Variant::try_from_str(s, &VariantTy::Instant);
    assert_eq!(p("10ms"), Ok(Variant::Instant(NanoSeconds(10_000_000))));
    assert_eq!(p("10us"), Ok(Variant::Instant(NanoSeconds(10_000))));
    assert_eq!(p("10 µs"), Ok(Variant::Instant(NanoSeconds(10_000))));
    assert_eq!(p("10ns"), Ok(Variant::Instant(NanoSeconds(10))));
    assert_eq!(p("10s"), Ok(Variant::Instant(NanoSeconds(10_000_000_000))));
    assert_eq!(p("1.5 s"), Ok(Variant::Instant(NanoSeconds(1_500_000_000))));
    assert_eq!(p("-2 ms"), Ok(Variant::Instant(NanoSeconds(-2_000_000))));
    assert_eq!(p("10"), Ok(Variant::Instant(NanoSeconds(10))));
    assert!(matches!(p("1.5 ns"), Err(Error::Lossy { .. })));
    assert!(matches!(p("10 h"), Err(Error::Parse { .. })));
}

#[test]
fn empty_displays_as_empty_string() {
    assert_eq!(Variant::Empty.to_string(), "");
    assert_eq!(
        Variant::Empty.convert_to(&VariantTy::Str),
        Ok(Variant::str(""))
    );
    assert_eq!(
        Variant::Empty.convert_to(&VariantTy::u32()),
        Err(Error::Empty)
    );
    assert_eq!(Variant::Empty.as_u32(), Err(Error::Empty));
}

#[test]
fn float_to_int_is_strict() {
    assert!(matches!(
        Variant::f32(1.9).convert_to(&VariantTy::i32()),
        Err(Error::Lossy { .. })
    ));
    assert!(matches!(
        Variant::f32(f32::NAN).convert_to(&VariantTy::u8()),
        Err(Error::Lossy { .. })
    ));
    assert!(matches!(
        Variant::f64(1e300).convert_to(&VariantTy::u64()),
        Err(Error::OutOfRange { .. })
    ));
    assert_eq!(
        Variant::f32(2.0).convert_to(&VariantTy::i32()),
        Ok(Variant::i32(2))
    );
    // lossy rounds and saturates
    assert_eq!(
        Variant::f32(1.9).convert_lossy(&VariantTy::i32()),
        Ok(Variant::i32(2))
    );
    assert_eq!(
        Variant::i32(-5).convert_lossy(&VariantTy::u32()),
        Ok(Variant::u32(0))
    );
    assert_eq!(
        Variant::i32(300).convert_lossy(&VariantTy::u8()),
        Ok(Variant::Number(Number::U8(255)))
    );
    assert!(
        Variant::f32(f32::NAN)
            .convert_lossy(&VariantTy::u8())
            .is_err()
    );
    // f64 overflowing f32
    assert!(Variant::f64(1e300).convert_to(&VariantTy::f32()).is_err());
    assert_eq!(
        Number::F64(1e300.into()).convert_lossy(NumberTy::F32),
        Ok(Number::F32(f32::INFINITY.into()))
    );
}

#[test]
fn si_unit_is_checked() {
    let ampere = si_ty(NumberTy::F32, Unit::base(BaseUnit::Ampere));
    assert!(matches!(
        Variant::try_from_str("5Vdc", &ampere),
        Err(Error::UnitMismatch { .. })
    ));
    let v = si_ty(NumberTy::F32, volt());
    assert_eq!(
        Variant::try_from_str("5Vdc", &v),
        Ok(Variant::si(F32(5.0), volt()))
    );
    assert!(matches!(
        Variant::try_from_str("5 A", &v),
        Err(Error::UnitMismatch { .. })
    ));
}

#[test]
fn si_respects_number_ty_and_prefix() {
    let ty = si_ty(NumberTy::F64, volt());
    let v = Variant::try_from_str("5mV", &ty).unwrap();
    assert_eq!(v.ty(), ty);
    assert_eq!(v, Variant::si(F64(0.005), volt()));

    let mv = Unit {
        prefix: Prefix::Milli,
        base: BaseUnit::Volt,
        exp: 1,
    };
    let ty = si_ty(NumberTy::I32, mv.clone());
    assert_eq!(
        Variant::try_from_str("1.5 V", &ty),
        Ok(Variant::si(1500i32, mv.clone()))
    );
    assert_eq!(
        Variant::try_from_str("7", &ty),
        Ok(Variant::si(7i32, mv.clone()))
    );
    assert!(matches!(
        Variant::try_from_str("1.5 mV", &ty),
        Err(Error::Lossy { .. })
    ));
    assert_eq!(
        Variant::si(1500i32, mv).as_base_unit(BaseUnit::Volt),
        Ok(Number::F64(1.5.into()))
    );
}

#[test]
fn si_bare_number_takes_column_unit() {
    let ty = si_ty(NumberTy::F32, volt());
    assert_eq!(
        Variant::try_from_str("5", &ty),
        Ok(Variant::si(F32(5.0), volt()))
    );
    assert_eq!(
        Variant::i32(5).convert_to(&ty),
        Ok(Variant::si(F32(5.0), volt()))
    );
}

#[test]
fn si_rescale() {
    let kv = Unit {
        prefix: Prefix::Kilo,
        base: BaseUnit::Volt,
        exp: 1,
    };
    let v = Variant::si(F32(1500.0), volt());
    assert_eq!(
        v.convert_to(&si_ty(NumberTy::F64, kv.clone())),
        Ok(Variant::si(F64(1.5), kv))
    );
    assert!(
        Variant::si(F32(1.0), volt())
            .convert_to(&si_ty(NumberTy::F32, Unit::base(BaseUnit::Ampere)))
            .is_err()
    );
}

#[test]
fn si_resistance_notation() {
    let ohm = Unit::base(BaseUnit::Ohm);
    let ty = si_ty(NumberTy::F32, ohm.clone());
    for s in ["4k7", "4.7k", "4.7kΩ", "4.7 kOhm", "4700", "4700R"] {
        assert_eq!(
            Variant::try_from_str(s, &ty),
            Ok(Variant::si(F32(4700.0), ohm.clone())),
            "{s}"
        );
    }
}

#[test]
fn si_mass() {
    let kg = Unit::new(Prefix::Kilo, BaseUnit::Gram, 1);
    let ty = si_ty(NumberTy::F64, kg.clone());
    assert_eq!(
        Variant::try_from_str("5kg", &ty),
        Ok(Variant::si(F64(5.0), kg.clone()))
    );
    assert_eq!(
        Variant::try_from_str("5 g", &ty),
        Ok(Variant::si(F64(0.005), kg.clone()))
    );
    let v = Variant::si(F64(2.5), kg.clone());
    assert_eq!(Variant::try_from_str(v.to_string(), &ty), Ok(v));
}

#[test]
fn money_display_is_exact() {
    let m = Variant::money("EUR", 2, 123456789012);
    assert_eq!(m.to_string(), "1234567890.12 EUR");
    assert_eq!(Variant::money("EUR", 2, -5).to_string(), "-0.05 EUR");
    assert_eq!(Variant::money("X", 12, 1).to_string(), "0.000000000001 X");
    assert_eq!(
        Variant::money("X", 30, 1).to_string(),
        "0.000000000000000000000000000001 X"
    );
    assert_eq!(Variant::money("", 0, 42).to_string(), "42");
}

#[test]
fn money_parse() {
    let ty = VariantTy::money("$", 2);
    let p = |s: &str| Variant::try_from_str(s, &ty);
    assert_eq!(p("12.34 $"), Ok(Variant::money("$", 2, 1234)));
    assert_eq!(p("$12.34"), Ok(Variant::money("$", 2, 1234)));
    assert_eq!(p("12"), Ok(Variant::money("$", 2, 1200)));
    assert_eq!(p("1,234.5"), Ok(Variant::money("$", 2, 123450)));
    assert_eq!(p("12,5"), Ok(Variant::money("$", 2, 1250)));
    assert_eq!(p("-0.05"), Ok(Variant::money("$", 2, -5)));
    assert!(matches!(p("12.345"), Err(Error::Lossy { .. })));
    assert!(matches!(p("12 EUR"), Err(Error::CurrencyMismatch { .. })));
    // conversions
    assert_eq!(
        Variant::f64(0.1).convert_to(&ty),
        Ok(Variant::money("$", 2, 10))
    );
    assert_eq!(
        Variant::money("$", 2, 1234).convert_to(&VariantTy::money("$", 4)),
        Ok(Variant::money("$", 4, 123400))
    );
    assert!(Variant::money("$", 4, 123456).convert_to(&ty).is_err());
    assert_eq!(
        Variant::money("$", 4, 123456).convert_lossy(&ty),
        Ok(Variant::money("$", 2, 1235))
    );
    assert!(Variant::money("€", 2, 1).convert_to(&ty).is_err());
    assert_eq!(Variant::money("$", 2, 1234).as_f64(), Ok(12.34));
    assert!(Variant::money("$", 2, 1234).as_i32().is_err());
    assert_eq!(Variant::money("$", 2, 1200).as_i32(), Ok(12));
}

#[cfg(feature = "serde")]
#[test]
fn map_json() {
    let m: Map = [
        (Variant::i32(1), Variant::str("a")),
        (Variant::str("k"), Variant::Bool(true)),
    ]
    .into_iter()
    .collect();
    let v = Variant::Map(m);
    let json = serde_json::to_string(&v).unwrap();
    let back: Variant = serde_json::from_str(&json).unwrap();
    assert_eq!(back, v);
}

#[cfg(feature = "serde")]
#[test]
fn variant_ty_json_format() {
    // mx3 stores VariantTy as JSON in the database; the unit became a UnitExpr in 0.5
    let ty = si_ty(NumberTy::F32, volt());
    assert_eq!(
        serde_json::to_string(&ty).unwrap(),
        r#"{"SI":{"number_ty":"F32","unit":{"Unit":{"prefix":"Unit","base":"Volt","exp":1}}}}"#
    );
    let ty = VariantTy::money("$", 2);
    assert_eq!(
        serde_json::to_string(&ty).unwrap(),
        r#"{"Money":{"currency":"$","precision":2}}"#
    );
    let ty = VariantTy::enumeration("Cond", vec!["New".to_string(), "Used".to_string()]);
    let json = serde_json::to_string(&ty).unwrap();
    assert_eq!(
        json,
        r#"{"Enum":{"name":"Cond","variants":["New","Used"]}}"#
    );
    assert_eq!(serde_json::from_str::<VariantTy>(&json).unwrap(), ty);
    assert_eq!(
        serde_json::to_string(&VariantTy::Instant).unwrap(),
        r#""Instant""#
    );
}

#[test]
fn errors_implement_std_error() {
    fn takes_error(_: &dyn std::error::Error) {}
    let e = Variant::try_from_str("x", &VariantTy::u32()).unwrap_err();
    takes_error(&e);
    assert_eq!(e.to_string(), r#"cannot parse "x" as U32: invalid integer"#);
    assert_eq!(Variant::Empty.as_non_empty_str(), Err(Error::Empty));
    assert_eq!(Variant::str("").as_non_empty_str(), Err(Error::Empty));
    assert!(matches!(
        Variant::i32(1).as_str(),
        Err(Error::CannotConvert(..))
    ));
}

#[test]
fn numeric_comparisons() {
    assert!(Number::I32(1).eq_value(&Number::I64(1)));
    assert!(Number::I32(1).eq_value(&Number::F64(1.0.into())));
    assert!(Number::I8(5).cmp_value(&Number::U8(1)).is_gt());
    assert!(Number::I8(-1).cmp_value(&Number::U128(u128::MAX)).is_lt());
    assert!(Variant::i32(5).sort_cmp(&Variant::u64(10)).is_lt());
    let kv = Unit {
        prefix: Prefix::Kilo,
        base: BaseUnit::Volt,
        exp: 1,
    };
    assert!(
        Variant::si(F32(2.0), kv)
            .sort_cmp(&Variant::si(F32(1500.0), volt()))
            .is_gt()
    );
    assert!(
        Variant::money("$", 2, 150)
            .sort_cmp(&Variant::money("$", 3, 1400))
            .is_gt()
    );
}

#[test]
fn bool_parse_and_convert() {
    for s in ["true", "TRUE", "yes", "Y", "on", "1", "✓"] {
        assert_eq!(
            Variant::try_from_str(s, &VariantTy::Bool),
            Ok(Variant::Bool(true)),
            "{s}"
        );
    }
    for s in ["false", "No", "n", "off", "0"] {
        assert_eq!(
            Variant::try_from_str(s, &VariantTy::Bool),
            Ok(Variant::Bool(false)),
            "{s}"
        );
    }
    assert_eq!(
        Variant::try_from_str("", &VariantTy::Bool),
        Err(Error::Empty)
    );
    assert!(Variant::try_from_str("maybe", &VariantTy::Bool).is_err());
    assert_eq!(Variant::i32(1).as_bool(), Ok(true));
    assert_eq!(Variant::f32(0.0).as_bool(), Ok(false));
    assert!(Variant::i32(2).as_bool().is_err());
    assert_eq!(
        Variant::i32(2).convert_lossy(&VariantTy::Bool),
        Ok(Variant::Bool(true))
    );
    assert_eq!(Variant::Bool(true).as_u8(), Ok(1));
}

#[test]
fn accessors_and_from() {
    assert_eq!(Variant::from(5u32), Variant::u32(5));
    assert_eq!(Variant::from("a"), Variant::str("a"));
    assert_eq!(Variant::from(None::<i32>), Variant::Empty);
    assert_eq!(Variant::from(Some(2.5f64)), Variant::f64(2.5));
    assert_eq!(u16::try_from(Variant::str("42")), Ok(42));
    assert_eq!(f64::try_from(&Variant::i32(3)), Ok(3.0));
    assert_eq!(String::try_from(Variant::str("x")), Ok("x".to_string()));
    assert_eq!(Variant::str("0x1F").as_u8(), Ok(31));
    assert_eq!(Variant::str("2.5").as_f32(), Ok(2.5));
    assert_eq!(Variant::Instant(NanoSeconds(7)).as_i64(), Ok(7));
    assert_eq!(Variant::str("2.5").as_number(), Ok(Number::F64(2.5.into())));
    assert_eq!(Variant::str("2").as_number(), Ok(Number::I64(2)));
    assert_eq!(
        Variant::str("2024-02-03").as_date(),
        Ok(chrono::NaiveDate::from_ymd_opt(2024, 2, 3).unwrap())
    );
    assert!(Variant::Binary(vec![1, 2]).as_bytes().is_ok());
}

#[test]
fn enums() {
    let variants: Vec<String> = vec!["New".into(), "Used".into(), "Broken".into()];
    let ty = VariantTy::enumeration("Condition", variants.clone());
    assert_eq!(
        Variant::try_from_str("used", &ty),
        Ok(Variant::enumeration("Condition", "Used", variants.clone()))
    );
    assert_eq!(
        Variant::try_from_str("Condition::Broken", &ty),
        Ok(Variant::enumeration(
            "Condition",
            "Broken",
            variants.clone()
        ))
    );
    assert!(matches!(
        Variant::try_from_str("Lost", &ty),
        Err(Error::WrongEnumVariantName { .. })
    ));
    let multi = VariantTy::MultiSelectionEnum {
        name: "Condition".into(),
        variants: variants.clone().into(),
    };
    let v = Variant::try_from_str("new, broken", &multi).unwrap();
    assert_eq!(v.to_string(), "New, Broken");
    assert_eq!(Variant::try_from_str(v.to_string(), &multi), Ok(v));
    assert_eq!(
        Variant::default_of(&ty),
        Variant::enumeration("Condition", "New", variants)
    );
}

#[test]
fn date_time_formats() {
    let ty = VariantTy::DateTime;
    let a = Variant::try_from_str("2024-01-02T03:04:05+01:00", &ty).unwrap();
    assert_eq!(a.to_string(), "2024-01-02T03:04:05+01:00");
    // chrono's own Display format
    let chrono_display = a.as_date_time().unwrap().to_string();
    assert_eq!(Variant::try_from_str(&chrono_display, &ty), Ok(a.clone()));
    // no offset -> UTC
    let utc = Variant::try_from_str("2024-01-02 03:04:05", &ty).unwrap();
    assert_eq!(utc.to_string(), "2024-01-02T03:04:05Z");
    assert_eq!(
        a.clone().convert_to(&VariantTy::Date),
        Ok(Variant::Date(
            chrono::NaiveDate::from_ymd_opt(2024, 1, 2).unwrap()
        ))
    );
    assert_eq!(
        Variant::try_from_str("02.01.2024", &VariantTy::Date),
        Variant::try_from_str("2024-01-02", &VariantTy::Date)
    );
    assert_eq!(
        Variant::try_from_str("12:30", &VariantTy::Time),
        Ok(Variant::Time(
            chrono::NaiveTime::from_hms_opt(12, 30, 0).unwrap()
        ))
    );
}

#[test]
fn tolerance() {
    let ty = VariantTy::Tolerance(NumberTy::F32);
    let sym = Variant::try_from_str("±5%", &ty).unwrap();
    assert_eq!(
        sym,
        Variant::Tolerance {
            min: Number::F32((-5.0).into()),
            min_unit: percent(),
            max: Number::F32(5.0.into()),
            max_unit: percent(),
        }
    );
    assert_eq!(sym.to_string(), "±5%");
    assert_eq!(Variant::try_from_str("+/-5%", &ty), Ok(sym.clone()));
    assert_eq!(Variant::try_from_str("5%", &ty), Ok(sym));
    let asym = Variant::try_from_str("-10%/+20%", &ty).unwrap();
    assert_eq!(asym.to_string(), "-10%/+20%");
    assert_eq!(Variant::try_from_str("+20%, -10%", &ty), Ok(asym));
    assert!(Variant::try_from_str("±5%", &VariantTy::Tolerance(NumberTy::U8)).is_err());
}

fn percent() -> UnitExpr {
    Unit::new(Prefix::Centi, BaseUnit::Ratio, 1).into()
}

#[test]
fn tolerance_with_units() {
    let ty = VariantTy::Tolerance(NumberTy::F64);
    let parse = |s: &str| Variant::try_from_str(s, &ty).unwrap();
    let tol = |min: f64, min_unit: UnitExpr, max: f64, max_unit: UnitExpr| Variant::Tolerance {
        min: Number::F64(min.into()),
        min_unit,
        max: Number::F64(max.into()),
        max_unit,
    };
    let pf: UnitExpr = Unit::new(Prefix::Pico, BaseUnit::Farad, 1).into();
    let v = parse("±0.25pF");
    assert_eq!(v, tol(-0.25, pf.clone(), 0.25, pf));
    assert_eq!(v.to_string(), "±0.25pF");
    assert_eq!(parse("± 0.25 pF"), v);

    let ppm_per_c = UnitExpr::from(Unit::new(Prefix::Micro, BaseUnit::Ratio, 1))
        / UnitExpr::from(BaseUnit::DegreeCelsius);
    let v = parse("±50ppm/°C");
    assert_eq!(v, tol(-50.0, ppm_per_c.clone(), 50.0, ppm_per_c));
    assert_eq!(v.to_string(), "±50ppm/°C");

    let v = parse("-20%, +80%");
    assert_eq!(v, tol(-20.0, percent(), 80.0, percent()));
    assert_eq!(v.to_string(), "-20%/+80%");
    assert_eq!(parse("-20%/+80%"), v);

    let v = parse("±1%");
    assert_eq!(v, tol(-1.0, percent(), 1.0, percent()));

    // a bare number is absolute, in the nominal's unit
    let v = parse("±0.1");
    assert_eq!(v, tol(-0.1, UnitExpr::one(), 0.1, UnitExpr::one()));
    assert_eq!(v.to_string(), "±0.1");
    assert!(Variant::try_from_str("±-5%", &ty).is_err());
    assert!(Variant::try_from_str("±5 furlongs/", &ty).is_err());
}

#[test]
fn si_range() {
    let ty = VariantTy::SIRange {
        number_ty: NumberTy::F32,
        start_inclusive: true,
        end_inclusive: true,
        unit: volt(),
    };
    let expected = Variant::SIRange {
        start: Number::F32(1.0.into()),
        start_inclusive: true,
        end: Number::F32(5.0.into()),
        end_inclusive: true,
        unit: volt(),
    };
    for s in [
        "[1, 5] V",
        "1V..5V",
        "1 .. 5",
        "[1000mV, 5V]",
        "1 to 5",
        "[1, 5]",
    ] {
        assert_eq!(Variant::try_from_str(s, &ty), Ok(expected.clone()), "{s}");
    }
    assert!(Variant::try_from_str("(1, 5] V", &ty).is_err());
    assert!(Variant::try_from_str("[5, 1] V", &ty).is_err());
}

#[test]
fn binary() {
    let v = Variant::Binary(vec![0x00, 0xAA, 0xBB, 0x0C]);
    assert_eq!(v.to_string(), "00 AA BB 0C");
    for s in ["00 AA BB 0C", "00aabb0c", "0x00AABB0C", "00:aa:bb:0c"] {
        assert_eq!(
            Variant::try_from_str(s, &VariantTy::Binary),
            Ok(v.clone()),
            "{s}"
        );
    }
    assert!(Variant::try_from_str("0", &VariantTy::Binary).is_err());
    assert!(Variant::try_from_str("zz", &VariantTy::Binary).is_err());
}

#[test]
fn list_and_map_literals() {
    let v = Variant::try_from_str(
        r#"[1, -2, 2.5, "a\"b", true, null, [], {"k": [1]}]"#,
        &VariantTy::List,
    )
    .unwrap();
    assert_eq!(
        v.to_string(),
        r#"[1, -2, 2.5, "a\"b", true, null, [], {"k": [1]}]"#
    );
    assert!(Variant::try_from_str("[1, 2", &VariantTy::List).is_err());
    assert!(Variant::try_from_str("{1}", &VariantTy::Map).is_err());
    assert!(Variant::try_from_str("[1] x", &VariantTy::List).is_err());
    let deep = "[".repeat(1000);
    assert!(Variant::try_from_str(deep, &VariantTy::List).is_err());
    assert_eq!(
        Variant::StrList(vec!["a".into(), "b".into()]).convert_to(&VariantTy::List),
        Ok(Variant::List(vec![Variant::str("a"), Variant::str("b")]))
    );
}

#[test]
fn si_compound_units() {
    // si_dynamic 0.3: unit expressions, ratios, derived units of one dimension (CONV-6)
    let ppm = Unit::new(Prefix::Micro, BaseUnit::Ratio, 1);
    let ppm_per_c = UnitExpr::from(ppm.clone()) / BaseUnit::DegreeCelsius;
    let ty = si_ty(NumberTy::F64, ppm_per_c.clone());
    let v = Variant::try_from_str("50 ppm/°C", &ty).unwrap();
    assert_eq!(v, Variant::si(F64(50.0), ppm_per_c.clone()));
    assert_eq!(v.to_string(), "50 ppm/°C");
    assert_eq!(Variant::try_from_str(v.to_string(), &ty), Ok(v));

    let ty = si_ty(NumberTy::F64, ppm.clone());
    assert_eq!(
        Variant::try_from_str("50ppm", &ty),
        Ok(Variant::si(F64(50.0), ppm.clone()))
    );
    assert_eq!(Variant::si(F64(50.0), ppm).num_base_f64(), Some(0.00005));

    let k_ohm = Unit::new(Prefix::Kilo, BaseUnit::Ohm, 1);
    let ty = si_ty(NumberTy::F64, k_ohm.clone());
    assert_eq!(
        Variant::try_from_str("4.7 V/mA", &ty),
        Ok(Variant::si(F64(4.7), k_ohm.clone()))
    );
    let v_per_a = UnitExpr::from(BaseUnit::Volt) / BaseUnit::Ampere;
    let v = Variant::si(F64(4700.0), v_per_a);
    assert_eq!(v.convert_to(&ty), Ok(Variant::si(F64(4.7), k_ohm)));

    let slew = UnitExpr::from(BaseUnit::Volt) / Unit::new(Prefix::Micro, BaseUnit::Second, 1);
    let ty = si_ty(NumberTy::F64, slew.clone());
    let v = Variant::try_from_str("10 V/us", &ty).unwrap();
    assert_eq!(v, Variant::si(F64(10.0), slew));
    assert_eq!(v.num_base_f64(), Some(1e7));

    // °C has an offset: no silent conversion to K
    let ty = si_ty(NumberTy::F64, BaseUnit::Kelvin);
    assert!(Variant::try_from_str("25 °C", &ty).is_err());
}

#[test]
fn si_mass_base_is_kg() {
    let g = Unit::base(BaseUnit::Gram);
    assert_eq!(Variant::si(F64(10.0), g.clone()).num_base_f64(), Some(0.01));
    let kg = Unit::new(Prefix::Kilo, BaseUnit::Gram, 1);
    assert_eq!(Variant::si(F64(2.5), kg.clone()).num_base_f64(), Some(2.5));
    assert_eq!(
        Variant::si(F64(2.5), kg).as_base_unit(BaseUnit::Gram),
        Ok(Number::F64(2500.0.into()))
    );
}
