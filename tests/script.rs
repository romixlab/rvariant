#![cfg(feature = "rhai")]

use chrono::NaiveDate;
use rvariant::script::Expr;
use rvariant::si_dynamic::{BaseUnit, Prefix, Unit};
use rvariant::*;

fn unit(prefix: Prefix, base: BaseUnit) -> Unit {
    Unit {
        prefix,
        base,
        exp: 1,
    }
}

fn kohm() -> Unit {
    unit(Prefix::Kilo, BaseUnit::Ohm)
}

fn ohm() -> Unit {
    unit(Prefix::Unit, BaseUnit::Ohm)
}

fn color(selected: &str) -> Variant {
    let variants: Vec<String> = vec!["Low".into(), "Medium".into(), "High".into()];
    Variant::enumeration("Level", selected, variants)
}

/// Row lookup for tests.
fn row(vars: &[(&str, Variant)]) -> impl Fn(&str) -> Option<Variant> {
    move |name| {
        vars.iter()
            .find(|(n, _)| *n == name)
            .map(|(_, v)| v.clone())
    }
}

fn eval(src: &str, vars: &[(&str, Variant)]) -> Result<Variant, Error> {
    Expr::compile(src)?.eval(row(vars))
}

#[test]
fn variables() {
    let e = Expr::compile("r.value * qty + r.base / qty + max(a, 1)").unwrap();
    assert_eq!(e.variables(), ["r", "qty", "a"]);
    assert!(Expr::compile("let x = 1; x").is_err());
    assert!(Expr::compile("loop {}").is_err());
}

#[test]
fn engine_is_shareable() {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<Expr>();
    assert_send_sync::<rhai::Engine>();
}

#[test]
fn plain_values() {
    let vars = [
        ("qty", Variant::u32(20)),
        ("name", Variant::str("R1")),
        ("ok", Variant::Bool(true)),
        ("empty", Variant::Empty),
    ];
    assert_eq!(eval("qty * 2 + 1", &vars), Ok(Variant::i64(41)));
    assert_eq!(eval("qty / 8.0", &vars), Ok(Variant::f64(2.5)));
    assert_eq!(eval("`${name}-x`", &vars), Ok(Variant::str("R1-x")));
    assert_eq!(
        eval("ok && name.starts_with(\"R\")", &vars),
        Ok(Variant::Bool(true))
    );
    assert_eq!(eval("empty == ()", &vars), Ok(Variant::Bool(true)));
    assert_eq!(eval("empty", &vars), Ok(Variant::Empty));
    assert_eq!(
        eval("if qty > 10 { \"many\" } else { \"few\" }", &vars),
        Ok(Variant::str("many"))
    );
    let e = Expr::compile("qty * 2").unwrap();
    assert_eq!(
        e.eval_as(row(&vars), &VariantTy::u16()),
        Ok(Variant::Number(Number::U16(40)))
    );
    let e = Expr::compile("qty * 10").unwrap();
    assert!(matches!(
        e.eval_as(row(&vars), &VariantTy::i8()),
        Err(Error::OutOfRange { .. })
    ));
}

#[test]
fn strict_integer_range() {
    let big = [("x", Variant::u64(u64::MAX))];
    assert!(matches!(eval("x", &big), Err(Error::OutOfRange { .. })));
}

#[test]
fn unknown_variable() {
    assert!(matches!(eval("nope + 1", &[]), Err(Error::Script(_))));
}

#[test]
fn si_arithmetic() {
    let vars = [
        ("r", Variant::si(4.7f32, kohm())),
        ("r2", Variant::si(300i32, ohm())),
        ("v", Variant::si(5i32, unit(Prefix::Unit, BaseUnit::Volt))),
    ];
    assert_eq!(eval("r * 2", &vars), Ok(Variant::si(9.4f32, kohm())));
    assert_eq!(eval("2 * r", &vars), Ok(Variant::si(9.4f32, kohm())));
    assert_eq!(eval("r + r2", &vars), Ok(Variant::si(5.0f32, kohm())));
    assert_eq!(eval("r2 + \"0.1k\"", &vars), Ok(Variant::si(400i32, ohm())));
    assert_eq!(eval("r2 / r2", &vars), Ok(Variant::f64(1.0)));
    assert_eq!(eval("r.value", &vars), Ok(Variant::f64(4.7f32 as f64)));
    assert_eq!(eval("r2.base", &vars), Ok(Variant::f64(300.0)));
    assert_eq!(eval("r.unit", &vars), Ok(Variant::str("kΩ")));
    assert_eq!(eval("r.kind", &vars), Ok(Variant::str("SI")));
    assert!(matches!(eval("r + v", &vars), Err(Error::Script(_))));
    assert!(eval("r * r", &vars).is_err());
    assert!(eval("r + 1", &vars).is_err());

    let e = Expr::compile("r + r2").unwrap();
    assert_eq!(
        e.eval_as(row(&vars), &VariantTy::si(NumberTy::U32, ohm())),
        Ok(Variant::si(5000u32, ohm()))
    );
}

#[test]
fn si_comparison() {
    let vars = [("r", Variant::si(4.7f32, kohm()))];
    assert_eq!(eval("r > \"1 kΩ\"", &vars), Ok(Variant::Bool(true)));
    assert_eq!(eval("r == \"4700 Ohm\"", &vars), Ok(Variant::Bool(true)));
    assert_eq!(eval("r < \"4k7\"", &vars), Ok(Variant::Bool(false)));
    assert_eq!(eval("\"10k\" > r", &vars), Ok(Variant::Bool(true)));
    assert!(eval("r > \"5 V\"", &vars).is_err());
    assert!(eval("r > 1000", &vars).is_err());
    assert_eq!(eval("r.base > 1000", &vars), Ok(Variant::Bool(true)));
}

#[test]
fn si_range_contains() {
    let range = Variant::SIRange {
        start: Number::F32(1.0.into()),
        start_inclusive: true,
        end: Number::F32(5.0.into()),
        end_inclusive: false,
        unit: unit(Prefix::Unit, BaseUnit::Volt),
    };
    let vars = [
        ("range", range),
        (
            "v",
            Variant::si(3300i32, unit(Prefix::Milli, BaseUnit::Volt)),
        ),
    ];
    assert_eq!(eval("v in range", &vars), Ok(Variant::Bool(true)));
    assert_eq!(eval("\"1V\" in range", &vars), Ok(Variant::Bool(true)));
    assert_eq!(eval("\"5V\" in range", &vars), Ok(Variant::Bool(false)));
    assert_eq!(eval("range.start.value", &vars), Ok(Variant::f64(1.0)));
}

#[test]
fn money() {
    let vars = [
        ("price", Variant::money("EUR", 2, 1234)),
        ("fee", Variant::money("EUR", 3, 500)),
        ("usd", Variant::money("USD", 2, 100)),
        ("qty", Variant::u32(3)),
    ];
    assert_eq!(
        eval("price * qty", &vars),
        Ok(Variant::money("EUR", 2, 3702))
    );
    assert_eq!(
        eval("price + fee", &vars),
        Ok(Variant::money("EUR", 3, 12840))
    );
    assert_eq!(
        eval("price + \"1 EUR\"", &vars),
        Ok(Variant::money("EUR", 2, 1334))
    );
    // 12.34 * 1.2 = 14.808 -> 14.81
    assert_eq!(
        eval("price * 1.2", &vars),
        Ok(Variant::money("EUR", 2, 1481))
    );
    // 12.34 / 3 = 4.1133 -> 4.11
    assert_eq!(eval("price / 3", &vars), Ok(Variant::money("EUR", 2, 411)));
    assert!(matches!(eval("price + usd", &vars), Err(Error::Script(_))));
    assert_eq!(eval("price > \"10 EUR\"", &vars), Ok(Variant::Bool(true)));
    assert_eq!(eval("price.amount", &vars), Ok(Variant::f64(12.34)));
    assert_eq!(eval("price.currency", &vars), Ok(Variant::str("EUR")));
    assert_eq!(
        eval("`total: ${price * qty}`", &vars),
        Ok(Variant::str("total: 37.02 EUR"))
    );
    assert_eq!(
        eval("\"total: \" + price", &vars),
        Ok(Variant::str("total: 12.34 EUR"))
    );
}

#[test]
fn enums() {
    let vars = [("level", color("Medium"))];
    assert_eq!(eval("level == \"medium\"", &vars), Ok(Variant::Bool(true)));
    assert_eq!(eval("level != \"High\"", &vars), Ok(Variant::Bool(true)));
    assert_eq!(eval("level > \"Low\"", &vars), Ok(Variant::Bool(true)));
    assert_eq!(eval("level < \"High\"", &vars), Ok(Variant::Bool(true)));
    assert_eq!(eval("level.selected", &vars), Ok(Variant::str("Medium")));
    assert!(matches!(
        eval("level == \"Huge\"", &vars),
        Err(Error::WrongEnumVariantName { .. }) | Err(Error::Script(_))
    ));

    let multi = Variant::MultiSelectionEnum {
        name: "Color".into(),
        selected: vec!["Red".into(), "Blue".into()],
        variants: vec!["Red".to_string(), "Green".into(), "Blue".into()].into(),
    };
    let vars = [("colors", multi)];
    assert_eq!(eval("\"red\" in colors", &vars), Ok(Variant::Bool(true)));
    assert_eq!(eval("\"Green\" in colors", &vars), Ok(Variant::Bool(false)));
}

#[test]
fn dates() {
    let vars = [(
        "d",
        Variant::Date(NaiveDate::from_ymd_opt(2024, 3, 15).unwrap()),
    )];
    assert_eq!(eval("d >= \"2024-01-01\"", &vars), Ok(Variant::Bool(true)));
    assert_eq!(
        eval("d.year * 100 + d.month", &vars),
        Ok(Variant::i64(202403))
    );
}

#[test]
fn lists_and_maps_roundtrip() {
    let m: Map = [("a", Variant::i64(1)), ("b", Variant::str("x"))]
        .into_iter()
        .collect();
    let vars = [
        ("tags", Variant::StrList(vec!["smd".into(), "0603".into()])),
        ("m", Variant::Map(m.clone())),
    ];
    assert_eq!(
        eval("tags.contains(\"0603\")", &vars),
        Ok(Variant::Bool(true))
    );
    assert_eq!(eval("m.a + 1", &vars), Ok(Variant::i64(2)));
    assert_eq!(eval("m", &vars), Ok(Variant::Map(m)));
    let e = Expr::compile("tags").unwrap();
    assert_eq!(
        e.eval_as(row(&vars), &VariantTy::StrList),
        Ok(vars[0].1.clone())
    );
}

#[test]
fn rich_values_pass_through() {
    let vars = [(
        "t",
        Variant::Tolerance {
            min: Number::I32(-5),
            min_percent: true,
            max: Number::I32(5),
            max_percent: true,
        },
    )];
    assert_eq!(eval("t", &vars), Ok(vars[0].1.clone()));
    assert_eq!(eval("t.max", &vars), Ok(Variant::i64(5)));
    assert_eq!(eval("t == \"±5%\"", &vars), Ok(Variant::Bool(true)));
}
