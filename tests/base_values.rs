//! `num_base_f64` / `bounds_base_f64`: values in their unit without prefix, for the search columns of
//! mx3's parameter facts (CONV-7). Equality is exact on purpose: the shift is done in decimal.

use rvariant::si_dynamic::{BaseUnit, Prefix, Unit, UnitExpr};
use rvariant::{F64, Number, NumberTy, Variant, VariantTy};

fn unit(prefix: Prefix, base: BaseUnit, exp: i8) -> UnitExpr {
    Unit { prefix, base, exp }.into()
}

fn parse(s: &str, ty: &VariantTy) -> Variant {
    Variant::try_from_str(s, ty).unwrap()
}

fn si(number_ty: NumberTy, prefix: Prefix, base: BaseUnit) -> VariantTy {
    VariantTy::si(number_ty, unit(prefix, base, 1))
}

fn f64(x: f64) -> Number {
    Number::F64(F64(x))
}

#[test]
fn si_values_drop_the_prefix() {
    let k_ohm = si(NumberTy::F64, Prefix::Kilo, BaseUnit::Ohm);
    assert_eq!(parse("13.7 kOhms", &k_ohm).num_base_f64(), Some(13700.0));
    assert_eq!(parse("13k7", &k_ohm).num_base_f64(), Some(13700.0));
    assert_eq!(parse("16200.00 Ohm", &k_ohm).num_base_f64(), Some(16200.0));
    let u_farad = si(NumberTy::F64, Prefix::Micro, BaseUnit::Farad);
    assert_eq!(parse("4.7µF", &u_farad).num_base_f64(), Some(4.7e-6));
    assert_eq!(parse("100nF", &u_farad).num_base_f64(), Some(1e-7));
    // integer and f32 columns: no f32 noise (0.1, not 0.10000000149)
    let k_ohm_u32 = si(NumberTy::U32, Prefix::Kilo, BaseUnit::Ohm);
    assert_eq!(parse("4", &k_ohm_u32).num_base_f64(), Some(4000.0));
    let ohm_f32 = si(NumberTy::F32, Prefix::Unit, BaseUnit::Ohm);
    assert_eq!(parse("0.1", &ohm_f32).num_base_f64(), Some(0.1));
    // the prefix is raised to the unit's exponent: 100 mm² is 1e-4 m²
    let area = Variant::si(100u32, unit(Prefix::Milli, BaseUnit::Meter, 2));
    assert_eq!(area.num_base_f64(), Some(1e-4));
    // a scalar has no bounds
    assert_eq!(parse("13k7", &k_ohm).bounds_base_f64(), None);
    assert_eq!(
        Variant::si(f64(f64::NAN), unit(Prefix::Kilo, BaseUnit::Ohm, 1)).num_base_f64(),
        None
    );
}

#[test]
fn celsius_stays_celsius() {
    let deg_c = si(NumberTy::F64, Prefix::Unit, BaseUnit::DegreeCelsius);
    assert_eq!(parse("-40", &deg_c).num_base_f64(), Some(-40.0));
    let milli_deg_c = Variant::si(500u32, unit(Prefix::Milli, BaseUnit::DegreeCelsius, 1));
    assert_eq!(milli_deg_c.num_base_f64(), Some(0.5));
}

#[test]
fn si_ranges_give_bounds() {
    let range = VariantTy::SIRange {
        number_ty: NumberTy::F64,
        start_inclusive: true,
        end_inclusive: true,
        unit: unit(Prefix::Unit, BaseUnit::DegreeCelsius, 1),
    };
    let v = parse("[-55, 125]", &range);
    assert_eq!(v.bounds_base_f64(), Some((-55.0, 125.0)));
    assert_eq!(v.num_base_f64(), None);

    let v = Variant::SIRange {
        start: f64(1.0),
        start_inclusive: false,
        end: f64(2.5),
        end_inclusive: true,
        unit: unit(Prefix::Kilo, BaseUnit::Hertz, 1),
    };
    assert_eq!(v.bounds_base_f64(), Some((1000.0, 2500.0)));
}

#[test]
fn tolerances_give_bounds() {
    let ty = VariantTy::Tolerance(NumberTy::F64);
    let v = parse("±5%", &ty);
    assert_eq!(v.bounds_base_f64(), Some((-0.05, 0.05)));
    assert_eq!(v.num_base_f64(), None);
    assert_eq!(parse("-10%/+20%", &ty).bounds_base_f64(), Some((-0.1, 0.2)));
    assert_eq!(
        parse("±50ppm/°C", &ty).bounds_base_f64(),
        Some((-5e-5, 5e-5))
    );
    assert_eq!(
        parse("±0.25pF", &ty).bounds_base_f64(),
        Some((-2.5e-13, 2.5e-13))
    );
    // percent on one side and ppm on the other: one dimension
    assert_eq!(
        parse("-50ppm/+1%", &ty).bounds_base_f64(),
        Some((-5e-5, 0.01))
    );
    assert_eq!(parse("±0.25", &ty).bounds_base_f64(), Some((-0.25, 0.25)));
    // percent on one side and a bare (absolute) number on the other has no common unit
    assert_eq!(parse("-1%/+0.25", &ty).bounds_base_f64(), None);
    // nor have pF and %
    assert_eq!(parse("-1%/+0.25pF", &ty).bounds_base_f64(), None);
}

#[test]
fn lists_span_their_elements() {
    let u_henry = unit(Prefix::Micro, BaseUnit::Henry, 1);
    let l = |x: f64| Variant::si(x, u_henry.clone());
    let two = Variant::List(vec![l(7.8), l(1.6)]);
    assert_eq!(two.bounds_base_f64(), Some((1.6e-6, 7.8e-6)));
    assert_eq!(two.num_base_f64(), None);

    let one = Variant::List(vec![l(7.8)]);
    assert_eq!(one.num_base_f64(), Some(7.8e-6));
    assert_eq!(one.bounds_base_f64(), Some((7.8e-6, 7.8e-6)));

    let range = Variant::SIRange {
        start: f64(0.5),
        start_inclusive: true,
        end: f64(10.0),
        end_inclusive: true,
        unit: u_henry.clone(),
    };
    let mixed = Variant::List(vec![l(7.8), range]);
    assert_eq!(mixed.bounds_base_f64(), Some((0.5e-6, 10e-6)));

    assert_eq!(Variant::List(vec![]).bounds_base_f64(), None);
    assert_eq!(
        Variant::List(vec![l(7.8), Variant::str("x")]).bounds_base_f64(),
        None
    );
}

#[test]
fn other_kinds() {
    assert_eq!(Variant::f64(2.5).num_base_f64(), Some(2.5));
    assert_eq!(Variant::u32(8).num_base_f64(), Some(8.0));
    assert_eq!(Variant::f32(0.1).num_base_f64(), Some(0.1));
    assert_eq!(Variant::str("13k7").num_base_f64(), None);
    assert_eq!(Variant::Empty.bounds_base_f64(), None);
    let colors = VariantTy::enumeration("Color", vec!["Red".to_string()]);
    assert_eq!(parse("Red", &colors).num_base_f64(), None);
}
