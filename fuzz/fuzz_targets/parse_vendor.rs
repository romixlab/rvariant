//! `Variant::parse_vendor` on any text and any of the types vendor values are read as: it must not panic and must
//! stay fast (libFuzzer's `-timeout` catches a slow input). The first byte picks the type, the rest is the text.
#![no_main]

use libfuzzer_sys::fuzz_target;
use rvariant::si_dynamic::{BaseUnit, Prefix, Unit};
use rvariant::{NumberTy, Variant, VariantTy};

fn ty(selector: u8) -> VariantTy {
    let celsius = BaseUnit::DegreeCelsius;
    match selector % 12 {
        0 => VariantTy::si(NumberTy::F64, BaseUnit::Ohm),
        1 => VariantTy::si(NumberTy::F32, Unit::new(Prefix::Kilo, BaseUnit::Ohm, 1)),
        2 => VariantTy::si(NumberTy::I64, BaseUnit::Volt),
        3 => VariantTy::si(NumberTy::F64, BaseUnit::Farad),
        4 => VariantTy::si(NumberTy::F64, BaseUnit::Meter),
        5 => VariantTy::si(NumberTy::F64, celsius.clone()),
        6 => VariantTy::SIRange {
            number_ty: NumberTy::F64,
            start_inclusive: true,
            end_inclusive: true,
            unit: celsius.into(),
        },
        7 => VariantTy::Tolerance(NumberTy::F64),
        8 => VariantTy::Number(NumberTy::F64),
        9 => VariantTy::Number(NumberTy::U16),
        10 => VariantTy::enumeration("Dielectric", vec!["C0G".to_string(), "X7R".to_string()]),
        _ => VariantTy::Str,
    }
}

fuzz_target!(|data: &[u8]| {
    let Some((&selector, text)) = data.split_first() else {
        return;
    };
    let Ok(text) = std::str::from_utf8(text) else {
        return;
    };
    if let Ok(v) = Variant::parse_vendor(text, &ty(selector)) {
        // What it gives is an ordinary value: these must not panic either
        let _ = v.value.to_string();
        let _ = v.value.num_base_f64();
        let _ = v.value.bounds_base_f64();
        for c in &v.cond {
            let _ = c.num_base_f64();
        }
    }
});
