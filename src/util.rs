use crate::Variant;
use si_dynamic::BaseUnit;

pub fn to_base_unit_f32_opt(v: &Option<Variant>, base_unit: BaseUnit) -> Option<f32> {
    let v = v
        .as_ref()
        .and_then(|c| c.as_base_unit(base_unit).ok())
        .and_then(|c| c.as_f32().ok());
    v
}
