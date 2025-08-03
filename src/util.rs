use crate::{Error, Variant};
use si_dynamic::BaseUnit;

pub fn to_base_unit_f32_opt(v: &Option<Variant>, base_unit: BaseUnit) -> Result<f32, Error> {
    let v = v
        .as_ref()
        .ok_or(Error::Verbatim("None".to_string()))
        .and_then(|c| c.as_base_unit(base_unit))
        .and_then(|c| c.as_f32());
    v
}
