use crate::{Error, Variant};
use si_dynamic::BaseUnit;

/// Value in `base_unit` (without prefix) as f32, `None` results in [`Error::Empty`].
pub fn to_base_unit_f32_opt(v: &Option<Variant>, base_unit: BaseUnit) -> Result<f32, Error> {
    v.as_ref()
        .ok_or(Error::Empty)
        .and_then(|c| c.as_base_unit(base_unit))
        .and_then(|c| c.as_f32())
}
