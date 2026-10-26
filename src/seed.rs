//! Loading stored values together with their type.

use crate::{Variant, VariantTy};
use serde::Deserialize;
use serde::de::{DeserializeSeed, Deserializer, Error as _};

/// Deserializes a [`Variant`] and restores it against the type it was stored for
/// ([`Variant::restore`]), so `Enum` / `MultiSelectionEnum` values get their name and variants
/// back: `VariantSeed(&ty).deserialize(&mut deserializer)`.
#[derive(Clone, Copy, Debug)]
pub struct VariantSeed<'a>(pub &'a VariantTy);

impl<'de> DeserializeSeed<'de> for VariantSeed<'_> {
    type Value = Variant;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Variant, D::Error> {
        Variant::deserialize(deserializer)?
            .restore(self.0)
            .map_err(D::Error::custom)
    }
}
