use crate::Variant;
use indexmap::IndexMap;
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};

/// Insertion-ordered map. Equality, ordering and hashing take the order of entries into account.
///
/// With `serde` it is serialized as a sequence of `[key, value]` pairs, so that non-string keys
/// work with formats like JSON. For non self-describing formats (bincode, postcard) the encoding
/// is identical to a native map.
#[derive(Clone, Debug, Default)]
pub struct Map(pub IndexMap<Variant, Variant>);

impl Map {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Eq for Map {}
impl PartialEq for Map {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}
impl PartialOrd for Map {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Map {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.iter().cmp(other.0.iter())
    }
}
impl Hash for Map {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.len().hash(state);
        self.0.iter().for_each(|x| x.hash(state));
    }
}

impl From<IndexMap<Variant, Variant>> for Map {
    fn from(m: IndexMap<Variant, Variant>) -> Self {
        Map(m)
    }
}

impl<K: Into<Variant>, V: Into<Variant>> FromIterator<(K, V)> for Map {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Map(iter
            .into_iter()
            .map(|(k, v)| (k.into(), v.into()))
            .collect())
    }
}

#[cfg(feature = "serde")]
mod serde_impl {
    use super::Map;
    use crate::Variant;
    use indexmap::IndexMap;
    use serde::de::{MapAccess, SeqAccess, Visitor};
    use serde::ser::SerializeSeq;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use std::fmt::Formatter;

    impl Serialize for Map {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            let mut seq = serializer.serialize_seq(Some(self.0.len()))?;
            for entry in &self.0 {
                seq.serialize_element(&entry)?;
            }
            seq.end()
        }
    }

    impl<'de> Deserialize<'de> for Map {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            struct MapVisitor;
            impl<'de> Visitor<'de> for MapVisitor {
                type Value = Map;

                fn expecting(&self, f: &mut Formatter) -> std::fmt::Result {
                    f.write_str("a sequence of [key, value] pairs or a map")
                }

                fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Map, A::Error> {
                    let mut m = IndexMap::with_capacity(seq.size_hint().unwrap_or(0));
                    while let Some((k, v)) = seq.next_element::<(Variant, Variant)>()? {
                        m.insert(k, v);
                    }
                    Ok(Map(m))
                }

                fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Map, A::Error> {
                    let mut m = IndexMap::with_capacity(map.size_hint().unwrap_or(0));
                    while let Some((k, v)) = map.next_entry::<Variant, Variant>()? {
                        m.insert(k, v);
                    }
                    Ok(Map(m))
                }
            }
            deserializer.deserialize_seq(MapVisitor)
        }
    }
}
