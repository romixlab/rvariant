# Changelog

All notable changes to this project are documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- Compound units on si_dynamic 0.3: `50 ppm/°C`, `10 V/us` parse into SI values, `50ppm` parses (was "expected ppm, found ppm") (PARSE-4).
- SI conversion between units of one dimension whose scales are powers of ten: `4.7 V/mA` parses for `kΩ`, `V/A` converts to `Ω` (CONV-6).
- Tolerances with units: `±0.25pF`, `±50ppm/°C`, `-20%, +80%` (PARSE-11).

### Changed

- **Breaking:** on si_dynamic 0.3: `Variant::SI`, `SIRange`, `VariantTy::SI`, `SIRange` and `Error::UnitMismatch` take a `UnitExpr` in place of a `Unit`; `Variant::si` and `VariantTy::si` take `impl Into<UnitExpr>`, so a `Unit` or `BaseUnit` still works there. `BaseUnit::Kilogram` is gone: kg is `Unit::new(Prefix::Kilo, BaseUnit::Gram, 1)`.
- **Breaking (serde):** the SI unit serializes as a `UnitExpr`: `{"unit":{"Unit":{"prefix":"Kilo","base":"Ohm","exp":1}}}` (was `{"unit":{"prefix":"Kilo","base":"Ohm","exp":1}}`), compound units as `{"Div":[a, b]}`, `{"Mul":[a, b]}`, `{"Pow":[a, n]}`. Stored `Variant` and `VariantTy` JSON with SI units needs converting once (SER-2).
- **Breaking:** `Variant::Tolerance` has `min_unit` and `max_unit` (`UnitExpr`) in place of `min_percent` and `max_percent`: percent is `Unit::new(Prefix::Centi, BaseUnit::Ratio, 1)`, a bare number `UnitExpr::one()` (PARSE-11).
- **Breaking (serde):** a tolerance serializes as `{"Tolerance":{"min":{"F64":-5.0},"min_unit":{"Unit":{"prefix":"Centi","base":"Ratio","exp":1}},"max":{"F64":5.0},"max_unit":{"Unit":{"prefix":"Centi","base":"Ratio","exp":1}}}}` (was `"min_percent":true`, `"max_percent":true`); stored tolerances need converting once: `true` → that `%` unit, `false` → `{"Unit":{"prefix":"Unit","base":"Ratio","exp":1}}` (SER-2).
- `bounds_base_f64` of a percent tolerance gives the ratio: `±5%` → (-0.05, 0.05), was (-5, 5); an absolute one (`±0.25pF`) gives farads (PARSE-11, CONV-7).
- `num_base_f64` and `bounds_base_f64` go through the unit's scale: a value in g gives kg (as before for kg), `ppm` gives the ratio (50 ppm → 5e-5); °C stays °C (CONV-7).

## [0.4.0] - 2026-10-26

### Added

- `Variant::restore(ty)` and `VariantSeed`, a `DeserializeSeed` that loads a value and restores it against its type (SER-3).
- `Variant::num_base_f64()` and `Variant::bounds_base_f64()`: a value, or a range's, tolerance's or list's lowest and highest value, as `f64` in its unit without prefix (13.7 kΩ → 13700, °C stays °C) (CONV-7).
- Golden JSON tests for every `Variant` and `VariantTy` kind, `tests/serde.rs` (SER-2).

### Breaking changes

- `Enum` and `MultiSelectionEnum` values serialize only their selection: `{"Enum":{"selected":"Green"}}` instead of `{"Enum":{"name":"Color","selected":"Green","variants":[...]}}` (SER-3). A loaded value has an empty name and no variants until `Variant::restore(ty)` (or `VariantSeed(&ty)`) gives them back from its `VariantTy`. JSON stored by 0.3 still loads (the extra fields are ignored); restore it the same way. Positional formats (msgpack `to_vec`, bincode) lose the two fields too, so both ends of such a wire need 0.4 and the receiver restores enum values from their type. Version 0.4.0.

## [0.3.2] - 2026-10-22

### Added

- `just test --changed` runs only the workspace crates changed against main and their dependents (`tpm test changed`, nextest's `rdeps()`); plain `just test` runs everything, as `just test-all` does (tpm ops#22oct-2049).

## [0.3.1] - 2026-10-22

### Changed

- `just install` comment says why it is only a build check: rvariant is a library with no binary to put on PATH.

## [0.3.0] - 2026-10-15

### Added

- AGENTS.md and FEATURES.md (feature tracker with stable IDs).
- `Variant::try_from_str` supports every type: Date, Time, DateTime, Enum (case-insensitive,
  `Name::Variant`), MultiSelectionEnum, Binary, List, Map, SIRange, Tolerance and Money.
- Integer parsing handles signs, `0x` / `0o` / `0b` prefixes, `_` separators and integral
  floats (`5.0`, `1e3`).
- SI parsing:
  - respects `number_ty` and the unit prefix;
  - treats a bare number as a value in the column's unit;
  - accepts resistor notation (`4k7`, `4700R`) and `Vdc` / `Vac` / `Ohm` aliases;
  - handles grams correctly.
- Conversions: Number ↔ SI, SI ↔ SI (prefix rescaling), Number ↔ Bool (0/1), Number ↔ Instant,
  Number ↔ Money, Money ↔ Money (same currency), DateTime ↔ Date / Time, StrList ↔ List,
  Enum ↔ MultiSelectionEnum.
- `Variant::convert_lossy` and `Number::convert_lossy`.
- Accessors:
  - numeric: `as_i8` … `as_u128`, `as_f32`, `as_f64`, `as_number`;
  - other types: `as_date`, `as_time`, `as_date_time`, `as_instant`, `as_bytes`, `as_list`,
    `as_map`, `as_str_list`, `as_enum_selected`, `as_non_empty_string`.
- `From` impls for primitives, strings, byte vectors, chrono types, `Map` / `IndexMap`,
  `Vec<Variant>`, `Vec<String>`, `NanoSeconds` and `Option<T>` (`None` becomes `Empty`).
- `TryFrom<Variant>` and `TryFrom<&Variant>` for the same types.
- `Number::cmp_value` / `eq_value` and `Variant::sort_cmp` compare by value across number
  types, SI prefixes and Money precision.
- `Variant::ty()`, `Variant::default_of` (returns a value of the requested type), and
  constructor helpers on `Variant` and `VariantTy`.
- Regression and round-trip test suites.

### Fixed

- Negative numbers parsed as unsigned types no longer wrap (`"-1"` as `u32` gave 4294967295)
  or panic in debug builds.
- Float to integer conversion no longer truncates silently (`1.9` → `1`, `NaN` → `0`).
- Instant suffixes `ms`, `us` and `ns` failed to parse.
- `Empty` converted to `Str` gave `"Empty"`.
- SI: the unit wasn't checked (`"5Vdc"` was accepted for an Ampere column), an `F64` column
  parsed to `F32`, and a bare number was rejected.
- Money lost precision when displayed and panicked for precision > 9.
- `Map` failed to serialize to JSON.
- `Bool` didn't accept `yes` / `no` / `on` / `off`, and `as_bool` rejected numbers.
- `as_non_empty_str` returned `Unimplemented` for `Empty`.

### Breaking changes

- `Enum` / `MultiSelectionEnum` / `Money` use `Arc<str>` for names and currencies and
  `Arc<[String]>` for enum variants (previously `Arc<String>` / `Arc<Vec<String>>`).
  Construct them with `"name".into()` or the `Variant::enumeration` / `Variant::money` helpers.
- `Error` was rewritten with `thiserror`. It now implements `Display` and `std::error::Error`.
  Its variants are `Parse`, `CannotConvert`, `OutOfRange`, `Lossy`, `UnitMismatch`,
  `CurrencyMismatch`, `WrongEnumVariantName`, `Empty` and `Internal`, and the enum is
  `#[non_exhaustive]`. The old `Unimplemented`, `Verbatim` and `ParseIntError`-style variants
  are gone.
- `convert_to` is strict. It returns `OutOfRange` or `Lossy` instead of wrapping, truncating or
  going through a string round-trip. Use the new `convert_lossy` to round and saturate.
- Parsing is strict too. Money and Instant reject excess decimal digits, and SI values with
  the wrong unit are rejected.
- Changed `Display` output (every format parses back to the same value):
  - `Empty` displays as `""` instead of `"Empty"`.
  - `Enum` shows only the selected variant (`Green`), and `MultiSelectionEnum` shows a
    comma-separated list.
  - `Binary` shows uppercase space-separated hex (`00 AA FF`).
  - `List` / `Map` use JSON-like literals.
  - `DateTime` uses RFC 3339.
  - `Instant` uses the largest exact unit (`1.5 ms`).
  - `Money` shows the exact decimal value followed by the currency.
- `Map` is serialized as a sequence of `[key, value]` pairs, so non-string keys work in JSON.
  The binary encoding (e.g. bincode) is unchanged.
- Removed the `rkyv` feature, which didn't compile.
- `chrono/serde` and `indexmap/serde` are only enabled by the `serde` feature.
- Removed the `parse_int` and `num-traits` dependencies and added `thiserror`.
- Depends on si_dynamic 0.2 (was 0.1).
The `VariantTy` serde format is unchanged, so `VariantTy` values that were already stored
still deserialize.

## [0.2.0] - 2026-09-07

First release on crates.io. Earlier history is in git.
