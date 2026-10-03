# Changelog

All notable changes to this project are documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- AGENTS.md and FEATURES.md (feature tracker with stable IDs).

## [0.3.0] - Unreleased

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

The `VariantTy` serde format is unchanged, so `VariantTy` values that were already stored
still deserialize.

### Added

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

## [0.2.0] - 2026-09-07

First release on crates.io. Earlier history is in git.
