# rvariant features and roadmap

This file is the single source of truth for what rvariant does, what is broken and what is planned, for humans
and AI agents alike. [CHANGELOG.md](CHANGELOG.md) records what changed and when; this file records the current
state. The README documents the text formats and conversion rules in detail.

Last full review: 3 Oct 2026 (main `3b6db8d`, 0.3.0 unreleased; branch `v0.4.0` at `378a575` adds rhai). Built from
the code, the README, the changelog and the test suites; all 45 tests pass and clippy is clean with all features.

## How to use this file

- **Status** of each item:
  - ✅ done
  - 🚧 in progress or partially done (the note says what is missing)
  - 🐛 implemented, but with known bugs
  - ⬜ stub: types or API exist but do nothing yet
  - 📋 planned
  - 💡 idea, not committed to
  - ⛔ blocked (the note says on what)
  - 🔍 probably done or obsolete, needs a check before closing
- **IDs** (`PARSE-2`, `CONV-3`) are stable: never renumber or reuse one. New items take the next free number of their
  area. Use the ID in commit messages, CHANGELOG entries and code `TODO`s (`// TODO(CONV-5): ...`).
- Items are grouped by area. Each area lists what works first, then open items by priority.
- When finishing work, update the item in the same commit: mark it ✅, add a pointer (module, test) and move it up
  to the done items of its area. Don't delete done items. Items that turn out obsolete go to
  [Dropped and superseded](#dropped-and-superseded) with a one-line reason.
- A bug you find but don't fix gets an entry (🐛 on the feature, or a new item) with the input that triggers it.
- Small code-level gaps stay as `TODO` comments; only those that limit users or block a feature get an item.

## Types (`TY`)

- ✅ **TY-1 `Variant` kinds**: Empty, Bool, Str, StrList, Number (8 integer widths up to 128 bit, f32, f64), SI,
  SIRange, Enum, MultiSelectionEnum, Binary, List, Map, Date, Time, DateTime, Instant, Money, Tolerance.
- ✅ **TY-2 `VariantTy` descriptor**: one per kind, parameterized (number type, unit, range inclusivity, enum
  name and variants, currency and precision). `Variant::ty()`, `VariantTy::default_value`, `Variant::default_of`.
- ✅ **TY-3 Constructors and helpers**: `Variant::str`, `i32` … `f64`, `si`, `enumeration`, `money`;
  `VariantTy::si`, `enumeration`, `money`.
- ✅ **TY-4 Shared names**: enum names, variants and currencies are `Arc<str>` / `Arc<[String]>`, cheap to clone
  per cell.

## Parsing (`PARSE`)

- ✅ **PARSE-1 `Variant::try_from_str` for every type**, whitespace-trimmed, `Error::Empty` on empty input
  (`src/parse.rs`, README *Text formats*).
- ✅ **PARSE-2 Integers**: signs, `0x` / `0o` / `0b`, `_` separators, integral floats (`5.0`, `1e3`); negative
  input for unsigned types is rejected, not wrapped.
- ✅ **PARSE-3 Bool**: `true/false`, `yes/no`, `y/n`, `on/off`, `1/0`, `t/f`, `✓/✗`, case-insensitive.
- ✅ **PARSE-4 SI values**: respects the column's number type and unit prefix, bare number = column unit, resistor
  notation (`4k7`, `4700R`), `Vdc` / `Vac` / `Ohm` aliases, grams handled correctly (works around si_dynamic
  UNIT-6), wrong unit rejected.
- ✅ **PARSE-5 SIRange** (`[1, 5] V`, `1V..5V`, `1 to 5`), bracket inclusivity must match the type.
- ✅ **PARSE-6 Enum / MultiSelectionEnum**: case-insensitive, `Name::Variant`, separators `,` `;` `|`.
- ✅ **PARSE-7 Money**: `12.34 EUR`, `EUR 12.34`, `1,234.56`, decimal comma; excess decimals rejected.
- ✅ **PARSE-8 Dates and times**: ISO, `2024/01/02`, `02.01.2024`, RFC 3339 / 2822, no offset = UTC.
- ✅ **PARSE-9 Instant** with `s` / `ms` / `us` / `µs` / `ns`; **Binary** hex in several spellings; **List / Map**
  JSON-like literals; **Tolerance** `±5%`, `-10%/+20%`.
- 📋 **PARSE-10 Exponents in resistor notation**: inherits si_dynamic RES-2 (`1e3` as a resistance fails). Check
  once si_dynamic is fixed and add a regression test.

## Display (`DISP`)

- ✅ **DISP-1 Round-trip guarantee**: `Display` output parses back to the same value for the same type
  (`tests/roundtrip.rs`).
- ✅ **DISP-2 Formats**: `Empty` as `""`, enums by selected variant, Binary `00 AA FF`, RFC 3339, Instant in the
  largest exact unit, Money exact with currency.
- 💡 **DISP-3 Locale-aware display**: decimal comma and digit grouping for UI display, separate from the
  round-trip format.

## Conversion and comparison (`CONV`)

- ✅ **CONV-1 Strict `convert_to`**: no wrapping, truncation or silent rounding; `OutOfRange` / `Lossy` errors
  (`src/convert.rs`).
- ✅ **CONV-2 `convert_lossy`**: rounds and saturates.
- ✅ **CONV-3 Conversion matrix**: Number ↔ SI, SI ↔ SI with prefix rescaling, Number ↔ Bool, Number ↔ Instant,
  Number ↔ Money, Money ↔ Money (same currency), DateTime ↔ Date / Time, StrList ↔ List, Enum ↔
  MultiSelectionEnum.
- ✅ **CONV-4 Value comparison**: `Number::cmp_value` / `eq_value`, `Variant::sort_cmp` across number types, SI
  prefixes and Money precision. Structural `Eq` / `Ord` / `Hash` stay as they are (`I32(1) != I64(1)`).
- ✅ **CONV-5 Accessors and `From` / `TryFrom`**: `as_i8` … `as_u128`, `as_f32`, `as_f64`, `as_bool`, `as_date`,
  `as_instant`, `as_bytes`, `as_list`, `as_map`, `as_enum_selected`, `as_non_empty_string`, ...; `From` for
  primitives, strings, chrono types, `Map`, `Vec`, `Option<T>`.
- 💡 **CONV-6 SI conversion between derived units** (`V/A` → `Ω`): needs si_dynamic EXPR-2.

## Serialization (`SER`)

- ✅ **SER-1 serde** (feature `serde`): all types; `Map` as a sequence of `[key, value]` pairs so non-string keys
  work in JSON. The `VariantTy` serde format is unchanged since 0.2 (`tests/regressions.rs`
  `variant_ty_json_format_is_unchanged`).

## Scripting (`SCR`)

- 🚧 **SCR-1 rhai expressions** (feature `rhai`, `src/script.rs`): `Expr::compile`, `variables()` for dependency
  tracking, `eval` / `eval_as` / `eval_with`; units, currency and enum variants are preserved, string operands are
  parsed as the other operand's type. Implemented with 13 tests on branch `v0.4.0` (`378a575`), **not merged to
  main, not pushed, no CHANGELOG entry and no version bump yet**. For egui_tabular computed columns and filters.

## Project (`PRJ`)

- ✅ **PRJ-1 Published on crates.io**: 0.2.0. 0.3.0 (strict parsing, breaking) is finished on main but
  unreleased; egui_tabular already uses it by path.
- 📋 **PRJ-2 Release 0.3.0 / 0.4.0**: decide whether rhai (SCR-1) ships in the same release, then publish so
  egui_tabular and mx3 can depend on a crates.io version.
- 💡 **PRJ-3 Fuzzing** `try_from_str` and the round-trip guarantee (`cargo fuzz`), since it parses user and CSV
  input.

## Dropped and superseded

- ❌ **rkyv feature**: removed in 0.3.0, it didn't compile.
