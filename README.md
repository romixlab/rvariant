# rvariant

Qt `QVariant` inspired dynamically typed value for Rust: a `Variant` value plus a parameterized
`VariantTy` type descriptor (SI unit, enum variants, currency, ...). Used for table cells,
CSV import and storing heterogeneous parameters.

```rust
use rvariant::{Variant, VariantTy, NumberTy, si_dynamic::{Unit, BaseUnit}};

let ohm = VariantTy::si(NumberTy::F32, Unit::base(BaseUnit::Ohm));
let r = Variant::try_from_str("4k7", &ohm)?;      // 4700 Ω
assert_eq!(r.to_string(), "4700 Ω");               // Display is parseable back
assert_eq!(Variant::try_from_str("-1", &VariantTy::u32()).is_err(), true);
let n: u16 = Variant::str("0x10").try_into()?;     // 16
```

## Text formats

`Display` output is always accepted by `Variant::try_from_str` for the same type.
Surrounding whitespace is ignored (except for `Str`); an empty input gives `Error::Empty`.

| Type | Display | Also accepted |
|---|---|---|
| Empty | `` | |
| Bool | `true` | `yes/no`, `y/n`, `on/off`, `1/0`, `t/f`, `✓/✗` (case-insensitive) |
| StrList | `a, ⛶, b` (⛶ = empty string) | separators `,` `;` tab |
| Number (ints) | `-42` | `+42`, `0x2A`, `0o52`, `0b101010`, `1_000`, `5.0`, `1e3` (if integral) |
| Number (floats) | `0.1` | `1e-3`, `inf`, `NaN`, `1_000.5` |
| SI | `4.7 kΩ` | `4.7k`, `4k7`, `4.7 kOhm` (resistance), `5mV`, `12 Vdc`, bare number = column unit |
| SIRange | `[1, 5] V` | `1V..5V`, `1 to 5`, `[1000mV, 5V]`; bracket inclusivity must match the type |
| Enum | `Green` | case-insensitive, `Color::Green` |
| MultiSelectionEnum | `Red, Blue` | separators `,` `;` `\|` |
| Binary | `00 AA FF` | `00aaff`, `0x00AAFF`, `00:aa:ff` |
| List / Map | `[1, "a", null]`, `{"k": 1.5}` | JSON-like literals; ints -> `I64`, floats -> `F64` |
| Date | `2024-01-02` | `2024/01/02`, `02.01.2024` |
| Time | `13:04:05.123` | `13:04` |
| DateTime | `2024-01-02T03:04:05+01:00` (RFC 3339) | chrono `Display`, RFC 2822, no offset = UTC, date only = midnight UTC |
| Instant | `1.5 ms` | `1500000`, `1.5ms`, `s` / `ms` / `us` / `µs` / `ns` |
| Money | `12.34 EUR` | `EUR 12.34`, `1,234.56`, `12,5` (decimal comma), `12` |
| Tolerance | `±5%`, `-10%/+20%` | `+/-5%`, `5%`, `+20%, -10%` |

## Conversions

`convert_to` is strict and never silently loses information:

* integer -> integer must be in range (no wrapping);
* float -> integer must be finite and integral;
* anything -> float rounds to nearest, but must not overflow to infinity;
* Money / Instant parsing rejects excess decimal digits;
* SI values are rescaled between compatible units (`mV` <-> `kV`), mismatched units are an error.

`convert_lossy` rounds and saturates instead. See `Variant::convert_to` docs for the full matrix.

`Number`'s `Eq` / `Ord` / `Hash` are structural (`I32(1) != I64(1)`); use `Number::cmp_value` or
`Variant::sort_cmp` to compare by value (also handles SI prefixes and Money precision).

## SI values

`Variant::SI { value, unit }` stores `value` expressed in `unit`, including its prefix:
`SI { value: 4.7, unit: kΩ }` is 4700 Ω. `as_base_unit` returns the value without prefix.

## Features

* `serde` - Serialize / Deserialize for all types. `Map` is serialized as a sequence of
  `[key, value]` pairs so that non-string keys work in JSON.
* `rhai` - [rhai](https://rhai.rs) expressions over `Variant` values (`rvariant::script`), e.g. for
  computed columns and filters. Units, currency and enum variants are preserved, string operands
  are parsed as the other operand's type:

  ```rust
  let e = Expr::compile("r * qty > \"10k\" && price < \"5 EUR\"")?;
  e.variables();                     // ["r", "qty", "price"] - for dependency tracking
  e.eval(|name| row.get(name))?;     // Variant::Bool
  ```

