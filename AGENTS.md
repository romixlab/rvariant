# Working on rvariant

Guidance for AI agents and contributors. Read this before changing code.

rvariant is a dynamically typed value for Rust, inspired by Qt's `QVariant`: a `Variant` value plus a
parameterized `VariantTy` type descriptor (SI unit, enum variants, currency, ...). It is the cell type of
egui_tabular and the parameter type of mx3 (component parameters, CSV import, BOM values), so parsing, display and
conversion rules are a contract that real stored data depends on. Published on crates.io.

## FEATURES.md is the source of truth

[FEATURES.md](FEATURES.md) lists every feature with its status, every known bug, and what is planned, with stable
IDs per area (`PARSE-2`, `CONV-3`, ...).

- **Read the relevant area before starting.** The guarantees (round-trip, strict conversion) are listed there and
  in the README; a change must keep them or say loudly that it breaks them.
- **Name IDs with a short slug when talking to the user** (answers, plans, summaries, tables):
  `SCR-1 rhai-expressions`, never a bare `SCR-1`. The slug is 2-4 kebab-case words from the item's title. Commit
  messages, CHANGELOG and code `TODO`s keep the bare ID.
- **Update it in the same commit** as the code: mark items ✅/🚧/🐛 with a pointer to the code, add bugs you find
  but don't fix (next free ID of the area), move obsolete items to *Dropped and superseded*. Never renumber or
  reuse IDs.
- Don't track status anywhere else (README checklists, TODO files). Code `TODO`s that matter reference an ID:
  `// TODO(CONV-5): ...`.

## CHANGELOG.md records every change

[CHANGELOG.md](CHANGELOG.md) is the history, FEATURES.md the current state; keep both.

- Every change a user of the crate would notice gets an entry under `## [Unreleased]` in the same commit:
  `### Breaking changes`, `### Added`, `### Changed`, `### Fixed`, `### Removed`. Short, with the feature ID in
  parentheses.
- Always record: a changed `Display` format, input that now parses to a different value or is now rejected, a
  changed serde format (stored `Variant` / `VariantTy` values in mx3 and egui_tabular files must still load, or the
  entry says how to migrate), and changed cargo features.
- Pure refactors and typo fixes don't need an entry.
- On a release, rename `[Unreleased]` to the version and date and start a new empty `[Unreleased]`.

The tpm repo's `/sync-repos` reads this file to log progress, so a missing entry means work nobody sees.

## Layout

One crate, edition 2024.

- `src/lib.rs` — `Variant`, `VariantTy`, `NanoSeconds`, constructors, `ty()`, `default_of`.
- `src/parse.rs` — `Variant::try_from_str` for every type; `src/literal.rs` — JSON-like `List` / `Map` literals.
- `src/display.rs` — `Display`, always parseable back by `try_from_str`.
- `src/convert.rs` — `convert_to` (strict) and `convert_lossy`; accessors and `From` / `TryFrom` impls.
- `src/number.rs` — `Number`, `NumberTy`, value comparison (`cmp_value`, `eq_value`).
- `src/si.rs` — SI values on top of si_dynamic (prefix rescaling, resistor notation, grams).
- `src/decimal.rs` — exact decimal handling for `Money`. `src/map.rs` — `Map` and its serde format.
- `src/script.rs` — rhai expressions over `Variant` (feature `rhai`).
- `tests/regressions.rs` (one test per fixed bug), `tests/roundtrip.rs` (display → parse), `tests/script.rs`.

si_dynamic (`../si_dynamic`, taken by path) does the unit parsing. Fix unit bugs there, with its own FEATURES.md
and CHANGELOG.md, rather than working around them here. egui_tabular and mx3 depend on this crate: after an API or
behaviour change, build them and say what they need to follow.

## Commands

```sh
cargo build --all-features
cargo clippy --all-features --all-targets -- -D warnings
cargo fmt
cargo test --all-features
cargo test --no-default-features      # the crate must also build without serde / rhai
```

Before declaring a change done: build, clippy without warnings, fmt, tests with and without features.

## Code conventions

- Parsing and conversion never lose information silently: `convert_to` returns `OutOfRange` / `Lossy` instead of
  wrapping, truncating or rounding; only `convert_lossy` rounds and saturates.
- `Display` output parses back to the same value for the same type. A new type or format comes with a round-trip
  test.
- No panics on user data: no `unwrap`/`expect`/`as` casts that can wrap on input-derived values.
- `Error` is `#[non_exhaustive]` and built with `thiserror`; add variants rather than stuffing text into
  `Internal`.

## Tests

- A bug fix adds a test to `tests/regressions.rs` named after the behaviour, failing before the fix.
- New formats or types: cases in `tests/roundtrip.rs`.
- Unit tests next to the code for small helpers.

## Commits

Conventional Commits: `feat: ...`, `fix: ...`, `feat!: ...` for breaking changes, `refactor: ...`, `build: ...`.
Short imperative summary, blank line, body with what and why; reference feature IDs
(`fix: reject excess Money decimals (PARSE-7)`).

Never commit on your own initiative. When a change is done, update FEATURES.md and CHANGELOG.md, then show the
proposed commit message and the files to stage, and ask. Approval covers that one commit only. Never push or
publish; the user's tooling does that.

## Versions

Every commit with real work bumps the version in the same commit (manifest + CHANGELOG entry), so any build
traces back to a commit:
- Patch for fixes and small changes, minor for features or anything breaking before 1.0, major only when the
  owner says so. In a workspace, only the crates that changed.
- Docs-only, CI-only and no-behaviour-change refactors skip it; a burst of follow-up fixes shares one bump.
- CLIs print version, git SHA and build time in `--version`, e.g. `tool 0.4.2 (a1b2c3d-dirty, built 3 Oct 2026
  18:20)`: a small `build.rs` without extra crates (`git rev-parse --short HEAD`, `-dirty` when
  `git status --porcelain` isn't empty, `rerun-if-changed` on `.git/HEAD` and `.git/index`, `unknown` without
  git). Firmware reports the same through `fw_info`. When touching a CLI that lacks it, add it.
