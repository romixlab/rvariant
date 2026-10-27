# List all recipes
default:
    @just --list

# The tests. `just test`: all of them (test-all), the run of the `tpm land` gate. `just test --changed`: only the
# workspace crates changed against main and the crates that depend on them (tpm test changed), what a session runs
# before tpm work ready.
test *flags:
    if [ "{{flags}}" = "--changed" ]; then tpm test changed -- cargo nextest run --workspace; else {{just_executable()}} test-all {{flags}}; fi

# Tests (nextest for the unit and integration tests, cargo test for doctests)
test-all:
    cargo nextest run --all-features
    cargo test --all-features --doc
    cargo nextest run --no-default-features

# Clippy (no warnings) and a format check
lint:
    cargo clippy --all-features --all-targets -- -D warnings
    cargo fmt --check

# Fuzz Variant::parse_vendor for SECS seconds (cargo-fuzz on nightly), seeded from tests/corpus/vendor.toml
fuzz secs="300":
    cd fuzz && cargo +nightly fuzz run parse_vendor -- -max_total_time={{secs}} -timeout=2 -max_len=600

# rvariant is a library with no binary, so there is nothing to put on PATH: install is a build check in the deploy profile
install:
    cargo build --profile deploy --all-features

# Same as install: bring this PC up to date with main
deploy: install
