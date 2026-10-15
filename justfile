# List all recipes
default:
    @just --list

# Tests (nextest for the unit and integration tests, cargo test for doctests)
test:
    cargo nextest run --all-features
    cargo test --all-features --doc
    cargo nextest run --no-default-features

# Clippy (no warnings) and a format check
lint:
    cargo clippy --all-features --all-targets -- -D warnings
    cargo fmt --check

# A library has nothing to install: check that it builds in the deploy profile
install:
    cargo build --profile deploy --all-features

# Same as install: bring this PC up to date with main
deploy: install
