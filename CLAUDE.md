# CLAUDE.md

See @README.md for project overview and API documentation on docs.rs.

## Commands

```bash
cargo test                                  # Run all tests (integration + doctests)
cargo test --test main                      # Run specific integration test
cargo test --doc                            # Run doctests only
cargo test --features debug                 # Show compile-time placeholder transforms
cargo clippy --fix
cargo fmt
```

## Architecture: Two Crates + Shared Code

**`ranting`** (main lib): Exports `Ranting` trait, `Noun` struct, `say!()` macro.  
**`ranting_derive`** (proc-macro): Implements `say!()`, `ack!()`, `nay!()`, `ask!()` macros.

**Key constraint**: Both crates have `src/language/english_shared.rs` (duplicated, not shared). This avoids circular dependencies but means inflection rules must be kept in sync. When editing grammar logic, update both copies.

**Macro flow**: Derive crate parses placeholders at compile-time, generates `format!()` calls. Runtime: `handle_placeholder()` applies final inflections via `Ranting` trait.

## Non-obvious behaviors

- **Positional arguments only** — `say!("{=x}", x)` works, named args do not yet.
- **Empty placeholders don't work** — `{}` is skipped; must name the variable.
- **Shared inflection code**: `english_shared.rs` exists in both `src/` and `ranting_derive/src/`. Changes to placeholder regex or enums must sync both.
- **Doctests in proc-macro crate**: `ranting_derive/src/lib.rs` can't run proc-macro examples easily; test in `ranting/src/lib.rs` instead.
- **Integration tests only**: Unit tests are sparse by design; test via macros in `tests/ranting/`.

## When adding grammar rules

1. Add logic to both `src/language/english.rs` and `ranting_derive/src/language/english_shared.rs`
2. Test with `cargo test --features debug` to see placeholder transforms
3. Add doctests or integration tests demonstrating the new inflection
