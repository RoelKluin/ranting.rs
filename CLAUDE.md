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

**Key constraint**: `src/language/english_shared.rs` (repo root) is the single canonical source for grammar code shared by both crates. `ranting_derive/src/language/english_shared.rs` is a thin `include!(concat!(env!("OUT_DIR"), "/english_shared_generated.rs"))` wrapper; `ranting_derive/build.rs` copies the canonical file into `OUT_DIR` at build time (dev builds read `../src/language/english_shared.rs` directly; packaged builds read the dereferenced `ranting_derive/data/english_shared_source.rs` symlink — same fallback pattern as the verb-table codegen below). **Only ever edit the repo-root copy** — the derive crate's copy is generated and must not be hand-edited. Because one file compiles under both `ranting`'s `strum 0.27` and `ranting_derive`'s `strum 0.24`, any new derive or `#[strum(...)]` attribute added there must stay valid across both major versions.

**Verb table codegen** (v1.0): The `IRREGULAR_PAST` table (118 entries, base→past) is generated at build time from `data/irregular_verbs.txt` (single source of truth). Two build.rs scripts emit crate-specific tables to `$OUT_DIR/irregular_verbs_generated.rs` and include them via `include!()`. The `ranting_derive` crate gets both `IRREGULAR_PAST` and `IRREGULAR_PAST_PARTICIPLE` tables; the `ranting` crate gets only `IRREGULAR_PAST` (used by `detect_tense`, which doesn't need participles). A symlink at `ranting_derive/data/irregular_verbs.txt` → `../../data/irregular_verbs.txt` enables independent packaging while maintaining single-source truth. **Do not manually edit the verb tables in `src/` or `ranting_derive/src/language/verb.rs`** — regenerate by editing `data/irregular_verbs.txt` and rebuilding.

**Macro flow**: Derive crate parses placeholders at compile-time, generates `format!()` calls. Runtime: `handle_placeholder()` applies final inflections via `Ranting` trait.

## Non-obvious behaviors

- **Positional arguments only** — `say!("{=x}", x)` works, named args do not yet.
- **Empty placeholders don't work** — `{}` is skipped; must name the variable.
- **Shared inflection code**: `english_shared.rs` is generated for `ranting_derive` from the repo-root `src/language/english_shared.rs` at build time (see "Key constraint" above) — edit only the repo-root copy; no manual sync needed.
- **Verb table codegen**: `data/irregular_verbs.txt` is the single source of truth. Changing it triggers rebuilds via `cargo:rerun-if-changed`. Do not edit `IRREGULAR_PAST` or `IRREGULAR_PAST_PARTICIPLE` in the Rust source files — they are generated.
- **Doctests in proc-macro crate**: `ranting_derive/src/lib.rs` can't run proc-macro examples easily; test in `ranting/src/lib.rs` instead.
- **Integration tests only**: Unit tests are sparse by design; test via macros in `tests/ranting/`.

## When adding grammar rules

1. **For new verb conjugation tables**: Edit `data/irregular_verbs.txt` (one line per verb: `base|past|participle`). The build scripts will regenerate the Rust tables at build time.
2. **For runtime inflection logic** (e.g., `detect_tense`): Edit `src/language/english.rs`. Test with `cargo test --features debug` to see placeholder transforms.
3. **For compile-time inflection logic** (e.g., `handle_param` in the derive macro): Edit `ranting_derive/src/lib.rs`. Both crates' code changes are independent after codegen separation.
4. Add doctests or integration tests demonstrating the new inflection in `tests/ranting/`.
