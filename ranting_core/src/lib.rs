// (c) Roel Kluin 2026 GPL v3
//! `ranting_core`: internal shared grammar engine for the `ranting` /
//! `ranting_derive` crate pair.
//!
//! This is **not** a stable public API — it exists purely so `ranting` and
//! `ranting_derive` can share grammar tables, the placeholder grammar, and
//! the verb conjugation engine without one crate generating a copy of the
//! other's source at build time (the pre-v1.2 `OUT_DIR`-copy mechanisms this
//! crate replaces are described in `CLAUDE.md`). Both consumers add it as an
//! ordinary path dependency — the `serde`/`serde_derive` pattern, where a
//! proc-macro crate and its companion runtime crate both depend on a shared
//! plain rlib.
//!
//! No item here should be considered covered by ranting's semver guarantees
//! just because `ranting` re-exports a few of them.

pub mod grammar;
pub mod placeholder;
pub mod verb_conjugate;
