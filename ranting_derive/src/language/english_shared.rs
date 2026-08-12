// (c) Roel Kluin 2022 GPL v3
//! Single source of truth: repo-root src/language/english_shared.rs.
//! Copied at build time into OUT_DIR (see build.rs) — do not edit here.

include!(concat!(env!("OUT_DIR"), "/english_shared_generated.rs"));
