// (c) Roel Kluin 2024 GPL v3
//! Single source of truth: repo-root src/language/verb_conjugate.rs.
//! Copied at build time into OUT_DIR (see build.rs) — do not edit here.

include!(concat!(env!("OUT_DIR"), "/verb_conjugate_generated.rs"));
