//! The falsification half of this crate: what Mandarin cannot get through `ranting`'s public
//! API. Mirrors the other five falsifiers' `tests/holes.rs` structure and naming.
//!
//! Each test asserts what the crate *actually* produces, not what Mandarin needs, and is named
//! after the numbered hole in this crate's README.md. They are pins: if a later change closes
//! one of these, the test fails and the hole gets struck from the README rather than quietly
//! rotting.

use ranting::say;
use ranting_zh::MandarinNoun;

// ------------------------- hole 1: the tense-marker pipeline is English-only --

#[test]
fn hole_1_a_tense_marker_always_composes_an_english_auxiliary() {
    // `MandarinNoun::inflect_verb_custom_with_context` substitutes the correct, invariant
    // Mandarin verb perfectly (proven by the bare-verb tests below) -- but its return value is
    // then unconditionally piped through `handle_tense_marker`'s English auxiliary composition,
    // which is plain string formatting, not a trait method call, and receives no signal at all
    // about which of `ranting`'s eleven tense markers fired. There is no override that can
    // suppress or replace the auxiliary, because there is no hook seam at that point to
    // override. `{?0 ...}` hides the noun so only the verb phrase renders, isolating the
    // auxiliary at sentence start.
    assert_eq!(say!("{?0 >eat}", MandarinNoun::mao()), "Will 吃");
    // Not marker-specific: every tense marker that implies an auxiliary hits the same wall,
    // regardless of which one. (`%`'s auxiliary selection also falls to its "any other subject"
    // default here, since `subjective()` returns a Chinese pronoun that can never match
    // `conjugate_auxiliary`'s closed English pronoun set -- a further, smaller symptom of the
    // same finding: the auxiliary-selection machinery is not just uncontrollable, its own
    // person/number selection silently stops working the moment the subject isn't English text.)
    assert_eq!(say!("{?0 %eat}", MandarinNoun::mao()), "Have 吃");
}

#[test]
fn hole_1_the_only_way_to_write_real_aspect_is_to_avoid_tense_markers_entirely() {
    // A bare, unmarked verb placeholder never reaches `handle_tense_marker` at all -- it goes
    // through `conjugate_verb` straight to the hook, so the substitution is clean.
    assert_eq!(say!("{?0 eat}.", MandarinNoun::mao()), "吃.");
    // Real Mandarin aspect marking (了/过/着) is reachable, but only by giving up tense markers
    // and writing the particle as literal trailing text on that same bare slot -- the same
    // "word choice is the caller's template" boundary `docs/EXTENSIBILITY.md` §2.12 already
    // names for other languages' particles, restated here for a third one.
    assert_eq!(say!("{?0 eat 了}.", MandarinNoun::mao()), "吃 了.");
}
