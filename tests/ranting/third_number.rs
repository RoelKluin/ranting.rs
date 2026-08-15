//! ROADMAP.md Phase 7 item 11: `Ranting::inflect` carries the placeholder's own numeral, so a
//! language with a third morphological number can render it *on the noun itself*.
//!
//! This is the one path six green gates did not cover before item 11 landed
//! (`docs/architecture-review-2026-08-14.md` §4.7): both falsifier crates hand-write `inflect`,
//! and every main-crate fixture was English, so "the counted noun gets the plural no matter what
//! the numeral said" compiled, passed and shipped. The Arabic spike found it by running the code
//! (`docs/superpowers/specs/2026-08-14-arabic-falsification-spike.md` §1). The fixture below is
//! that spike's `ArNoun`, minus the parts that were only there to probe: كِتاب has a singular, a
//! **dual** (`kitābān`, exactly two) and a plural (`kutub`, three or more), which English cannot
//! distinguish and `to_plural: bool` alone cannot express.

use ranting::*;
use std::fmt;

/// A noun with three number forms. `count` is the only signal that can pick the middle one.
#[derive(Clone, Copy)]
struct ArNoun;

impl fmt::Display for ArNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "kitab")
    }
}

impl Ranting for ArNoun {
    fn name(&self, uc: bool) -> String {
        capitalize_if("kitab", uc)
    }

    fn subjective(&self) -> &str {
        "it"
    }

    fn is_plural(&self) -> bool {
        false
    }

    fn inflect(
        &self,
        to_plural: bool,
        uc: bool,
        _case: GrammaticalCase,
        count: Option<PlaceholderCount>,
    ) -> String {
        // The dual is asked for by the count and by nothing else. Note that it is *not* keyed on
        // `to_plural`: `{$n kitab}` with `n = 2` is plural agreement in English terms, so a fork
        // that read only `to_plural` would render `kutub` here, which is the defect item 11 fixed.
        if count.map(|c| c.value) == Some(2) {
            return capitalize_if("kitaban", uc);
        }
        if to_plural {
            capitalize_if("kutub", uc)
        } else {
            capitalize_if("kitab", uc)
        }
    }

    fn skip_article(&self) -> bool {
        true
    }

    fn inflect_numeral_custom(
        &self,
        numeral: &str,
        count: Option<i64>,
        _style: NumeralStyle,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
    ) -> Option<String> {
        // Arabic-Indic digits for both channels, so the assertions below can tell the numeral
        // apart from the noun at a glance. `count` is the hook's own pre-existing `Option<i64>`
        // (item 8) -- item 11 is about `inflect` getting the same signal, not this one.
        if numeral.is_empty() {
            return None;
        }
        let n = count?;
        Some(
            n.to_string()
                .chars()
                .map(|c| char::from_u32(c as u32 - '0' as u32 + 0x660).unwrap_or(c))
                .collect(),
        )
    }
}

/// Renders whatever `count` it was handed, so a test can assert the value rather than a form.
#[derive(Clone, Copy)]
struct CountProbe;

impl fmt::Display for CountProbe {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "thing")
    }
}

impl Ranting for CountProbe {
    fn name(&self, uc: bool) -> String {
        capitalize_if("thing", uc)
    }
    fn subjective(&self) -> &str {
        "it"
    }
    fn is_plural(&self) -> bool {
        false
    }
    fn inflect(
        &self,
        _to_plural: bool,
        _uc: bool,
        _case: GrammaticalCase,
        count: Option<PlaceholderCount>,
    ) -> String {
        match count {
            None => "unnumbered".to_string(),
            Some(c) => format!("counted-{}", c.value),
        }
    }
    fn skip_article(&self) -> bool {
        true
    }
}

/// The headline: three counts, three noun forms, one placeholder shape.
#[test]
fn the_counted_noun_can_render_a_third_number() {
    let book = ArNoun;
    assert_eq!(say!("I have {$0 1}", 1, book), "I have ١ kitab");
    assert_eq!(say!("I have {$0 1}", 2, book), "I have ٢ kitaban");
    assert_eq!(say!("I have {$0 1}", 3, book), "I have ٣ kutub");
}

/// `{#n noun}` — the spelled-out channel — carries the same count. Item 8 moved this one from
/// compile time to runtime, so both numeral channels reach `inflect` by the same route.
#[test]
fn the_spelled_out_numeral_channel_carries_the_count_too() {
    let book = ArNoun;
    assert_eq!(say!("I have {#0 1}", 2, book), "I have ٢ kitaban");
    assert_eq!(say!("I have {#0 1}", 3, book), "I have ٣ kutub");
}

/// The `Cell` side-channel the spike rejected failed exactly here: having smuggled a count into
/// the entity, it rendered the dual for *every* later placeholder in the same template. A real
/// parameter cannot, because the second placeholder wrote no numeral and so passes `None`.
#[test]
fn the_count_does_not_leak_into_a_later_placeholder() {
    let book = ArNoun;
    assert_eq!(
        say!("I have {$0 1} and {+1}", 2, book),
        "I have ٢ kitaban and kutub"
    );
    assert_eq!(
        say!("I have {$0 1} and {1}", 2, book),
        "I have ٢ kitaban and kitab"
    );
}

/// A placeholder that wrote no numeral passes `None`, which is **not** a count of one: an
/// unnumbered singular and `{$n noun}` with `n = 1` are distinguishable, which is what lets a
/// fork treat "one book" and "a book" differently if its grammar does.
#[test]
fn no_numeral_is_none_rather_than_one() {
    let thing = CountProbe;
    // `{+thing}` rather than a bare `{thing}`: a placeholder with no marker at all renders through
    // `Display` and never reaches `inflect` — pre-existing macro behavior item 11 does not change.
    // `+` asks for a form, so it does.
    assert_eq!(say!("I have {+thing}", thing), "I have unnumbered");
    assert_eq!(say!("I have {$0 1}", 1, thing), "I have 1 counted-1");
}

/// English is unaffected: nothing in the crate reads `count` inside `inflect`, and a
/// derive-generated impl ignores it. This is the byte-identity half of the invariant.
#[test]
fn english_output_is_unchanged_by_the_new_parameter() {
    let book = Noun::new("book", "it");
    assert_eq!(say!("I have {$0 1}", 1, book), "I have 1 book");
    assert_eq!(say!("I have {$0 1}", 2, book), "I have 2 books");
    assert_eq!(say!("I have {+book}", book), "I have books");
}

/// `Many` fills the count gap from its own length when the placeholder supplied none — the same
/// substitution every `_custom` pair already made, extended to `inflect` for consistency. It only
/// ever delegates at `len() == 1`, so the value it supplies is `1`.
#[test]
fn many_fills_the_count_gap_from_its_own_length() {
    let one = Many(vec![CountProbe]);
    // The placeholder wrote no numeral, and the item still sees a count — `Many`'s own length.
    assert_eq!(say!("I have {+one}", one), "I have counted-1");

    // A bare item in the same position sees `None`, which is what makes the line above a
    // substitution rather than a coincidence.
    let bare = CountProbe;
    assert_eq!(say!("I have {+bare}", bare), "I have unnumbered");

    // An explicit placeholder numeral still wins: `or_else` fills the gap, never overrides.
    let counted = Many(vec![ArNoun]);
    assert_eq!(say!("I have {$0 1}", 2, counted), "I have ٢ kitaban");

    // Two items: `Many` does not delegate at all, so there is no `inflect` call to ride along
    // with — the joined names render instead, exactly as before item 11.
    let two = Many(vec![CountProbe, CountProbe]);
    assert_eq!(say!("I have {+two}", two), "I have thing and thing");
}
