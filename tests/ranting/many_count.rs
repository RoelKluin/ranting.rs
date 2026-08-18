// (c) Roel Kluin 2026 MIT
// `Many` supplying its own length as the placeholder count (ROADMAP.md Phase 6
// item 15, open question 3 of
// docs/superpowers/specs/2026-08-13-number-categories.md).
//
// A bare placeholder (no `#`/`$` numeral) hands every count-carrying hook
// `count: None` — but a one-item `Many` genuinely knows its own count is 1, so
// it substitutes that in place of reporting no-numeral-here. The fix is
// scoped to the one-item delegation arm: `Many` with zero or 2+ items has no
// single item to delegate a hook call to at all (`elision.rs`'s
// `many_with_two_items_does_not_elide` pins that those arms stay `None`), so
// there is no hook invocation for a count to accompany.
use ranting::*;
use std::fmt;

/// Reports the count it was handed by `inflect_verb_custom`, or that none
/// arrived at all.
struct CountProbe;

impl fmt::Display for CountProbe {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "probe")
    }
}

impl Ranting for CountProbe {
    fn name(&self, uc: bool) -> String {
        capitalize_if("probe", uc)
    }
    fn subjective(&self) -> &str {
        "they"
    }
    fn is_plural(&self) -> bool {
        true
    }
    fn inflect(
        &self,
        to_plural: bool,
        uc: bool,
        _case: GrammaticalCase,
        _count: Option<PlaceholderCount>,
    ) -> String {
        capitalize_if(if to_plural { "probes" } else { "probe" }, uc)
    }
    fn skip_article(&self) -> bool {
        false
    }
    fn inflect_verb_custom(
        &self,
        _subject: &str,
        _verb: &str,
        _as_plural: bool,
        count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        let word = match count {
            Some(c) => format!("arrive[{}]", c.value),
            None => "arrive[none]".to_string(),
        };
        Some(capitalize_if(&word, uc))
    }
}

#[test]
fn empty_many_still_reports_no_numeral_here() {
    let none: Many<CountProbe> = Many(vec![]);
    // Zero items: `is_plural()` is true (0 != 1), but there is no single item
    // to delegate the verb hook to, so the custom hook — and its count
    // substitution — is never reached; English's own conjugation renders.
    assert_eq!(say!("{=none arrive}."), "They arrive.".to_string());
}

#[test]
fn single_item_many_supplies_its_length_as_the_count() {
    let one = Many(vec![CountProbe]);
    // A one-item `Many` singularizes the delegated pronoun ("it", not "they") —
    // pre-existing behavior, unrelated to this item — but the verb hook still
    // gets `count: Some(1)` in place of the `None` a bare placeholder used to
    // hand it.
    assert_eq!(say!("{=one arrive}."), "It arrive[1].".to_string());
}

#[test]
fn multi_item_many_still_reports_no_numeral_here() {
    let two = Many(vec![CountProbe, CountProbe]);
    // 2+ items: the same structural limitation as the empty case — no single
    // item to delegate to — so the default English conjugation renders
    // untouched, exactly as it did before this item landed.
    assert_eq!(say!("{=two arrive}."), "They arrive.".to_string());
}

#[test]
fn explicit_numeral_wins_over_many_own_length() {
    // When the placeholder does carry a numeral, that value is what reaches
    // the hook — `Many` only substitutes its own length when the placeholder
    // supplied no numeral (`count: None`) in the first place.
    let one = Many(vec![CountProbe]);
    // Sentence-initial `$var`: `uc` is dropped at the digit rather than carried
    // on to the pronoun (docs/architecture-review-2026-08-15.md §1.11).
    assert_eq!(
        say!("{$n =one arrive}.", n = 3),
        "3 they arrive[3].".to_string()
    );
}
