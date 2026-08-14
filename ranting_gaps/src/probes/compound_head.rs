//! Hyphenated compounds, which pluralize their **head** rather than their tail.
//!
//! `mother-in-law` → `mothers-in-law`, not `mother-in-laws`. `ranting` had no notion of a
//! compound head, so `{+mother_in_law}` rendered `"mother-in-laws"`; **fixed** alongside the
//! regular rules (ROADMAP.md Phase 7 item 10), and this probe now guards the fix differentially.
//! See [`super::regular_plural`] for what an empty finding means.
//!
//! Kept separate from [`super::regular_plural`] even though both are `Ranting::inflect()`
//! falling through to append-`s`, because the fix is different in kind: one is a spelling rule,
//! the other needs to identify a head, and a fork implementing either would touch different code.
//! Separating them also stops one from inflating the other's occurrence count in the index.

use crate::corpus::Corpus;
use crate::english::compound_plural;
use crate::finding::{Case, Confidence, Finding, Kind};

pub fn probe(
    corpus: &Corpus,
    min_occurrences: usize,
    limit: usize,
    unattested: bool,
) -> Option<Finding> {
    let mut cases = Vec::new();
    for (word, facts) in corpus.nouns_by_frequency(min_occurrences) {
        if !word.contains('-') {
            continue;
        }
        // `compound_plural` returns `None` for a compound with no head structure
        // (`t-shirt`, `merry-go-round`), which pluralizes on the tail exactly as `ranting`
        // already does. Those are not findings.
        let Some(expected) = compound_plural(word) else {
            continue;
        };
        let rendered = super::regular_plural::ranting_plural(word);
        if rendered == expected {
            continue;
        }
        // Same attestation gate as the regular-plural probe: a compound plural nobody in the
        // corpus ever writes is a rule the tool believes, not evidence the corpus supplies.
        let confidence = if corpus.attests(&expected) {
            Confidence::Attested
        } else if unattested {
            Confidence::Certain
        } else {
            continue;
        };
        cases.push(Case {
            subject: word.to_string(),
            ranting_renders: rendered,
            expected: expected.clone(),
            confidence,
            occurrences: facts.as_noun,
            examples: super::quote(&facts.examples),
        });
    }

    Some(
        Finding {
            id: "compound-head-plural",
            title: "Hyphenated compounds pluralize the wrong element",
            kind: Kind::Gap,
            cause: "`Ranting::inflect()` treats the noun form as one opaque string and appends \
                    `plural_end` to the end of it. A hyphenated compound whose head is its first \
                    element -- the shape `X-<preposition>-Y` (`mother-in-law`, `passer-by`) or \
                    `X-<postposed adjective>` (`court-martial`) -- pluralizes that head instead.",
            why_it_fails: "`{+noun}` on such a compound renders \"mother-in-laws\" where English \
                           writes \"mothers-in-law\". Unlike the regular-plural gap, this one \
                           cannot be worked around with the `plural_end` attribute either: the \
                           `-s` has to be inserted in the middle of the word, and `plural_end` \
                           can only append.",
            what_ranting_needs:
                "Nothing structural -- `src/language/plurals.rs::compound_plural` landed with the \
                 regular rules (ROADMAP.md Phase 7 item 10): split on `-`, and when the second \
                 element is a preposition or a postposed adjective, pluralize the first element \
                 and rejoin, with `t-shirt` and `merry-go-round` pinned as words that must keep \
                 tail pluralization. A case listed below is therefore a divergence between that \
                 and `ranting_gaps/src/english.rs::compound_plural`, not a missing feature.",
            cases,
        }
        .finish(limit),
    )
}

#[cfg(test)]
mod tests {
    use super::super::regular_plural::ranting_plural;
    use crate::english::compound_plural;

    /// Kept as a live regression guard now that the head rule has landed, for the same reason as
    /// `super::regular_plural`'s: the differential comparison is the probe.
    #[test]
    fn ranting_pluralizes_the_head_of_a_headed_compound() {
        assert_eq!(
            compound_plural("mother-in-law").as_deref(),
            Some("mothers-in-law")
        );
        assert_eq!(ranting_plural("mother-in-law"), "mothers-in-law");
        assert_eq!(ranting_plural("passer-by"), "passers-by");
    }

    /// A compound with no head structure must agree with `ranting`, so it never becomes a case.
    #[test]
    fn headless_compounds_are_not_findings() {
        assert_eq!(compound_plural("t-shirt"), None);
        assert_eq!(ranting_plural("t-shirt"), "t-shirts");
    }
}
