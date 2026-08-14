//! Hyphenated compounds, which pluralize their **head** rather than their tail.
//!
//! `mother-in-law` → `mothers-in-law`, not `mother-in-laws`. `ranting` has no notion of a
//! compound head, so `{+mother_in_law}` renders `"mother-in-laws"` -- confirmed by running it.
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
                "Head detection in `src/language/plurals.rs`, before the append-`plural_end` \
                 fallback: split on `-`, and when the second element is a preposition or a \
                 postposed adjective, pluralize the first element and rejoin. \
                 `ranting_gaps/src/english.rs::compound_plural` is a reference implementation, \
                 with `t-shirt` and `merry-go-round` pinned as words that must keep tail \
                 pluralization. Worth noting how small this is: the whole rule is a `split('-')` \
                 and a two-word lookup, and it removes a failure the `plural_end` escape hatch \
                 cannot reach.",
            cases,
        }
        .finish(limit),
    )
}

#[cfg(test)]
mod tests {
    use super::super::regular_plural::ranting_plural;
    use crate::english::compound_plural;

    #[test]
    fn ranting_pluralizes_the_tail_of_a_headed_compound() {
        assert_eq!(ranting_plural("mother-in-law"), "mother-in-laws");
        assert_eq!(
            compound_plural("mother-in-law").as_deref(),
            Some("mothers-in-law")
        );
    }

    /// A compound with no head structure must agree with `ranting`, so it never becomes a case.
    #[test]
    fn headless_compounds_are_not_findings() {
        assert_eq!(compound_plural("t-shirt"), None);
        assert_eq!(ranting_plural("t-shirt"), "t-shirts");
    }
}
