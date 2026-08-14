//! Prenominal adjectives -- measured, never reported as work.
//!
//! `docs/EXTENSIBILITY.md` §2.12 established word order as a **permanent boundary**: `ranting`
//! inflects words within a template, and the order of those words is the template's, which is the
//! caller's. The `!`/`!!` degree slot is post-noun only, so `the small dog` has no placeholder
//! shape at all -- the adjective has to be literal text outside it, where nothing agrees with
//! anything.
//!
//! This probe exists to answer a question the boundary decision left open: *how often does it
//! actually bite?* "English puts adjectives before nouns" is a fact everyone already knows; "42%
//! of determiner-headed noun phrases in this corpus carry a prenominal adjective" is a number,
//! and a number is what tells you whether the boundary is a footnote or the main event for a
//! given application. Reported as [`Kind::Boundary`] so it is never mistaken for a roadmap item.

use crate::corpus::Corpus;
use crate::english::looks_like_adjective;
use crate::finding::{Case, Confidence, Finding, Kind};

pub fn probe(corpus: &Corpus, min_occurrences: usize, limit: usize) -> Option<Finding> {
    let mut cases = Vec::new();
    for (word, facts) in corpus.nouns_by_frequency(min_occurrences) {
        // The determiner cue put this word in noun position; if it also looks like an adjective,
        // what actually followed the determiner was `<adjective> <noun>` and the noun is a token
        // further on -- out of any placeholder's reach.
        if !looks_like_adjective(word) {
            continue;
        }
        cases.push(Case {
            subject: word.to_string(),
            ranting_renders: format!("literal text outside the placeholder: `the {word} {{noun}}`"),
            expected: format!("agreeing with the noun, as `{{noun !{word}}}` does post-noun"),
            confidence: Confidence::Heuristic,
            occurrences: facts.as_noun,
            examples: super::quote(&facts.examples),
        });
    }

    Some(
        Finding {
            id: "word-order-prenominal-adjective",
            title: "Prenominal adjectives have no placeholder slot (permanent boundary)",
            kind: Kind::Boundary,
            cause: "The `!`/`!!` degree slot in `PH_EXT`'s `post` group is post-noun only. There \
                    is no pre-noun adjective slot, and per `docs/EXTENSIBILITY.md` §2.12 there \
                    will not be one: word order belongs to the template, and the template belongs \
                    to the caller.",
            why_it_fails: "An English attributive adjective precedes its noun, so it has to be \
                           written as literal text outside the placeholder, where it receives no \
                           agreement and no degree inflection. This is the same structural \
                           mismatch `ranting_i18n` records as its German hole 8 (`der kleine \
                           Hund`) -- and the reason `ranting_es` was chosen as the second \
                           falsifier, since Spanish's post-nominal `el gato negro` lands exactly \
                           where the `!` slot renders. English itself is on the German side of \
                           that split, which is easy to lose sight of.",
            what_ranting_needs:
                "Nothing. This is a decision on record, not an unfilled gap, and the count below \
                 is evidence about its cost rather than a request. What the count is *for*: if a \
                 given application's text is mostly bare noun phrases, the boundary is a \
                 footnote; if it is adjective-heavy prose, most of its noun phrases cannot be \
                 expressed as a single placeholder at all, and that is worth knowing before \
                 adopting the crate rather than after. Treat a high number as an argument about \
                 scope, not as a bug report.",
            cases,
        }
        .finish(limit),
    )
}

#[cfg(test)]
mod tests {
    use crate::english::looks_like_adjective;

    #[test]
    fn recognises_common_and_suffixed_adjectives() {
        for word in ["small", "great", "dangerous", "beautiful", "effective"] {
            assert!(looks_like_adjective(word), "{word} should look adjectival");
        }
    }

    /// The heuristic under-counts on purpose; what it must not do is claim plain nouns.
    #[test]
    fn plain_nouns_are_not_mistaken_for_adjectives() {
        for word in ["dog", "house", "water", "template", "placeholder"] {
            assert!(!looks_like_adjective(word), "{word} is not an adjective");
        }
    }
}
