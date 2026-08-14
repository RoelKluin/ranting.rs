//! Post-nominal adjective agreement (gender + number) across the 3 known adjectives × 4 known
//! nouns, checked against `crate::spanish::adjective_agree` and graded by corpus attestation.
//!
//! The one probe closest in spirit to `ranting_gaps::probes::regular_plural`: `ranting_es`'s
//! `-o`/`-a` gender swap and vowel-vs-consonant plural suffix choice are genuinely general rules
//! (unlike noun gender/plural, which are hand-listed per word), differentially checked here,
//! reachable only through the 3 known adjectives.

use crate::corpus::Corpus;
use crate::finding::{Case, Confidence, Finding, Kind};
use crate::spanish;
use ranting::{AdjectiveDegree, GrammaticalCase, Ranting};
use ranting_es::SpanishNoun;

const ADJECTIVES: [&str; 3] = ["negro", "pequeño", "azul"];

fn nouns() -> [(&'static str, SpanishNoun, SpanishNoun); 4] {
    [
        ("gato", SpanishNoun::gato(), SpanishNoun::gato().plural()),
        ("casa", SpanishNoun::casa(), SpanishNoun::casa().plural()),
        (
            "problema",
            SpanishNoun::problema(),
            SpanishNoun::problema().plural(),
        ),
        ("agua", SpanishNoun::agua(), SpanishNoun::agua().plural()),
    ]
}

pub fn probe(corpus: &Corpus, limit: usize) -> Option<Finding> {
    let mut cases = Vec::new();
    for (name, singular, plural) in nouns() {
        let feminine = singular.noun_class().as_str() == ranting_es::lexicon::FEMININE;
        for as_plural in [false, true] {
            let entity = if as_plural { plural } else { singular };
            let noun_word = entity.name(false);
            for adjective in ADJECTIVES {
                let rendered = entity
                    .inflect_adjective_custom(
                        adjective,
                        AdjectiveDegree::Comparative, // ignored by ranting_es -- agreement-only
                        GrammaticalCase::Name,
                        entity.noun_class(),
                        as_plural,
                        None,
                        false,
                    )
                    .unwrap_or_default();
                let expected = spanish::adjective_agree(adjective, feminine, as_plural);
                // Postnominal order (noun then adjective) is the correct bigram direction:
                // `el gato negro`, never `negro gato`.
                let occurrences = corpus.attests_bigram(&noun_word, &rendered);
                let confidence = if occurrences > 0 {
                    Confidence::Attested
                } else {
                    Confidence::Certain
                };
                cases.push(Case {
                    subject: format!(
                        "{name} ({}) {adjective}",
                        if as_plural { "plural" } else { "singular" }
                    ),
                    ranting_renders: rendered,
                    expected,
                    confidence,
                    occurrences,
                    examples: Vec::new(),
                });
            }
        }
    }
    cases.retain(|c| c.ranting_renders != c.expected);

    Some(
        Finding {
            id: "adjective-agreement",
            title: "Post-nominal adjective agreement across the closed lexicon",
            kind: Kind::Gap,
            cause: "`SpanishNoun::inflect_adjective_custom` (`ranting_es/src/noun.rs`) calls \
                    `ranting_es::lexicon::adjective_form`, which swaps `-o`/`-a` for gender and \
                    picks `+s`/`+es` for number.",
            why_it_fails: "A mismatch means the adjective hook and the independent oracle in \
                           `ranting_es_gaps::spanish::adjective_agree` disagree on the rendered \
                           form for one of the 3 known adjectives against one of the 4 known \
                           nouns.",
            what_ranting_needs: "Check which side is wrong: `ranting_es::lexicon::adjective_form` \
                                  or this crate's independent copy in `spanish.rs`.",
            cases,
        }
        .finish(limit),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ranting_es_agrees_with_the_reference_rules_today() {
        let corpus = Corpus::default();
        let finding = probe(&corpus, 40).expect("probe always returns a finding");
        assert!(
            finding.cases.is_empty(),
            "no mismatches expected today: {:?}",
            finding.cases
        );
    }
}
