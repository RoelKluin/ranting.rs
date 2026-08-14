//! The two obligatory preposition+article fusions (`de`+`el`→`del`, `a`+`el`→`al`), checked
//! against `crate::spanish::fuse_preposition` and graded by corpus attestation.
//!
//! Only checks the closed 2×8 grid (`de`/`a` × the 8 article surface forms) -- it does not scan
//! free corpus text for `de`/`a` followed by an arbitrary capitalized word, so it cannot hit the
//! proper-noun false-positive class a wider probe would need to guard against (`de El Salvador`,
//! `a El Paso` do not fuse). Documented here as a boundary condition for anyone tempted to widen
//! this probe later, not as something this version needs to handle.

use crate::corpus::Corpus;
use crate::finding::{Case, Confidence, Finding, Kind};
use crate::spanish;
use ranting::{GrammaticalCase, Ranting};
use ranting_es::SpanishNoun;

const PREPOSITIONS: [&str; 2] = ["de", "a"];
const ARTICLES: [&str; 8] = ["el", "la", "los", "las", "un", "una", "unos", "unas"];

pub fn probe(corpus: &Corpus, limit: usize) -> Option<Finding> {
    // Any noun works: `inflect_preposition_custom` doesn't consult `self`.
    let entity = SpanishNoun::gato();
    let mut cases = Vec::new();
    for preposition in PREPOSITIONS {
        for article in ARTICLES {
            let hook_result = entity.inflect_preposition_custom(
                preposition,
                article,
                GrammaticalCase::Name,
                entity.noun_class(),
                false,
                None,
                false,
            );
            let expected_fused = spanish::fuse_preposition(preposition, article);
            let rendered = hook_result
                .clone()
                .unwrap_or_else(|| format!("{preposition} {article}"));
            let expected = expected_fused
                .map(str::to_string)
                .unwrap_or_else(|| format!("{preposition} {article}"));
            let occurrences = match expected_fused {
                Some(fused) => corpus.word_count(fused),
                None => corpus.attests_bigram(preposition, article),
            };
            let confidence = if occurrences > 0 {
                Confidence::Attested
            } else {
                Confidence::Certain
            };
            cases.push(Case {
                subject: format!("{preposition} + {article}"),
                ranting_renders: rendered,
                expected,
                confidence,
                occurrences,
                examples: Vec::new(),
            });
        }
    }
    cases.retain(|c| c.ranting_renders != c.expected);

    Some(
        Finding {
            id: "preposition-fusion",
            title: "Preposition+article fusion (de+el, a+el)",
            kind: Kind::Gap,
            cause: "`SpanishNoun::inflect_preposition_custom` (`ranting_es/src/noun.rs`) fuses \
                    exactly `(\"de\",\"el\")`->\"del\" and `(\"a\",\"el\")`->\"al\", declining \
                    every other pair.",
            why_it_fails: "A mismatch means the hook and `ranting_es_gaps::spanish::\
                           fuse_preposition` disagree about which of the 16 preposition+article \
                           pairs contract.",
            what_ranting_needs: "Check which side is wrong: `SpanishNoun::inflect_preposition_\
                                  custom` or this crate's independent copy in `spanish.rs`.",
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
