//! Article selection (`el`/`la`/`los`/`las`, `un`/`una`/`unos`/`unas`) across the 4 known nouns,
//! checked against `crate::spanish`'s independent oracle and graded by corpus attestation.
//!
//! This is the probe that reconfirms, from corpus evidence rather than assertion, the three
//! interesting hand-coded decisions `ranting_es/README.md` calls out: `el problema` (not
//! `la problema` -- the `-ma` masculine trap), `el agua`/`un agua` (euphony on a feminine noun),
//! and `las aguas`/`unas aguas` (euphony reverting in the plural).

use crate::corpus::Corpus;
use crate::finding::{Case, Confidence, Finding, Kind};
use crate::spanish;
use ranting::{GrammaticalCase, Ranting};
use ranting_es::SpanishNoun;

/// The closed noun lexicon, enumerated here rather than discovered: name, singular entity,
/// plural entity, and `euphonic_el` -- a lexicon fact `ranting_es`'s public API has no accessor
/// for (it is read only internally, from `self.entry`), so it is carried alongside as documented
/// data from `ranting_es/README.md`'s "`el agua`: a real euphony rule" section.
fn nouns() -> [(&'static str, SpanishNoun, SpanishNoun, bool); 4] {
    [
        (
            "gato",
            SpanishNoun::gato(),
            SpanishNoun::gato().plural(),
            false,
        ),
        (
            "casa",
            SpanishNoun::casa(),
            SpanishNoun::casa().plural(),
            false,
        ),
        (
            "problema",
            SpanishNoun::problema(),
            SpanishNoun::problema().plural(),
            false,
        ),
        (
            "agua",
            SpanishNoun::agua(),
            SpanishNoun::agua().plural(),
            true,
        ),
    ]
}

pub fn probe(corpus: &Corpus, limit: usize) -> Option<Finding> {
    let mut cases = Vec::new();
    for (name, singular, plural, euphonic_el) in nouns() {
        let feminine = singular.noun_class().as_str() == ranting_es::lexicon::FEMININE;
        for as_plural in [false, true] {
            let entity = if as_plural { plural } else { singular };
            let noun_word = entity.name(false);
            for (marker, definite) in [("the", true), ("a", false)] {
                let rendered = entity
                    .inflect_article_custom(
                        marker,
                        name,
                        GrammaticalCase::Name,
                        entity.noun_class(),
                        as_plural,
                        None,
                        false,
                    )
                    .unwrap_or_default();
                let expected = if definite {
                    spanish::definite_article(feminine, as_plural, euphonic_el)
                } else {
                    spanish::indefinite_article(feminine, as_plural, euphonic_el)
                }
                .to_string();
                let occurrences = corpus.attests_bigram(&rendered, &noun_word);
                let confidence = if occurrences > 0 {
                    Confidence::Attested
                } else {
                    Confidence::Certain
                };
                let subject = format!("{rendered} {noun_word}");
                cases.push(Case {
                    subject,
                    ranting_renders: rendered,
                    expected,
                    confidence,
                    occurrences,
                    examples: Vec::new(),
                });
            }
        }
    }
    // Every case is a comparison, not an error report -- keep only actual mismatches.
    cases.retain(|c| c.ranting_renders != c.expected);

    Some(
        Finding {
            id: "article-agreement",
            title: "Article selection across the closed noun lexicon",
            kind: Kind::Gap,
            cause: "`SpanishNoun::inflect_article_custom` (`ranting_es/src/noun.rs`) selects \
                    `el`/`la`/`los`/`las`/`un`/`una`/`unos`/`unas` from the noun's `NounClass` \
                    plus its entity-carried `euphonic_el` flag.",
            why_it_fails: "A mismatch here means the article hook and the independent oracle in \
                           `ranting_es_gaps::spanish` disagree about which article a known noun \
                           takes -- most interestingly for `problema` (masculine despite `-a`) \
                           and `agua` (euphonic `el`/`un` in the singular, reverting to `las`/ \
                           `unas` in the plural).",
            what_ranting_needs: "Check which side is wrong: `ranting_es::lexicon` or this \
                                  crate's independent copy in `spanish.rs`. Both encode the same \
                                  claim from two different places on purpose.",
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
