//! How much of the corpus's determiner-headed noun phrases fall outside the 4-noun closed
//! lexicon -- a `Kind::Boundary` measurement, not an agreement check. There is nothing to compare
//! for an uncovered noun: it correctly falls through to `ranting`'s English rendering, pinned by
//! `ranting_es`'s own `an_unknown_verb_falls_through_to_english_rather_than_being_guessed` and its
//! adjective/numeral siblings (see `probes::NOT_HOLES`).
//!
//! Structural analog of `ranting_gaps::probes::word_order`, but there is no analog of
//! `word_order` itself here: Spanish's postnominal adjectives mean that boundary never reproduces
//! (see `ranting_es/README.md`'s "Why Spanish, after German"), and `probes::run_all` already
//! drops empty-case findings, so a probe built to always find zero cases would be dead code. The
//! non-applicability is recorded in this crate's own README instead, mirroring how
//! `ranting_es/README.md` itself has a "Holes that do not reproduce here" section.

use crate::corpus::Corpus;
use crate::finding::{Case, Confidence, Finding, Kind};

const KNOWN_NOUNS: [&str; 4] = ["gato", "casa", "problema", "agua"];

pub fn probe(corpus: &Corpus, limit: usize) -> Option<Finding> {
    let nouns = corpus.nouns_by_frequency(1);
    let total: usize = nouns.iter().map(|(_, f)| f.as_noun).sum();
    let covered: usize = nouns
        .iter()
        .filter(|(w, _)| KNOWN_NOUNS.contains(w))
        .map(|(_, f)| f.as_noun)
        .sum();

    let mut cases = vec![Case {
        subject: "all determiner-headed noun phrases".to_string(),
        ranting_renders: format!("{covered}/{total} in the closed lexicon"),
        expected: "n/a — this is a coverage measurement, not a rendering comparison".to_string(),
        confidence: Confidence::Attested,
        occurrences: total,
        examples: Vec::new(),
    }];

    for (word, facts) in nouns
        .iter()
        .filter(|(w, _)| !KNOWN_NOUNS.contains(w))
        .take(limit)
    {
        cases.push(Case {
            subject: (*word).to_string(),
            ranting_renders: "falls through to English (not in the closed lexicon)".to_string(),
            expected: "n/a — correct, documented behavior".to_string(),
            confidence: Confidence::Attested,
            occurrences: facts.as_noun,
            examples: super::quote(&facts.examples),
        });
    }

    Some(Finding {
        id: "lexicon-coverage",
        title: "How much of the corpus falls outside the closed lexicon",
        kind: Kind::Boundary,
        cause: "`ranting_es`'s lexicon (`ranting_es/src/lexicon.rs`) hand-lists exactly 4 nouns, \
                4 verbs, 3 adjectives and numerals 0..=12 -- there is no general noun-gender, \
                noun-pluralization, or verb-conjugation rule to extend to new words.",
        why_it_fails: "Not a failure: a noun outside the closed set falls through to `ranting`'s \
                       English rendering, which is correct, pinned behavior (see \
                       `probes::NOT_HOLES`'s 'partial-lexicon-falls-through' entry). This finding \
                       exists to measure scope, not to list bugs.",
        what_ranting_needs: "Nothing -- this is a reference lexicon exercising `ranting`'s public \
                              API end-to-end for four nouns, not a Spanish NLP vocabulary. This \
                              count is evidence about scope: what fraction of real Spanish noun \
                              phrases a 4-noun/4-verb/3-adjective closed lexicon can express, \
                              which is the number a reader deciding whether to grow the lexicon \
                              needs, not a bug list.",
        cases,
    }.finish(limit + 1)) // +1: the summary case plus up to `limit` uncovered words
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::corpus::{Corpus, WordFacts};

    #[test]
    fn reports_a_sane_summary_on_empty_input() {
        let corpus = Corpus::default();
        let finding = probe(&corpus, 10).expect("always returns a finding");
        assert_eq!(
            finding.cases.len(),
            1,
            "only the summary case on empty input"
        );
        assert!(finding.cases[0].ranting_renders.starts_with("0/0"));
    }

    #[test]
    fn known_nouns_are_excluded_from_the_uncovered_list() {
        let mut corpus = Corpus::default();
        corpus.words.insert(
            "gato".to_string(),
            WordFacts {
                as_noun: 3,
                ..Default::default()
            },
        );
        corpus.words.insert(
            "perro".to_string(),
            WordFacts {
                as_noun: 5,
                ..Default::default()
            },
        );
        let finding = probe(&corpus, 10).expect("always returns a finding");
        assert!(
            finding.cases.iter().all(|c| c.subject != "gato"),
            "a known noun must never appear in the uncovered list"
        );
        assert!(finding.cases.iter().any(|c| c.subject == "perro"));
    }
}
