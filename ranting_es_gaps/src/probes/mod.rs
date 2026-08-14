//! The probes, and the contract they all follow.
//!
//! A probe takes the corpus and returns at most one [`Finding`]. Unlike `ranting_gaps`'s probes,
//! which *nominate* candidates from an open English vocabulary, every probe here *enumerates* its
//! cases directly from `ranting_es`'s closed lexicon (4 nouns × article/number combinations, 3
//! adjectives × 4 nouns × number, 4 verbs × 6 persons, 2 prepositions × 8 article forms) and uses
//! the corpus only to grade each enumerated comparison's [`Confidence`](crate::finding::Confidence)
//! -- never to decide whether a case exists at all. See `crate::corpus`'s module doc for why.

use crate::corpus::Corpus;
use crate::finding::Finding;

mod adjective_agreement;
mod article_agreement;
mod lexicon_coverage;
mod preposition_fusion;
mod verb_person;

/// Behaviors `ranting_es/README.md` records as correct-and-intentional, each cited by name. A
/// probe must never emit a `Finding` whose cause overlaps one of these -- if a probe's own
/// comparison logic would produce one, that is a bug in the probe, not a finding about
/// `ranting_es`. Not machine-checked beyond the citation itself; kept as a concrete checklist so
/// a probe author has something to check against instead of re-deriving the judgment call.
/// Referenced only from this module's own tests -- documented dead code in the non-test build,
/// same standing as `ranting_core::grammar`'s still-unused `Article`/`DemonstrativePronoun`.
#[allow(dead_code)]
pub const NOT_HOLES: &[(&str, &str)] = &[
    (
        "partial-lexicon-falls-through",
        "README.md 'Also observed, not holes': an unknown verb/adjective/numeral returns None \
         and renders through ranting's English default -- correct, pinned behavior, not a gap.",
    ),
    (
        "numeral-capitalization",
        "README.md 'Also observed, not holes': a bare numeral placeholder spends its \
         sentence-initial capital on the following noun, not the numeral -- an engine-level \
         property of uc allocation, not specific to Spanish.",
    ),
    (
        "adjective-apocope",
        "README.md 'Also observed, not holes': bueno->buen/grande->gran apocope is prenominal-only \
         and structurally unreachable from the postnominal ! slot this lexicon can render.",
    ),
    (
        "orthographic-plural-spelling",
        "README.md 'Also observed, not holes': feliz->felices-style spelling changes are \
         sidestepped by choosing adjectives (negro/pequeño/azul) that need no exception, not \
         solved by a rule.",
    ),
    (
        "possessive-agrees-with-possessor-not-possessed",
        "README.md 'Also observed, not holes': su/suyo can't see the possessed noun -- no hook \
         signal carries it, for either language.",
    ),
    (
        "capitalize-not-overridden",
        "README.md 'Also observed, not holes': Spanish orthography already matches the English \
         sentence-start-only capitalization default.",
    ),
    (
        "pro-drop",
        "README.md 'Also observed, not holes': whether a subject pronoun is written at all is a \
         template choice, not a ranting signal -- low pronoun+verb bigram attestation is expected, \
         not a finding. See verb_person's own doc comment.",
    ),
];

/// Run every probe and collect non-empty findings, ranked by attested occurrences.
pub fn run_all(corpus: &Corpus, limit: usize) -> Vec<Finding> {
    let mut findings: Vec<Finding> = [
        article_agreement::probe(corpus, limit),
        adjective_agreement::probe(corpus, limit),
        preposition_fusion::probe(corpus, limit),
        verb_person::probe(corpus, limit),
        lexicon_coverage::probe(corpus, limit),
    ]
    .into_iter()
    .flatten()
    .filter(|f| !f.cases.is_empty())
    .collect();
    findings.sort_by_key(|f| std::cmp::Reverse(f.occurrences()));
    findings
}

/// Format an occurrence list for a report: `path:line — sentence`, trimmed to one line each.
/// Identical to `ranting_gaps::probes::quote`.
pub(crate) fn quote(examples: &[crate::corpus::Occurrence]) -> Vec<String> {
    examples
        .iter()
        .map(|o| {
            let mut sentence = o.sentence.split_whitespace().collect::<Vec<_>>().join(" ");
            if sentence.chars().count() > 140 {
                sentence = sentence.chars().take(137).collect::<String>() + "...";
            }
            format!("{}:{} — {sentence}", o.file.display(), o.line)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `NOT_HOLES` is a documentation checklist, not a machine-enforced gate -- but a duplicate
    /// or empty entry would defeat its purpose as a checklist, so pin the shape at least.
    #[test]
    fn not_holes_has_no_duplicate_keys() {
        let mut keys: Vec<&str> = NOT_HOLES.iter().map(|(k, _)| *k).collect();
        let before = keys.len();
        keys.sort_unstable();
        keys.dedup();
        assert_eq!(keys.len(), before, "NOT_HOLES has a duplicate key");
        assert_eq!(
            NOT_HOLES.len(),
            7,
            "one entry per README 'Also observed' item"
        );
    }

    /// `run_all` must never emit a finding whose `id` collides with a `NOT_HOLES` key -- catches
    /// the easy version of a probe accidentally re-deriving a judgment call already on record.
    #[test]
    fn no_probe_id_collides_with_a_not_hole_key() {
        let corpus = Corpus::default();
        for finding in run_all(&corpus, 40) {
            assert!(
                !NOT_HOLES.iter().any(|(key, _)| *key == finding.id),
                "finding id {} collides with a NOT_HOLES key",
                finding.id
            );
        }
    }
}
