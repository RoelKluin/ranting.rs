//! Present-tense verb agreement across the 4 known verbs × 6 persons, checked against
//! `crate::spanish::conjugate` and graded by corpus attestation.
//!
//! **Attestation keys off the bare conjugated form** (`"habla"`), never the pronoun+verb bigram
//! (`"él habla"`). Spanish pro-drop means real text writes `hablo` far more often than
//! `yo hablo` -- see `ranting_es/README.md`'s pro-drop note, cited in `probes::NOT_HOLES`. Keying
//! off the bigram would make every case here report `Certain` instead of `Attested` on almost any
//! real corpus, which would read as "this tool finds nothing useful" rather than what it actually
//! is: near-universal pro-drop in the source text.
//!
//! `vosotros`/`vosotras` forms are Spain-specific; a Latin-American-sourced corpus will show low
//! or zero attestation for them. That is a corpus-selection fact, not a finding -- don't misread
//! a `Certain`-only `vosotros` row as a gap.
//!
//! Also reconfirms, via the two known nouns' own `él`/`ella` subjective forms, the crate's most
//! interesting design point: `usted` borrows third-person-**singular** agreement (`usted habla`
//! == `el gato habla`), unlike German `Sie`, which borrows third-person-plural.

use crate::corpus::Corpus;
use crate::finding::{Case, Confidence, Finding, Kind};
use crate::spanish;
use ranting::Ranting;
use ranting_es::{SpanishNoun, SpanishPerson};

const VERBS: [&str; 4] = ["hablar", "comer", "vivir", "ser"];

fn persons() -> [(&'static str, SpanishPerson, usize); 6] {
    [
        ("yo", SpanishPerson::YO, 0),
        ("tú", SpanishPerson::TU, 1),
        ("usted", SpanishPerson::USTED, 2),
        ("nosotros", SpanishPerson::NOSOTROS, 3),
        ("vosotros", SpanishPerson::VOSOTROS, 4),
        ("ustedes", SpanishPerson::USTEDES, 5),
    ]
}

fn attest(corpus: &Corpus, form: &str) -> (usize, Confidence) {
    let occurrences = corpus.word_count(form);
    let confidence = if occurrences > 0 {
        Confidence::Attested
    } else {
        Confidence::Certain
    };
    (occurrences, confidence)
}

pub fn probe(corpus: &Corpus, limit: usize) -> Option<Finding> {
    let mut cases = Vec::new();
    for verb in VERBS {
        for (label, person, index) in persons() {
            let rendered = person
                .inflect_verb_custom(person.subjective(), verb, person.is_plural(), None, false)
                .unwrap_or_default();
            let expected = spanish::conjugate(verb, index)
                .unwrap_or_default()
                .to_string();
            let (occurrences, confidence) = attest(corpus, &rendered);
            cases.push(Case {
                subject: format!("{verb} / {label}"),
                ranting_renders: rendered,
                expected,
                confidence,
                occurrences,
                examples: Vec::new(),
            });
        }
        // Reconfirm usted's third-person-singular borrowing against a real noun entity, whose
        // subjective() is "él"/"ella"/"ellos"/"ellas" rather than a person label -- exercising
        // Person::from_subject's plural-only fallback path, not the labeled-subject path above.
        for (name, entity, index) in [
            ("gato", SpanishNoun::gato(), 2usize),
            ("gatos", SpanishNoun::gato().plural(), 5usize),
        ] {
            let rendered = entity
                .inflect_verb_custom(entity.subjective(), verb, entity.is_plural(), None, false)
                .unwrap_or_default();
            let expected = spanish::conjugate(verb, index)
                .unwrap_or_default()
                .to_string();
            let (occurrences, confidence) = attest(corpus, &rendered);
            cases.push(Case {
                subject: format!("{verb} / {name} (noun, not person)"),
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
            id: "verb-person-agreement",
            title: "Present-tense verb agreement across the closed verb set",
            kind: Kind::Gap,
            cause: "`inflect_verb_custom` (`ranting_es/src/noun.rs` and `src/person.rs`) maps a \
                    subject label to a `Person` via `Person::from_subject`, then looks up the \
                    conjugated form in `ranting_es::lexicon`'s closed verb table.",
            why_it_fails: "A mismatch means the hook and `ranting_es_gaps::spanish::conjugate` \
                           disagree about a conjugated form for one of the 4 known verbs at one \
                           of the 6 persons.",
            what_ranting_needs: "Check which side is wrong: `ranting_es::lexicon::VERBS` or this \
                                  crate's independent copy in `spanish.rs`.",
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
