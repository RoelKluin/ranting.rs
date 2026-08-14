//! The probes, and the contract they all follow.
//!
//! A probe takes the corpus and returns at most one [`Finding`]. Each one must be able to show
//! its work: every case it emits carries the output `ranting` actually produced, obtained by
//! calling the real inflection, never by asserting what it would do.
//!
//! **What is deliberately not here.** Two probes were designed and dropped before implementation:
//! *invariant plurals* (`sheep`, `fish` -- detect a noun the corpus only ever writes one way) and
//! *unlisted irregular verbs* (`slay`/`slew` absent from `data/irregular_verbs.txt`). Both need to
//! infer a word's correct inflection from attestation patterns rather than from spelling, and both
//! produce findings a human would have to hand-filter. A gap finder whose output needs triage is
//! not obviously better than no gap finder, so they wait for either a lexicon or a reason.

use crate::corpus::Corpus;
use crate::finding::Finding;

mod compound_head;
mod pre_word_homograph;
mod regular_plural;
mod word_order;

/// Run every probe. `min_occurrences` filters out corpus noise; `limit` bounds each finding's
/// case list; `unattested` admits corrections the corpus never writes.
///
/// Attestation is the tool's main precision control, and it is doing the job a part-of-speech
/// tagger would otherwise have to. The determiner cue nominates `is`, `as` and `only` as nouns
/// (markdown puts a determiner in front of all three), and the plural rules then produce `ises`,
/// `ases` and `onlies` -- corrections no English text contains. Requiring the corpus to attest
/// the corrected form drops all of them without anyone having to write a stoplist and guess.
pub fn run_all(
    corpus: &Corpus,
    min_occurrences: usize,
    limit: usize,
    unattested: bool,
) -> Vec<Finding> {
    let mut findings: Vec<Finding> = [
        regular_plural::probe(corpus, min_occurrences, limit, unattested),
        compound_head::probe(corpus, min_occurrences, limit, unattested),
        pre_word_homograph::probe(corpus, min_occurrences, limit),
        word_order::probe(corpus, min_occurrences, limit),
    ]
    .into_iter()
    .flatten()
    .filter(|f| !f.cases.is_empty())
    .collect();
    // The index is ranked by how much of the corpus each cause actually accounts for. That
    // ordering is the tool's opinion about what to fix first.
    findings.sort_by_key(|f| std::cmp::Reverse(f.occurrences()));
    findings
}

/// Format an occurrence list for a report: `path:line — sentence`, trimmed to one line each.
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
