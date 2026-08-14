//! What a probe reports, and the vocabulary the report is written in.
//!
//! Copied near-verbatim from `ranting_gaps/src/finding.rs`. One deliberate difference:
//! `Confidence` has no `Heuristic` variant here. `ranting_gaps`'s `Heuristic` rests on a
//! determiner cue being right about a word's part of speech -- a real uncertainty, since that
//! tool nominates candidates from an open vocabulary. This crate never nominates: every case it
//! reports comes from enumerating `ranting_es`'s closed lexicon, so there is no part-of-speech
//! guess anywhere to be `Heuristic` about. See `crate::corpus`'s module doc for the full
//! enumerate-then-attest rationale.

use serde::Serialize;

/// Whether a category is something `ranting_es` could fix, or something it has decided not to.
///
/// Mirrors `ranting_gaps::finding::Kind` exactly. `ranting_es/README.md`'s "Holes that do not
/// reproduce here" and "Also observed, not holes" sections are this crate's `Boundary` material
/// in prose form; see `crate::probes::NOT_HOLES` for the checklist a probe must never contradict.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum Kind {
    /// A defect or missing feature. Actionable.
    Gap,
    /// Working as designed, by a decision on record. Measured, never actionable.
    Boundary,
}

/// How much the tool is willing to stand behind a finding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum Confidence {
    /// Decidable from `ranting_es`'s own behavior alone: the tool ran the real hook and compared
    /// it with the independent oracle in `crate::spanish`. The corpus never happened to write
    /// this exact form, or wasn't consulted for this case.
    Certain,
    /// As `Certain`, and the corpus independently attests the compared form -- the text really
    /// does write it. The stronger of the two grades, not a gate: this crate enumerates every
    /// comparison up front, so attestation only grades confidence, it never decides inclusion.
    Attested,
}

/// One word or construction from `ranting_es`'s closed lexicon, with its evidence.
#[derive(Debug, Clone, Serialize)]
pub struct Case {
    /// The word, phrase, or construction under test, as written.
    pub subject: String,
    /// What `ranting_es` actually renders. Computed by calling the real hook, never asserted --
    /// a finding without this is a claim, not evidence.
    pub ranting_renders: String,
    /// What the independent Spanish oracle in `crate::spanish` says instead.
    pub expected: String,
    pub confidence: Confidence,
    /// Times the corpus attests the compared form at all (0 if never attested).
    pub occurrences: usize,
    /// Up to `corpus::MAX_EXAMPLES` sentences, each `path:line`-prefixed.
    pub examples: Vec<String>,
}

/// A group of cases sharing one root cause -- the unit of the report, exactly as in
/// `ranting_gaps`.
#[derive(Debug, Clone, Serialize)]
pub struct Finding {
    /// Kebab-case slug; becomes the directory name under `failures/`.
    pub id: &'static str,
    pub title: &'static str,
    pub kind: Kind,
    /// The mechanical reason, in terms of `ranting_es`'s own code: which hook, which entry.
    pub cause: &'static str,
    /// What goes wrong for a user as a result.
    pub why_it_fails: &'static str,
    /// The concrete change `ranting_es` would need. For a `Boundary`, why there isn't one.
    pub what_ranting_needs: &'static str,
    /// Cases, most frequent first.
    pub cases: Vec<Case>,
}

impl Finding {
    /// Total corpus attestation across every case -- the number the index ranks by.
    pub fn occurrences(&self) -> usize {
        self.cases.iter().map(|c| c.occurrences).sum()
    }

    /// Sort cases by attestation and keep the report bounded. Returns `self` so probes can
    /// finish with `.finish(limit)`.
    pub fn finish(mut self, limit: usize) -> Self {
        self.cases.sort_by(|a, b| {
            b.occurrences
                .cmp(&a.occurrences)
                .then(a.subject.cmp(&b.subject))
        });
        self.cases.truncate(limit);
        self
    }
}
