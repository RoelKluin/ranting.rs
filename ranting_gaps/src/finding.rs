//! What a probe reports, and the vocabulary the report is written in.
//!
//! The shape here is the answer to the four questions the tool exists to answer for each case:
//! what is the cause, why does it fail, how common is it, and what does `ranting` need. Anything
//! a probe wants to say that doesn't fit one of those fields is commentary and belongs in the
//! probe's own doc comment instead.

use serde::Serialize;

/// Whether a category is something `ranting` could fix, or something it has decided not to.
///
/// This distinction is the reason the tool is usable as a roadmap input rather than a complaint
/// generator. `docs/EXTENSIBILITY.md` §2.12 established word order as a *permanent boundary*:
/// `ranting` inflects words within a template, and the order of those words is the caller's. A
/// tool that reported prenominal adjectives as bugs would be arguing with a decision already
/// taken. Counting how often the boundary is reached is still worth doing -- that is data about
/// how much of real English the crate can't express -- so boundaries are collected, ranked, and
/// reported, just never as work.
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
    /// Decidable from `ranting`'s own behavior: the tool ran the inflection and compared it with
    /// a rule that is a function of spelling alone. No corpus judgement involved.
    Certain,
    /// As `Certain`, and the corpus independently attests the corrected form -- the text really
    /// does write `flies`. The strongest evidence available without a lexicon.
    Attested,
    /// Rests on the determiner heuristic in [`crate::corpus`] being right about this word's part
    /// of speech. Check the quoted sentences before acting.
    Heuristic,
}

/// One word or construction that `ranting` cannot handle, with its evidence.
#[derive(Debug, Clone, Serialize)]
pub struct Case {
    /// The word or construction as the corpus writes it.
    pub subject: String,
    /// What `ranting` actually renders. Computed by running the inflection, never asserted --
    /// a finding without this is a claim, not evidence.
    pub ranting_renders: String,
    /// What English requires instead.
    pub expected: String,
    pub confidence: Confidence,
    /// Times the corpus put this word in the position the probe cares about.
    pub occurrences: usize,
    /// Up to `corpus::MAX_EXAMPLES` sentences, each `path:line`-prefixed.
    pub examples: Vec<String>,
}

/// A group of cases sharing one root cause. This -- not the individual word -- is the unit of the
/// report, because a single missing rule can produce thousands of broken words and listing them
/// separately would bury the one fact that matters.
#[derive(Debug, Clone, Serialize)]
pub struct Finding {
    /// Kebab-case slug; becomes the directory name under `failures/`.
    pub id: &'static str,
    pub title: &'static str,
    pub kind: Kind,
    /// The mechanical reason, in terms of `ranting`'s own code: which function, which fallback.
    pub cause: &'static str,
    /// What goes wrong for a user as a result.
    pub why_it_fails: &'static str,
    /// The concrete change `ranting` would need. For a `Boundary`, why there isn't one.
    pub what_ranting_needs: &'static str,
    /// Cases, most frequent first.
    pub cases: Vec<Case>,
}

impl Finding {
    /// Total corpus occurrences across every case -- the number the index ranks by.
    pub fn occurrences(&self) -> usize {
        self.cases.iter().map(|c| c.occurrences).sum()
    }

    /// Sort cases by frequency and keep the report bounded. Returns `self` so probes can finish
    /// with `.finish(limit)`.
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
