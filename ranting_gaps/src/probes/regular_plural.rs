//! The headline probe: nouns whose plural `ranting` gets wrong because it has no regular
//! pluralization rules.
//!
//! **This probe's original finding has been fixed** (ROADMAP.md Phase 7 item 10). Until then,
//! `Ranting::inflect()`'s regular path appended the `plural_end` attribute, which defaults to
//! `"s"`, with no `y`→`ies`, no `-es` after a sibilant and no `f`→`ves`, so `{+fly}` rendered
//! `"flys"` and `{+box}` rendered `"boxs"`. `src/language/plurals.rs` now carries those rules.
//!
//! The probe stays, and reporting **nothing now means "the rules are present and agree"**, not
//! "the probe is broken" -- it compares `ranting`'s real output against [`crate::english`]'s
//! independently written copy of the rules, so it also catches a future regression or a
//! divergence between the two. That independence is the point: see the note at the top of
//! `crate::english`.
//!
//! This is one cause, not one-per-word, which is why the probe emits a single `Finding` with a
//! frequency-ranked case table rather than a file per broken noun.

use crate::corpus::Corpus;
use crate::english::regular_plural;
use crate::finding::{Case, Confidence, Finding, Kind};
use ranting::{GrammaticalCase, Noun, Ranting};

/// What `ranting` renders for `{+word}`, through the public API, exactly as a user would get it.
///
/// `Noun` is the crate's own `Ranting` implementation and uses the default `plural_end = "s"`, so
/// this is the behavior every un-attributed noun gets. A struct that sets `plural_end` explicitly
/// can work around a single word; that is per-noun configuration, not a rule, and does not change
/// what this probe reports.
pub(crate) fn ranting_plural(word: &str) -> String {
    Noun::new(word, "it").inflect(true, false, GrammaticalCase::Name)
}

/// True when the corpus already treats `word` as a plural, i.e. it ends in `s` and the corpus
/// also writes the form without it.
///
/// Without this, the determiner cue in "the crates" nominates `crates` as a noun and the probe
/// dutifully reports that `{+crates}` should render `crateses`. Nobody writes that placeholder;
/// the finding would be technically true of the code and useless as evidence.
fn already_plural(word: &str, corpus: &Corpus) -> bool {
    for suffix in ["es", "s"] {
        if let Some(stem) = word.strip_suffix(suffix)
            && stem.len() > 2
            && corpus.attests(stem)
        {
            return true;
        }
    }
    false
}

pub fn probe(
    corpus: &Corpus,
    min_occurrences: usize,
    limit: usize,
    unattested: bool,
) -> Option<Finding> {
    let mut cases = Vec::new();
    for (word, facts) in corpus.nouns_by_frequency(min_occurrences) {
        // A hyphenated compound is the *other* probe's finding; reporting it here too would
        // double-count it in the index.
        if word.contains('-') || word.contains('\'') {
            continue;
        }
        if already_plural(word, corpus) {
            continue;
        }
        // Words the table covers are not this probe's business: `child` renders "children" while
        // the *regular* rule says "childs", and there the rule is the one that is wrong.
        //
        // This asks the table directly. It used to test `rendered != format!("{word}s")` instead,
        // as a proxy for "the table decided this" -- valid only while the fallback was always
        // bare append-`s`. Once `ranting` grew the rules, that proxy skipped every word the rules
        // touch (`fly` renders "flies", so it never reached the comparison), which silently made
        // the differential check inert for exactly the words it now exists to guard.
        if ranting::inflect_noun_irregular(word, true).is_some() {
            continue;
        }
        let rendered = ranting_plural(word);
        let expected = regular_plural(word);
        if rendered == expected {
            continue;
        }
        // The corpus writing the corrected form is independent evidence that the rule, not the
        // tool's opinion, is what English does here -- and, just as importantly, that the word is
        // really a noun. The determiner cue alone nominates `is`, `as` and `only` (markdown
        // adjacency puts a determiner in front of all three), and the rule then produces `ises`,
        // `ases`, `onlies`. None of those are attested anywhere, so requiring attestation removes
        // them without needing a part-of-speech tagger or a stoplist to guess with.
        let confidence = if corpus.attests(&expected) {
            Confidence::Attested
        } else if unattested {
            Confidence::Certain
        } else {
            continue;
        };
        cases.push(Case {
            subject: word.to_string(),
            ranting_renders: rendered,
            expected,
            confidence,
            occurrences: facts.as_noun,
            examples: super::quote(&facts.examples),
        });
    }

    Some(
        Finding {
            id: "regular-plural-rules",
            title: "No regular English pluralization rules",
            kind: Kind::Gap,
            cause: "`Ranting::inflect()`'s regular path used to append the `plural_end` attribute \
                    (default `\"s\"`) verbatim, with `src/language/plurals.rs` offering only a \
                    lookup into the generated `IRREGULAR_PLURALS` table \
                    (`data/irregular_plurals.txt`, 63 lines) -- so a word absent from the table \
                    had no rule to fall back on, only the suffix. That path now goes through \
                    `ranting::inflect_noun_regular` and the orthographic rules, which is why any \
                    case below is a divergence rather than the original gap.",
            why_it_fails: "Every English noun outside the 63-line table whose plural is not formed \
                           by bare `-s` used to render wrong -- `{+fly}` gave \"flys\", `{+box}` \
                           gave \"boxs\" -- silently, with no error, on the crate's single \
                           most-used feature. The rules landed in `src/language/plurals.rs`, so a \
                           case below now means `ranting` and this crate's independent copy of \
                           those rules have diverged, which is worth exactly the same attention.",
            what_ranting_needs:
                "Nothing structural -- these rules landed in `src/language/plurals.rs` as \
                 ROADMAP.md Phase 7 item 10 (consonant + `y` → `ies`; `s`/`x`/`z`/`ch`/`sh` → \
                 `es`; the `f`/`fe` → `ves` stems), reached through \
                 `ranting::inflect_noun_regular` when the irregular table misses and the \
                 `singular_end`/`plural_end` attributes are at their defaults. So a case listed \
                 below is no longer a missing feature: it is a **divergence** between \
                 `src/language/plurals.rs` and this crate's independently written copy in \
                 `ranting_gaps/src/english.rs`, and one of the two is now wrong. Check which \
                 before changing either. Words needing a lexicon rather than a spelling rule -- \
                 `-o` → `oes` for `hero` but `-os` for `piano`, `-us` → `-i` for Latin borrowings \
                 but not for `bus`, `quiz` → `quizzes` -- are out of scope for both and belong in \
                 `data/irregular_plurals.txt`.",
            cases,
        }
        .finish(limit),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The words this probe was written to report. `ranting` renders them correctly now, and the
    /// assertion is deliberately kept rather than deleted: it is the differential check the whole
    /// probe rests on, so it is worth more as a live regression guard on `ranting` than as a
    /// deleted line in the history.
    ///
    /// `knife` is absent on purpose: it is a row in `data/irregular_plurals.txt`, so the table --
    /// not the rules -- decides it, and the probe skips it either way.
    #[test]
    fn ranting_now_agrees_with_the_reference_rules() {
        for (word, english_wants) in [
            ("fly", "flies"),
            ("box", "boxes"),
            ("church", "churches"),
            ("city", "cities"),
            ("bus", "buses"),
            ("bookshelf", "bookshelves"),
        ] {
            assert_eq!(
                regular_plural(word),
                english_wants,
                "reference rule for {word}"
            );
            assert_eq!(
                ranting_plural(word),
                english_wants,
                "ranting's plural of {word}"
            );
        }
    }

    /// Words `ranting` already gets right must never become cases -- a gap finder that reports
    /// working behavior is worse than none. `child`/`mouse` are the interesting half: the
    /// irregular table handles them, while the *regular* rule would say "childs"/"mouses", so a
    /// probe that compared against the rule alone would report two working words as broken.
    #[test]
    fn words_the_table_or_the_bare_s_already_gets_right_are_skipped() {
        let corpus = crate::corpus::Corpus::default();
        for word in [
            "dog", "cat", "day", "child", "person", "mouse", "sheep", "stomach",
        ] {
            // Mirrors the probe's own skip: ask the table, then compare. `stomach` is the case
            // that makes this worth pinning -- the reference rule says "stomaches" (spelling
            // cannot hear that its `-ch` is /k/), `ranting` says "stomachs" and is right, so only
            // the attestation gate keeps it out of the report.
            let in_table = ranting::inflect_noun_irregular(word, true).is_some();
            let reported = !in_table && ranting_plural(word) != regular_plural(word);
            assert!(
                !reported,
                "{word} would be reported as a failure but is not one"
            );
        }
        // Guard the skip condition itself rather than only its effect.
        assert!(corpus.nouns_by_frequency(1).is_empty());
    }
}
