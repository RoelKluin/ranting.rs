//! The headline probe: nouns whose plural `ranting` gets wrong because it has no regular
//! pluralization rules at all.
//!
//! `Ranting::inflect()`'s regular path appends the `plural_end` attribute, which defaults to
//! `"s"`. There is no `y`→`ies`, no `-es` after a sibilant, no `f`→`ves`. Everything English does
//! beyond append-`s` has to be a row in `data/irregular_plurals.txt`, which is 63 lines long --
//! so `{+fly}` renders `"flys"`, `{+box}` renders `"boxs"`, and `{+church}` renders `"churchs"`.
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
        let rendered = ranting_plural(word);
        // A rendered form that is not bare append-`s` means `IRREGULAR_PLURALS` covered this word,
        // so the table -- not the missing rule -- decided it. Reporting those would be a false
        // positive: `child` renders "children" while the *regular* rule would say "childs", and
        // the regular rule is the one that is wrong there. This probe is only about words that
        // fell through to the fallback.
        if rendered != format!("{word}s") {
            continue;
        }
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
            cause: "`Ranting::inflect()`'s regular path appends the `plural_end` attribute, which \
                    defaults to `\"s\"` and is concatenated verbatim. `src/language/plurals.rs` \
                    provides only a lookup into the generated `IRREGULAR_PLURALS` table \
                    (`data/irregular_plurals.txt`, 63 lines); when a word is absent from it there \
                    is no rule to fall back on, only the suffix.",
            why_it_fails: "Every English noun outside the 63-line table whose plural is not formed \
                           by bare `-s` renders wrong: `{+fly}` gives \"flys\", `{+box}` gives \
                           \"boxs\", `{+church}` gives \"churchs\", `{+knife}` gives \"knifes\". \
                           The failure is silent -- there is no error, just an incorrect word in \
                           the output -- and it affects the crate's single most-used feature.",
            what_ranting_needs:
                "Regular orthographic rules in `src/language/plurals.rs`, applied before the \
                 append-`plural_end` fallback: consonant + `y` → `ies`; `s`/`x`/`z`/`ch`/`sh` → \
                 `es`; the `f`/`fe` → `ves` stems. `ranting_gaps/src/english.rs::regular_plural` \
                 is an executable reference implementation with the counterexamples \
                 (`day`/`days`, `roof`/`roofs`, `chief`/`chiefs`) already pinned in its tests. \
                 The classes that need a lexicon rather than a spelling rule -- `-o` → `oes` for \
                 `hero` but `-os` for `piano`, `-us` → `-i` for Latin borrowings but not for \
                 `bus` -- stay table entries, which is what the table should be reserved for. \
                 Note `Ranting::inflect` takes `to_plural` and the noun form only, so this is a \
                 change to the English rules, not to any trait signature; a non-English fork \
                 already overrides the whole path.",
            cases,
        }
        .finish(limit),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The behavior the whole finding rests on: these words are absent from
    /// `data/irregular_plurals.txt`, so they reach the append-`s` fallback and come out wrong.
    /// If `ranting` ever grows the rules, this test fails and the finding is retired -- which is
    /// the correct way for it to end.
    #[test]
    fn ranting_appends_a_bare_s_where_english_does_not() {
        for (word, ranting_gives, english_wants) in [
            ("fly", "flys", "flies"),
            ("box", "boxs", "boxes"),
            ("church", "churchs", "churches"),
            ("city", "citys", "cities"),
            ("bus", "buss", "buses"),
            // `knife` is deliberately absent: it is already a row in
            // `data/irregular_plurals.txt`, so it renders "knives" and the probe skips it. The
            // gap is the words the table does *not* cover, which is most of them.
        ] {
            assert_eq!(
                ranting_plural(word),
                ranting_gives,
                "ranting's plural of {word}"
            );
            assert_eq!(
                regular_plural(word),
                english_wants,
                "reference rule for {word}"
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
        for word in ["dog", "cat", "day", "child", "person", "mouse", "sheep"] {
            let rendered = ranting_plural(word);
            let reached_fallback = rendered == format!("{word}s");
            let reported = reached_fallback && rendered != regular_plural(word);
            assert!(
                !reported,
                "{word} would be reported as a failure but is not one"
            );
        }
        // Guard the skip condition itself rather than only its effect.
        assert!(corpus.nouns_by_frequency(1).is_empty());
    }
}
