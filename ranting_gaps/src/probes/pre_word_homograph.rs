//! Nouns that collide with `ranting`'s closed pre-noun vocabulary -- the `{can can}` case.
//!
//! A placeholder's `pre` slot accepts a fixed set of articles and modal/auxiliary verbs. Several
//! of those words are also ordinary English nouns: *a can*, *a will*, *the haves*, *a must*,
//! *with all his might*. When such a word is the noun **and** an article is inside the
//! placeholder, the parse takes it as the pre-word and the placeholder loses a token.
//!
//! Verified by compiling, not asserted:
//!
//! ```text
//! say!("A {can can} hold water.")     -> "A can can hold water."    // correct
//! say!("{The can can} hold water.")   -> "Can can hold water."      // "The" is gone
//! say!("{The +can can} hold water.")  -> "The cans can hold water." // correct
//! say!("{The *can can} hold water.")  -> "The can can hold water."  // correct -- `*` fixes it
//! ```
//!
//! So the collision is real but narrow, one marker already fixes it, and the report says both
//! rather than flagging every occurrence of `can` in the corpus as broken.
//!
//! The oracle is `ranting_core::ph_ext::parse` itself. Restating the pre-word list here would
//! duplicate `ph_ext::MODAL_WORDS`, and this repo has a standing problem with exactly that kind
//! of hand-kept duplication -- `CLAUDE.md` records `PH_START` and `SENTENCE_TRIGGER_CHARS` as two
//! lists kept in step by hand with "no mechanism preventing drift". Asking the parser is the one
//! answer that cannot go stale.

use crate::corpus::Corpus;
use crate::finding::{Case, Confidence, Finding, Kind};
use ranting_core::ph_ext;

/// A neutral post-noun verb used to build the probe placeholder. Any word that is not itself a
/// pre-word would do; `hold` is the README's own example for `can`.
const PROBE_VERB: &str = "hold";

/// Describe how `ph_ext` reads `{The <word> hold}`, or `None` if it reads it correctly.
///
/// Correct means the noun slot got `word`. When the pre-word branch wins instead, the noun slot
/// gets the *verb* and the placeholder has silently lost a token.
fn misparse(word: &str) -> Option<String> {
    let inner = format!("The {word} {PROBE_VERB}");
    let parsed = ph_ext::parse(&inner).ok()?;
    let noun = parsed.name("noun")?.as_str();
    if noun == word {
        return None;
    }
    let pre = parsed.name("pre").map(|s| s.as_str()).unwrap_or("");
    let post = parsed.name("post").map(|s| s.as_str()).unwrap_or("");
    Some(format!("pre={pre:?} noun={noun:?} post={post:?}"))
}

/// The `pre` slot's *article* half. Excluded from this probe because an English article is never
/// an English noun, so `{The the hold}` is not a construction anybody writes -- the collision is
/// vacuous. They still reach the probe, because the determiner cue in `corpus.rs` fires on prose
/// that quotes them ("the `the` article"), which is the same markdown adjacency that puts `is` and
/// `as` in noun position. The modal half (`can`, `will`, `must`, `have`) is the real finding: those
/// *are* nouns in English.
const ARTICLES: &[&str] = &["a", "an", "some", "the", "these", "those"];

pub fn probe(corpus: &Corpus, min_occurrences: usize, limit: usize) -> Option<Finding> {
    let mut cases = Vec::new();
    for (word, facts) in corpus.nouns_by_frequency(min_occurrences) {
        if ARTICLES.contains(&word) {
            continue;
        }
        let Some(actual) = misparse(word) else {
            continue;
        };
        cases.push(Case {
            subject: word.to_string(),
            ranting_renders: actual,
            expected: format!(
                "pre={:?} noun={word:?} post={:?}",
                "The ",
                format!(" {PROBE_VERB}")
            ),
            // The determiner heuristic decided this word is a noun; the parse result itself is
            // certain, but whether the corpus really uses it as a noun is not.
            confidence: Confidence::Heuristic,
            occurrences: facts.as_noun,
            examples: super::quote(&facts.examples),
        });
    }

    Some(
        Finding {
            id: "pre-word-homograph",
            title: "Nouns that collide with the closed pre-noun vocabulary",
            kind: Kind::Gap,
            cause: "`ranting_core::grammar::PH_EXT`'s `pre` group accepts a closed set of \
                    articles plus modal/auxiliary verbs (`can`, `may`, `shall`, `will`, `are`, \
                    `were`, `have`, `had`, `do`, `could`, `would`, `should`, `must`, `might`, each \
                    optionally with `n't`). Some of those words are also ordinary nouns. When one \
                    is the placeholder's noun *and* an article is inside the placeholder, the \
                    pre-word reading wins and consumes a token.",
            why_it_fails: "`say!(\"{The can can} hold water.\")` renders \"Can can hold water.\" \
                           -- the article is gone. The three neighbouring shapes are all correct: \
                           `\"A {can can} hold water.\"` (article outside the placeholder), \
                           `{The +can can}`, and `{=can can}`. So this is not \"`can` is broken\", \
                           it is one specific shape, and the failure is silent output corruption \
                           rather than a compile error. Note the two columns below are *parses*, \
                           not rendered strings: the render is only obtainable by compiling, and \
                           the one verified render is quoted here. Read the case table with the \
                           `heuristic` label in mind: the *parse* is certain, but whether a given \
                           word is really used as a noun rests on the determiner cue, which \
                           over-fires on prose that quotes grammar words. `can`, `will` and \
                           `must` are genuine English nouns; check the quoted sentences before \
                           trusting the rest.",
            what_ranting_needs: "Documentation, not code -- checked by compiling, not assumed. \
                 `say!(\"{The *can can} hold water.\")` renders \"The can can hold water.\", so \
                 the `*` marker already fixes every case in the table below. What is missing is \
                 any way to find that out: README.md's only `*` example is \
                 `\"A {*can can} contain water.\"`, which puts the article *outside* the \
                 placeholder -- and that shape renders correctly *without* `*`, so the example \
                 demonstrates the marker in the one position where it changes nothing. A reader \
                 hitting the real failure has no reason to reach for `*`, and no error message \
                 to prompt them; the output is silently wrong. Fix: change the README example to \
                 `{The *can can}` and say what `*` is for -- disambiguating a noun that is also \
                 in the closed pre-word vocabulary. Leaving the parser alone is the right call \
                 here: preferring the token-consuming reading would change how every existing \
                 template parses, to buy what one documented character already buys.",
            cases,
        }
        .finish(limit),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The user-reported case, and the reason this probe exists. Note the probe deliberately
    /// tests the *unmarked* shape: `{The *can can}` renders correctly, so a probe built on the
    /// `*`-marked form would find nothing and the documentation gap would stay invisible.
    #[test]
    fn can_is_read_as_a_pre_word_and_loses_the_noun_slot() {
        let actual = misparse("can").expect("`can` collides with the modal in the pre slot");
        assert!(
            actual.contains("noun=\"hold\""),
            "the noun slot should have been taken by the verb, got: {actual}"
        );
    }

    /// An ordinary noun must parse cleanly, or the probe would report the entire corpus.
    #[test]
    fn ordinary_nouns_do_not_collide() {
        for word in ["dog", "cat", "fly", "box", "house", "gato"] {
            assert_eq!(
                misparse(word),
                None,
                "{word} must not be reported as a pre-word collision"
            );
        }
    }
}
