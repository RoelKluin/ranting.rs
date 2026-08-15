// (c) Roel Kluin 2026 MIT
// A zero-length article no longer emits its separator (ROADMAP.md Phase 6 item 11).
//
// `inflect_article_custom` returning `""` is the only way a fork can say "no article renders
// here" — German's indefinite plural ("Hunde bellen") has no article at all. Before this fix the
// separator between the (empty) article and whatever followed it was pushed unconditionally,
// rendering a stray leading space at the start of a placeholder, or a doubled space mid-sentence.
// Found by ranting_i18n's German lexicon (Phase 6 item 10, hole 6).
use ranting::*;
use std::fmt;

/// A minimal German-style noun: only the indefinite plural article is empty ("Hunde bellen");
/// the definite article ("die Hunde") and definite/indefinite singular ("der Hund"/"ein Hund")
/// still render normally, so the fix must not touch those paths.
struct Hund;

impl fmt::Display for Hund {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Hund")
    }
}

impl Ranting for Hund {
    fn name(&self, uc: bool) -> String {
        if uc {
            "Hund".to_string()
        } else {
            "hund".to_string()
        }
    }
    fn subjective(&self) -> &str {
        "they"
    }
    fn is_plural(&self) -> bool {
        false
    }
    fn inflect(
        &self,
        to_plural: bool,
        uc: bool,
        _case: GrammaticalCase,
        _count: Option<PlaceholderCount>,
    ) -> String {
        let word = if to_plural { "Hunde" } else { "Hund" };
        capitalize_if(word, uc)
    }
    fn skip_article(&self) -> bool {
        false
    }
    fn inflect_article_custom(
        &self,
        article: &str,
        _noun_singular: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        match article {
            "a" if as_plural => Some(String::new()),
            "a" => Some(capitalize_if("ein", uc)),
            "the" if as_plural => Some(capitalize_if("die", uc)),
            "the" => Some(capitalize_if("der", uc)),
            _ => None,
        }
    }
}

#[test]
fn empty_article_leaves_no_leading_space() {
    assert_eq!(say!("{a +0 bellen}.", Hund), "Hunde bellen.");
}

#[test]
fn empty_article_leaves_no_doubled_space_mid_sentence() {
    assert_eq!(say!("Dort {a +0 bellen}.", Hund), "Dort Hunde bellen.");
}

#[test]
fn empty_article_before_a_numeral_swallows_the_numeral_leading_space_instead() {
    // `$var` renders in the numeral slot, ahead of the noun — the separator swallowed is
    // whichever comes first, so here it's the numeral's own leading space, not `noun_space`.
    let hund = Hund;
    assert_eq!(say!("{a $0 hund bellen}.", 3, hund), "3 Hunde bellen.");
}

#[test]
fn non_empty_articles_are_unaffected() {
    assert_eq!(say!("{the +0} bellen.", Hund), "Die Hunde bellen.");
    assert_eq!(say!("{a -0} bellt.", Hund), "Ein Hund bellt.");
    assert_eq!(say!("{the -0} bellt.", Hund), "Der Hund bellt.");
}

#[test]
fn elide_article_custom_is_still_never_called_for_a_zero_length_article() {
    // The post-assembly splice is skipped whenever the recorded article span is empty — the
    // separator is swallowed before that splice would even run, not by it. Probe panics if the
    // hook is reached.
    struct ElisionProbe;
    impl fmt::Display for ElisionProbe {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            Hund.fmt(f)
        }
    }
    impl Ranting for ElisionProbe {
        fn name(&self, uc: bool) -> String {
            Hund.name(uc)
        }
        fn subjective(&self) -> &str {
            Hund.subjective()
        }
        fn is_plural(&self) -> bool {
            Hund.is_plural()
        }
        fn inflect(
            &self,
            to_plural: bool,
            uc: bool,
            case: GrammaticalCase,
            _count: Option<PlaceholderCount>,
        ) -> String {
            Hund.inflect(to_plural, uc, case, None)
        }
        fn skip_article(&self) -> bool {
            Hund.skip_article()
        }
        fn inflect_article_custom(
            &self,
            article: &str,
            noun_singular: &str,
            case: GrammaticalCase,
            class: NounClass,
            as_plural: bool,
            count: Option<PlaceholderCount>,
            uc: bool,
        ) -> Option<String> {
            Hund.inflect_article_custom(article, noun_singular, case, class, as_plural, count, uc)
        }
        fn elide_article_custom(
            &self,
            _article: &str,
            _separator: &str,
            _following: &str,
            _case: GrammaticalCase,
            _class: NounClass,
            _as_plural: bool,
            _count: Option<PlaceholderCount>,
        ) -> Option<String> {
            panic!("elide_article_custom must not be called for a zero-length article");
        }
    }
    assert_eq!(say!("{a +0 bellen}.", ElisionProbe), "Hunde bellen.");
}

/// A per-entity `skip_article()` suppression is a second, unrelated way to get a zero-length
/// article — the fix applies to it too, since the swallow decision is keyed off the recorded
/// `article_span`, not off which mechanism emptied it.
struct NoArticleNoun;

impl fmt::Display for NoArticleNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Alaska")
    }
}

impl Ranting for NoArticleNoun {
    fn name(&self, uc: bool) -> String {
        capitalize_if("Alaska", uc)
    }
    fn subjective(&self) -> &str {
        "it"
    }
    fn is_plural(&self) -> bool {
        false
    }
    fn inflect(
        &self,
        _to_plural: bool,
        uc: bool,
        _case: GrammaticalCase,
        _count: Option<PlaceholderCount>,
    ) -> String {
        capitalize_if("Alaska", uc)
    }
    fn skip_article(&self) -> bool {
        true
    }
}

#[test]
fn skip_article_leaves_no_stray_separator_either() {
    assert_eq!(
        say!("Dort {the 0} liegt.", NoArticleNoun),
        "Dort Alaska liegt."
    );
}
