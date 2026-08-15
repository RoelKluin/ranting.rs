// (c) Roel Kluin 2026 MIT
// NounClass (ROADMAP.md Phase 6 item 2) is the lexical-gender / noun-class
// channel: an open-ended label carried *by the entity* and handed to
// `inflect_article_custom`/`inflect_pronoun_custom` (and their `_with_context`
// twins), so a non-English fork no longer has to keep an external
// `HashMap<&str, Gender>` keyed by the noun's display string — a table that
// breaks on homographs (`der Band`/`das Band`), on names, and on nouns built at
// runtime.
//
// The worked example below is the one the roadmap item asks for: `der Hund`,
// `die Katze` and `das Haus` selected from a single `inflect_article_custom`
// body, differing only in the class each noun declares.
use ranting::*;
use std::fmt;

/// One German noun type for all three genders — deliberately one struct, so the
/// article choice below is provably one code path, not three specialized impls.
struct GermanNoun {
    word: &'static str,
    class: NounClass,
}

impl GermanNoun {
    fn new(word: &'static str, class: &'static str) -> Self {
        GermanNoun {
            word,
            class: NounClass::new(class),
        }
    }
}

impl fmt::Display for GermanNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.word)
    }
}

impl Ranting for GermanNoun {
    fn name(&self, uc: bool) -> String {
        capitalize_if(self.word, uc)
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
        capitalize_if(self.word, uc)
    }
    fn skip_article(&self) -> bool {
        false
    }

    fn noun_class(&self) -> NounClass {
        self.class
    }

    fn inflect_article_custom(
        &self,
        article: &str,
        _noun_singular: &str,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        if article != "the" {
            return None;
        }
        // The full definite-article table a fork needs: class picks the column,
        // case picks the row. Nothing here looks at the noun's spelling.
        let form = match (class.as_str(), case) {
            (_, _) if as_plural => "die",
            ("masculine", GrammaticalCase::Objective) => "den",
            ("masculine", _) => "der",
            ("feminine", _) => "die",
            ("neuter", _) => "das",
            // No class declared: nothing to decline on, so let English through.
            _ => return None,
        };
        Some(capitalize_if(form, uc))
    }

    fn inflect_pronoun_custom(
        &self,
        _subject: &str,
        _case: PronounCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        // A case-declining fork keeps showing the noun's own name after the
        // article rather than collapsing to an English-style pronoun; see
        // tests/ranting/grammatical_case.rs for the same override and why the
        // `{the =noun}` form needs it.
        Some(capitalize_if(self.word, uc))
    }
}

#[test]
fn three_genders_select_three_articles_from_one_code_path() {
    let hund = GermanNoun::new("Hund", "masculine");
    let katze = GermanNoun::new("Katze", "feminine");
    let haus = GermanNoun::new("Haus", "neuter");

    assert_eq!(say!("{the =0}", hund), "Der Hund");
    assert_eq!(say!("{the =0}", katze), "Die Katze");
    assert_eq!(say!("{the =0}", haus), "Das Haus");
}

#[test]
fn class_and_case_combine_in_the_article_hook() {
    let hund = GermanNoun::new("Hund", "masculine");
    let katze = GermanNoun::new("Katze", "feminine");

    // Masculine is the only gender whose accusative differs from its nominative
    // — exactly the interaction that needs both channels present at once.
    assert_eq!(say!("Ich sehe {the @0}.", hund), "Ich sehe den Hund.");
    assert_eq!(say!("Ich sehe {the @0}.", katze), "Ich sehe die Katze.");
}

#[test]
fn homographs_with_different_classes_are_distinguishable() {
    // The motivating failure of a display-string-keyed gender table: two nouns
    // spelled identically, with different genders. Keyed by name they are one
    // entry; carried on the entity they are two.
    let music_band = GermanNoun::new("Band", "feminine"); // die Band (a music group)
    let ribbon = GermanNoun::new("Band", "neuter"); // das Band (a ribbon)

    assert_eq!(say!("{the =0}", music_band), "Die Band");
    assert_eq!(say!("{the =0}", ribbon), "Das Band");
}

#[test]
fn the_pronoun_hook_receives_the_class_too() {
    struct Probe;
    thread_local! {
        static SEEN: std::cell::RefCell<Vec<NounClass>> = const { std::cell::RefCell::new(Vec::new()) };
    }
    impl fmt::Display for Probe {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "Katze")
        }
    }
    impl Ranting for Probe {
        fn name(&self, uc: bool) -> String {
            capitalize_if("Katze", uc)
        }
        fn subjective(&self) -> &str {
            "she"
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
            capitalize_if("Katze", uc)
        }
        fn skip_article(&self) -> bool {
            false
        }
        fn noun_class(&self) -> NounClass {
            NounClass::new("feminine")
        }
        fn inflect_pronoun_custom(
            &self,
            _subject: &str,
            _case: PronounCase,
            class: NounClass,
            _as_plural: bool,
            _count: Option<PlaceholderCount>,
            _uc: bool,
        ) -> Option<String> {
            SEEN.with(|s| s.borrow_mut().push(class));
            None // fall back to English so output is unaffected
        }
    }

    assert_eq!(say!("{=0}", Probe), "She");
    SEEN.with(|s| assert_eq!(s.borrow().as_slice(), &[NounClass::new("feminine")]));
}

#[test]
fn a_noun_that_declares_no_class_reports_unset() {
    // Additivity, from the hook's side: an impl that never sets a class sees
    // `NounClass::UNSET`, which is what it would have got before this channel
    // existed — and `is_unset()` is true for it.
    struct Probe;
    thread_local! {
        static SEEN: std::cell::RefCell<Vec<NounClass>> = const { std::cell::RefCell::new(Vec::new()) };
    }
    impl fmt::Display for Probe {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "dog")
        }
    }
    impl Ranting for Probe {
        fn name(&self, uc: bool) -> String {
            capitalize_if("dog", uc)
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
            capitalize_if("dog", uc)
        }
        fn skip_article(&self) -> bool {
            false
        }
        fn inflect_article_custom(
            &self,
            _article: &str,
            _noun_singular: &str,
            _case: GrammaticalCase,
            class: NounClass,
            _as_plural: bool,
            _count: Option<PlaceholderCount>,
            _uc: bool,
        ) -> Option<String> {
            SEEN.with(|s| s.borrow_mut().push(class));
            None
        }
    }

    assert_eq!(say!("{the 0}", Probe), "The dog");
    SEEN.with(|s| {
        assert_eq!(s.borrow().as_slice(), &[NounClass::UNSET]);
        assert!(s.borrow()[0].is_unset());
        assert_eq!(s.borrow()[0].as_str(), "");
    });
}

#[test]
fn noun_carries_a_class_via_with_noun_class() {
    // `Noun` gets its class from the `#[ranting(gender = "$")]` field.
    let hund = Noun::new("Hund", "he").with_noun_class(NounClass::new("masculine"));
    assert_eq!(hund.noun_class(), NounClass::new("masculine"));
    assert_eq!(hund.noun_class().as_str(), "masculine");

    // Unset by default.
    let plain = Noun::new("dog", "it");
    assert!(plain.noun_class().is_unset());
    assert_eq!(say!("{the plain}"), "The dog");

    // Additivity, from the rendering side: `ranting` never reads the class, so
    // a classed `Noun` renders byte-identically to an unclassed twin.
    let unclassed = Noun::new("Hund", "he");
    assert_eq!(
        say!("{the hund} {=hund are} {`hund}."),
        "The Hund he is his."
    );
    assert_eq!(
        say!("{the unclassed} {=unclassed are} {`unclassed}."),
        say!("{the hund} {=hund are} {`hund}.")
    );
}

#[test]
fn derived_gender_attribute_sets_the_class() {
    #[derive_ranting]
    #[ranting(subject = "he", name = "Hund", gender = "masculine")]
    struct Hund {}

    #[derive_ranting]
    #[ranting(subject = "it", name = "Haus", gender = "neuter")]
    struct Haus {}

    // No `gender` attribute at all: the trait default applies, unchanged.
    #[derive_ranting]
    #[ranting(subject = "it", name = "Ding")]
    struct Ding {}

    assert_eq!(Hund {}.noun_class(), NounClass::new("masculine"));
    assert_eq!(Haus {}.noun_class(), NounClass::new("neuter"));
    assert!(Ding {}.noun_class().is_unset());
}

#[test]
fn wrappers_delegate_the_class_of_their_inner_value() {
    let katze = || Noun::new("Katze", "she").with_noun_class(NounClass::new("feminine"));

    assert_eq!(Box::new(katze()).noun_class(), NounClass::new("feminine"));
    assert_eq!(
        Maybe(Some(katze())).noun_class(),
        NounClass::new("feminine")
    );
    assert_eq!(Many(vec![katze()]).noun_class(), NounClass::new("feminine"));

    // Nothing to report: an absent `Maybe`, and a `Many` whose members may
    // carry differing classes.
    assert!(Maybe::<Noun>(None).noun_class().is_unset());
    assert!(
        Many(vec![katze(), Noun::new("Hund", "he")])
            .noun_class()
            .is_unset()
    );
    assert!(Many::<Noun>(vec![]).noun_class().is_unset());
}
