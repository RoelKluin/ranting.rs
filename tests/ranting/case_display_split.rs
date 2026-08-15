// (c) Roel Kluin 2026 MIT
// ROADMAP.md Phase 6 item 19: a case marker used to do two jobs at once -- it told
// `inflect_article_custom` the noun's grammatical role *and* switched the noun slot from the
// name to a pronoun. A fork that overrides `inflect_pronoun_custom` to return the noun's own
// name (as `tests/ranting/grammatical_case.rs` demonstrates, and as `ranting_i18n`'s
// pre-item-19 `GermanNoun` did via its own `Render::Name`/`Render::Pronoun` flag) could get
// "Der Mann bellt." but then lost real pronouns for that same entity everywhere else --
// `say!("Ich sehe {@0}.", named)` rendered "Ich sehe Hund." instead of "Ich sehe ihn."
//
// The fix reuses `*` -- already a case-marker-position character, previously synonymous with no
// marker at all (`CaseKind::Name`) -- fused with a real case marker: `{the *=noun}` case-marks
// the placeholder exactly as `{the =noun}` does (the article hook still sees
// `GrammaticalCase::Subjective`) but keeps rendering the noun's name. No new marker character
// was added.
use ranting::*;
use std::fmt;

/// A fork whose `inflect_pronoun_custom` always returns a real pronoun -- unconditionally,
/// unlike `tests/ranting/grammatical_case.rs`'s `GermanNoun`, which returns the name
/// unconditionally instead. Item 19's whole point is that a *single* entity like this can still
/// get a case-correct article with the name displayed, by writing `*=`/`*@` instead of `=`/`@`.
#[derive(Clone, Copy)]
struct Mann; // "Mann" (man), masculine

thread_local! {
    static SEEN_ARTICLE_CASE: std::cell::RefCell<Vec<GrammaticalCase>> = const { std::cell::RefCell::new(Vec::new()) };
}

impl fmt::Display for Mann {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Mann")
    }
}

impl Ranting for Mann {
    fn name(&self, uc: bool) -> String {
        capitalize_if("Mann", uc)
    }
    fn subjective(&self) -> &str {
        "he"
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
        capitalize_if("Mann", uc)
    }
    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_article_custom(
        &self,
        article: &str,
        _noun_singular: &str,
        case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        SEEN_ARTICLE_CASE.with(|s| s.borrow_mut().push(case));
        if article != "the" {
            return None;
        }
        let form = match case {
            GrammaticalCase::Objective => "den",
            _ => "der",
        };
        Some(capitalize_if(form, uc))
    }

    fn inflect_pronoun_custom(
        &self,
        _subject: &str,
        case: PronounCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        // Unconditionally a real pronoun -- the opposite of grammatical_case.rs's `GermanNoun`,
        // and the choice that used to make case-marked-but-name-displayed unreachable.
        let word = match case {
            PronounCase::Subjective => "er",
            PronounCase::Objective => "ihn",
            PronounCase::PossessiveDeterminer | PronounCase::PossessivePronoun => "sein",
            PronounCase::Reflexive => "sich",
        };
        Some(capitalize_if(word, uc))
    }
}

#[test]
fn fused_marker_renders_name_while_bare_marker_still_renders_the_pronoun() {
    let m = Mann;
    // Bare `=`/`@`: real pronouns, same entity, same case.
    assert_eq!(say!("{=0} bellt.", m), "Er bellt.");
    assert_eq!(say!("Ich sehe {@0}.", m), "Ich sehe ihn.");
    // Fused `*=`/`*@`: the name, with the article still case-correct.
    assert_eq!(say!("{the *=0} bellt.", m), "Der Mann bellt.");
    assert_eq!(say!("Ich sehe {the *@0}.", m), "Ich sehe den Mann.");
}

#[test]
fn fused_marker_hands_the_article_hook_the_same_grammatical_case_as_the_bare_marker() {
    SEEN_ARTICLE_CASE.with(|s| s.borrow_mut().clear());
    let _ = say!("{the *=0}", Mann);
    let _ = say!("{the *@0}", Mann);
    SEEN_ARTICLE_CASE.with(|s| {
        assert_eq!(
            s.borrow().as_slice(),
            &[GrammaticalCase::Subjective, GrammaticalCase::Objective]
        );
    });
}

#[test]
fn fused_marker_reaches_inflects_own_case_parameter_too() {
    // The noun-slot render path both fused and bare-Name/Hidden markers share calls
    // `Ranting::inflect(as_pl, uc, case.into())` -- for the fused marker `case` is the real
    // variant, not always `Name`, so a fork's own `inflect()` sees it too.
    struct Probe;
    thread_local! {
        static SEEN_INFLECT_CASE: std::cell::RefCell<Vec<GrammaticalCase>> = const { std::cell::RefCell::new(Vec::new()) };
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
            case: GrammaticalCase,
            _count: Option<PlaceholderCount>,
        ) -> String {
            SEEN_INFLECT_CASE.with(|s| s.borrow_mut().push(case));
            capitalize_if("dog", uc)
        }
        fn skip_article(&self) -> bool {
            false
        }
    }
    let _ = say!("{*=0}", Probe);
    let _ = say!("{*@0}", Probe);
    SEEN_INFLECT_CASE.with(|s| {
        assert_eq!(
            s.borrow().as_slice(),
            &[GrammaticalCase::Subjective, GrammaticalCase::Objective]
        );
    });
}

#[test]
fn bare_star_marker_is_unaffected() {
    // A bare `*` (no following case marker) is unchanged: still `CaseKind::Name`, still just
    // marks which word is the placeholder's Ranting element for verb agreement purposes -- the
    // README's "{*jane who have}" pattern.
    let jane = Noun::new("Jane", "I");
    assert_eq!(say!("{*jane who have} book"), "Jane who have book");
}

#[test]
fn fused_marker_on_a_plain_english_noun_is_byte_identical_in_shape_to_name_rendering() {
    // No fork hooks at all: English `Noun` never overrides `inflect_pronoun_custom`, so this
    // pins that the fused marker's English behavior is exactly "render the name" -- useful even
    // without a non-English fork, e.g. to force name display while still tagging a placeholder's
    // grammatical role for a hook that only cares about `GrammaticalCase`.
    let alice = Noun::new("Alice", "she");
    assert_eq!(say!("{=alice}", alice), "She");
    assert_eq!(say!("{*=alice}", alice), "Alice");
    assert_eq!(say!("{@alice}", alice), "Her");
    assert_eq!(say!("{*@alice}", alice), "Alice");
}

#[test]
fn existing_bare_case_markers_are_untouched_by_the_new_grammar() {
    // Regression guard: extending `case`'s grammar to accept the fused form must not change what
    // any existing bare marker means. Every case-marker/entity pairing from
    // tests/ranting/grammatical_case.rs and the crate's own README examples still renders
    // exactly as before.
    let jordan = Noun::new("Jordan", "they");
    assert_eq!(
        say!("{=jordan are} a wonderful friend."),
        "They are a wonderful friend."
    );
    assert_eq!(
        say!("This is {`jordan} favorite book."),
        "This is their favorite book."
    );
}
