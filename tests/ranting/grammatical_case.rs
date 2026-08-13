// (c) Roel Kluin 2026 MIT
// GrammaticalCase (added to inflect_article_custom/_with_context) lets a
// case-declining language's fork pick the article form based on the noun's
// own case marker (`{the =noun}` vs `{the @noun}`), not just gender/number.
// This is the fix for the gap the German spike in
// docs/architecture-review-2026-08-13.md found: previously
// inflect_article_custom couldn't distinguish "Der Mann bellt." (nominative
// subject) from "den Mann" (accusative object) — both calls looked
// identical. Combined with an inflect_pronoun_custom override that keeps
// displaying the noun's name instead of falling back to an English pronoun,
// case-correct German articles are now reachable through the trait alone.
use ranting::*;
use std::fmt;

#[derive(Clone, Copy)]
struct GermanNoun; // "Mann" (man), masculine

impl fmt::Display for GermanNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Mann")
    }
}

impl Ranting for GermanNoun {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("Mann", uc)
    }
    fn subjective(&self) -> &str {
        "he"
    }
    fn is_plural(&self) -> bool {
        false
    }
    fn inflect(&self, _to_plural: bool, uc: bool) -> String {
        uc_1st_if("Mann", uc)
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
        uc: bool,
    ) -> Option<String> {
        if article != "the" {
            return None;
        }
        let form = match case {
            GrammaticalCase::Objective => "den",
            _ => "der",
        };
        Some(uc_1st_if(form, uc))
    }

    fn inflect_pronoun_custom(
        &self,
        _subject: &str,
        _case: PronounCase,
        _class: NounClass,
        _as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        // A case-declining fork typically keeps showing the noun's own name
        // here rather than collapsing to an English-style pronoun.
        Some(uc_1st_if("Mann", uc))
    }
}

#[test]
fn nominative_and_accusative_articles_differ_by_case_marker() {
    let m = GermanNoun;
    assert_eq!(say!("{the =0} bellt.", m), "Der Mann bellt.");
    assert_eq!(say!("Ich sah {the @0}.", m), "Ich sah den Mann.");
}

#[test]
fn bare_placeholder_with_no_case_marker_reports_name_case() {
    // No case marker on the noun (`{the noun}`) — the hook can't know
    // subject-vs-object here, matching English's own lack of distinction.
    struct Probe;
    thread_local! {
        static SEEN: std::cell::RefCell<Vec<GrammaticalCase>> = const { std::cell::RefCell::new(Vec::new()) };
    }
    impl fmt::Display for Probe {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "dog")
        }
    }
    impl Ranting for Probe {
        fn name(&self, uc: bool) -> String {
            uc_1st_if("dog", uc)
        }
        fn subjective(&self) -> &str {
            "it"
        }
        fn is_plural(&self) -> bool {
            false
        }
        fn inflect(&self, _to_plural: bool, uc: bool) -> String {
            uc_1st_if("dog", uc)
        }
        fn skip_article(&self) -> bool {
            false
        }
        fn inflect_article_custom(
            &self,
            _article: &str,
            _noun_singular: &str,
            case: GrammaticalCase,
            _class: NounClass,
            _as_plural: bool,
            _uc: bool,
        ) -> Option<String> {
            SEEN.with(|s| s.borrow_mut().push(case));
            None // fall back to English so output is unaffected
        }
    }

    let _ = say!("{the 0}", Probe);
    SEEN.with(|s| assert_eq!(s.borrow().as_slice(), &[GrammaticalCase::Name]));
}
