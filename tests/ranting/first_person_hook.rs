// (c) Roel Kluin 2026 MIT
// Tests for ROADMAP.md Phase 6 item 16: `Ranting::is_first_person_subject_custom`, the hook
// that lets a fork whose first-person subject label isn't "I"/"we" still participate in
// `say_with!()`'s narration-viewpoint override (`narration::resolve_viewpoint`). This closes
// open question 1 of docs/superpowers/specs/2026-08-13-pronoun-inventory.md — previously
// `is_first_person_subject` was a hard-coded `matches!(subject, "I" | "we")` with no hook, so a
// German-labeled narrator ("ich"/"wir") got a silent no-op from `narration_person`.
use ranting::*;
use std::fmt;

struct GermanNarrator {
    name: &'static str,
    subject: &'static str,
}

impl fmt::Display for GermanNarrator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Ranting for GermanNarrator {
    fn name(&self, uc: bool) -> String {
        capitalize_if(self.name, uc)
    }
    fn subjective(&self) -> &str {
        self.subject
    }
    fn is_plural(&self) -> bool {
        self.subject == "wir"
    }
    fn inflect(
        &self,
        _to_plural: bool,
        uc: bool,
        _case: GrammaticalCase,
        _count: Option<PlaceholderCount>,
    ) -> String {
        capitalize_if(self.name, uc)
    }
    fn skip_article(&self) -> bool {
        true
    }

    fn is_first_person_subject_custom(&self, subject: &str) -> bool {
        matches!(subject, "ich" | "wir")
    }
}

/// Same declared subjects, but without the hook override — pins the pre-item-16 gap this hook
/// closes: a fork that forgets (or chooses not) to override `is_first_person_subject_custom`
/// gets a silent no-op, exactly as `is_first_person_subject` behaved before this hook existed.
struct UnhookedGermanNarrator {
    name: &'static str,
    subject: &'static str,
}

impl fmt::Display for UnhookedGermanNarrator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Ranting for UnhookedGermanNarrator {
    fn name(&self, uc: bool) -> String {
        capitalize_if(self.name, uc)
    }
    fn subjective(&self) -> &str {
        self.subject
    }
    fn is_plural(&self) -> bool {
        self.subject == "wir"
    }
    fn inflect(
        &self,
        _to_plural: bool,
        uc: bool,
        _case: GrammaticalCase,
        _count: Option<PlaceholderCount>,
    ) -> String {
        capitalize_if(self.name, uc)
    }
    fn skip_article(&self) -> bool {
        true
    }
}

#[test]
fn hook_lets_non_english_first_person_label_get_viewpoint_override() {
    let narrator = GermanNarrator {
        name: "Klaus",
        subject: "ich",
    };
    let third = NarrationContext::new().narration_person(Person::Third);
    assert_eq!(say_with!(third, "{=0 <arrive}", narrator), "They arrived");

    let second = NarrationContext::new().narration_person(Person::Second);
    assert_eq!(say_with!(second, "{=0 <arrive}", narrator), "You arrived");
}

#[test]
fn hook_covers_plural_first_person_label_too() {
    let narrator = GermanNarrator {
        name: "Wir",
        subject: "wir",
    };
    let third = NarrationContext::new().narration_person(Person::Third);
    assert_eq!(say_with!(third, "{=0 <arrive}", narrator), "They arrived");
}

#[test]
fn without_the_hook_a_non_english_label_is_a_silent_no_op() {
    // `narration_person` has no effect at all here: the third-person override result is
    // byte-identical to plain `say!()` (no context, no override in play). The unrecognized
    // "ich" label separately degrades to "it"'s forms — the pre-existing, documented fallback
    // for any label `SubjectPronoun::from_str` doesn't recognize — but that happens regardless
    // of `narration_person`, which is exactly the silent no-op this hook fixes.
    let narrator = UnhookedGermanNarrator {
        name: "Klaus",
        subject: "ich",
    };
    let third = NarrationContext::new().narration_person(Person::Third);
    assert_eq!(say_with!(third, "{=0 <arrive}", narrator), "It arrived");
    assert_eq!(
        say_with!(third, "{=0 <arrive}", narrator),
        say!("{=0 <arrive}", narrator)
    );
}

#[test]
fn english_first_person_labels_are_unaffected_by_the_hook_existing() {
    let hero = Noun::new("Hero", "I");
    let third = NarrationContext::new().narration_person(Person::Third);
    assert_eq!(say_with!(third, "{=0 <walk}", hero), "They walked");
}
