//! Phase 3 item 7: "Recursive Type Inflection" — `Many<T>` (wraps `Vec<T>`), `Maybe<T>`
//! (wraps `Option<T>`), and the direct `Box<T>` blanket impl, for `T: Ranting`.
//!
//! `Vec<T>` and `Option<T>` cannot get a blanket `Ranting` impl directly — `Ranting: Display`
//! requires `Vec<T>`/`Option<T>` to implement `Display`, but that's an orphan-rule violation
//! (foreign trait, foreign non-`#[fundamental]` type) regardless of `T`. `Box<T>` doesn't have
//! this problem since `std` already provides `Display` for it. See `src/collections.rs` for the
//! full writeup.

use ranting::*;

#[test]
fn many_empty_is_plural_they_and_skips_article() {
    let none: Many<Noun> = Many(vec![]);
    // Zero items is treated as plural ("they") rather than singular, same as "zero cats" takes
    // plural agreement in English ("there are no cats", not "there is no cat").
    assert!(none.is_plural());
    assert_eq!(none.subjective(), "they");
    assert_eq!(none.name(false), "");
    assert!(none.skip_article());
    assert_eq!(
        say!("There {=none are} no items."),
        "There they are no items.".to_string()
    );
    assert_eq!(say!("Items: {none}."), "Items: .".to_string());
}

#[test]
fn many_single_item_delegates_to_that_item() {
    let solo = Many(vec![Noun::new("Alice", "she")]);
    assert!(!solo.is_plural());
    assert_eq!(solo.subjective(), "she");
    assert_eq!(say!("{=solo are} ready."), "She is ready.".to_string());
    assert_eq!(say!("{solo}"), "Alice".to_string());
}

#[test]
fn many_multiple_items_join_names_and_pluralize() {
    let heroes = Many(vec![Noun::new("Alice", "she"), Noun::new("Bob", "he")]);
    assert!(heroes.is_plural());
    assert_eq!(heroes.subjective(), "they");
    assert_eq!(say!("{=heroes are} ready."), "They are ready.".to_string());
    assert_eq!(say!("{heroes}"), "Alice and Bob".to_string());
}

#[test]
fn many_three_items_oxford_comma_free_join() {
    let trio = Many(vec![
        Noun::new("Alice", "she"),
        Noun::new("Bob", "he"),
        Noun::new("Carl", "he"),
    ]);
    assert_eq!(say!("{trio}"), "Alice, Bob and Carl".to_string());
    assert_eq!(
        say!("{=trio are} present."),
        "They are present.".to_string()
    );
}

#[test]
fn many_uppercase_applies_to_first_char_only() {
    let heroes = Many(vec![Noun::new("alice", "she"), Noun::new("bob", "he")]);
    assert_eq!(
        say!("{^heroes} are here."),
        "Alice and bob are here.".to_string()
    );
}

#[test]
fn maybe_none_is_empty_and_skips_article() {
    let none: Maybe<Noun> = Maybe(None);
    assert!(!none.is_plural());
    assert_eq!(none.subjective(), "it");
    assert_eq!(none.name(false), "");
    assert!(none.skip_article());
    assert_eq!(
        say!("There {=none are} nothing."),
        "There it is nothing.".to_string()
    );
    assert_eq!(say!("Name: {none}."), "Name: .".to_string());
}

#[test]
fn maybe_some_delegates_entirely_to_the_item() {
    let some = Maybe(Some(Noun::new("Alex", "they")));
    assert!(some.is_plural());
    assert_eq!(some.subjective(), "they");
    assert_eq!(say!("{=some are} here."), "They are here.".to_string());
    assert_eq!(
        say!("This is {`some} book."),
        "This is their book.".to_string()
    );
}

#[test]
fn boxed_noun_delegates_entirely_to_the_inner_value() {
    let boxed: Box<Noun> = Box::new(Noun::new("Carl", "he"));
    assert!(!boxed.is_plural());
    assert_eq!(boxed.subjective(), "he");
    assert_eq!(say!("{=boxed are} boxed."), "He is boxed.".to_string());
    assert_eq!(
        say!("{`boxed} name is {boxed}."),
        "His name is Carl.".to_string()
    );
}

#[test]
fn box_of_many_and_many_of_box_compose() {
    // Nesting: Box<Many<Noun>> and Many<Box<Noun>> both need to work, since the wrappers are
    // ordinary Ranting impls, not special-cased terminal types.
    let boxed_many: Box<Many<Noun>> = Box::new(Many(vec![
        Noun::new("Alice", "she"),
        Noun::new("Bob", "he"),
    ]));
    assert_eq!(
        say!("{=boxed_many are} here."),
        "They are here.".to_string()
    );

    let many_boxed: Many<Box<Noun>> = Many(vec![
        Box::new(Noun::new("Alice", "she")),
        Box::new(Noun::new("Bob", "he")),
    ]);
    assert_eq!(say!("{many_boxed}"), "Alice and Bob".to_string());
}

/// A struct that overrides `inflect_verb_custom` and `inflect_pronoun_custom`, to verify that
/// `Box`/`Many`/`Maybe` forward the custom-inflection hooks rather than silently falling back to
/// English defaults (the trait's own default impls return `None`, so a wrapper that doesn't
/// forward would lose the override).
#[derive(Clone, Copy)]
struct PirateNoun;

impl std::fmt::Display for PirateNoun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name(false))
    }
}

impl Ranting for PirateNoun {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("swab", uc)
    }

    fn subjective(&self) -> &str {
        "he"
    }

    fn is_plural(&self) -> bool {
        false
    }

    fn inflect(&self, to_plural: bool, uc: bool, _case: GrammaticalCase) -> String {
        if to_plural {
            uc_1st_if("swabs", uc)
        } else {
            uc_1st_if("swab", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_verb_custom(
        &self,
        _subject: &str,
        verb: &str,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        match verb {
            "be" | "is" | "am" | "are" => Some(uc_1st_if("be", uc)),
            _ => None,
        }
    }

    fn inflect_pronoun_custom(
        &self,
        subject: &str,
        case: PronounCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        if subject == "he" && case == PronounCase::Subjective {
            Some(uc_1st_if("arr", uc))
        } else {
            None
        }
    }
}

#[test]
fn box_forwards_custom_inflection_hooks() {
    let pirate: Box<PirateNoun> = Box::new(PirateNoun);
    assert_eq!(say!("{=pirate are} here."), "Arr be here.".to_string());
}

#[test]
fn maybe_forwards_custom_inflection_hooks() {
    let pirate: Maybe<PirateNoun> = Maybe(Some(PirateNoun));
    assert_eq!(say!("{=pirate are} here."), "Arr be here.".to_string());
}

#[test]
fn many_single_item_forwards_custom_inflection_hooks() {
    let pirate: Many<PirateNoun> = Many(vec![PirateNoun]);
    assert_eq!(say!("{=pirate are} here."), "Arr be here.".to_string());
}

#[test]
fn many_multi_item_falls_back_to_english_for_custom_hooks() {
    // With more than one item there's no single element to delegate a per-item custom hook to,
    // so the collection falls back to the built-in English rules rather than guessing.
    let pirates: Many<PirateNoun> = Many(vec![PirateNoun, PirateNoun]);
    assert_eq!(say!("{=pirates are} here."), "They are here.".to_string());
}
