use ranting::*;
use ranting_derive::*;

#[test]
fn test_irregular_plural_with_noun() {
    let child = Noun::new("child", "it");
    // Note: Placeholders at sentence start are capitalized by default
    let result = say!("{+0}", child);
    assert_eq!(result, "Children");
}

#[test]
fn test_irregular_singular_with_noun() {
    let children = Noun::new("children", "they");
    let result = say!("{-0}", children);
    assert_eq!(result, "Child");
}

#[test]
fn test_irregular_plural_with_article() {
    let person = Noun::new("person", "it");
    let result = say!("{the +0}", person);
    assert!(result.contains("people"));
}

#[test]
fn test_irregular_plural_with_verb() {
    let mouse = Noun::new("mouse", "it");
    // Use + to pluralize the noun
    let result = say!("{+0 run}", mouse);
    assert_eq!(result, "Mice run");
}

#[test]
fn test_irregular_plural_mid_sentence() {
    let person = Noun::new("person", "it");
    // Lowercase placeholder when not at sentence start (comma prefix for lowercase)
    let result = say!("The person is actually a {,+0}.", person);
    assert_eq!(result, "The person is actually a people.");
}

#[test]
fn test_fallback_regular_plural() {
    let book = Noun::new("book", "it");
    let result = say!("{+0}", book);
    assert_eq!(result, "Books");
}

#[test]
fn test_irregular_plural_round_trip() {
    let person = Noun::new("person", "it");
    let plural = say!("One {,+0} is here.", person);
    // Extract the lowercase plural from the sentence
    assert!(plural.contains("people"));
}

#[test]
fn test_derive_macro_irregular_plural() {
    #[derive_ranting]
    #[ranting(name = "child", subject = "it")]
    struct Child;

    let result = say!("{+0}", Child);
    assert_eq!(result, "Children");
}

#[test]
fn test_derive_macro_irregular_singular() {
    #[derive_ranting]
    #[ranting(name = "children", subject = "they")]
    struct Children;

    let result = say!("{-0}", Children);
    assert_eq!(result, "Child");
}
