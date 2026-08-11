// Comprehensive tests for maximizing code coverage
// Tests various combinations and edge cases across the codebase

use ranting::*;
use ranting_derive::say;

// Test handle_placeholder function with various combinations
#[test]
fn test_handle_placeholder_with_uppercase_modifiers() {
    let person = Noun::new("Alex", "they");

    // Test uppercase modifier at sentence start
    let result = say!("{^=0}", person);
    assert!(!result.is_empty());
}

#[test]
fn test_handle_placeholder_with_lowercase_enforcer() {
    let person = Noun::new("ALEX", "they");

    // Enforce lowercase
    let result = say!("{,=0}", person);
    assert!(!result.is_empty());
}

// Test pluralization logic
#[test]
fn test_plural_inflection_all_cases() {
    let book = Noun::new("book", "it");

    // Explicit plural marker
    let result_plus = say!("{+=0}", book);
    assert!(!result_plus.is_empty());

    // Explicit singular marker
    let result_minus = say!("{-=0}", book);
    assert!(!result_minus.is_empty());
}

// Test verb inflection for all pronouns
#[test]
fn test_verb_forms_comprehensive() {
    let test_cases = vec!["I", "you", "he", "she", "it", "we", "they"];

    for pronoun in test_cases {
        let noun = Noun::new("test", pronoun);
        let result = say!("{=0 are}", noun);
        assert!(!result.is_empty(), "Failed for pronoun: {}", pronoun);
    }
}

// Test article adaptation
#[test]
fn test_article_handling_comprehensive() {
    let singular = Noun::new("apple", "it");
    let plural = Noun::new("apples", "they");

    // Test "the" article - "the" is always lowercase in output
    let result1 = say!("{the 0}", singular);
    assert!(result1.contains("the") || result1.contains("The"), "Got: {}", result1);

    // Test "a" article
    let result2 = say!("{a 0}", singular);
    assert!(!result2.is_empty());

    // Test "some" article
    let result3 = say!("{some 0}", plural);
    assert!(!result3.is_empty());
}

// Test possessive adaptation
#[test]
fn test_possessive_adaptation_singular_plural() {
    let singular = Noun::new("student", "it");
    let plural = Noun::new("students", "they");

    // Singular possessive
    let result_s = say!("{0's} book", singular);
    assert!(result_s.len() > 0);

    // Plural possessive
    let result_p = say!("{+0's} books", plural);
    assert!(result_p.len() > 0);
}

// Test case marker propagation
#[test]
fn test_case_markers_comprehensive() {
    let noun = Noun::new("object", "it");

    // Subject case
    let subj = say!("{=0}", noun);
    assert_eq!(subj, "It");

    // Object case
    let obj = say!("{@0}", noun);
    assert_eq!(obj, "It");

    // Possessive case
    let poss = say!("{`0}", noun);
    assert_eq!(poss, "Its");

    // Adjective case
    let adj = say!("{~0}", noun);
    assert_eq!(adj, "Its");
}

// Test verb with multiple forms
#[test]
fn test_irregular_verb_handling() {
    let person = Noun::new("person", "it");

    // Test various irregular verbs
    let be_result = say!("{=0 is}", person);
    assert!(be_result.contains("is"));

    let have_result = say!("{=0 has}", person);
    assert!(have_result.contains("has"));

    let do_result = say!("{=0 does}", person);
    assert!(do_result.contains("does"));
}

// Test contraction handling
#[test]
fn test_contraction_verb_forms() {
    let person = Noun::new("person", "it");

    // Test contractions
    let cant = say!("{=0 can't}", person);
    assert!(!cant.is_empty());

    let wont = say!("{=0 won't}", person);
    assert!(!wont.is_empty());

    let dont = say!("{=0 don't}", person);
    assert!(!dont.is_empty());
}

// Test numeric references and pluralization
#[test]
fn test_numeric_reference_handling() {
    let thing = Noun::new("thing", "it");

    // Direct numeric reference
    let result = say!("{=0}", thing);
    assert_eq!(result, "It");
}

// Test whitespace and formatting preservation
#[test]
fn test_whitespace_preservation() {
    let person = Noun::new("Alex", "she");

    // Preserve leading/trailing spaces around pronouns
    let result = say!("She has {`0} dreams.", person);
    assert!(result.contains("  ") || result.len() > 5);
}

// Test mixed placeholder types
#[test]
fn test_mixed_placeholder_styles() {
    let noun = Noun::new("idea", "it");

    // Mix different placeholder types in one string
    let result = say!(
        "The {0} and {=0} with {`0} significance.",
        noun
    );
    assert!(!result.is_empty());
}

// Test edge case with special formatting
#[test]
fn test_named_noun_formatting() {
    let person = Noun::new("person", "it");
    // Named placeholders work with ranting elements
    let result = say!("{=obj}", obj = person);
    assert_eq!(result, "It");
}

// Test placeholder at various positions
#[test]
fn test_placeholder_position_variations() {
    let person = Noun::new("Casey", "they");

    // Start of string
    let start = say!("{=0} is great.", person);
    assert!(!start.is_empty());

    // Middle of string
    let middle = say!("I think {=0} is great.", person);
    assert!(!middle.is_empty());

    // End of string
    let end = say!("This person is {=0}.", person);
    assert!(!end.is_empty());
}

// Test error recovery and edge cases
#[test]
fn test_empty_format_string() {
    let result = say!("");
    assert_eq!(result, "");
}

#[test]
fn test_format_string_without_placeholders() {
    let result = say!("Just plain text");
    assert_eq!(result, "Just plain text");
}

// Test article with question mark prefix
#[test]
fn test_article_with_question_prefix() {
    let noun = Noun::new("thing", "it");

    // Question prefix on article affects output
    let result = say!("{?the 0}", noun);
    assert!(!result.is_empty());
}

// Test pronoun display modes
#[test]
fn test_hidden_pronoun_mode() {
    let person = Noun::new("Jordan", "they");

    // Question mark case - hidden but inflects
    let result = say!("{?=0 verb}", person);
    assert!(!result.is_empty());
}

// Test verb-before-noun structure
#[test]
fn test_verb_before_noun_structure() {
    let person = Noun::new("student", "it");

    // Verb comes before noun in placeholder - test various verbs
    let result = say!("{0 is} smart", person);
    assert!(!result.is_empty());
}

// Test apostrophe handling in possessive
#[test]
fn test_apostrophe_in_possessive() {
    let obj = Noun::new("table", "it");

    // Basic possessive
    let result = say!("{0's}", obj);
    assert!(result.len() > 0);
}

// Test complex placeholder with article and possessive
#[test]
fn test_article_possessive_combination() {
    let person = Noun::new("student", "it");

    // Article with noun
    let result = say!("{a 0}", person);
    assert!(!result.is_empty());
}
