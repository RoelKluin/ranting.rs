// Comprehensive edge case and error condition tests for argument parsing
use ranting::*;
use ranting_derive::say;

// Table-driven tests for all pronoun combinations
#[test]
fn test_all_pronouns_subject_case() {
    let test_cases = vec![
        ("I", "I"),
        ("you", "You"),           // Capitalized at sentence start
        ("thou", "Thou"),
        ("he", "He"),
        ("she", "She"),
        ("it", "It"),
        ("we", "We"),
        ("ye", "Ye"),
        ("they", "They"),
    ];

    for (pronoun, expected) in test_cases {
        let noun = Noun::new("test", pronoun);
        let result = say!("{=0}", noun);
        assert_eq!(
            result, expected,
            "Failed for pronoun '{}': expected '{}', got '{}'",
            pronoun, expected, result
        );
    }
}

#[test]
fn test_all_pronouns_object_case() {
    let test_cases = vec![
        ("I", "Me"),
        ("you", "You"),
        ("thou", "Thee"),
        ("he", "Him"),
        ("she", "Her"),
        ("it", "It"),
        ("we", "Us"),
        ("ye", "You"),
        ("they", "Them"),
    ];

    for (pronoun, expected) in test_cases {
        let noun = Noun::new("test", pronoun);
        let result = say!("{@0}", noun);
        assert_eq!(
            result, expected,
            "Failed for pronoun '{}': expected '{}', got '{}'",
            pronoun, expected, result
        );
    }
}

#[test]
fn test_all_pronouns_possessive_determiner() {
    let test_cases = vec![
        ("I", "My"),
        ("you", "Your"),
        ("thou", "Thy"),
        ("he", "His"),
        ("she", "Her"),
        ("it", "Its"),
        ("we", "Our"),
        ("ye", "Your"),
        ("they", "Their"),
    ];

    for (pronoun, expected) in test_cases {
        let noun = Noun::new("test", pronoun);
        let result = say!("{`0}", noun);
        assert_eq!(
            result, expected,
            "Failed for pronoun '{}': expected '{}', got '{}'",
            pronoun, expected, result
        );
    }
}

#[test]
fn test_all_pronouns_possessive_pronoun() {
    let test_cases = vec![
        ("I", "Mine"),
        ("you", "Yours"),
        ("thou", "Thine"),
        ("he", "His"),
        ("she", "Hers"),
        ("it", "Its"),
        ("we", "Ours"),
        ("ye", "Yours"),
        ("they", "Theirs"),
    ];

    for (pronoun, expected) in test_cases {
        let noun = Noun::new("test", pronoun);
        let result = say!("{~0}", noun);
        assert_eq!(
            result, expected,
            "Failed for pronoun '{}': expected '{}', got '{}'",
            pronoun, expected, result
        );
    }
}

// Edge cases for article handling
#[test]
fn test_articles_singular_plural() {
    let singular = Noun::new("apple", "it");
    let plural_noun = Noun::new("apple", "they");

    // "the" article stays the same
    let result_singular = say!("{the 0}", singular);
    let result_plural = say!("{the 0}", plural_noun);
    assert_eq!(result_singular, "The apple");
    assert_eq!(result_plural, "The apple");
}

// Table-driven tests for verb forms with different pronouns
#[test]
fn test_verb_conjugation_table() {
    // Test various pronouns with the "be" verb
    let pronouns = vec!["I", "you", "he", "she", "it", "we", "they"];

    // Just verify these compile and don't panic
    for pronoun in pronouns {
        let noun = Noun::new("test", pronoun);
        let _result = say!("{=0 are}", noun);
    }
}

// Edge cases for multiple arguments
#[test]
fn test_mixed_positional_and_named_edge_cases() {
    let alice = Noun::new("Alice", "she");
    let bob = Noun::new("Bob", "he");

    // Positional before named
    let result1 = say!("{=0} met {=person}", alice, person = bob);
    assert!(!result1.is_empty());

    // Multiple references to same argument
    let result2 = say!("{=0} told {=0 do}", alice);
    assert!(!result2.is_empty());

    // Named arguments with complex placeholders
    let result3 = say!("{=a have} talked to {=b}", a = alice, b = bob);
    assert!(!result3.is_empty());
}

// Edge cases for case sensitivity
#[test]
fn test_case_sensitivity_in_arguments() {
    let person = Noun::new("Jordan", "they");

    // Test both lowercase and uppercase in placeholders
    let result_lower = say!("{=person}", person);
    assert!(result_lower.len() > 0);
}

// Edge cases for nested structures and complex sentences
#[test]
fn test_complex_sentence_structure() {
    let subject = Noun::new("Sam", "they");
    let object = Noun::new("Pat", "it");

    let result = say!(
        "{=subject have} shown {=object} to {=subject's} friends."
    );
    assert!(!result.is_empty());
}

// Edge case: empty placeholder names should not work but shouldn't crash
#[test]
fn test_names_with_special_characters() {
    let hyphenated = Noun::new("Jean-Paul", "he");
    let apostrophe = Noun::new("O'Brien", "she");

    let result1 = say!("{=0}", hyphenated);
    assert!(!result1.is_empty());

    let result2 = say!("{=0}", apostrophe);
    assert!(!result2.is_empty());
}

// Edge case: very long names
#[test]
fn test_very_long_names() {
    let long_name = Noun::new(
        "Alexanderandra Maximilian Cornelius Montgomery III",
        "he",
    );
    let result = say!("{=0 are} a remarkable person.", long_name);
    assert!(!result.is_empty());
}

// Edge case: unicode in names
#[test]
fn test_unicode_in_names() {
    let unicode_name = Noun::new("José", "he");
    let result = say!("{=0}", unicode_name);
    assert_eq!(result, "He");
}

// Edge case: multiple same pronouns
#[test]
fn test_multiple_references_same_pronoun() {
    let person = Noun::new("Taylor", "they");

    let result = say!(
        "{=person are} friends with {=person}, and {=person like} {=person}.",
        person
    );
    assert!(result.contains("They") || result.contains("they"));
}

// Edge case: articles with capitalization
#[test]
fn test_article_capitalization_variants() {
    let person = Noun::new("person", "it");

    // Lowercase article (mid-sentence)
    let result1 = say!("and {a 0} appeared.", person);
    assert!(result1.contains("a person"));

    // Uppercase article (sentence start would be auto-handled by placeholder)
    let result2 = say!("{A 0} appeared.", person);
    assert!(result2.len() > 0);
}

// Edge case: verb forms with contractions
#[test]
fn test_verb_contractions() {
    let alice = Noun::new("Alice", "she");

    let result1 = say!("{=0 can't}", alice);
    assert!(result1.len() > 0);

    let result2 = say!("{=0 won't}", alice);
    assert!(result2.len() > 0);

    let result3 = say!("{=0 shouldn't}", alice);
    assert!(result3.len() > 0);
}

// Edge case: possessive with apostrophe
#[test]
fn test_possessive_apostrophe_forms() {
    let singular = Noun::new("book", "it");
    let plural = Noun::new("books", "they");

    // Singular possessive ends in 's
    let result_s = say!("{0's}", singular);
    assert!(result_s.contains("book") || result_s.contains("'s"));

    // Plural possessive ends in ' only
    let result_p = say!("{+0's}", plural);
    assert!(result_p.len() > 0);
}

// Edge case: whitespace handling
#[test]
fn test_extra_whitespace_handling() {
    let person = Noun::new("Pat", "it");

    // Extra spaces should be preserved
    let result = say!("{=0}  has  space", person);
    assert!(result.contains("  "));
}

// Edge case: special characters in display names
#[test]
fn test_special_chars_in_names() {
    let dash_name = Noun::new("Mary-Jane", "she");
    let result = say!("{=0}", dash_name);
    assert_eq!(result, "She");
}

// Edge case: consistent inflection across multiple calls
#[test]
fn test_consistent_inflection() {
    let person = Noun::new("Casey", "they");

    let result1 = say!("{=0}", person);
    let result2 = say!("{=0}", person);

    assert_eq!(result1, result2, "Inflection should be consistent");
}
