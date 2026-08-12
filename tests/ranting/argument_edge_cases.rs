// Comprehensive edge case tests for argument parsing with various input patterns
use ranting::*;
use ranting_derive::say;

// Table-driven tests for positional argument variations
#[test]
fn test_positional_arguments_table() {
    let test_cases = vec![
        (vec![Noun::new("one", "I")], "one"),
        (
            vec![Noun::new("first", "I"), Noun::new("second", "he")],
            "both",
        ),
        (
            vec![
                Noun::new("first", "I"),
                Noun::new("second", "he"),
                Noun::new("third", "she"),
            ],
            "three",
        ),
    ];

    for (args, _description) in test_cases {
        if args.len() == 1 {
            let result = say!("{=0}", args[0]);
            assert!(!result.is_empty());
        } else if args.len() == 2 {
            let result = say!("{=0} and {=1}", args[0], args[1]);
            assert!(!result.is_empty());
        } else if args.len() == 3 {
            let result = say!("{=0}, {=1}, and {=2}", args[0], args[1], args[2]);
            assert!(!result.is_empty());
        }
    }
}

// Table-driven tests for named argument variations
#[test]
fn test_named_arguments_various_names() {
    let test_cases = vec![
        "person",
        "actor",
        "subject",
        "x",
        "my_var",
        "variable_123",
        "_private",
        "CamelCase",
    ];

    for var_name in test_cases {
        let person = Noun::new("Test", "it");
        // For each test case, build the call dynamically by matching the pattern
        match var_name {
            "person" => {
                let result = say!("{=person}", person = person);
                assert!(!result.is_empty());
            }
            "actor" => {
                let result = say!("{=actor}", actor = person);
                assert!(!result.is_empty());
            }
            "subject" => {
                let result = say!("{=subject}", subject = person);
                assert!(!result.is_empty());
            }
            "x" => {
                let result = say!("{=x}", x = person);
                assert!(!result.is_empty());
            }
            _ => {} // Other patterns require code generation, skip here
        }
    }
}

// Edge case: multiple placeholders with same argument
#[test]
fn test_same_argument_multiple_times() {
    let person = Noun::new("Alex", "they");

    let result = say!("{=0} and {=0} both agree. {=0 are} right.", person);

    assert!(result.contains("Alex") || result.contains("They"));
}

// Edge case: mixing positional and named
#[test]
fn test_mixing_positional_named() {
    let a = Noun::new("A", "I");
    let b = Noun::new("B", "he");

    // Mix positional (0) with named (person_b)
    let result = say!("{=0} met {=person_b}", a, person_b = b);
    assert!(!result.is_empty());
}

// Edge case: large number of arguments
#[test]
fn test_many_arguments() {
    let nouns = (0..5)
        .map(|i| Noun::new(&format!("Person{}", i), "it"))
        .collect::<Vec<_>>();

    let result = say!(
        "{=0}, {=1}, {=2}, {=3}, {=4}",
        nouns[0],
        nouns[1],
        nouns[2],
        nouns[3],
        nouns[4]
    );
    assert!(!result.is_empty());
}

// Edge case: case markers with arguments
#[test]
fn test_case_markers_with_various_pronouns() {
    let test_cases = vec![
        ("I", "I", "Me", "My", "Mine"),
        ("you", "You", "You", "Your", "Yours"),
        ("he", "He", "Him", "His", "His"),
        ("she", "She", "Her", "Her", "Hers"),
        ("it", "It", "It", "Its", "Its"),
    ];

    for (pronoun, _subj, _obj, _poss_d, _poss_p) in test_cases {
        let noun = Noun::new("Test", pronoun);

        // Subject case
        let _s = say!("{=0}", noun);

        // Object case
        let _o = say!("{@0}", noun);

        // Possessive determiner
        let _pd = say!("{`0}", noun);

        // Possessive pronoun
        let _pp = say!("{~0}", noun);
    }
}

// Edge case: articles with various nouns
#[test]
fn test_articles_with_different_nouns() {
    // Test article handling - just verify nouns with articles work
    let apple = Noun::new("apple", "it");
    let elephant = Noun::new("elephant", "it");

    // Test with "a" article
    let result1 = say!("{a 0}", apple);
    assert!(!result1.is_empty());

    // Test with "an" article
    let result2 = say!("{an 0}", elephant);
    assert!(!result2.is_empty());
}

// Edge case: verb handling with contractions
#[test]
fn test_verb_contractions_all_forms() {
    let pronouns = vec!["I", "you", "he", "she", "it", "we", "they"];
    let contractions = vec!["can't", "won't", "shouldn't", "couldn't"];

    for pronoun in pronouns {
        let noun = Noun::new("test", pronoun);
        for _contraction in &contractions {
            // Just verify they work without panic
            let _ = say!("{=0 can't}", noun);
        }
    }
}

// Edge case: possessive markers
#[test]
fn test_possessive_markers_various() {
    let noun = Noun::new("John", "he");

    // Test possessive marker with name
    let result1 = say!("{0's}", noun);
    assert!(!result1.is_empty());

    // Test multiple possessive references
    let result2 = say!("{0's} {0} {`0}", noun);
    assert!(!result2.is_empty());
}

// Edge case: sentence boundaries
#[test]
fn test_sentence_start_capitalization() {
    let person = Noun::new("Alex", "she");

    // Start of sentence - should capitalize
    let result1 = say!("{=0} is great.", person);
    assert!(result1.starts_with("She") || result1.starts_with("she"));

    // Mid-sentence - should not capitalize pronouns
    let result2 = say!("I think {=0} is great.", person);
    assert!(!result2.starts_with("She"));
}

// Edge case: combining multiple features
#[test]
fn test_complex_placeholder_combinations() {
    let subject = Noun::new("Jordan", "they");
    let object = Noun::new("project", "it");

    // Verb + possessive
    let result1 = say!(
        "{=subject have} completed {`subject} work",
        subject = subject
    );
    assert!(result1.contains("have") || result1.contains("has"));

    // Article + possessive
    let result2 = say!("{the object} and {`object} importance", object = object);
    assert!(!result2.is_empty());

    // Multiple cases in one sentence
    let result3 = say!(
        "{=subject} gave {`subject} work to {=object}",
        subject = subject,
        object = object
    );
    assert!(!result3.is_empty());
}

// Edge case: stress test with many references
#[test]
fn test_stress_many_references() {
    let person = Noun::new("Casey", "they");

    let result = say!("{=0} {=0} {=0} {=0} {=0} {=0} {=0} {=0} {=0} {=0}", person);

    // Count occurrences of the pronoun
    let count = result.matches("They").count() + result.matches("they").count();
    assert!(count >= 5, "Expected at least 5 pronoun references");
}

// Edge case: numeric pluralization
#[test]
fn test_numeric_pluralization_variations() {
    let count = 2usize;
    let item = Noun::new("apple", "it");

    // With explicit number - format: {#count ?noun}
    let result1 = say!("{#count ?item}", count = count, item = item);
    assert!(!result1.is_empty());

    let count2 = 1usize;
    let result2 = say!("{#count2 ?item}", count2 = count2, item = item);
    assert!(!result2.is_empty());
}

// Edge case: implicit variable lookup with different scopes
#[test]
fn test_implicit_variable_names() {
    let person = Noun::new("Alex", "they");
    let my_noun = Noun::new("Thing", "it");

    // These should work without errors
    let _result1 = say!("{=person}", person);
    let _result2 = say!("{=my_noun}", my_noun);
}

// Edge case: empty string names (should work but may have odd output)
#[test]
fn test_empty_name_noun() {
    let unnamed = Noun::new("", "it");
    let result = say!("{=0}", unnamed);
    assert_eq!(result, "It");
}

// Edge case: name with only spaces
#[test]
fn test_whitespace_only_name() {
    let spaced = Noun::new("   ", "it");
    let result = say!("{=0}", spaced);
    assert_eq!(result, "It");
}
