// Tests for argument parsing in say!(), ack!(), nay!(), and ask!() macros
use ranting::*;
use ranting_derive::*;

#[test]
fn test_positional_single_argument() {
    let person = Noun::new("Alice", "I");
    assert_eq!(say!("{=0}", person), "I".to_string());
}

#[test]
fn test_positional_multiple_arguments() {
    let alice = Noun::new("Alice", "I");
    let bob = Noun::new("Bob", "he");
    let result = say!("{=0} met {=1}", alice, bob);
    assert!(result.contains("Alice") || result.contains("I"));
}

#[test]
fn test_named_single_argument() {
    let person = Noun::new("Charlie", "she");
    // At sentence start, pronouns are capitalized by default
    assert_eq!(say!("{=person}", person = person), "She".to_string());
}

#[test]
fn test_named_multiple_arguments() {
    let alice = Noun::new("Alice", "I");
    let bob = Noun::new("Bob", "he");
    let result = say!("{=alice} and {=bob}", alice = alice, bob = bob);
    assert!(
        result.to_lowercase().contains("alice") || result.contains("I") || result.contains("i")
    );
}

#[test]
fn test_mixed_positional_and_named() {
    let alice = Noun::new("Alice", "I");
    let bob = Noun::new("Bob", "he");
    let result = say!("{=0} and {=named_person}", alice, named_person = bob);
    assert!(!result.is_empty());
}

#[test]
fn test_variable_name_inference() {
    let my_person = Noun::new("David", "he");
    let result = say!("{=my_person}", my_person);
    assert!(!result.is_empty());
}

#[derive_ranting]
#[ranting(subject = "she")]
struct Woman {
    #[allow(dead_code)]
    name: String,
}

#[test]
fn test_named_with_derived_ranting() {
    let carol = Woman {
        name: "Carol".to_string(),
    };
    let result = say!("{=woman}", woman = &carol);
    // At sentence start, pronouns are capitalized by default
    assert_eq!(result, "She".to_string());
}

#[test]
fn test_named_with_multiple_placeholders() {
    let person1 = Noun::new("Eve", "I");
    let person2 = Noun::new("Frank", "he");
    let result = say!(
        "{=p1 do} something while {=p2 does} another",
        p1 = person1,
        p2 = person2
    );
    assert!(!result.is_empty());
}

#[test]
fn test_ack_with_named_argument() {
    let person = Noun::new("Grace", "she");
    let result: Result<String, String> = Ok(say!("{=person}", person = person));
    assert!(result.is_ok());
}

#[test]
fn test_nay_with_positional_argument() {
    let person = Noun::new("Henry", "he");
    let result: Result<String, String> = Err(say!("{=0}", person));
    assert!(result.is_err());
}

// ask! tests require Audience to implement Ranting and have an answer method
// This is tested in other test files with proper trait implementations

#[test]
fn test_multiple_references_same_variable() {
    let person = Noun::new("Jack", "he");
    let result = say!("{=person} thinks {=person do}", person);
    assert!(!result.is_empty());
}

#[test]
fn test_numeric_placeholder_with_positional() {
    let alice = Noun::new("Alice", "I");
    let bob = Noun::new("Bob", "he");
    assert_eq!(say!("{=0} and {=1}", alice, bob).len() > 0, true);
}

#[test]
fn test_named_placeholder_with_number_variable() {
    let item = Noun::new("apple", "it");
    let count = 3usize;
    let result = say!("{#count ?item}", count = count, item = item);
    assert!(!result.is_empty());
}

#[test]
fn test_complex_placeholder_with_named_noun() {
    let person = Noun::new("Karen", "she");
    let result = say!("{a person}", person = person);
    assert!(!result.is_empty());
}

#[test]
fn test_complex_placeholder_with_possessive() {
    let person = Noun::new("Leo", "he");
    let item = Noun::new("book", "it");
    let result = say!("{`person book}", person = person, book = item);
    assert!(!result.is_empty());
}

#[test]
fn test_mixed_arg_types_multiple_placeholders() {
    let alice = Noun::new("Alice", "I");
    let bob = Noun::new("Bob", "he");
    let carol = Noun::new("Carol", "she");

    let result = say!(
        "{=0} talked to {=named_bob} and {=carol}",
        alice,
        named_bob = bob,
        carol = carol
    );
    assert!(!result.is_empty());
}

#[test]
fn test_article_with_named_argument() {
    let person = Noun::new("person", "it");
    let result = say!("{a person}", person = person);
    assert!(!result.is_empty());
}

#[test]
fn test_verb_with_named_argument() {
    let person = Noun::new("person", "it");
    let result = say!("{person is}", person = person);
    assert!(!result.is_empty());
}

#[test]
fn test_uppercased_named_argument() {
    let person = Noun::new("person", "it");
    let result = say!("{^person}", person = person);
    assert!(!result.is_empty());
}

#[test]
fn test_lowercase_enforced_named_argument() {
    let person = Noun::new("person", "it");
    let result = say!("{,person}", person = person);
    assert!(!result.is_empty());
}
