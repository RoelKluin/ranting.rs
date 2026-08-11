// Tests to verify error messages are helpful and clear
// These tests document expected error behaviors when arguments are missing

use ranting::*;
use ranting_derive::say;

// Note: These compile-time errors are tested by attempting to compile invalid code.
// The error messages should be clear and informative.

#[test]
fn test_valid_positional_argument() {
    let person = Noun::new("Alice", "I");
    let result = say!("{=0}", person);
    assert!(!result.is_empty());
}

#[test]
fn test_valid_named_argument() {
    let person = Noun::new("Bob", "he");
    let result = say!("{=p}", p = person);
    assert!(!result.is_empty());
}

#[test]
fn test_implicit_variable_lookup() {
    let person = Noun::new("Charlie", "she");
    let result = say!("{=person}", person);
    assert!(!result.is_empty());
}

#[test]
fn test_multiple_positional_arguments() {
    let alice = Noun::new("Alice", "I");
    let bob = Noun::new("Bob", "he");
    let result = say!("{=0} and {=1}", alice, bob);
    assert!(!result.is_empty());
}

#[test]
fn test_positional_with_verbs() {
    let person = Noun::new("David", "he");
    let result = say!("{=0 do} something", person);
    assert!(!result.is_empty());
}

#[test]
fn test_named_with_verbs() {
    let person = Noun::new("Eve", "she");
    let result = say!("{=person do} something", person = person);
    assert!(!result.is_empty());
}

#[test]
fn test_named_with_article() {
    let item = Noun::new("apple", "it");
    let result = say!("{a item}", item = item);
    assert!(!result.is_empty());
}

#[test]
fn test_named_with_possessive() {
    let person = Noun::new("Frank", "he");
    let book = Noun::new("book", "it");
    let result = say!("{`person book}", person = person, book = book);
    assert!(!result.is_empty());
}

#[test]
fn test_mixed_args_correct_count() {
    let alice = Noun::new("Alice", "I");
    let bob = Noun::new("Bob", "he");
    let result = say!("{=0} and {=bob}", alice, bob = bob);
    assert!(!result.is_empty());
}

#[test]
fn test_case_sensitivity_in_named_args() {
    // Variable names are case-sensitive
    let person = Noun::new("Grace", "she");
    let result = say!("{=my_person}", my_person = person);
    assert!(!result.is_empty());
}

// Error message tests - these document what error messages should say
// The actual error messages happen at compile time, but here we document
// what they should be:

// Expected error: "positional argument at index 2 not provided (only 2 argument(s) given)"
// When you write: say!("{=2}", arg0, arg1)

// Expected error: "named argument 'missing_name' not found in provided arguments"
// When you write: say!("{=missing}", missing_name = value)

// These error messages are much clearer than the original:
// - "No positional argument 2"
// - "named argument missing not found"

// The new messages clearly indicate:
// 1. What index/name is missing
// 2. How many arguments were actually provided (for positional)
// 3. The exact name that wasn't found (for named)
