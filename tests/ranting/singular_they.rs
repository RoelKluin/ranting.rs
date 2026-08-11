// Test singular they/them support (gender-neutral pronouns)
use ranting::*;
use ranting_derive::say;

#[test]
fn test_singular_they_subject() {
    let alex = Noun::new("Alex", "they");
    let result = say!("{=alex}", alex);
    assert_eq!(result, "They");
    println!("✓ Subject (nominative): {}", result);
}

#[test]
fn test_singular_they_object() {
    let alex = Noun::new("Alex", "they");
    let result = say!("{@alex}", alex);
    assert_eq!(result, "Them");
    println!("✓ Object (accusative): {}", result);
}

#[test]
fn test_singular_they_possessive() {
    let alex = Noun::new("Alex", "they");
    let result = say!("{`alex}", alex);
    assert_eq!(result, "Their");
    println!("✓ Possessive determiner: {}", result);
}

#[test]
fn test_singular_they_possessive_pronoun() {
    let alex = Noun::new("Alex", "they");
    let result = say!("{~alex}", alex);
    assert_eq!(result, "Theirs");
    println!("✓ Possessive pronoun: {}", result);
}

#[test]
fn test_singular_they_with_multiple_people() {
    let alex = Noun::new("Alex", "they");
    let casey = Noun::new("Casey", "they");
    let result = say!(
        "{=alex} and {=casey} went to the store. {=alex} bought milk.",
        alex,
        casey
    );
    println!("✓ Multiple people: {}", result);
    assert!(result.contains("They") && result.contains("and"));
}

#[test]
fn test_singular_they_verb_agreement() {
    let alex = Noun::new("Alex", "they");
    let result = say!("{=alex are} a great friend.", alex);
    assert!(result.contains("They are"));
    println!("✓ Verb agreement: {}", result);
}

#[test]
fn test_singular_they_name_display() {
    let alex = Noun::new("Alexandria", "they");
    let result = say!("{alex}", alex);
    assert_eq!(result, "Alexandria");
    println!("✓ Name display: {}", result);
}

#[test]
fn test_singular_they_in_sentence() {
    let alex = Noun::new("Alex", "they");
    let jordan = Noun::new("Jordan", "she");
    let result = say!(
        "{=alex have} agreed to help {=jordan with} {`alex} project.",
        alex,
        jordan
    );
    println!("✓ In sentence: {}", result);
    assert!(result.contains("They have") || result.contains("they have"));
}
