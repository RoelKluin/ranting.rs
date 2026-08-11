// Comprehensive examples of inclusive language using singular they/them pronouns
use ranting::*;
use ranting_derive::*;

#[test]
fn test_workplace_introduction() {
    let mentor = Noun::new("Casey", "they");
    let result = say!(
        "{=mentor are} your team mentor. {=mentor have} extensive experience and {?mentor is} excited to help you.",
        mentor = mentor
    );
    assert!(result.contains("They are") && result.contains("have"));
    println!("✓ Workplace intro: {}", result);
}

#[test]
fn test_student_introduction() {
    let student = Noun::new("Jordan", "they");
    let result = say!(
        "In class, {=student share} {`student} perspective on programming.",
        student = student
    );
    println!("✓ Student intro: {}", result);
    // Note: pronouns are lowercase when not at sentence start
    assert!((result.contains("They share") || result.contains("they share")) && result.contains("their"));
}

#[test]
fn test_colleague_feedback() {
    let colleague = Noun::new("Morgan", "they");
    let result = say!(
        "{=colleague have} provided valuable feedback. {=colleague's} insights will help us improve.",
        colleague = colleague
    );
    println!("✓ Colleague feedback: {}", result);
}

#[test]
fn test_friend_introduction() {
    let friend = Noun::new("Alex", "they");
    let result = say!(
        "Meet my friend {friend}! {=friend are} a software engineer, and {=friend love} cooking.",
        friend = friend
    );
    println!("✓ Friend intro: {}", result);
}

#[test]
fn test_multiple_pronouns_in_text() {
    let doctor = Noun::new("Dr. Riley", "they");
    let result = say!(
        "{=doctor are} here for your checkup. {=doctor will} review {`doctor} notes and discuss {`doctor} recommendations.",
        doctor = doctor
    );
    assert!(result.contains("They are") && result.contains("their"));
    println!("✓ Multiple pronouns: {}", result);
}

#[derive_ranting]
#[ranting(subject = "they", name = "designer")]
struct Designer {
    #[allow(dead_code)]
    specialization: String,
}

#[test]
fn test_derived_ranting_with_singular_they() {
    let designer = Designer {
        specialization: "UX".to_string(),
    };
    let result = say!(
        "{=designer have} a keen eye for detail. {=designer are} talented!",
        designer = &designer
    );
    println!("✓ Derived Ranting: {}", result);
    assert!(result.contains("They have") && result.contains("are"));
}

#[test]
fn test_pronoun_cases_singular_they() {
    let person = Noun::new("Sam", "they");

    // Subject case
    let subj = say!("{=person}", person);
    assert_eq!(subj, "They");

    // Object case
    let obj = say!("{@person}", person);
    assert_eq!(obj, "Them");

    // Possessive determiner
    let poss_det = say!("{`person}", person);
    assert_eq!(poss_det, "Their");

    // Possessive pronoun
    let poss_pron = say!("{~person}", person);
    assert_eq!(poss_pron, "Theirs");

    println!("✓ All pronoun cases for singular they: {} / {} / {} / {}", subj, obj, poss_det, poss_pron);
}

#[test]
fn test_verb_conjugation_with_singular_they() {
    let person = Noun::new("Taylor", "they");

    // Singular they uses plural verb forms
    let result1 = say!("{=person are}", person);
    assert_eq!(result1, "They are");

    let result2 = say!("{=person have}", person);
    assert_eq!(result2, "They have");

    let result3 = say!("{=person do}", person);
    assert_eq!(result3, "They do");

    println!("✓ Verb conjugations: {} / {} / {}", result1, result2, result3);
}

#[test]
fn test_diversity_statement() {
    let ceo = Noun::new("our CEO", "they");
    let result = say!(
        "{=ceo believe} in creating an inclusive workplace where {=ceo value} everyone's unique perspective. \
         {=ceo are} committed to {`ceo} vision of diversity and belonging."
    );
    println!("✓ Diversity statement: {}", result);
}
