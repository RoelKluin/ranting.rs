// Tests for verb tense detection and past/continuous verb handling.
// Verifies that past-tense and continuous-form verbs are not incorrectly
// conjugated with 3rd-person-singular -s suffix (the "walkeds" bug).

use ranting::*;
use ranting_derive::say;

#[test]
fn test_past_verb_no_spurious_s_regular_walked() {
    // Regular past tense "walked" should not get a spurious -s suffix
    let she = Noun::new("person", "she");
    assert_eq!(say!("{=0 walked}", she), "She walked");

    let he = Noun::new("person", "he");
    assert_eq!(say!("{=0 walked}", he), "He walked");

    let it = Noun::new("thing", "it");
    assert_eq!(say!("{=0 walked}", it), "It walked");
}

#[test]
fn test_past_verb_no_spurious_s_other_regular() {
    // Other regular past verbs should not get -s suffix
    assert_eq!(say!("{=0 talked}", Noun::new("person", "he")), "He talked");
    assert_eq!(say!("{=0 wanted}", Noun::new("person", "she")), "She wanted");
    assert_eq!(say!("{=0 played}", Noun::new("person", "it")), "It played");
}

#[test]
fn test_past_verb_no_spurious_s_third_person() {
    // Test that 3rd-person-singular (he/she/it) doesn't add -s to past tense
    let test_cases = vec![
        ("he", "He"),
        ("she", "She"),
        ("it", "It"),
    ];

    for (pronoun, capitalized) in test_cases {
        let noun = Noun::new("person", pronoun);
        let result = say!("{=0 walked}", noun);
        assert_eq!(
            result, format!("{} walked", capitalized),
            "Failed for pronoun: {}",
            pronoun
        );
    }
}

#[test]
fn test_irregular_past_went() {
    // Irregular past form "went" should be preserved
    assert_eq!(say!("{=0 went}", Noun::new("person", "he")), "He went");
    assert_eq!(say!("{=0 went}", Noun::new("person", "she")), "She went");
}

#[test]
fn test_irregular_past_other() {
    // Other irregular past forms should be preserved
    assert_eq!(say!("{=0 saw}", Noun::new("person", "it")), "It saw");
    assert_eq!(say!("{=0 took}", Noun::new("person", "he")), "He took");
    assert_eq!(say!("{=0 made}", Noun::new("person", "she")), "She made");
}

#[test]
fn test_continuous_form_walking() {
    // Continuous form "walking" should not get a spurious -s suffix
    let she = Noun::new("person", "she");
    assert_eq!(say!("{=0 walking}", she), "She walking");
}

#[test]
fn test_continuous_form_other() {
    // Other continuous forms should not get -s suffix
    assert_eq!(say!("{=0 running}", Noun::new("person", "he")), "He running");
    assert_eq!(say!("{=0 talking}", Noun::new("person", "it")), "It talking");
    assert_eq!(say!("{=0 playing}", Noun::new("person", "she")), "She playing");
}

#[test]
fn test_present_verb_gets_s_for_third_person() {
    // Present tense base verbs SHOULD get the -s suffix for 3rd person
    assert_eq!(say!("{=0 walk}", Noun::new("cat", "it")), "It walks");
    assert_eq!(say!("{=0 talk}", Noun::new("person", "he")), "He talks");
    assert_eq!(say!("{=0 play}", Noun::new("person", "she")), "She plays");
}

#[test]
fn test_present_verb_no_s_for_first_person() {
    // Present tense verbs should NOT get -s for first person
    assert_eq!(say!("{=0 walk}", Noun::new("person", "I")), "I walk");
    assert_eq!(say!("{=0 talk}", Noun::new("person", "we")), "We talk");
}

#[test]
fn test_past_verb_all_pronouns() {
    // Test that past tense works correctly for all pronouns
    let test_cases = vec![
        ("I", "I"),
        ("you", "You"),
        ("he", "He"),
        ("she", "She"),
        ("it", "It"),
        ("we", "We"),
        ("they", "They"),
    ];

    for (pronoun, capitalized) in test_cases {
        let noun = Noun::new("person", pronoun);
        let result = say!("{=0 walked}", noun);
        assert_eq!(
            result, format!("{} walked", capitalized),
            "Failed for pronoun: {}",
            pronoun
        );
    }
}

#[test]
fn test_mixed_sentence_with_past_verbs() {
    // Test past verbs in a more complex sentence context
    let person = Noun::new("Alex", "they");
    let result = say!("{=0 walked} and {=0 talked}.", person);
    assert_eq!(result, "They walked and they talked.");
}

#[test]
fn test_past_verb_with_contraction_didnt() {
    // Test past tense with n't contractions
    let he = Noun::new("person", "he");
    let result = say!("{=0 didn't}", he);
    // "didn't" should pass through as-is, not get -s added
    assert_eq!(result, "He didn't");
}

#[test]
fn test_irregular_past_was_were() {
    // "was" and "were" are in the irregular verb table, should work correctly
    let i = Noun::new("person", "I");
    let he = Noun::new("person", "he");
    let they = Noun::new("person", "they");

    // "was" is in IRREGULAR_VERBS_1ST for "I"
    assert_eq!(say!("{=0 was}", i), "I was");
    // "was" is in IRREGULAR_VERBS_3RD for "he"
    assert_eq!(say!("{=0 was}", he), "He was");
    // "were" should be handled by detect_tense (it's in IRREGULAR_PAST table)
    assert_eq!(say!("{=0 were}", they), "They were");
}

// Tests for tense markers in say!() macro
#[test]
fn tense_marker_past_regular() {
    let person = Noun::new("Alex", "he");
    assert_eq!(say!("{=0 <walk}", person), "He walked");
    assert_eq!(say!("{=0 <talk}", person), "He talked");
    assert_eq!(say!("{=0 <play}", person), "He played");
}

#[test]
fn tense_marker_past_irregular() {
    let person = Noun::new("Alex", "she");
    assert_eq!(say!("{=0 <go}", person), "She went");
    assert_eq!(say!("{=0 <see}", person), "She saw");
    assert_eq!(say!("{=0 <take}", person), "She took");
}

#[test]
fn tense_marker_continuous_basic() {
    let person = Noun::new("Alex", "he");
    assert_eq!(say!("{=0 =walk}", person), "He is walking");
    assert_eq!(say!("{=0 =talk}", person), "He is talking");
    assert_eq!(say!("{=0 =play}", person), "He is playing");
}

#[test]
fn tense_marker_continuous_silent_e() {
    let person = Noun::new("Alex", "she");
    assert_eq!(say!("{=0 =make}", person), "She is making");
    assert_eq!(say!("{=0 =like}", person), "She is liking");
}

#[test]
fn tense_marker_continuous_consonant_doubling() {
    let person = Noun::new("Alex", "it");
    assert_eq!(say!("{=0 =run}", person), "It is running");
    assert_eq!(say!("{=0 =sit}", person), "It is sitting");
}

#[test]
fn tense_marker_continuous_ie_to_y() {
    let person = Noun::new("Alex", "they");
    assert_eq!(say!("{=0 =lie}", person), "They are lying");
    assert_eq!(say!("{=0 =tie}", person), "They are tying");
}

#[test]
fn tense_marker_future_returns_base() {
    // Future tense marker > now correctly inserts "will" before the base verb
    let person = Noun::new("Alex", "he");
    assert_eq!(say!("{=0 >walk}", person), "He will walk");
    assert_eq!(say!("{=0 >go}", person), "He will go");
}

#[test]
fn tense_markers_all_pronouns() {
    let test_cases = vec![
        ("I", "I"),
        ("you", "You"),
        ("he", "He"),
        ("she", "She"),
        ("it", "It"),
        ("we", "We"),
        ("they", "They"),
    ];

    for (pronoun, capitalized) in test_cases {
        let person = Noun::new("person", pronoun);
        let result = say!("{=0 <walk}", person);
        assert_eq!(
            result, format!("{} walked", capitalized),
            "Failed for pronoun: {}",
            pronoun
        );
    }
}

#[test]
fn tense_markers_in_sentence() {
    let person = Noun::new("Alex", "they");
    let result = say!("{=0 <walk} and {=0 <talk}.", person);
    assert_eq!(result, "They walked and they talked.");
}

#[test]
fn tense_markers_mixed_sentence() {
    let person = Noun::new("Jordan", "she");
    let result = say!("{=0 <start} {=0 =run} the race.", person);
    assert_eq!(result, "She started she is running the race.");
}

#[test]
fn tense_marker_past_continuous_basic() {
    let person = Noun::new("Alex", "he");
    assert_eq!(say!("{=0 <=walk}", person), "He was walking");
    assert_eq!(say!("{=0 <=talk}", person), "He was talking");
    assert_eq!(say!("{=0 <=play}", person), "He was playing");
}

#[test]
fn tense_marker_past_continuous_silent_e() {
    let person = Noun::new("Alex", "she");
    assert_eq!(say!("{=0 <=make}", person), "She was making");
    assert_eq!(say!("{=0 <=like}", person), "She was liking");
}

#[test]
fn tense_marker_past_continuous_consonant_doubling() {
    let person = Noun::new("Alex", "it");
    assert_eq!(say!("{=0 <=run}", person), "It was running");
    assert_eq!(say!("{=0 <=sit}", person), "It was sitting");
}

#[test]
fn tense_marker_past_continuous_ie_to_y() {
    let person = Noun::new("Alex", "they");
    assert_eq!(say!("{=0 <=lie}", person), "They were lying");
    assert_eq!(say!("{=0 <=tie}", person), "They were tying");
}

#[test]
fn tense_marker_past_continuous_all_pronouns() {
    let test_cases = vec![
        ("I", "I"),
        ("you", "You"),
        ("he", "He"),
        ("she", "She"),
        ("it", "It"),
        ("we", "We"),
        ("they", "They"),
    ];

    for (pronoun, capitalized) in test_cases {
        let person = Noun::new("person", pronoun);
        let result = say!("{=0 <=walk}", person);
        // Singular (I, he, she, it) use "was"; plural (you, we, they) use "were"
        let expected = if matches!(pronoun, "I" | "he" | "she" | "it") {
            format!("{} was walking", capitalized)
        } else {
            format!("{} were walking", capitalized)
        };
        assert_eq!(result, expected, "Failed for pronoun: {}", pronoun);
    }
}

#[test]
fn tense_marker_past_continuous_was_were_distinction() {
    // Test that "was" is used for singular (I, he, she, it)
    assert_eq!(say!("{=0 <=run}", Noun::new("person", "I")), "I was running");
    assert_eq!(say!("{=0 <=run}", Noun::new("person", "he")), "He was running");
    assert_eq!(say!("{=0 <=run}", Noun::new("person", "she")), "She was running");
    assert_eq!(say!("{=0 <=run}", Noun::new("person", "it")), "It was running");

    // Test that "were" is used for plural (you, we, they)
    assert_eq!(say!("{=0 <=run}", Noun::new("person", "you")), "You were running");
    assert_eq!(say!("{=0 <=run}", Noun::new("person", "we")), "We were running");
    assert_eq!(say!("{=0 <=run}", Noun::new("person", "they")), "They were running");
}
