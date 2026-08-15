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
    assert_eq!(
        say!("{=0 wanted}", Noun::new("person", "she")),
        "She wanted"
    );
    assert_eq!(say!("{=0 played}", Noun::new("person", "it")), "It played");
}

#[test]
fn test_past_verb_no_spurious_s_third_person() {
    // Test that 3rd-person-singular (he/she/it) doesn't add -s to past tense
    let test_cases = vec![("he", "He"), ("she", "She"), ("it", "It")];

    for (pronoun, capitalized) in test_cases {
        let noun = Noun::new("person", pronoun);
        let result = say!("{=0 walked}", noun);
        assert_eq!(
            result,
            format!("{} walked", capitalized),
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

// `test_continuous_form_walking` and `test_continuous_form_other` used to live here, pinning
// `say!("{=0 walking}")` -> "She walking" (and running/talking/playing likewise). That behavior
// is now a *compile error*: a bare -ing form in an unmarked verb slot never gets an auxiliary,
// so the template was a silent writer error rendering ungrammatical text
// (docs/architecture-review-2026-08-15.md §1.8). The macro now rejects it and names the intended
// spellings -- `{=0 walk}` for the present, `{=0 =walk}` for the progressive. There is no
// trybuild harness, so the rejection is pinned by unit tests on `check_unmarked_verb_slot` in
// `ranting_derive/src/lib.rs`; the test below pins what those intended spellings render.
#[test]
fn test_continuous_intent_spellings_render_grammatically() {
    let she = Noun::new("person", "she");
    assert_eq!(say!("{=0 walk}", she), "She walks");
    assert_eq!(say!("{=0 =walk}", she), "She is walking");
    assert_eq!(
        say!("{=0 =run}", Noun::new("person", "he")),
        "He is running"
    );
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
            result,
            format!("{} walked", capitalized),
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
            result,
            format!("{} walked", capitalized),
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
    assert_eq!(
        say!("{=0 <=run}", Noun::new("person", "I")),
        "I was running"
    );
    assert_eq!(
        say!("{=0 <=run}", Noun::new("person", "he")),
        "He was running"
    );
    assert_eq!(
        say!("{=0 <=run}", Noun::new("person", "she")),
        "She was running"
    );
    assert_eq!(
        say!("{=0 <=run}", Noun::new("person", "it")),
        "It was running"
    );

    // Test that "were" is used for plural (you, we, they)
    assert_eq!(
        say!("{=0 <=run}", Noun::new("person", "you")),
        "You were running"
    );
    assert_eq!(
        say!("{=0 <=run}", Noun::new("person", "we")),
        "We were running"
    );
    assert_eq!(
        say!("{=0 <=run}", Noun::new("person", "they")),
        "They were running"
    );
}

#[test]
fn tense_marker_present_perfect_basic() {
    let person = Noun::new("Alex", "he");
    assert_eq!(say!("{=0 %walk}", person), "He has walked");
    assert_eq!(say!("{=0 %talk}", person), "He has talked");
    assert_eq!(say!("{=0 %play}", person), "He has played");
}

#[test]
fn tense_marker_present_perfect_irregular() {
    let person = Noun::new("Alex", "she");
    assert_eq!(say!("{=0 %go}", person), "She has gone");
    assert_eq!(say!("{=0 %see}", person), "She has seen");
    assert_eq!(say!("{=0 %eat}", person), "She has eaten");
    assert_eq!(say!("{=0 %take}", person), "She has taken");
    assert_eq!(say!("{=0 %write}", person), "She has written");
}

#[test]
fn tense_marker_present_perfect_all_pronouns() {
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
        let result = say!("{=0 %walk}", person);
        let expected = if matches!(pronoun, "he" | "she" | "it") {
            format!("{} has walked", capitalized)
        } else {
            format!("{} have walked", capitalized)
        };
        assert_eq!(result, expected, "Failed for pronoun: {}", pronoun);
    }
}

#[test]
fn tense_marker_past_perfect_basic() {
    let person = Noun::new("Alex", "he");
    assert_eq!(say!("{=0 <%walk}", person), "He had walked");
    assert_eq!(say!("{=0 <%talk}", person), "He had talked");
    assert_eq!(say!("{=0 <%play}", person), "He had played");
}

#[test]
fn tense_marker_past_perfect_irregular() {
    let person = Noun::new("Alex", "she");
    assert_eq!(say!("{=0 <%go}", person), "She had gone");
    assert_eq!(say!("{=0 <%see}", person), "She had seen");
    assert_eq!(say!("{=0 <%eat}", person), "She had eaten");
    assert_eq!(say!("{=0 <%take}", person), "She had taken");
}

#[test]
fn tense_marker_past_perfect_all_pronouns() {
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
        let result = say!("{=0 <%walk}", person);
        // "had" is invariant for all persons
        assert_eq!(
            result,
            format!("{} had walked", capitalized),
            "Failed for pronoun: {}",
            pronoun
        );
    }
}

#[test]
fn tense_marker_with_trailing_words() {
    // Regression for Phase 4 item 3: the old `~TENSE~MARKER:WORD` string
    // sentinel, when the tense-marked verb had trailing words, relied on
    // splitting `post` at its last whitespace to separate the sentinel from
    // the trailing word — which instead split *inside* the sentinel,
    // pushing "~TENSE~<:went " verbatim into the output and mis-conjugating
    // "home" as a verb ("He ~TENSE~<:went homes"). The typed `PostSpec::Tense
    // { word, trailing, .. }` carries `word` and `trailing` as separate
    // fields the macro already split apart, so there is nothing to
    // mis-split at runtime.
    let person = Noun::new("person", "he");
    assert_eq!(say!("{=0 <go home}", person), "He went home");
}

#[test]
fn tense_marker_perfect_mixed_sentence() {
    let person = Noun::new("Jordan", "she");
    let result = say!("{=0 %finish} {=0 <%start} before dinner.", person);
    assert_eq!(result, "She has finished she had started before dinner.");
}

// Regression for docs/architecture-review-2026-08-15.md §1.6 (ROADMAP.md
// Phase 8 item 6): a phrasal/compound verb used to take the bare
// third-person-singular -s on its *last* word instead of its head, because
// which suffix branch fired was decided by the spelling of the particle, not
// the verb — "pick up" -> "pick ups", "stick to" -> "stick toes", "get by"
// -> "get bies". The fix conjugates the head word only and re-appends the
// remainder unchanged.

#[test]
fn phrasal_verb_pick_up_third_person() {
    assert_eq!(
        say!("{=0 pick up} the sword.", Noun::new("person", "he")),
        "He picks up the sword."
    );
    assert_eq!(
        say!("{=0 pick up} the sword.", Noun::new("person", "she")),
        "She picks up the sword."
    );
    assert_eq!(
        say!("{=0 pick up} the sword.", Noun::new("thing", "it")),
        "It picks up the sword."
    );
}

#[test]
fn phrasal_verb_stick_to_third_person() {
    assert_eq!(
        say!("{=0 stick to} the plan.", Noun::new("person", "he")),
        "He sticks to the plan."
    );
    assert_eq!(
        say!("{=0 stick to} the plan.", Noun::new("person", "she")),
        "She sticks to the plan."
    );
    assert_eq!(
        say!("{=0 stick to} the plan.", Noun::new("thing", "it")),
        "It sticks to the plan."
    );
}

#[test]
fn phrasal_verb_get_by_third_person() {
    assert_eq!(
        say!("{=0 get by} on little.", Noun::new("person", "he")),
        "He gets by on little."
    );
    assert_eq!(
        say!("{=0 get by} on little.", Noun::new("person", "she")),
        "She gets by on little."
    );
    assert_eq!(
        say!("{=0 get by} on little.", Noun::new("thing", "it")),
        "It gets by on little."
    );
}

#[test]
fn phrasal_verb_first_and_second_person_unchanged() {
    // First and second person never add -s, so the phrasal verb must render
    // exactly as written for both.
    assert_eq!(
        say!("{=0 pick up} the sword.", Noun::new("person", "I")),
        "I pick up the sword."
    );
    assert_eq!(
        say!("{=0 pick up} the sword.", Noun::new("person", "you")),
        "You pick up the sword."
    );
    assert_eq!(
        say!("{=0 stick to} the plan.", Noun::new("person", "we")),
        "We stick to the plan."
    );
    assert_eq!(
        say!("{=0 get by} on little.", Noun::new("people", "they")),
        "They get by on little."
    );
}

#[test]
fn single_word_verb_control_unaffected() {
    // Single-word verbs must stay byte-identical to before the head/tail split.
    assert_eq!(
        say!("{=0 walk} home.", Noun::new("person", "he")),
        "He walks home."
    );
    assert_eq!(
        say!("{=0 walk} home.", Noun::new("person", "she")),
        "She walks home."
    );
    assert_eq!(
        say!("{=0 walk} home.", Noun::new("thing", "it")),
        "It walks home."
    );
    assert_eq!(
        say!("{=0 walk} home.", Noun::new("person", "I")),
        "I walk home."
    );
    assert_eq!(
        say!("{=0 walk} home.", Noun::new("person", "you")),
        "You walk home."
    );
}
