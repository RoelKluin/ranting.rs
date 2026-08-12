// Cookbook recipes — backed tests for docs/COOKBOOK.md.
// Each test verifies one recipe's code example compiles and produces expected output.

use ranting::*;
use ranting_derive::say;

#[test]
fn recipe_1_game_npc_dialogue() {
    // Recipe 1: Game NPC dialogue with pronouns, tense, possessives.
    // An NPC speaks about themselves and their past actions.

    let merchant = Noun::new("Merchant", "he");

    let dialogue = say!("{=merchant walk} wares. {=merchant <sell} yesterday.");
    assert_eq!(dialogue, "He walks wares. He sold yesterday.");
}

#[test]
fn recipe_2_chatbot_singular_plural() {
    // Recipe 2: Chatbot responses that adapt to singular/plural subjects.

    fn bot_acknowledge(who: Noun) -> String {
        say!("{=who have} registered!")
    }

    // Singular user
    let user = Noun::new("User", "you");
    assert_eq!(bot_acknowledge(user), "You have registered!");

    // Multiple users (plural subject)
    let team = Noun::new("team", "they");
    assert_eq!(bot_acknowledge(team), "They have registered!");
}

#[test]
fn recipe_3_interactive_fiction_branching() {
    // Recipe 3: Interactive fiction with conditional tense markers.
    // Different text branches show past, present, and future actions.

    let protagonist = Noun::new("Hero", "I");

    // If the treasure was already found (past action)
    let past_narrative = say!("{=protagonist <discover} chamber. {=protagonist =search} inside.");
    assert_eq!(past_narrative, "I discovered chamber. I am searching inside.");

    // If the treasure will be found (future action)
    let future_narrative = say!("{=protagonist >discover} chamber. {=protagonist =search} inside.");
    assert_eq!(future_narrative, "I will discover chamber. I am searching inside.");
}

#[test]
fn recipe_4_user_profile_generation() {
    // Recipe 4: User profile generation with custom data.
    // Each profile displays the user's name and pronouns correctly.

    let alice = Noun::new("Alice", "she");
    let bob = Noun::new("Bob", "he");
    let jordan = Noun::new("Jordan", "they");

    assert_eq!(say!("{=alice walk}."), "She walks.");
    assert_eq!(say!("{=bob walk}."), "He walks.");
    assert_eq!(say!("{=jordan walk}."), "They walk.");
}

#[test]
fn recipe_5_plural_handling_singulars() {
    // Recipe 5: Singular/plural form handling.
    // Text adapts article and verb forms based on plurality.

    let cat = Noun::new("cat", "it");

    // Singular reference
    assert_eq!(say!("{=cat walk}"), "It walks");

    // Plural reference (forced with +)
    assert_eq!(say!("{+=cat walk}"), "They walk");
}

#[test]
fn recipe_6_gender_neutral_pronouns() {
    // Recipe 6: Gender-neutral pronouns (singular they).
    // Ensure they/them forms work with verb agreement.

    let alex = Noun::new("Alex", "they");

    assert_eq!(say!("{=alex have} voice."), "They have voice.");
    assert_eq!(say!("{=alex walk} fast."), "They walk fast.");
}

#[test]
fn recipe_7_verb_forms_tense() {
    // Recipe 7: Verb tense forms in different conjugations.
    // Demonstrate various tense marker insertions.

    let friend = Noun::new("Chris", "they");

    assert_eq!(say!("{=friend walk}"), "They walk");
    assert_eq!(say!("{=friend <walk}"), "They walked");
    assert_eq!(say!("{=friend =walk}"), "They are walking");
}

#[test]
fn recipe_8_mixed_tense_narrative() {
    // Recipe 8: Narrative using multiple tenses in sequence.
    // Past setting, ongoing action, future consequence.

    let protagonist = Noun::new("Sam", "she");

    let story = say!("{=protagonist <arrive} gates. {=protagonist =search} treasure. {=protagonist >find} it.");
    assert_eq!(story, "She arrived gates. She is searching treasure. She will find it.");
}

#[test]
fn recipe_9_custom_data_with_noun() {
    // Recipe 9: Using Noun with custom data structures.
    // Create character profiles with automatic pronoun support.

    struct Character {
        noun: Noun,
    }

    let merlin = Character {
        noun: Noun::new("Merlin", "he"),
    };

    let text = say!("{=0 walk} slowly.", merlin.noun);
    assert_eq!(text, "He walks slowly.");
}

#[test]
fn recipe_10_clarity_with_pronouns() {
    // Recipe 10: Using explicit pronouns for clarity in dialogue.
    // Clear pronoun references improve readability.

    let alice = Noun::new("Alice", "she");

    // Using pronouns naturally
    let text = say!("{=alice walk} fast.");
    assert_eq!(text, "She walks fast.");

    // Multiple subjects with clear pronouns
    let text2 = say!("{*alice walk} fast.");
    assert_eq!(text2, "Alice walks fast.");
}
