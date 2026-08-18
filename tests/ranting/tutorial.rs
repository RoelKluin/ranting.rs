// Tutorial examples — backed tests for docs/TUTORIAL.md sections.
// Each test verifies one section's code example compiles and produces expected output.

use ranting::*;
use ranting_derive::say;

#[test]
fn section_1_why_say_vs_format() {
    // docs/TUTORIAL.md Section 1: demonstrates the pronoun-agreement problem.
    // format!() can't handle inflection; say!() solves it.

    fn say_this(who: Noun, title: &Noun) -> String {
        say!("{=who do} say {`who title are} {who}.")
    }

    let title = Noun::new("name", "it");

    // Singular subjects automatically conjugate verbs
    assert_eq!(
        say_this(Noun::new("Jane", "I"), &title),
        "I do say my name is Jane."
    );

    // Third person singular: verb changes
    assert_eq!(
        say_this(Noun::new("Tarzan", "he"), &title),
        "He does say his name is Tarzan."
    );

    // Singular "they": the `` {`who title are} `` backtick-possessive form
    // previously hardcoded to_plural=false when substituting `who`'s
    // possessive, so "they" was silently singularized to "its" instead of
    // "their" (see docs/architecture-review-2026-08-13.md section 5).
    assert_eq!(
        say_this(Noun::new("Jordan", "they"), &title),
        "They do say their name is Jordan."
    );
}

#[test]
fn section_2_first_say_pronouns() {
    // docs/TUTORIAL.md Section 2: basic pronouns, subjects, case markers.

    let jane = Noun::new("Jane", "I");
    let tarzan = Noun::new("Tarzan", "he");
    let pat = Noun::new("Pat", "they");
    let jeffersons = Noun::new("The Jeffersons", "they");

    // Subject form (=): displays as the pronoun
    assert_eq!(say!("{=jane}"), "I");
    assert_eq!(say!("{=tarzan}"), "He");
    assert_eq!(say!("{=pat}"), "They");

    // Possessive determiner (`): displays as possessive (auto-capitalized at sentence start)
    assert_eq!(say!("{`jane}"), "My");
    assert_eq!(say!("{`tarzan}"), "His");
    assert_eq!(say!("{`pat}"), "Their");

    // Display name only (*): leaves pronoun logic in place for verb agreement. `who` isn't
    // special syntax -- it's the first word of the noun's own verb slot, so it only stays
    // unconjugated here because `jeffersons`'s declared pronoun ("they") is plural. A
    // third-person-singular noun's `who` would itself get conjugated (wrongly) -- there is
    // currently no way to write an inert relative pronoun right after a noun's own case marker
    // in one placeholder.
    assert_eq!(
        say!("{*jeffersons who have} a book."),
        "The Jeffersons who have a book."
    );
}

#[test]
fn section_3_tense_markers_past() {
    // docs/TUTORIAL.md Section 3: tense markers — past tense (<)

    let kate = Noun::new("Kate", "she");

    // Bare verb in say!() defaults to present
    assert_eq!(say!("{=kate walk}"), "She walks");

    // < marker signals past tense — verb conjugation to past form
    assert_eq!(say!("{=kate <walk}"), "She walked");
}

#[test]
fn section_3_tense_markers_continuous() {
    // docs/TUTORIAL.md Section 3: tense markers — present continuous (=)

    let luis = Noun::new("Luis", "he");

    // = marker signals present continuous — "is" + -ing form
    assert_eq!(say!("{=luis =run}"), "He is running");
}

#[test]
fn section_3_tense_markers_future() {
    // docs/TUTORIAL.md Section 3: tense markers — future (>)

    let sophia = Noun::new("Sophia", "she");

    // > marker signals future — "will" + base verb
    assert_eq!(say!("{=sophia >paint}"), "She will paint");
}

#[test]
fn section_3_tense_markers_past_continuous() {
    // docs/TUTORIAL.md Section 3: tense markers — past continuous (<=)

    let alex = Noun::new("Alex", "they");

    // <= marker signals past continuous — "were" + -ing form
    assert_eq!(say!("{=alex <=dance}"), "They were dancing");
}

#[test]
fn section_3_tense_markers_present_perfect() {
    // docs/TUTORIAL.md Section 3: tense markers — present perfect (%)

    let morgan = Noun::new("Morgan", "she");

    // % marker signals present perfect — "has" + -ed form
    assert_eq!(say!("{=morgan %finish}"), "She has finished");
}

#[test]
fn section_3_tense_markers_past_perfect() {
    // docs/TUTORIAL.md Section 3: tense markers — past perfect (<%)

    let jordan = Noun::new("Jordan", "he");

    // <% marker signals past perfect — "had" + -ed form
    assert_eq!(say!("{=jordan <%leave}"), "He had left");
}

#[test]
fn section_4_common_pitfalls_auto_uppercase() {
    // docs/TUTORIAL.md Section 4: sentence-start placeholders auto-uppercase.

    let avery = Noun::new("Avery", "she");

    // Placeholder at sentence start → first char auto-uppercased
    let s = say!("{=avery run} quickly.");
    assert_eq!(s, "She runs quickly."); // Verb also conjugates for 3rd person

    // Placeholder mid-sentence → no auto-uppercase
    let s = say!("When {=avery run}, she is fast.");
    assert_eq!(s, "When she runs, she is fast."); // Verb conjugates in mid-sentence too
}

#[test]
fn section_4_placeholder_grammar_articles() {
    // docs/TUTORIAL.md Section 4: articles in placeholders.

    let dog = Noun::new("dog", "it");

    // Indefinite article 'a' — note: at sentence start, auto-capitalizes
    assert_eq!(say!("{a dog}"), "A dog");

    // Definite article 'the'
    assert_eq!(say!("{the dog}"), "The dog");
}

#[test]
fn section_4_placeholder_grammar_plurality() {
    // docs/TUTORIAL.md Section 4: forcing singular/plural (+ and -).

    let person = Noun::new("person", "it");

    // - forces singular (default) — "person" becomes "it"
    assert_eq!(say!("{-=0 do}", person), "It does");

    // + forces plural — "person" becomes "people"
    assert_eq!(say!("{+=0 do}", person), "They do");
}

#[test]
fn section_5_debug_feature() {
    // docs/TUTORIAL.md Section 5: --features debug prints macro expansion.
    // This test just verifies the macro itself works; actual compile-time
    // output only appears when built with --features debug.

    let morgan = Noun::new("Morgan", "she");

    // The macro expands to a format!() call internally.
    // With --features debug, cargo would print the transformed code.
    let result = say!("{=morgan walk}");
    assert_eq!(result, "She walks");
}
