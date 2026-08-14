//! The regular pluralization rules (ROADMAP.md Phase 7 item 10), through `say!()`.
//!
//! Before these landed, `Ranting::inflect()` had no rules at all: a noun the irregular table
//! missed had `plural_end` (default `"s"`) appended verbatim, so every assertion in
//! `regular_rules_apply_through_a_placeholder` used to render with a bare `s` stuck on the end
//! ("flys", "boxs", "citys"). The unit tests in `src/language/plurals.rs` pin the rules
//! themselves; these pin that a placeholder actually reaches them.

use ranting::*;

#[test]
fn regular_rules_apply_through_a_placeholder() {
    for (singular, plural) in [
        ("fly", "flies"),
        ("city", "cities"),
        ("entity", "entities"),
        ("box", "boxes"),
        ("match", "matches"),
        ("bush", "bushes"),
        ("glass", "glasses"),
        ("bus", "buses"),
        // The `-ves` stems are rows in `data/irregular_plurals.txt` as bare words, so the rule
        // only ever decides a compound.
        ("bookshelf", "bookshelves"),
        ("housewife", "housewives"),
    ] {
        let noun = Noun::new(singular, "it");
        assert_eq!(say!("{,+0}", noun), plural, "plural of {singular}");
    }
}

/// A rule that "fixed" `fly` by also breaking `day` would be no improvement, so the
/// counterexamples are pinned at the `say!()` level too.
#[test]
fn words_that_take_a_bare_s_are_untouched() {
    for (singular, plural) in [
        ("day", "days"),
        ("boy", "boys"),
        ("dog", "dogs"),
        ("roof", "roofs"),
        ("chief", "chiefs"),
    ] {
        let noun = Noun::new(singular, "it");
        assert_eq!(say!("{,+0}", noun), plural, "plural of {singular}");
    }
}

/// The irregular table still runs first, and it disagrees with the rules on purpose: the regular
/// rule would say "childs"/"mouses".
#[test]
fn the_irregular_table_still_wins() {
    for (singular, plural) in [("child", "children"), ("mouse", "mice"), ("sheep", "sheep")] {
        let noun = Noun::new(singular, "it");
        assert_eq!(say!("{,+0}", noun), plural);
    }
}

/// A hyphenated compound pluralizes its head. This is the one failure the `plural_end` attribute
/// could not work around even in principle -- the `-s` lands in the middle of the word.
#[test]
fn hyphenated_compounds_pluralize_their_head() {
    let relative = Noun::new("mother-in-law", "it");
    assert_eq!(say!("{,+0}", relative), "mothers-in-law");

    // No head structure: the tail takes the `-s`, exactly as before.
    let shirt = Noun::new("t-shirt", "it");
    assert_eq!(say!("{,+0}", shirt), "t-shirts");
}

/// The compatibility contract: a struct that declares `plural_end` has stated its own rule, and
/// keeps the literal strip-and-append it always got. Without this, an impl using `plural_end` as
/// an escape hatch -- including a non-English one -- would silently acquire English orthography.
#[test]
fn a_declared_plural_end_still_wins_over_the_rules() {
    #[derive_ranting]
    #[ranting(name = "Fuchs", subject = "it", plural_end = "e")]
    struct German {}

    let fox = German {};
    assert_eq!(say!("{,+0}", fox), "Fuchse");

    // And the default-attribute path would have said otherwise: `Fuchs` ends in a sibilant.
    let as_english = Noun::new("Fuchs", "it");
    assert_eq!(say!("{,+0}", as_english), "Fuchses");
}

/// Declaring `plural_end = "s"` is not the same as leaving it alone, and this is the whole
/// reason the mode is chosen by *whether the attribute was written* rather than by its value.
///
/// A language whose loanword plurals are a bare `-s` -- German `Partys`/`Babys`, Dutch, Danish
/// -- needs exactly this: append `s`, apply no English orthography. Testing the value instead
/// made that request indistinguishable from the default, so it silently got the English rules
/// and there was no opt-out at all short of a decoy `singular_end`. Consonant + `y` is the class
/// where the two paths actually diverge; those names were right by accident before the rules
/// landed, so this is the one place the rules made previously-correct output wrong.
#[test]
fn declaring_the_default_suffix_still_opts_out_of_the_rules() {
    #[derive_ranting]
    #[ranting(name = "Party", subject = "it", plural_end = "s")]
    struct Loanword {}

    assert_eq!(say!("{,+0}", Loanword {}), "Partys");

    // Same word without the attribute: English orthography, and wrong for German.
    #[derive_ranting]
    #[ranting(name = "Party", subject = "it")]
    struct English {}

    assert_eq!(say!("{,+0}", English {}), "Parties");

    // Either attribute alone is enough -- the gate is "neither was written", not "plural_end
    // was not written". This arm is the one a later simplification would drop.
    #[derive_ranting]
    #[ranting(name = "Party", subject = "it", singular_end = "")]
    struct SingularEndOnly {}

    assert_eq!(say!("{,+0}", SingularEndOnly {}), "Partys");
}

/// `Noun` has no attributes to declare, so the same opt-out is a constructor -- otherwise the
/// crate's own convenience type would be the one `Ranting` impl with no way out of English
/// spelling.
#[test]
fn a_noun_can_declare_its_own_suffix_at_runtime() {
    let english = Noun::new("Party", "it");
    assert_eq!(say!("{,+0}", english), "Parties");

    let german = Noun::new("Party", "it").with_plural_end("s");
    assert_eq!(say!("{,+0}", german), "Partys");

    // Stripping works the same way, and either half alone is enough to leave the rules behind.
    let fuchs = Noun::new("Fuchs", "it")
        .with_singular_end("s")
        .with_plural_end("se");
    assert_eq!(say!("{,+0}", fuchs), "Fuchse");
}

/// The rules run on the name as written, so a capitalized noun stays capitalized across a stem
/// rewrite -- `City` must not become `Cityies` or `cities`.
#[test]
fn case_is_preserved_across_a_stem_rewrite() {
    let city = Noun::new("City", "it");
    assert_eq!(say!("{,+0}", city), "Cities");
}

/// An all-caps name must come out all-caps, exactly as the irregular table's path already
/// guarantees — the first cut of these rules rendered `"CITIes"`.
#[test]
fn an_all_caps_name_stays_all_caps() {
    let city = Noun::new("CITY", "it");
    assert_eq!(say!("{,+0}", city), "CITIES");
    let boxes = Noun::new("BOX", "it");
    assert_eq!(say!("{,+0}", boxes), "BOXES");
}

/// A rule that only appends leaves the name as written, so an interior capital survives.
#[test]
fn interior_capitals_survive_a_plain_append() {
    let phone = Noun::new("iPhone", "it");
    assert_eq!(say!("{,+0}", phone), "iPhones");
}
