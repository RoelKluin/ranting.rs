//! What Japanese *does* reach through `ranting`'s public API. The falsification half — what it
//! cannot — is `tests/holes.rs`.

use ranting::{NarrationContext, Register, ask, heed, say, say_with};
use ranting_ja::{JapaneseNoun, Shopkeeper};

// ------------------------------------------------ register: the reason this crate was built --

#[test]
fn politeness_is_driven_by_register_alone() {
    // No pronoun in the template, no entity state, no marker — the *only* thing that differs
    // between these two lines is `NarrationContext::register`. That is what German and Spanish
    // structurally cannot show: in both of those, politeness is a pronoun slot, so it rides the
    // addressee's declared subject label and `register` stays inert.
    let neko = JapaneseNoun::neko();
    let formal = NarrationContext::new().register(Register::Formal);
    let casual = NarrationContext::new().register(Register::Casual);

    assert_eq!(say_with!(formal, "{0 are}", neko), "猫 です");
    assert_eq!(say_with!(casual, "{0 are}", neko), "猫 だ");
}

#[test]
fn register_can_vary_per_utterance_within_one_scene() {
    // The ROADMAP worried that keigo varies per *addressee* — formal to a stranger, plain to a
    // friend, in the same scene — which would make it addressee-scoped like T-V rather than
    // story-scoped, and so the wrong axis for `NarrationContext`.
    //
    // It does vary that way, and it is not a problem: `NarrationContext` is **per-call**. The two
    // lines below are two contexts in one scene. "Story-wide" in the crate's own docs describes
    // the intended use, not a constraint the type imposes (ROADMAP.md Phase 7 item 13).
    let sensei = JapaneseNoun::sensei();
    let hito = JapaneseNoun::hito();
    let formal = NarrationContext::new().register(Register::Formal);
    let casual = NarrationContext::new().register(Register::Casual);

    assert_eq!(say_with!(formal, "{0 go}", sensei), "先生 いらっしゃいます");
    assert_eq!(say_with!(casual, "{0 go}", hito), "人 行く");
}

#[test]
fn sonkeigo_is_a_substitution_and_needs_no_extra_signal() {
    // Honorific verbs are a different *word*, not a suffix — 食べる → 召し上がる. That is a lookup
    // keyed by verb, register and who the subject is, and `inflect_verb_custom_with_context` has
    // all three (`&self` carries the honorific flag). Nothing new was needed at the seam.
    let formal = NarrationContext::new().register(Register::Formal);
    assert_eq!(
        say_with!(formal, "{0 eat}", JapaneseNoun::sensei()),
        "先生 召し上がります"
    );
    assert_eq!(
        say_with!(formal, "{0 eat}", JapaneseNoun::hito()),
        "人 食べます"
    );

    // Plain register, honored subject: sonkeigo has a plain form too, so the two axes are
    // genuinely independent rather than one being a proxy for the other.
    let casual = NarrationContext::new().register(Register::Casual);
    assert_eq!(
        say_with!(casual, "{0 eat}", JapaneseNoun::sensei()),
        "先生 召し上がる"
    );
}

#[test]
fn say_and_a_context_free_say_with_both_give_the_plain_form() {
    // The invariant every fork holds: without a `register` override, `say_with!()` reproduces
    // `say!()` exactly.
    let neko = JapaneseNoun::neko();
    let ctx = NarrationContext::default();
    assert_eq!(say!("{0 are}", neko), "猫 だ");
    assert_eq!(say_with!(ctx, "{0 are}", neko), say!("{0 are}", neko));
}

#[test]
fn an_unknown_verb_declines_rather_than_being_mangled() {
    // Same decline-rather-than-guess contract the other three forks use.
    let formal = NarrationContext::new().register(Register::Formal);
    assert_eq!(
        say_with!(formal, "{0 sing}", JapaneseNoun::neko()),
        "猫 sing"
    );
}

// ------------------------------------------------------------- numeral classifiers --

#[test]
fn the_classifier_is_read_off_the_noun_not_off_noun_class() {
    // The item 3 spike's stated question was whether `NounClass` can carry a classifier, or
    // whether that misuses a parameter documented as a gender/lexical-class label. The question
    // **dissolves**: which counter a noun takes is a property of that noun, so the hook's `&self`
    // is already enough. `NounClass` stays `UNSET` in this crate.
    assert_eq!(say!("{#0 1}", 1, JapaneseNoun::neko()), "一匹の 猫");
    assert_eq!(say!("{#0 1}", 3, JapaneseNoun::hito()), "三人の 人");
    assert_eq!(say!("{#0 1}", 2, JapaneseNoun::hon()), "二本の 本");
}

#[test]
fn counter_forms_are_a_table_not_a_suffix() {
    // 一匹 is *ippiki*, 三匹 is *sanbiki*: the sound changes mean nothing here is derivable by
    // appending a counter to a numeral. This is the same shape as an irregular plural table, and
    // the seam is indifferent to it — the hook returns an opaque `String`.
    let neko = JapaneseNoun::neko();
    assert_eq!(say!("{#0 1}", 1, neko), "一匹の 猫");
    assert_eq!(say!("{#0 1}", 2, neko), "二匹の 猫");
    assert_eq!(say!("{#0 1}", 3, neko), "三匹の 猫");
}

#[test]
fn a_count_past_the_table_declines_and_english_shows_through() {
    // The counter table stops at five. Past it the hook returns `None` and `ranting`'s own
    // speller renders the numeral — mixed-script output, which is the decline-rather-than-guess
    // contract being *visibly* wrong rather than plausibly wrong, exactly as an unmodelled verb
    // is. Asserted rather than left to chance, since a fork extending the vocabulary needs to
    // know what the boundary looks like.
    let neko = JapaneseNoun::neko();
    assert_eq!(say!("{#0 1}", 6, neko), "six 猫");
    assert_eq!(say!("{$0 1}", 6, neko), "6 猫");
}

#[test]
fn both_numeral_channels_take_the_counter() {
    // `$n` asks for digits, but a Japanese numeral is not usable without its counter, so this
    // channel gets the same treatment rather than falling through to bare digits.
    let hon = JapaneseNoun::hon();
    assert_eq!(say!("{$0 1}", 2, hon), "二本の 本");
    assert_eq!(say!("{#0 1}", 2, hon), "二本の 本");
}

// ------------------------------------------------------------------ input parsing --

#[test]
fn heed_works_on_spaced_command_style_input() {
    // The `heed!()` boundary is whitespace, permanently (ROADMAP.md Phase 6 item 9), and it is
    // script-agnostic rather than ASCII-only: this is the same shape as the English examples.
    assert_eq!(heed!("取る {item}", "取る 剣"), Some("剣".to_string()));
    assert_eq!(
        heed!("{item} を 取る", "剣 を 取る"),
        Some("剣".to_string())
    );
}

#[test]
fn ask_routes_command_style_input_to_its_audience() {
    // What `ask!()` is genuinely good for in Japanese: command input, which games and CLIs write
    // with spaces anyway. `tests/holes.rs` pins the prose case, where it narrows to almost
    // nothing.
    let player = JapaneseNoun::hito();
    let shop = Shopkeeper;
    assert_eq!(
        ask!(player, shop, "取る {item}", "取る 剣"),
        Some("剣を売ります。".to_string())
    );
    assert_eq!(
        ask!(player, shop, "取る {item}", "取る 石"),
        Some("それはありません。".to_string())
    );
    // A literal that does not match returns `None` **without** calling `answer()` — still worth
    // something even for a language whose prose it cannot segment.
    assert_eq!(ask!(player, shop, "取る {item}", "買う 剣"), None);
}
