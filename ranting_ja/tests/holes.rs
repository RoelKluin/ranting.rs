//! The falsification half of ROADMAP.md Phase 7 item 6: what Japanese cannot get through
//! `ranting`'s public API. Mirrors the other three falsifiers' structure and naming.
//!
//! Each test asserts what the crate *actually* produces, not what Japanese needs. They are pins:
//! if a later change closes one, the test fails and the hole gets struck from the README rather
//! than quietly rotting.

use ranting::{NarrationContext, Register, ask, heed, say, say_with};
use ranting_ja::{JapaneseNoun, Shopkeeper};

// ------------------------------- hole 1: ✅ closed — the numeral binds to its noun --

// ROADMAP.md Phase 7 item 12 added `Ranting::elide_numeral_custom`, the numeral-side twin of
// `elide_article_custom`: same post-assembly splice, same may-replace-all-three contract.
// `JapaneseNoun` answers it, and 「一匹の猫」 renders correctly. Kept as `hole_1_*` (not renamed,
// not renumbered) so it stays findable from the README and ROADMAP cross-references; the
// assertions below now show the bound forms instead of the previous `一匹の 猫`.
//
// This crate is why the item exists. It was built first and shipped the wrong output, because
// unlike Arabic's dual the gap had no workaround to encode — which is exactly why the item 4
// build decision did not block it on a fix.

#[test]
fn hole_1_the_numeral_now_binds_to_its_noun() {
    let neko = JapaneseNoun::neko();
    assert_eq!(say!("{#0 1}", 1, neko), "一匹の猫");
    assert_eq!(say!("{$0 1}", 3, neko), "三匹の猫");
    assert_eq!(say!("{#0 1}", 2, JapaneseNoun::hon()), "二本の本");

    // The hook is not called when there is no numeral to bind, so a bare noun is untouched.
    assert_eq!(say!("{0}", neko), "猫");
}

#[test]
fn hole_1b_a_hidden_numeral_now_leaves_no_space_behind() {
    // The escape hatch this crate would have needed if item 12 had not landed: hide the numeral
    // and write the counter phrase in the template. It used to leave a stray leading space —
    // `docs/architecture-review-2026-08-14.md` §1.6, *pinned* by `tests/ranting/numeral.rs`
    // rather than flagged, so it read as intended for two phases. Fixed by ROADMAP.md Phase 7
    // item 13, which is why both halves of hole 1 are struck at once.
    let neko = JapaneseNoun::neko();
    assert_eq!(say!("{?$0 1}", 1, neko), "猫");
    assert_eq!(say!("一匹の{?$0 1}", 1, neko), "一匹の猫");
}

// ------------------------------- hole 2: unspaced prose cannot be parsed or segmented --

#[test]
fn hole_2_unspaced_input_returns_none_rather_than_a_wrong_capture() {
    // Whitespace is the only word boundary in `heed!()`/`ask!()`, permanently (ROADMAP.md Phase 6
    // item 9). Natural Japanese prose has no spaces, so a template whose segments abut cannot
    // match.
    //
    // The failure is **honest**: `None`, never an invented split. That is the same
    // don't-silently-guess stance that makes two zero-gap captures a *compile* error, and it is
    // the right behavior — but it does mean this crate cannot parse a natural sentence.
    assert_eq!(heed!("{item}を取る", "剣を取る"), None);
    assert_eq!(heed!("{a}的{b}", "私の剣"), None);

    // The escape hatch: an unspaced clause is exactly one `\S+` token, so a bare capture hands
    // the whole run back for the caller's own segmenter.
    assert_eq!(heed!("{clause}", "剣を取る"), Some("剣を取る".to_string()));
}

#[test]
fn hole_3_ask_degenerates_to_a_function_call_on_prose_input() {
    // For `heed!()` the escape hatch above is fine — the caller wanted a string and got one. For
    // `ask!()` it is thinner: the pitch is "parse the input, then call `answer()` with the
    // captures", and against unspaced prose every template collapses to a single `{clause}`
    // capture, so `answer()` receives the raw utterance and does *all* the work.
    //
    // A real narrowing, not a falsification — `ask!()` still routes to the right audience, and
    // still returns `None` without calling `answer()` when a literal does not match. The honest
    // summary is that it is useful for Japanese *command* input and not for Japanese *prose*.
    let player = JapaneseNoun::hito();
    let shop = Shopkeeper;

    // The template Japanese actually wants cannot match at all:
    assert_eq!(ask!(player, shop, "{item}を取る", "剣を取る"), None);

    // What is left is a bare capture — at which point `ask!()` is a function call:
    assert_eq!(
        ask!(player, shop, "{item}", "剣"),
        Some("剣を売ります。".to_string())
    );
}

// ------------------------------- hole 4: particles and word order are template text --

#[test]
fn hole_4_case_particles_are_template_text_not_grammatical_case() {
    // Japanese marks case with postpositional particles — が (nominative), を (accusative), に
    // (dative). They are separate morphemes *after* the noun, so they live in the template's
    // literal text and `GrammaticalCase` never comes into it. `JapaneseNoun::inflect` ignores its
    // `case` parameter entirely.
    //
    // A boundary rather than a gap: this is the word-order boundary (`docs/EXTENSIBILITY.md`
    // §2.12), which SOV-with-postpositions reconfirms from outside Indo-European.
    let neko = JapaneseNoun::neko();
    let hon = JapaneseNoun::hon();
    let formal = NarrationContext::new().register(Register::Formal);

    // And the boundary costs less than it looks like it should. A verb has to hang off *some*
    // placeholder, which suggests a verb-final clause needs a noun repeated at the end — but the
    // hidden marker `?` solves it exactly: the noun renders nothing and the verb still conjugates,
    // giving idiomatic SOV with correct politeness and no duplication.
    assert_eq!(
        say_with!(formal, "{0}が{1}を{?0 see}", neko, hon),
        "猫が本を見ます"
    );

    // Hanging the verb off the object placeholder works too, and is what you would write if the
    // particle were not needed:
    assert_eq!(
        say_with!(formal, "{0}が{1 see}", neko, hon),
        "猫が本 見ます"
    );
}

// ------------------------------- hole 5: Register cannot express finer gradation --

#[test]
fn hole_5_register_has_three_values_and_keigo_has_more_levels() {
    // `Register` is a **closed enum** in `ranting`: Formal / Neutral / Casual. Japanese politeness
    // has more distinctions than that — teineigo, sonkeigo and kenjougo are three axes, not three
    // points on one — so this lexicon has to map two of the three values onto the same form.
    //
    // Not a design failure, and worth stating precisely: the escape hatch is
    // `NarrationContext::dialect`, an *open* `Option<&'static str>` the crate never interprets. A
    // fork wanting five levels uses that. What is unavailable is doing so through `register`
    // itself.
    let neko = JapaneseNoun::neko();
    let neutral = NarrationContext::new().register(Register::Neutral);
    let casual = NarrationContext::new().register(Register::Casual);
    assert_eq!(
        say_with!(neutral, "{0 are}", neko),
        say_with!(casual, "{0 are}", neko)
    );
}
