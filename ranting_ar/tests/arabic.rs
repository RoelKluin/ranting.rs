//! What Arabic *does* reach through `ranting`'s public API. The falsification half — what it
//! cannot — is `tests/holes.rs`.
//!
//! Every assertion here is real Arabic, not a mechanism demonstration: the point of a third
//! falsifier is that the two existing ones are both Indo-European and fusional.

use ranting::{say, say_with};
use ranting_ar::ArabicNoun;

// --------------------------------------------------- the third number, on the noun --

#[test]
fn the_dual_renders_on_the_counted_noun() {
    // ROADMAP.md Phase 7 item 11's reason for existing. `to_plural: bool` cannot express three
    // forms; the placeholder's own numeral is what picks the middle one, and it reaches
    // `Ranting::inflect` as `count`.
    let kitab = ArabicNoun::kitab();
    assert_eq!(say!("{$0 1}", 1, kitab), "١ كتاب");
    assert_eq!(say!("{$0 1}", 2, kitab), "٢ كتابان");
    assert_eq!(say!("{$0 1}", 3, kitab), "٣ كتب");
}

#[test]
fn the_dual_works_for_sound_and_broken_plurals_alike() {
    // `كتب` is a broken plural (an internal vowel change), `معلمون`/`طالبات` are sound ones (a
    // suffix). The distinction is invisible at this seam — every form is a table row — which is
    // the honest answer to "does root-and-pattern morphology need anything from the API?". It
    // does not: the hook returns an opaque `String`.
    let muallim = ArabicNoun::muallim();
    assert_eq!(say!("{$0 1}", 2, muallim), "٢ معلمان");
    assert_eq!(say!("{$0 1}", 5, muallim), "٥ معلمون");

    let taliba = ArabicNoun::taliba();
    assert_eq!(say!("{$0 1}", 2, taliba), "٢ طالبتان");
    assert_eq!(say!("{$0 1}", 5, taliba), "٥ طالبات");
}

#[test]
fn the_spelled_out_numeral_channel_reaches_the_dual_too() {
    // Both numeral channels carry the count, so a template may write either.
    let kitab = ArabicNoun::kitab();
    assert_eq!(say!("{#0 1}", 2, kitab), "اثنان كتابان");
    assert_eq!(say!("{#0 1}", 1, kitab), "واحد كتاب");
}

// ------------------------------------------------------- the article, bound and assimilated --

#[test]
fn the_definite_article_is_written_bound_to_its_noun() {
    // `ال` takes no space. Only `elide_article_custom` can drop the separator — that is the
    // post-assembly design `docs/EXTENSIBILITY.md` §2.7 records, and this is its first real user
    // (the Phase 7 item 1 audit found it overridden by neither German nor Spanish).
    assert_eq!(say!("{the 0}", ArabicNoun::qamar()), "القمر");
    assert_eq!(say!("{the 0}", ArabicNoun::kitab()), "الكتاب");
}

#[test]
fn the_article_assimilates_before_a_sun_letter() {
    // Fourteen sun letters double the following consonant and swallow the `ل`; the fourteen moon
    // letters leave it alone. The trigger is `following.chars().next()`, which is all the hook's
    // two-string signature offers — and it is enough, which was one of the spike's open questions.
    assert_eq!(say!("{the 0}", ArabicNoun::shams()), "الشّمس");
    assert_eq!(say!("{the 0}", ArabicNoun::taliba()), "الطّالبة");

    // Moon letters, for contrast, in the same shape.
    assert_eq!(say!("{the 0}", ArabicNoun::qamar()), "القمر");
    assert_eq!(say!("{the 0}", ArabicNoun::muallim()), "المعلم");
}

#[test]
fn the_article_binds_to_whichever_number_was_rendered() {
    // The hook runs *after* assembly, so `following` is the noun as actually inflected — the
    // dual and plural bind and assimilate exactly like the singular, with no extra signal.
    let shams = ArabicNoun::shams();
    assert_eq!(say!("{the +0}", shams), "الشّموس");
    assert_eq!(say!("{the 0}", ArabicNoun::kitab().plural()), "الكتب");
}

#[test]
fn there_is_no_indefinite_article() {
    // Arabic marks indefiniteness by the *bare* noun. The hook returns an empty string rather
    // than declining, because declining would let `ranting` render English "a"/"an".
    assert_eq!(say!("{a 0}", ArabicNoun::kitab()), "كتاب");
    assert_eq!(say!("{a 0}", ArabicNoun::shams()), "شمس");
}

// ------------------------------------------------------------------- verb agreement --

#[test]
fn the_verb_agrees_in_person_gender_and_number() {
    let muallim = ArabicNoun::muallim();
    assert_eq!(say!("{0 كتب}", muallim), "معلم كتب");
    assert_eq!(say!("{+0 كتب}", muallim), "معلمون كتبوا");

    let taliba = ArabicNoun::taliba();
    assert_eq!(say!("{0 كتب}", taliba), "طالبة كتبت");
    assert_eq!(say!("{+0 كتب}", taliba), "طالبات كتبن");
}

#[test]
fn the_verb_agrees_in_the_dual() {
    // Phase 6 item 14's count channel was already sufficient *here* before item 11 — this is the
    // half of Arabic dual that worked, while the noun beside it rendered the plural. The two
    // halves now agree, which is the whole point of the ordering item 4 chose.
    let muallim = ArabicNoun::muallim();
    assert_eq!(say!("{$0 1 كتب}", 2, muallim), "٢ معلمان كتبا");
    assert_eq!(say!("{$0 1 كتب}", 5, muallim), "٥ معلمون كتبوا");

    let taliba = ArabicNoun::taliba();
    assert_eq!(say!("{$0 1 كتب}", 2, taliba), "٢ طالبتان كتبتا");
}

#[test]
fn an_unknown_verb_declines_rather_than_being_mangled() {
    // The decline-rather-than-guess contract both other forks use: a verb outside this closed
    // vocabulary falls through to `ranting`'s own conjugation rather than being silently
    // mis-inflected. Note what that fallback actually produces here — the bare `sing`, not
    // `sings`: English agreement is driven by the *subject label*, and this crate's is `هو`,
    // which `ranting` does not recognize and degrades to non-agreement on. That degradation is
    // deliberate (Phase 4 item 4 made unrecognized subjects degrade instead of panicking) and it
    // is what makes an unmodelled verb visibly wrong rather than plausibly wrong.
    let muallim = ArabicNoun::muallim();
    assert_eq!(say!("{0 sing}", muallim), "معلم sing");
}

// ----------------------------------------------------------------------- numerals --

#[test]
fn digits_render_in_arabic_indic_form() {
    let kitab = ArabicNoun::kitab();
    assert_eq!(say!("{$0 1}", 12, kitab), "١٢ كتب");
    // A `:fmt` width spec survives, because the hook transcribes what English rendered rather
    // than re-formatting the count.
    assert_eq!(say!("{$0 1:>3}", 12, kitab), " ١٢ كتب");
}

#[test]
fn spelled_numerals_take_the_opposite_gender_polarity() {
    // Arabic's gender polarity: 3-10 take the gender *opposite* to the noun they count. This is
    // the sharpest thing in the crate that `count` and `NounClass` together have to be sufficient
    // for — both arrive at `inflect_numeral_custom`, and they are.
    assert_eq!(say!("{#0 1}", 3, ArabicNoun::kitab()), "ثلاثة كتب"); // masculine noun
    assert_eq!(say!("{#0 1}", 3, ArabicNoun::taliba()), "ثلاث طالبات"); // feminine noun

    // 1 and 2 agree normally rather than polarizing.
    assert_eq!(say!("{#0 1}", 1, ArabicNoun::taliba()), "واحدة طالبة");
    assert_eq!(say!("{#0 1}", 2, ArabicNoun::taliba()), "اثنتان طالبتان");
}

// ------------------------------------------------------------------------ pronouns --

#[test]
fn subject_pronouns_have_a_dual_too() {
    let muallim = ArabicNoun::muallim();
    assert_eq!(say!("{=0}", muallim), "هو");
    assert_eq!(say!("{+=0}", muallim), "هم");
    assert_eq!(say!("{=0}", ArabicNoun::taliba()), "هي");
    // `هما` is the dual, reached through the same count channel as the noun and the verb.
    assert_eq!(say!("{$0 =1}", 2, muallim), "٢ هما");
}

// ------------------------------------------------------------------------- context --

#[test]
fn say_with_reproduces_say_when_nothing_is_overridden() {
    // The invariant every fork is expected to hold: `say_with!()` with a default context is
    // byte-identical to `say!()`.
    let ctx = ranting::NarrationContext::default();
    let kitab = ArabicNoun::kitab();
    assert_eq!(say_with!(ctx, "{$0 1}", 2, kitab), say!("{$0 1}", 2, kitab));
}
