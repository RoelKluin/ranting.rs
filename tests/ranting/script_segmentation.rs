// (c) Roel Kluin 2026 MIT
//
// ROADMAP.md Phase 6 item 9: heed!(), ask!() and #[derive(Heed)] — all three
// go through ranting_derive's compile_heed_template — are restricted,
// permanently and by decision, to input whose capture boundaries are marked by
// whitespace. These
// tests pin what that restriction actually is — which is narrower than "no
// CJK": the compiled regex is byte/script-agnostic (`{name}` is `\S+`), so a
// non-ASCII script works fine as long as the *input* separates the pieces the
// template wants to split. What is unsupported is a template whose literal and
// capture segments abut with no whitespace, because build_heed_pattern joins
// every pair of adjacent segments with a mandatory `\s+`.
//
// The failure mode is the point: an unsegmented template returns None, never a
// wrong capture. See tests/ranting/heed.rs for the space-delimited behavior in
// general, and README.md's heed!() section for the user-facing statement.

use ranting::{Answerable, Heed, Noun, Ranting, ask, heed};

// --- Supported: non-ASCII scripts with whitespace-separated input ------------

#[test]
fn japanese_with_spaced_input_captures_normally() {
    assert_eq!(heed!("取る {item}", "取る 剣"), Some("剣".to_string()));
}

#[test]
fn chinese_with_spaced_input_captures_normally() {
    assert_eq!(heed!("拿 {item}", "拿 剑"), Some("剑".to_string()));
}

#[test]
fn thai_with_spaced_input_captures_normally() {
    assert_eq!(heed!("เอา {item}", "เอา ดาบ"), Some("ดาบ".to_string()));
}

#[test]
fn spaced_non_ascii_supports_every_capture_kind() {
    assert_eq!(
        heed!("{item} を {$count} 個 {target...}", "剣 を 3 個 森 の 中"),
        Some(("剣".to_string(), 3u64, "森 の 中".to_string()))
    );
}

#[test]
fn greedy_capture_after_spaced_non_ascii_literal() {
    assert_eq!(
        heed!("剣を {action...}", "剣を 森 で 取る"),
        Some("森 で 取る".to_string())
    );
}

// --- Unsupported: unsegmented input returns None, never a wrong capture ------

#[test]
fn unspaced_japanese_returns_none_not_a_wrong_capture() {
    // `{item}を取る` needs whitespace before the `を取る` literal; the input has
    // none. The honest answer is no match — not `item = "剣"` guessed by
    // regex backtracking.
    assert_eq!(heed!("{item}を取る", "剣を取る"), None);
}

#[test]
fn unspaced_greedy_capture_returns_none() {
    // `.+?` would happily match "取る" here; the mandatory `\s+` after the
    // `剣を` literal is what blocks it, not the capture pattern.
    assert_eq!(heed!("剣を{action...}", "剣を取る"), None);
}

#[test]
fn unspaced_numeric_capture_returns_none() {
    assert_eq!(heed!("{$n}個", "3個"), None);
    // The same template does match once the input separates the pieces.
    assert_eq!(heed!("{$n} 個", "3 個"), Some(3u64));
}

#[test]
fn unspaced_thai_returns_none() {
    assert_eq!(heed!("เอา{item}", "เอาดาบ"), None);
}

#[test]
fn unspaced_chinese_between_two_captures_returns_none() {
    assert_eq!(heed!("{a}的{b}", "我的剑"), None);
}

// --- The escape hatch: capture the unsegmented run whole ---------------------

#[test]
fn a_whole_unsegmented_clause_is_one_word_capture() {
    // `{name}` is "one whitespace-delimited token", and an unspaced Japanese
    // clause is exactly one such token. So a caller who wants to run their own
    // segmenter can take the run whole and split it themselves.
    assert_eq!(heed!("{clause}", "剣を取る"), Some("剣を取る".to_string()));
}

#[test]
fn a_greedy_capture_also_takes_the_unsegmented_run_whole() {
    assert_eq!(
        heed!("{clause...}", "剣を取る"),
        Some("剣を取る".to_string())
    );
}

#[test]
fn a_trailing_unsegmented_run_is_captured_whole_after_a_spaced_literal() {
    assert_eq!(
        heed!("命令 {rest...}", "命令 剣を取る"),
        Some("剣を取る".to_string())
    );
}

// --- The one carve-out: a punctuation-only literal abuts without whitespace --

#[test]
fn a_capture_may_abut_a_punctuation_only_literal() {
    // build_heed_pattern exempts punctuation-only literals from the leading
    // `\s+`, and that exemption is script-agnostic too: the ideographic comma
    // U+3001 is not alphanumeric, so `{item...}、` needs no space before `、`.
    // Note what it does *not* buy: only the boundary immediately before the
    // punctuation is exempt — `取る` after it still needs whitespace.
    assert_eq!(
        heed!("{item...}、 取る", "剣、 取る"),
        Some("剣".to_string())
    );
    assert_eq!(heed!("{item}、 取る", "剣、 取る"), Some("剣".to_string()));
}

#[test]
fn punctuation_glued_to_a_word_in_the_template_is_not_a_carve_out() {
    // `、取る` is a single whitespace-delimited template token containing
    // alphanumeric characters, so it is an ordinary literal and takes the
    // mandatory `\s+` — the exemption is per-segment, not per-character.
    assert_eq!(heed!("{item...}、取る", "剣、取る"), None);
}

// --- ask!() inherits the same restriction (same template compiler) -----------

struct Samurai;

impl Answerable for Samurai {
    type Captures = String;
    fn answer(&self, _speaker: &dyn Ranting, item: String) -> String {
        format!("「{item}」を取った。")
    }
}

#[test]
fn ask_matches_spaced_non_ascii_input() {
    let player = Noun::new("player", "you");
    assert_eq!(
        ask!(player, Samurai, "取る {item}", "取る 剣"),
        Some("「剣」を取った。".to_string())
    );
}

#[test]
fn ask_returns_none_on_unsegmented_input_without_calling_answer() {
    let player = Noun::new("player", "you");
    assert_eq!(ask!(player, Samurai, "{item}を取る", "剣を取る"), None);
}

// --- #[derive(Heed)] inherits it too (it calls compile_heed_template) --------

#[derive(Heed, Debug, PartialEq)]
#[heed(template = "取る {item}")]
struct Toru {
    item: String,
}

#[derive(Heed, Debug, PartialEq)]
#[heed(template = "{item}を取る")]
struct ToruUnspaced {
    item: String,
}

#[test]
fn derive_heed_matches_spaced_non_ascii_input() {
    assert_eq!(
        Toru::heed("取る 剣"),
        Some(Toru {
            item: "剣".to_string()
        })
    );
}

#[test]
fn derive_heed_returns_none_on_unsegmented_input() {
    assert_eq!(ToruUnspaced::heed("剣を取る"), None);
}
