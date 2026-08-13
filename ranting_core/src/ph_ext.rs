// (c) Roel Kluin 2026 MIT
//! Hand-written tokenizer for the placeholder-internals grammar previously
//! matched by [`crate::grammar::PH_EXT`] (ROADMAP.md Phase 4 item 6).
//!
//! `PH_EXT` is kept in `grammar.rs` as the *reference grammar* -- the
//! authoritative spec this module implements by hand -- and this module's
//! `tests` submodule differentially fuzzes the two against each other (see
//! below), but [`parse`] is what `ranting_derive` actually calls now. The
//! payoff (per the roadmap item): precise error spans instead of the old
//! blanket "Error in placeholder" message, and no more regex-dialect
//! coupling between `ranting_core` and the two consumer crates.
//!
//! ## Reading the grammar
//!
//! `PH_EXT` is (reformatted from `grammar.rs`):
//!
//! ```text
//! ^(?x)
//!     (?P<uc>[,^])?+
//!     (?P<pre>(?:
//!         \??[aA]n?|\??[sS]ome|\??[tT]he|[Tt]h[eo]se|`[\w-]+|
//!         (?:[cC]an(?:'t)?|[mM]ay|(?:[sS]ha|[wW]i)ll|
//!         (?:(?:[aA]|[wW]e)re|[hH]a(?:d|ve)|[dD]o|(?:[cCwW]|[sS]h)ould|[mM](?:us|igh)t)(?:n't)?+)
//!         (?:\s+(?:\??an?|\??some|\??the|th[eo]se|`[\w-]+))?
//!     )(?:\s+[\w-]+)*?\s+)?+
//!     (?P<nr>[+-]|(?:\#|\??\$)\w+\s+)?+
//!     (?P<case>[`=@~*?%])?+
//!     (?P<noun>[\w-]+)
//!     (?P<post>\s+[<=>%!]*(?:[\w-]+\s+)*?(?:[\w-]+')?[\w-]+|'\w*)?$
//! ```
//!
//! Every optional group except `post` is written with what looks like a
//! *possessive* quantifier (`?+`) in the source. Two experiments show that
//! name is misleading twice over:
//!
//! 1. **It isn't possessive/atomic.** `(?:a)?+a` against `"a"` still matches
//!    (true possessive semantics would reject it: the `?+` would commit to
//!    consuming the `a`, leaving nothing for the mandatory trailing `a`).
//!    `regex` is an automaton engine, not a backtracking VM, and it doesn't
//!    special-case `?+` into a real atomic cut -- the syntax is accepted but
//!    not enforced as irrevocable.
//! 2. **It isn't even "optional" (zero-or-one).** `(?P<x>[ab])?+c` against
//!    `"abc"` matches with `x = "b"` -- i.e. the group matched *twice*
//!    (`a` then `b`), and the named capture retains only the *last*
//!    repetition's span, exactly like an ordinary repeated capture group.
//!    `X?+` empirically behaves like `X*` (zero-or-more, greedy): the `?`
//!    makes one repetition of `X` optional, and the `+` suffix repeats that
//!    "optional `X`" one-or-more times, which nets out to "zero or more
//!    `X`, with only the final repetition's capture kept". This is real,
//!    exercised behavior, not a corner case: `{=?w weren't}` in
//!    `src/language/english.rs` relies on `case` matching a two-character
//!    run (`=` then `?`), retaining only `?`, with `=` still consumed as
//!    part of the group.
//!
//! So `uc`, `pre`, `nr`, and `case` are each genuinely **repeated,
//! backtracking, greedy-star** groups -- not simple optional presence
//! checks. [`parse`] implements this with one generic engine,
//! [`star_candidates`]: given a "one repetition" step function (returning
//! that repetition's candidate end-offsets, in the same priority order the
//! regex alternation would try them), it explores every reachable
//! repetition count via a small depth-first search, preferring more
//! repetitions first (greedy) and yielding, for each reachable state, the
//! *last* repetition's span (or `None` for zero repetitions) paired with
//! the cumulative end offset -- mirroring "named capture keeps only the
//! final iteration" exactly. [`parse`] nests four such searches (uc, pre,
//! nr, case) and tries combinations outer-to-inner until the mandatory
//! `noun` and optional `post` both parse. The search space stays tiny for
//! realistic placeholder text (a handful of repetitions of short atoms), so
//! this is cheap despite being "backtracking" in the literal sense.
//!
//! One more piece completes the picture: `pre`'s own trailing
//! `(?:\s+[\w-]+)*?` ("extra words") is a genuinely *lazy* quantifier
//! (unlike the greedy star groups above), and unlike what an earlier
//! version of this doc claimed, it's reachable in practice, not just in
//! theory -- see [`finish_pre_candidates`]'s doc for a real example from
//! this crate's own doctests (`` {can `man pair of #0 boots remain} ``,
//! where `pre` has to swallow two extra words before `nr`/`noun` can even
//! start). [`finish_pre_candidates`] yields every extra-word count in lazy
//! (fewest-first) order, and [`star_candidates`]'s backtracking is what
//! actually reaches for a non-zero count when zero fails downstream.

/// A parsed span within the placeholder-internals text, mirroring the
/// handful of `regex::Match` methods `ranting_derive`'s `handle_param`
/// already relied on (`as_str`/`start`/`end`) so that call site's shape
/// barely changes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PhSpan<'a> {
    text: &'a str,
    start: usize,
    end: usize,
}

impl<'a> PhSpan<'a> {
    pub fn as_str(&self) -> &'a str {
        self.text
    }
    pub fn start(&self) -> usize {
        self.start
    }
    pub fn end(&self) -> usize {
        self.end
    }
}

fn span(s: &str, start: usize, end: usize) -> PhSpan<'_> {
    PhSpan {
        text: &s[start..end],
        start,
        end,
    }
}

/// The result of successfully parsing one placeholder's internals -- the
/// hand-written replacement for `regex::Captures` at this call site. Named
/// groups match `PH_EXT`'s: `uc`, `pre`, `nr`, `case`, `noun` (always
/// present), `post`.
#[derive(Debug, Clone, Copy)]
pub struct PhExtMatch<'a> {
    uc: Option<PhSpan<'a>>,
    pre: Option<PhSpan<'a>>,
    nr: Option<PhSpan<'a>>,
    case: Option<PhSpan<'a>>,
    noun: PhSpan<'a>,
    post: Option<PhSpan<'a>>,
    whole: PhSpan<'a>,
}

impl<'a> PhExtMatch<'a> {
    /// Named-group lookup, mirroring `regex::Captures::name`.
    pub fn name(&self, key: &str) -> Option<PhSpan<'a>> {
        match key {
            "uc" => self.uc,
            "pre" => self.pre,
            "nr" => self.nr,
            "case" => self.case,
            "noun" => Some(self.noun),
            "post" => self.post,
            _ => None,
        }
    }

    /// The whole matched (i.e. whole input) span, mirroring
    /// `regex::Captures::get(0)` -- used only for a whole-placeholder error
    /// span in `handle_param`.
    pub fn whole(&self) -> PhSpan<'a> {
        self.whole
    }
}

/// A parse failure, with a byte-offset span into the placeholder-internals
/// text (i.e. relative to the string passed to [`parse`], *not* the whole
/// `say!()` format string -- callers are expected to offset it, exactly as
/// they previously offset `PHE`'s capture spans).
#[derive(Debug, Clone)]
pub struct PhExtError {
    pub start: usize,
    pub end: usize,
    pub message: String,
}

fn is_word(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn is_word_or_hyphen(c: char) -> bool {
    is_word(c) || c == '-'
}

fn leading_ws_len(s: &str) -> usize {
    s.chars()
        .take_while(|c| c.is_whitespace())
        .map(char::len_utf8)
        .sum()
}

fn leading_word_or_hyphen_len(s: &str) -> usize {
    s.chars()
        .take_while(|&c| is_word_or_hyphen(c))
        .map(char::len_utf8)
        .sum()
}

fn leading_word_len(s: &str) -> usize {
    s.chars()
        .take_while(|&c| is_word(c))
        .map(char::len_utf8)
        .sum()
}

/// `\??[aA]n?` -- optional `?`, then `a`/`A`, then optional (lowercase-only)
/// `n`.
fn match_a1(rest: &str) -> Option<usize> {
    let mut len = 0;
    let mut r = rest;
    if let Some(stripped) = r.strip_prefix('?') {
        len += 1;
        r = stripped;
    }
    let c0 = r.chars().next()?;
    if c0 != 'a' && c0 != 'A' {
        return None;
    }
    len += c0.len_utf8();
    r = &r[c0.len_utf8()..];
    if r.starts_with('n') {
        len += 1;
    }
    Some(len)
}

/// `\??[sS]ome`
fn match_a2(rest: &str) -> Option<usize> {
    let mut len = 0;
    let mut r = rest;
    if let Some(stripped) = r.strip_prefix('?') {
        len += 1;
        r = stripped;
    }
    let c0 = r.chars().next()?;
    if c0 != 's' && c0 != 'S' {
        return None;
    }
    r = &r[c0.len_utf8()..];
    if let Some(rest2) = r.strip_prefix("ome") {
        let _ = rest2;
        Some(len + c0.len_utf8() + 3)
    } else {
        None
    }
}

/// `\??[tT]he`
fn match_a3(rest: &str) -> Option<usize> {
    let mut len = 0;
    let mut r = rest;
    if let Some(stripped) = r.strip_prefix('?') {
        len += 1;
        r = stripped;
    }
    let c0 = r.chars().next()?;
    if c0 != 't' && c0 != 'T' {
        return None;
    }
    r = &r[c0.len_utf8()..];
    if r.starts_with("he") {
        Some(len + c0.len_utf8() + 2)
    } else {
        None
    }
}

/// `[Tt]h[eo]se` -- no leading `?` here, unlike the other article words.
fn match_a4(rest: &str) -> Option<usize> {
    let mut chars = rest.chars();
    let c0 = chars.next()?;
    if c0 != 't' && c0 != 'T' {
        return None;
    }
    if chars.next()? != 'h' {
        return None;
    }
    let c2 = chars.next()?;
    if c2 != 'e' && c2 != 'o' {
        return None;
    }
    if rest[c0.len_utf8() + 1 + c2.len_utf8()..].starts_with("se") {
        Some(c0.len_utf8() + 1 + c2.len_utf8() + 2)
    } else {
        None
    }
}

/// `` `[\w-]+ `` -- a backtick followed by one or more word/hyphen chars.
fn match_backtick(rest: &str) -> Option<usize> {
    let r = rest.strip_prefix('`')?;
    let wlen = leading_word_or_hyphen_len(r);
    if wlen == 0 { None } else { Some(1 + wlen) }
}

/// All candidate lengths for the lowercase-only nested article that may
/// follow a modal verb: `\??an?|\??some|the|th[eo]se|` word (no leading `?`
/// on `the`/`th[eo]se` here, and no uppercase variants at all -- see the
/// module doc's grammar dump: this nested alternative is textually distinct
/// from the top-level one), in priority order.
///
/// Returns every alternative that locally matches, not just the first --
/// `the` is a textual prefix of `these`/`those`, so (same backtracking
/// rationale as [`pre_candidates`]) trying only the first match would wrongly
/// commit to `the` inside `these thing` and never recover `these`.
fn match_nested_article_candidates(rest: &str) -> Vec<usize> {
    let mut out = Vec::new();
    // \??an?
    {
        let mut len = 0;
        let mut r = rest;
        if let Some(stripped) = r.strip_prefix('?') {
            len += 1;
            r = stripped;
        }
        if let Some(stripped) = r.strip_prefix('a') {
            len += 1;
            r = stripped;
            if r.starts_with('n') {
                len += 1;
            }
            out.push(len);
        }
    }
    // \??some
    {
        let mut len = 0;
        let mut r = rest;
        if let Some(stripped) = r.strip_prefix('?') {
            len += 1;
            r = stripped;
        }
        if r.starts_with("some") {
            out.push(len + 4);
        }
    }
    if rest.starts_with("the") {
        out.push(3);
    }
    if rest.starts_with("these") || rest.starts_with("those") {
        out.push(5);
    }
    if let Some(len) = match_backtick(rest) {
        out.push(len);
    }
    out
}

struct ModalWord {
    lower: &'static str,
    upper: &'static str,
    contraction: Option<&'static str>,
}

const MODAL_WORDS: &[ModalWord] = &[
    ModalWord {
        lower: "can",
        upper: "Can",
        contraction: Some("'t"),
    },
    ModalWord {
        lower: "may",
        upper: "May",
        contraction: None,
    },
    ModalWord {
        lower: "shall",
        upper: "Shall",
        contraction: None,
    },
    ModalWord {
        lower: "will",
        upper: "Will",
        contraction: None,
    },
    ModalWord {
        lower: "are",
        upper: "Are",
        contraction: Some("n't"),
    },
    ModalWord {
        lower: "were",
        upper: "Were",
        contraction: Some("n't"),
    },
    ModalWord {
        lower: "had",
        upper: "Had",
        contraction: Some("n't"),
    },
    ModalWord {
        lower: "have",
        upper: "Have",
        contraction: Some("n't"),
    },
    ModalWord {
        lower: "do",
        upper: "Do",
        contraction: Some("n't"),
    },
    ModalWord {
        lower: "could",
        upper: "Could",
        contraction: Some("n't"),
    },
    ModalWord {
        lower: "would",
        upper: "Would",
        contraction: Some("n't"),
    },
    ModalWord {
        lower: "should",
        upper: "Should",
        contraction: Some("n't"),
    },
    ModalWord {
        lower: "must",
        upper: "Must",
        contraction: Some("n't"),
    },
    ModalWord {
        lower: "might",
        upper: "Might",
        contraction: Some("n't"),
    },
];

/// The modal-verb (+ optional contraction) branch of `pre`'s first atom.
fn match_modal(rest: &str) -> Option<usize> {
    for word in MODAL_WORDS {
        for base in [word.lower, word.upper] {
            if let Some(after) = rest.strip_prefix(base) {
                let mut len = base.len();
                if let Some(suffix) = word.contraction
                    && after.starts_with(suffix)
                {
                    len += suffix.len();
                }
                return Some(len);
            }
        }
    }
    None
}

/// All candidate end-offsets for finishing `pre`'s tail --
/// `(?:\s+[\w-]+)*?\s+` -- starting right after its fixed "first word" atom,
/// in **lazy** priority order (fewest "extra words" first, since this
/// quantifier is genuinely lazy, unlike the greedy `?+`/star groups
/// elsewhere in the grammar).
///
/// An earlier version of this module argued this loop could never fire --
/// reasoning that if there's no whitespace immediately after the first
/// word, no repetition count could rescue it, since each repetition also
/// starts with `\s+` at that same fixed position. That's true as far as it
/// goes, but it missed that when whitespace *is* immediately available (the
/// overwhelmingly common case), the lazy loop still faces a real choice:
/// stop now (0 extras), or swallow one extra word and look for whitespace
/// again further on. Only the "stop now" option was ever tried, so a real
/// placeholder from this crate's own doctests --
/// `` {can `man pair of #0 boots remain} `` -- was rejected: it needs `pre`
/// to swallow two extra words (`pair`, `of`) after `` `man `` so that `nr`
/// (`#0 `) and `noun` (`boots`) can start where the grammar actually
/// intends. [`star_candidates`]'s backtracking search is what actually
/// exercises the non-zero counts this function offers -- it only reaches
/// for them when 0 extras leads to an overall parse failure downstream.
fn finish_pre_candidates(s: &str, after_first: usize) -> Vec<usize> {
    let mut out = Vec::new();
    let mut extras_end = after_first;
    loop {
        let ws_len = leading_ws_len(&s[extras_end..]);
        if ws_len == 0 {
            break;
        }
        // Candidate: stop here, using this whitespace run as pre's mandatory
        // trailing `\s+`.
        out.push(extras_end + ws_len);
        // Try to extend: consume one more `\s+[\w-]+` extra-word repetition
        // instead, reusing the same whitespace as that repetition's leading
        // `\s+`.
        let word_len = leading_word_or_hyphen_len(&s[extras_end + ws_len..]);
        if word_len == 0 {
            break;
        }
        extras_end += ws_len + word_len;
    }
    out
}

/// All candidate end-offsets for *one repetition* of `pre`'s inner pattern
/// at `pos`, in the same priority order the regex engine would explore them
/// (each of the six `FIRST` alternatives in turn, "with nested article"
/// before "without" for the modal branch, and -- within each -- fewest
/// extra words before more, per [`finish_pre_candidates`]). Does **not**
/// include the "no match" fallback -- that's [`star_candidates`]'s job,
/// since `pre` (like `uc`/`nr`/`case`) is a repeated group, not a simple
/// optional (see the module doc).
fn pre_one_rep(s: &str, pos: usize) -> Vec<usize> {
    let rest = &s[pos..];
    let mut out = Vec::new();
    for matcher in [match_a1, match_a2, match_a3, match_a4, match_backtick] {
        if let Some(len) = matcher(rest) {
            out.extend(finish_pre_candidates(s, pos + len));
        }
    }
    if let Some(len) = match_modal(rest) {
        let after_modal = pos + len;
        let ws_len = leading_ws_len(&s[after_modal..]);
        if ws_len > 0 {
            for art_len in match_nested_article_candidates(&s[after_modal + ws_len..]) {
                out.extend(finish_pre_candidates(s, after_modal + ws_len + art_len));
            }
        }
        out.extend(finish_pre_candidates(s, after_modal));
    }
    out
}

/// Candidate end-offsets for *one repetition* of `nr`'s inner pattern at
/// `pos`.
fn nr_one_rep(s: &str, pos: usize) -> Vec<usize> {
    match_nr(s, pos).into_iter().collect()
}

/// Candidate end-offset for *one repetition* of `case`'s inner pattern
/// (a single case-marker char) at `pos`.
fn case_one_rep(s: &str, pos: usize) -> Vec<usize> {
    match s[pos..].chars().next() {
        Some(c) if is_case_char(c) => vec![pos + c.len_utf8()],
        _ => Vec::new(),
    }
}

/// Candidate end-offset for *one repetition* of `uc`'s inner pattern
/// (a single `,`/`^` char) at `pos`.
fn uc_one_rep(s: &str, pos: usize) -> Vec<usize> {
    match s[pos..].chars().next() {
        Some(c) if c == ',' || c == '^' => vec![pos + c.len_utf8()],
        _ => Vec::new(),
    }
}

/// The generic "zero-or-more, greedy, backtracking" engine behind `uc`,
/// `pre`, `nr`, and `case` (see the module doc for why all four need this
/// instead of simple optional-presence logic). `one_rep(s, pos)` returns
/// the candidate end-offsets for exactly one repetition starting at `pos`,
/// in priority order.
///
/// Returns every reachable `(last_repetition_span, cumulative_end_offset)`
/// pair, ordered with more repetitions (greedier) first and zero
/// repetitions (`last_repetition_span = None`) last -- exactly the order
/// [`parse`]'s nested search should try them in.
fn star_candidates(
    s: &str,
    pos: usize,
    one_rep: impl Fn(&str, usize) -> Vec<usize> + Copy,
) -> Vec<(Option<(usize, usize)>, usize)> {
    fn go(
        s: &str,
        pos: usize,
        last: Option<(usize, usize)>,
        one_rep: impl Fn(&str, usize) -> Vec<usize> + Copy,
        out: &mut Vec<(Option<(usize, usize)>, usize)>,
    ) {
        for end in one_rep(s, pos) {
            if end > pos {
                go(s, end, Some((pos, end)), one_rep, out);
            }
        }
        out.push((last, pos));
    }
    let mut out = Vec::new();
    go(s, pos, None, one_rep, &mut out);
    out
}

/// `(?P<nr>[+-]|(?:\#|\??\$)\w+\s+)?+`
fn match_nr(s: &str, pos: usize) -> Option<usize> {
    let rest = &s[pos..];
    let c0 = rest.chars().next()?;
    if c0 == '+' || c0 == '-' {
        return Some(pos + c0.len_utf8());
    }
    let mut idx = if c0 == '#' {
        c0.len_utf8()
    } else if c0 == '?' {
        let r = &rest[c0.len_utf8()..];
        if !r.starts_with('$') {
            return None;
        }
        c0.len_utf8() + 1
    } else if c0 == '$' {
        c0.len_utf8()
    } else {
        return None;
    };
    let word_len = leading_word_len(&rest[idx..]);
    if word_len == 0 {
        return None;
    }
    idx += word_len;
    let ws_len = leading_ws_len(&rest[idx..]);
    if ws_len == 0 {
        return None;
    }
    idx += ws_len;
    Some(pos + idx)
}

fn is_case_char(c: char) -> bool {
    matches!(c, '`' | '=' | '@' | '~' | '*' | '?' | '%')
}

/// A single whitespace-delimited token from `post`'s word content is valid
/// iff it has zero apostrophes (a plain `[\w-]+` word), or -- only when
/// `allow_apostrophe` (i.e. it's the last token) -- exactly one apostrophe
/// with non-empty `[\w-]+` content on both sides (a contraction like
/// `don't`, matching `(?:[\w-]+')?[\w-]+`).
fn token_valid(tok: &str, allow_apostrophe: bool) -> bool {
    match tok.find('\'') {
        None => tok.chars().all(is_word_or_hyphen),
        Some(_) if !allow_apostrophe => false,
        Some(idx) => {
            let before = &tok[..idx];
            let after = &tok[idx + 1..];
            !before.is_empty()
                && !after.is_empty()
                && !after.contains('\'')
                && before.chars().all(is_word_or_hyphen)
                && after.chars().all(is_word_or_hyphen)
        }
    }
}

/// `(?P<post>\s+[<=>%!]*(?:[\w-]+\s+)*?(?:[\w-]+')?[\w-]+|'\w*)?$`
///
/// Returns the end offset on success -- always `s.len()` when it succeeds,
/// since `post` is immediately followed by `$` in the source grammar.
fn match_post(s: &str, pos: usize) -> Option<usize> {
    let rest = &s[pos..];
    if let Some(r) = rest.strip_prefix('\'') {
        let wlen = leading_word_len(r);
        return Some(pos + 1 + wlen);
    }
    let ws_len = leading_ws_len(rest);
    if ws_len == 0 {
        return None;
    }
    let mut idx = ws_len;
    idx += rest[idx..]
        .chars()
        .take_while(|c| matches!(c, '<' | '=' | '>' | '%' | '!'))
        .map(char::len_utf8)
        .sum::<usize>();
    let body = &rest[idx..];
    if body.is_empty()
        || body.starts_with(char::is_whitespace)
        || body.ends_with(char::is_whitespace)
    {
        return None;
    }
    let tokens: Vec<&str> = body.split_whitespace().collect();
    let n = tokens.len();
    for (i, tok) in tokens.iter().enumerate() {
        if !token_valid(tok, i + 1 == n) {
            return None;
        }
    }
    Some(pos + rest.len())
}

fn describe(tail: &str) -> String {
    if tail.is_empty() {
        "end of placeholder".to_string()
    } else {
        let excerpt: String = tail.chars().take(24).collect();
        format!("`{excerpt}`")
    }
}

/// Parses one placeholder's internals (the text `PH_START` captured between
/// `{` and `:`/`}`) into a [`PhExtMatch`], or a precise [`PhExtError`] with a
/// byte-offset span into `s` and a specific message -- e.g. "expected
/// article or verb, found `...`" for a malformed `post` word, replacing the
/// old blanket "Error in placeholder" message that used to fire whenever
/// `PH_EXT.is_match(s)` returned `false`.
pub fn parse(s: &str) -> Result<PhExtMatch<'_>, PhExtError> {
    // Backtracking search over four repeated (star) groups -- uc, pre, nr,
    // case -- in priority order (more repetitions first, per group), until
    // one combination lets the mandatory noun and optional post both parse.
    // See the module doc for why all four need real repetition/backtracking
    // support rather than simple optional-presence checks despite being
    // written with `?+` in the source grammar.
    for (uc_span, pos0) in star_candidates(s, 0, uc_one_rep) {
        for (pre_span, pos1) in star_candidates(s, pos0, pre_one_rep) {
            for (nr_span, pos2) in star_candidates(s, pos1, nr_one_rep) {
                for (case_span, pos3) in star_candidates(s, pos2, case_one_rep) {
                    let noun_len = leading_word_or_hyphen_len(&s[pos3..]);
                    if noun_len == 0 {
                        continue;
                    }
                    let noun_end = pos3 + noun_len;
                    let post = if noun_end == s.len() {
                        None
                    } else {
                        match match_post(s, noun_end) {
                            Some(end) if end == s.len() => Some(span(s, noun_end, end)),
                            _ => continue,
                        }
                    };
                    return Ok(PhExtMatch {
                        uc: uc_span.map(|(a, b)| span(s, a, b)),
                        pre: pre_span.map(|(a, b)| span(s, a, b)),
                        nr: nr_span.map(|(a, b)| span(s, a, b)),
                        case: case_span.map(|(a, b)| span(s, a, b)),
                        noun: span(s, pos3, noun_end),
                        post,
                        whole: span(s, 0, s.len()),
                    });
                }
            }
        }
    }

    // Every combination failed. For the error message/span, re-derive the
    // simplest baseline (uc/pre/nr/case all absent) -- the most common
    // failure shape (garbage where a variable name or a post-noun word was
    // expected), and good enough for a specific, actionable message even
    // though it isn't necessarily the exact combination the search
    // exhausted last. See ROADMAP.md Phase 4 item 6 for why full precision
    // here wasn't pursued: `PH_EXT`'s own error behavior on truly malformed
    // input was never specified beyond "reject it", so any specific,
    // correctly-scoped message is a strict improvement over the old
    // blanket "Error in placeholder".
    let noun_len = leading_word_or_hyphen_len(s);
    if noun_len == 0 {
        return Err(PhExtError {
            start: 0,
            end: s.len(),
            message: format!("expected a noun or variable name, found {}", describe(s)),
        });
    }
    let noun_end = noun_len;
    Err(PhExtError {
        start: noun_end,
        end: s.len(),
        message: format!(
            "expected article or verb, found {}",
            describe(&s[noun_end..])
        ),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::PH_EXT;
    use regex::Regex;

    fn ph_ext_re() -> Regex {
        Regex::new(PH_EXT).expect("PH_EXT must remain a valid regex")
    }

    /// For each `input`, both the reference `PH_EXT` regex and the
    /// hand-written [`parse`] must agree: either both reject it, or both
    /// accept it with identical `uc`/`pre`/`nr`/`case`/`noun`/`post` capture
    /// text. This is the parity check backing this module's design claims
    /// (especially the "extra words never fire" argument in the module
    /// doc) -- not just a spot-check of a few examples.
    fn assert_parity(re: &Regex, input: &str) {
        let regex_caps = re.captures(input);
        let hand = parse(input);
        match (regex_caps, hand) {
            (None, Err(_)) => {}
            (Some(caps), Ok(m)) => {
                for name in ["uc", "pre", "nr", "case", "noun", "post"] {
                    let expected = caps.name(name).map(|x| x.as_str());
                    let actual = m.name(name).map(|x| x.as_str());
                    assert_eq!(
                        expected, actual,
                        "group {name:?} mismatch for input {input:?}: regex={expected:?} hand={actual:?}"
                    );
                }
            }
            (None, Ok(m)) => {
                panic!("hand parser accepted {input:?} as {m:?} but PH_EXT rejects it")
            }
            (Some(caps), Err(e)) => {
                panic!("PH_EXT accepted {input:?} as {caps:?} but hand parser rejects it: {e:?}")
            }
        }
    }

    #[test]
    fn parity_curated_corpus() {
        let re = ph_ext_re();
        let inputs = [
            "who",
            "=who",
            "@who",
            "`who",
            "~who",
            "%who",
            "?who",
            "*who",
            "who's",
            "who'",
            "a item",
            "an item",
            "?a item",
            "?an item",
            "some item",
            "?some item",
            "the item",
            "The item",
            "these items",
            "those items",
            "`person book",
            "`who's",
            "haven't =who",
            "can go",
            "can't go",
            "may go",
            "shall go",
            "will go",
            "are going",
            "aren't going",
            "were going",
            "weren't going",
            "had gone",
            "hadn't gone",
            "have gone",
            "haven't gone",
            "do go",
            "don't go",
            "could go",
            "couldn't go",
            "would go",
            "wouldn't go",
            "should go",
            "shouldn't go",
            "must go",
            "mustn't go",
            "might go",
            "mightn't go",
            "do the thing",
            "do a thing",
            "do some thing",
            "do these thing",
            "do those thing",
            "do `who thing",
            "+items",
            "-items",
            "#5 items",
            "$n items",
            "?$n items",
            ",who",
            "^who",
            ",=who",
            "^`who",
            "who <run",
            "who =run",
            "who >run",
            "who <=run",
            "who %run",
            "who <%run",
            "who !good",
            "who !!good",
            "who do run",
            "who don't run",
            "who could have run",
            "who run away",
            "who don't run away",
            "",
            "  ",
            "who ",
            " who",
            "=",
            "= ",
            "who(",
            "who run(",
            "who !!!good",
            "who !good bad",
            "wh@o",
            "who run away today",
            "who don't run away today",
            // Case-marker *runs* (see the module doc's `?+` == `X*` finding):
            // real usage in src/language/english.rs relies on these.
            "=?w weren't",
            "=?w're",
            "?=0 verb",
            "??w",
            ",,,who",
            "^^who",
        ];
        for input in inputs {
            assert_parity(&re, input);
        }
    }

    proptest::proptest! {
        #[test]
        fn parity_fuzzed(
            uc in proptest::option::of(proptest::sample::select(vec![',', '^'])),
            pre_word in proptest::option::of(proptest::sample::select(vec![
                "a", "an", "?a", "?an", "some", "?some", "the", "The", "these", "Those",
                "`ref", "can", "can't", "may", "will", "are", "aren't", "do", "don't",
                "could", "couldn't", "do the", "do a", "will `x",
            ])),
            nr in proptest::option::of(proptest::sample::select(vec![
                "+", "-", "#5 ", "$n ", "?$n ",
            ])),
            case in proptest::option::of(proptest::sample::select(vec![
                '`', '=', '@', '~', '*', '?', '%',
            ])),
            noun in proptest::sample::select(vec!["noun", "who", "item-thing", "x1"]),
            post in proptest::option::of(proptest::sample::select(vec![
                "'s", "'", " run", " =run", " <run", " !good", " !!good",
                " don't run", " run away", " run away today",
            ])),
        ) {
            let mut s = String::new();
            if let Some(c) = uc { s.push(c); }
            if let Some(p) = pre_word { s.push_str(p); s.push(' '); }
            if let Some(n) = nr { s.push_str(n); }
            if let Some(c) = case { s.push(c); }
            s.push_str(noun);
            if let Some(p) = post { s.push_str(p); }
            let re = ph_ext_re();
            assert_parity(&re, &s);
        }
    }

    #[test]
    fn error_message_specific_for_bad_post() {
        let err = parse("who run(").unwrap_err();
        assert!(
            err.message.contains("expected article or verb"),
            "got: {}",
            err.message
        );
    }

    #[test]
    fn error_message_specific_for_missing_noun() {
        let err = parse("=").unwrap_err();
        assert!(
            err.message.contains("expected a noun or variable name"),
            "got: {}",
            err.message
        );
    }

    /// `"a noun don't run"` looked at first like a `regex`-crate capture
    /// inconsistency (`pre` reported as starting mid-string). It isn't: it's
    /// a legitimate parse once `pre`'s lazy "extra words" are accounted for
    /// (see [`finish_pre_candidates`]'s doc) -- `pre` swallows "noun" as an
    /// extra word after `"a"`, then matches a *second* top-level repetition,
    /// `"don't "` (a bare modal-verb `pre`, no article), leaving `noun =
    /// "run"` with no `post` at all. Kept as a named regression test (not
    /// just folded into the curated-corpus list) because it's exactly the
    /// input that first exposed the gap in this module's design.
    #[test]
    fn pre_extra_words_then_second_repetition() {
        let re = ph_ext_re();
        assert_parity(&re, "a noun don't run");
        let m = parse("a noun don't run").expect("valid per the grammar");
        assert_eq!(m.name("pre").map(|s| s.as_str()), Some("don't "));
        assert_eq!(m.name("noun").map(|s| s.as_str()), Some("run"));
        assert_eq!(m.name("post"), None);
    }
}
