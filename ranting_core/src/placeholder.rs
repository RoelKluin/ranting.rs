// (c) Roel Kluin 2026 MIT
//! Typed placeholder-spec types shared between `ranting_derive` (which bakes
//! them, as compile-time-constant literal expressions, from `say!()`/
//! `say_with!()` template parsing) and `ranting` (which consumes them at
//! runtime in `handle_placeholder`/`handle_placeholder_with_context`).
//!
//! ROADMAP.md Phase 4 item 3: replaces the old `caps: [&str; 5]` array and
//! the `"~TENSE~MARKER:WORD"` / `"~DEGREE~WORD[:TRAILING]"` string sentinels
//! that used to be folded into that array's `post` slot and re-parsed at
//! runtime via `strip_prefix`/`split_once`. The macro already knows, at
//! compile time, which of these shapes a given post-noun word is (a
//! tense-marked verb, a degree-marked adjective, a plain verb, the literal
//! possessive-`'s`, or nothing at all) -- encoding that as a string and
//! re-parsing it on every call was pure overhead, and left "fallback if
//! marker/colon parsing fails" branches in the runtime for states the macro
//! can never actually produce. [`PostSpec`] makes those states
//! unrepresentable instead: there is no string to fail to parse.
//!
//! ## The overloaded `=` marker
//!
//! `=` appears in two different placeholder positions with two different
//! meanings:
//! - **before** the noun (the `case` capture group): subjective pronoun
//!   case, e.g. `{=who}` -> "he"/"they".
//! - **after** the noun (a `post` tense marker): continuous tense,
//!   e.g. `{who =run}` -> "is running".
//!
//! Previously both were carried in bare `&str` fields (`case: "="` vs.
//! `post: "~TENSE~=:running"`), so the overload was disambiguated only by
//! *which array slot the string sat in*, combined with a string prefix on
//! one side. [`PlaceholderSpec`] gives each position its own typed field --
//! `case: CaseKind` (`CaseKind::Subjective` before the noun) vs.
//! `post: PostSpec::Tense(TenseMarker::Continuous, ..)` after it -- so the
//! two meanings can never be confused at the type level, and there is no
//! shared string representation left that would need disambiguating.

/// The pronoun/inflection case requested before or in place of the noun
/// (the `case` capture group: `` ` ``, `=`, `@`, `~`, `%`, `?`, or none/`*`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaseKind {
    /// No case marker (or the `*` "mark this word as the Ranting element"
    /// marker, which doesn't change how the noun itself renders): render
    /// the noun's own name via `Ranting::inflect()`.
    Name,
    /// `?` marker: subject in inflection, but neither the variable nor its
    /// leading space is displayed.
    Hidden,
    /// `=`: subjective pronoun ("he"/"they"/...).
    Subjective,
    /// `@`: objective pronoun ("him"/"them"/...).
    Objective,
    /// `` ` ``: possessive determiner ("his"/"their"/...).
    PossessiveDeterminer,
    /// `~`: possessive pronoun ("his"/"theirs"/...).
    PossessivePronoun,
    /// `%`: reflexive ("himself"/"themselves"/...).
    Reflexive,
}

impl CaseKind {
    /// Maps a parsed `case` capture-group string to its typed form. Used
    /// only by `ranting_derive` when baking a [`PlaceholderSpec`] literal
    /// from the `PH_EXT` regex capture.
    pub const fn from_marker(marker: &str) -> Self {
        match marker.as_bytes() {
            b"=" => CaseKind::Subjective,
            b"@" => CaseKind::Objective,
            b"`" => CaseKind::PossessiveDeterminer,
            b"~" => CaseKind::PossessivePronoun,
            b"%" => CaseKind::Reflexive,
            b"?" => CaseKind::Hidden,
            _ => CaseKind::Name,
        }
    }
}

/// A tense marker attached to a post-noun verb (`<`, `=`, `>`, `<=`, `%`,
/// `<%`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TenseMarker {
    /// `<` -- simple past, no auxiliary.
    Past,
    /// `=` -- present continuous ("is running").
    Continuous,
    /// `>` -- future ("will run").
    Future,
    /// `<=` -- past continuous ("was running").
    PastContinuous,
    /// `%` -- present perfect ("has run").
    PresentPerfect,
    /// `<%` -- past perfect ("had run").
    PastPerfect,
}

impl TenseMarker {
    /// Maps a parsed marker string to its typed form. Used by
    /// `ranting_derive` when baking a [`PostSpec::Tense`] literal.
    pub const fn from_marker(marker: &str) -> Option<Self> {
        match marker.as_bytes() {
            b"<" => Some(TenseMarker::Past),
            b"=" => Some(TenseMarker::Continuous),
            b">" => Some(TenseMarker::Future),
            b"<=" => Some(TenseMarker::PastContinuous),
            b"%" => Some(TenseMarker::PresentPerfect),
            b"<%" => Some(TenseMarker::PastPerfect),
            _ => None,
        }
    }

    /// The marker text `handle_tense_marker` and the runtime tense-override
    /// machinery (`narration::form_for_marker`/`marker_and_form_for_tense`)
    /// key auxiliary-verb selection off of -- preserved as a lookup so that
    /// code, which legitimately still works in terms of the marker (e.g. to
    /// pick an auxiliary verb table entry), doesn't need its own parallel
    /// string table.
    pub const fn as_marker_str(self) -> &'static str {
        match self {
            TenseMarker::Past => "<",
            TenseMarker::Continuous => "=",
            TenseMarker::Future => ">",
            TenseMarker::PastContinuous => "<=",
            TenseMarker::PresentPerfect => "%",
            TenseMarker::PastPerfect => "<%",
        }
    }
}

/// What (if anything) follows the noun, and how to render it. Replaces the
/// old `~TENSE~`/`~DEGREE~` string sentinels folded into the `post` slot of
/// `caps: [&str; 5]`.
#[derive(Debug, Clone, Copy)]
pub enum PostSpec {
    /// No post-noun word.
    None,
    /// Literal `'` / `'s` -- possessive-s, e.g. `{noun's}`.
    PossessiveS,
    /// A verb with no tense marker, exactly as captured (including its
    /// leading whitespace, and possibly more than one word -- e.g. a
    /// modal phrase). `ranting_derive` doesn't split this one down further
    /// at compile time; the runtime's last-word conjugation + leading-word
    /// passthrough logic is unchanged from before this refactor. `say!()`
    /// bakes it fully conjugated; `say_with!()` bakes the base form.
    Verb(&'static str),
    /// A tense-marked verb (`<`, `=`, `>`, `<=`, `%`, `<%`). `word` is the
    /// compile-time-conjugated form for `say!()` or the uninflected base
    /// form for `say_with!()` (matching the pre-refactor `~TENSE~`
    /// payload); `trailing` is any words after it (`""` if none).
    Tense {
        leading_space: &'static str,
        marker: TenseMarker,
        word: &'static str,
        trailing: &'static str,
    },
    /// A comparative/superlative degree word (`!`/`!!`), already fully
    /// resolved at compile time in `word` -- no subject/number/tense
    /// agreement applies to it. `trailing` is any words after it.
    Degree {
        leading_space: &'static str,
        word: &'static str,
        trailing: &'static str,
    },
}

/// Everything `handle_placeholder`/`handle_placeholder_with_context` need to
/// know about one `say!()`/`say_with!()` placeholder, baked as a
/// compile-time-constant literal by `ranting_derive` from the parsed
/// [`PH_EXT`](crate::grammar::PH_EXT) capture groups. Replaces the old
/// `caps: [&str; 5]` array.
#[derive(Debug, Clone, Copy)]
pub struct PlaceholderSpec {
    /// Literal text before the noun (an article word, verb/auxiliary text,
    /// or a possessive backtick placeholder later substituted at runtime
    /// with another noun's possessive form). Still free text: articles and
    /// verbs occurring here are open-ended natural-language words, and
    /// (unlike the `post` sentinel encodings) the runtime's
    /// article-vs-verb-vs-possessive dispatch for this slot depends on
    /// state only known at runtime (`Ranting::skip_article()`), not purely
    /// on the template text -- see `ranting::get_article_or_so`.
    pub pre: &'static str,
    pub plurality: &'static str,
    pub noun_space: &'static str,
    pub case: CaseKind,
    pub post: PostSpec,
}
