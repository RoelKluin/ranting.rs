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

/// Which degree marker was written on a post-noun adjective (`!` or `!!`).
///
/// `ranting_derive` resolves the English degree form at compile time
/// regardless, but a non-English implementation needs to know *which* degree
/// was asked for -- French picks between `noir`, `plus noir` and `le plus
/// noir` -- so the marker is baked alongside the resolved word rather than
/// being consumed by it. Mirrored into the public
/// `ranting::AdjectiveDegree` for the `inflect_adjective_custom` hook.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DegreeKind {
    /// `!` -- comparative ("better", "more careful").
    Comparative,
    /// `!!` -- superlative ("best", "most careful").
    Superlative,
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
    /// A comparative/superlative degree word (`!`/`!!`). `word` is the
    /// English degree form, already fully resolved at compile time -- no
    /// subject/number/tense agreement applies to it *in English*.
    /// `trailing` is any words after it.
    ///
    /// `base` is the adjective exactly as written in the placeholder and
    /// `degree` the marker that was written on it, both kept because
    /// English's resolved `word` is not recoverable back into them and a
    /// language whose adjectives agree with their noun (gender, number,
    /// case) has to inflect the base itself. Same shape and same reason as
    /// `say_with!()` baking the uninflected base verb into
    /// [`PostSpec::Tense`]: bake what runtime cannot re-derive.
    Degree {
        leading_space: &'static str,
        base: &'static str,
        degree: DegreeKind,
        word: &'static str,
        trailing: &'static str,
    },
}

/// Which numeral notation a placeholder's `#var`/`$var` marker asked for.
///
/// Mirrored into the public `ranting::NumeralStyle` for the
/// `inflect_numeral_custom` hook -- like [`CaseKind`] and [`DegreeKind`],
/// and unlike `ranting::NounClass`/`OrthographyRole`, because the marker is
/// *written in the placeholder*: there is something at the macro<->runtime
/// seam to mirror.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumeralKind {
    /// `#var` -- the number spelled out ("two"). `ranting` renders this at
    /// runtime from [`NumeralSpec`]'s count, so a numeral hook can spell it
    /// in another language.
    Words,
    /// `$var` -- the number as the argument's own `Display` output, with
    /// any `:fmt` spec applied. ASCII digits in English.
    Digits,
}

/// The numeral slot of a placeholder: which notation was asked for, and the
/// whitespace that separates it from whatever precedes it.
///
/// `None` on a [`PlaceholderSpec`] means "no numeral is rendered here" --
/// which covers both a placeholder without a `#var`/`$var` marker at all
/// *and* a hidden one (`` {?$n noun} ``), where the number is used for
/// number agreement but never rendered. Keeping the leading space inside
/// this struct rather than in a field of its own makes a space without a
/// numeral unrepresentable, and (unlike leaving it baked into the rendered
/// string, as it was before ROADMAP.md Phase 6 item 8) keeps it out of the
/// text a numeral hook is handed and returns a replacement for.
#[derive(Debug, Clone, Copy)]
pub struct NumeralSpec {
    pub kind: NumeralKind,
    pub leading_space: &'static str,
}

/// Which article keyword (if any) a word in the `pre` slot represents --
/// `ranting::get_article_or_so`'s runtime `match article_form { "the" =>
/// .., "a" | "an" | "some" => .., "these" | "those" => .., _ => None }`,
/// moved to a typed compile-time classification.
///
/// This does *not* replace `get_article_or_so`'s runtime behavior: the
/// `Ranting::skip_article()` gate, the `inflect_article_custom_with_context`
/// hook, and `a`-vs-`an`/singular-vs-plural rendering are all still
/// per-noun-instance runtime decisions. Only the "which keyword is this"
/// dispatch -- previously a runtime string match -- is decided here.
///
/// ROADMAP.md's `get_article_or_so` fixability note has the full proof, but
/// in short: `pre`'s first word can be classified from the placeholder's
/// literal template text alone in every case, including the one case where
/// it looks like it can't --  a `` ` `` possessive-substitution sentinel
/// embedded in that first word means the actual runtime text is a
/// possessive determiner or `Name's` form, but that vocabulary can never
/// coincide with a real article keyword, so `Other` is always correct for
/// it. The second, "chained" classification (`pre`'s second word, used only
/// when the first word turned out not to be an article) is reached *only*
/// when `pre` has no `` ` `` sentinel anywhere -- see
/// [`PlaceholderSpec::pre_chained_kind`] -- at which point it's a plain
/// compile-time-literal substring too.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArticleKind {
    /// `the`.
    The,
    /// `a`, `an`, or `some`.
    AAnSome,
    /// `these` or `those`.
    TheseThose,
    /// Not a recognized article keyword -- render as a verb, or (for a
    /// possessive-substituted word) as the runtime-substituted text as-is.
    Other,
}

impl ArticleKind {
    /// Classifies `word` (with or without a leading `!` emphasis marker,
    /// exactly as `ranting::get_article_or_so` receives it) the same way
    /// its runtime `match` used to. `ranting_derive` doesn't call this
    /// directly (see `CaseKind`/`TenseMarker`'s own `from_marker` for why:
    /// it does the equivalent match itself, at proc-macro build time, to
    /// emit `quote!` tokens naming the variant) -- kept as the canonical,
    /// tested reference implementation the macro's copy is checked against.
    pub const fn classify(word: &str) -> Self {
        let bytes = word.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == b'`' {
                // A possessive-substitution sentinel: the real runtime text
                // is a possessive determiner/`Name's` form, never a real
                // article keyword -- see this type's doc comment.
                return ArticleKind::Other;
            }
            i += 1;
        }
        // trim_start_matches(['!', '?']), byte-wise, to stay a const fn.
        //
        // `?` matters as much as `!` here: it is the "display depends on the entity's
        // `no_article`" marker README documents (`{?the 0}`), and leaving it attached
        // classified `?the` as `Other`, which is the *pre-noun verb* path — so the article was
        // conjugated as a verb and `{?the dog}` rendered "?thes dog". See
        // docs/architecture-review-2026-08-14.md §1.5.
        let mut start = 0;
        while start < bytes.len() && (bytes[start] == b'!' || bytes[start] == b'?') {
            start += 1;
        }
        let article_form = bytes.split_at(start).1;
        match article_form {
            b"the" => ArticleKind::The,
            b"a" | b"an" | b"some" => ArticleKind::AAnSome,
            b"these" | b"those" => ArticleKind::TheseThose,
            _ => ArticleKind::Other,
        }
    }
}

#[cfg(test)]
mod article_kind_tests {
    use super::ArticleKind;

    #[test]
    fn classifies_the() {
        assert_eq!(ArticleKind::classify("the"), ArticleKind::The);
    }

    #[test]
    fn classifies_a_an_some() {
        assert_eq!(ArticleKind::classify("a"), ArticleKind::AAnSome);
        assert_eq!(ArticleKind::classify("an"), ArticleKind::AAnSome);
        assert_eq!(ArticleKind::classify("some"), ArticleKind::AAnSome);
    }

    #[test]
    fn classifies_these_those() {
        assert_eq!(ArticleKind::classify("these"), ArticleKind::TheseThose);
        assert_eq!(ArticleKind::classify("those"), ArticleKind::TheseThose);
    }

    #[test]
    fn classifies_non_keyword_as_other() {
        assert_eq!(ArticleKind::classify("can"), ArticleKind::Other);
        assert_eq!(ArticleKind::classify(""), ArticleKind::Other);
    }

    #[test]
    fn strips_leading_bang_before_classifying() {
        assert_eq!(ArticleKind::classify("!the"), ArticleKind::The);
        assert_eq!(ArticleKind::classify("!these"), ArticleKind::TheseThose);
    }

    /// The `?` marker (README's `{?the 0}`, "display depends on the entity's `no_article`")
    /// has to come off too. It didn't, so `?the` classified as `Other` -- the pre-noun *verb*
    /// path -- and `{?the dog}` rendered "?thes dog". See
    /// docs/architecture-review-2026-08-14.md §1.5 and
    /// `tests/ranting/article_classification.rs`.
    #[test]
    fn strips_leading_question_mark_before_classifying() {
        assert_eq!(ArticleKind::classify("?the"), ArticleKind::The);
        assert_eq!(ArticleKind::classify("?a"), ArticleKind::AAnSome);
        assert_eq!(ArticleKind::classify("?an"), ArticleKind::AAnSome);
        assert_eq!(ArticleKind::classify("?some"), ArticleKind::AAnSome);
        // A word that is not an article keyword stays `Other` with the marker attached or not.
        assert_eq!(ArticleKind::classify("?can"), ArticleKind::Other);
    }

    #[test]
    fn is_case_sensitive() {
        // Matches get_article_or_so's literal `match` -- callers that need
        // case-insensitivity (call site 1 in handle_placeholder_impl)
        // lowercase before calling, exactly as they did before this type
        // existed.
        assert_eq!(ArticleKind::classify("The"), ArticleKind::Other);
        assert_eq!(ArticleKind::classify("THE"), ArticleKind::Other);
    }

    #[test]
    fn possessive_sentinel_always_classifies_other() {
        // A `` ` `` anywhere in the word means its real runtime content is
        // a possessive-substituted string, which can never coincide with a
        // real article keyword -- see this module's fixability note.
        assert_eq!(ArticleKind::classify("`"), ArticleKind::Other);
        assert_eq!(ArticleKind::classify("a`"), ArticleKind::Other);
    }
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
    /// verbs occurring here are open-ended natural-language words, and the
    /// runtime's article-vs-verb-vs-possessive dispatch for this slot
    /// depends on state only known at runtime (`Ranting::skip_article()`,
    /// custom inflection hooks) -- see `ranting::get_article_or_so`. Which
    /// article *keyword* a word represents, though, is now precomputed --
    /// see `pre_kind`/`pre_chained_kind`.
    pub pre: &'static str,
    /// Compile-time classification of `pre`'s first word (lowercased, as
    /// `ranting::get_article_or_so`'s first call site does before
    /// classifying).
    pub pre_kind: ArticleKind,
    /// Compile-time classification of `pre`'s second word -- used only when
    /// `pre_kind` is `Other` and `pre` has no `` ` `` possessive sentinel
    /// (that combination is the only one where `ranting::get_article_or_so`
    /// is called a second time). *Not* lowercased first, matching the
    /// case-sensitive second call site; when the second call is never
    /// reached this is unobserved and its value doesn't matter.
    pub pre_chained_kind: ArticleKind,
    pub plurality: &'static str,
    /// The numeral slot, or `None` when nothing numeric renders here (no
    /// `#var`/`$var` marker, or a hidden one). See [`NumeralSpec`].
    pub numeral: Option<NumeralSpec>,
    pub noun_space: &'static str,
    pub case: CaseKind,
    /// Whether `case` was written as the fused `*`-prefixed form (`` {the *=noun} ``, ROADMAP.md
    /// Phase 6 item 19) rather than the bare marker (`` {the =noun} ``). `case` itself still
    /// reports the real grammatical role either way -- `inflect_article_custom`/
    /// `elide_article_custom` see the same `GrammaticalCase` regardless -- but this bit tells the
    /// noun-slot renderer to render the noun's own name (`Ranting::inflect`, the same path
    /// `CaseKind::Name` takes) instead of switching to a pronoun (`inflect_pronoun_custom`). It
    /// exists because one marker char can't carry both "which case" and "name or pronoun" --
    /// `CaseKind`'s seven variants already map one-to-one onto the seven case-marker characters,
    /// so there was no spare bit on `case` alone to add. Always `false` when `case` is
    /// `CaseKind::Name` or `CaseKind::Hidden` (`*` with no following case marker, or no marker at
    /// all, already renders the name).
    pub display_as_name: bool,
    pub post: PostSpec,
    /// Whether `ranting_core::grammar::PH_START`'s `pre` capture put this placeholder at the
    /// start of a sentence (ROADMAP.md Phase 6 item 17) — computed once, at compile time, from
    /// the same `at_sentence_start` check that already feeds the `uc` argument
    /// `handle_placeholder`/`_with_context` are called with. `uc` conflates "sentence-initial"
    /// with "forced uppercase by a `^`/`,` marker or an uppercase pre-text word"; this field
    /// is the sentence-position signal alone, so a fork's `Ranting::capitalize`/
    /// `capitalize_with_context` can tell the two apart (docs/superpowers/specs/
    /// 2026-08-13-word-order-feasibility.md, open question 2).
    pub sentence_start: bool,
    /// The literal word (with its trailing whitespace) immediately preceding this placeholder in
    /// the template, when `PH_START`'s `pre` capture matched its word-run branch rather than a
    /// sentence-start marker, `{{`, or nothing (ROADMAP.md Phase 6 item 26,
    /// docs/superpowers/specs/2026-08-13-preposition-fusion.md option (b)). `None` covers every
    /// other case, including "no preceding text at all" -- there is no separate variant for that,
    /// since `handle_placeholder_impl` only needs to know whether there is a word to try fusing.
    /// When `Some`, `handle_placeholder_impl` renders this text itself (instead of the macro
    /// baking it as inert literal format-string text) so that `Ranting::inflect_preposition_custom`
    /// can replace it together with the article that follows -- `"de"` + `"el"` -> `"del"`.
    pub preposition: Option<&'static str>,
}
