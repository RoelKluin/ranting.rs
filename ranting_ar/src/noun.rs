//! `ArabicNoun` — the `Ranting` implementation, and the only file in the crate that touches the
//! public trait seam.

use crate::lexicon::{self, FEMININE, NounEntry, Person};
use ranting::*;
use std::fmt;

/// One Arabic noun from the closed vocabulary.
///
/// `plural` is the same per-occurrence entity state `SpanishNoun` carries. There is deliberately
/// **no** `dual` field: the dual is asked for by the placeholder's numeral and by nothing else,
/// which is exactly the boundary README hole 1 records. Adding a field here would work around the
/// hole rather than pin it.
#[derive(Copy, Clone)]
pub struct ArabicNoun {
    entry: &'static NounEntry,
    plural: bool,
}

impl ArabicNoun {
    const fn new(entry: &'static NounEntry) -> Self {
        ArabicNoun {
            entry,
            plural: false,
        }
    }

    /// `كتاب` "book" — broken plural, moon letter.
    pub const fn kitab() -> Self {
        Self::new(&lexicon::KITAB)
    }

    /// `شمس` "sun" — feminine, **sun letter**, so the article assimilates.
    pub const fn shams() -> Self {
        Self::new(&lexicon::SHAMS)
    }

    /// `قمر` "moon" — masculine, moon letter.
    pub const fn qamar() -> Self {
        Self::new(&lexicon::QAMAR)
    }

    /// `معلم` "teacher" (m.) — sound masculine plural.
    pub const fn muallim() -> Self {
        Self::new(&lexicon::MUALLIM)
    }

    /// `طالبة` "student" (f.) — sound feminine plural, sun letter.
    pub const fn taliba() -> Self {
        Self::new(&lexicon::TALIBA)
    }

    /// Make this reference plural. A placeholder's `+`/`-`/`#n`/`$n` markers override it per
    /// occurrence, exactly as for an English `Noun`.
    pub const fn plural(mut self) -> Self {
        self.plural = true;
        self
    }

    /// The form for a rendered number. `Some(2)` is the dual; everything else falls back to the
    /// two-way singular/plural split `to_plural` expresses.
    fn form(&self, to_plural: bool, count: Option<i64>) -> &'static str {
        match count {
            Some(2) => self.entry.dual,
            _ => {
                if to_plural {
                    self.entry.plural
                } else {
                    self.entry.singular
                }
            }
        }
    }

    fn is_feminine(&self) -> bool {
        self.entry.class == FEMININE
    }
}

impl fmt::Display for ArabicNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.form(self.plural, None))
    }
}

impl Ranting for ArabicNoun {
    fn name(&self, uc: bool) -> String {
        capitalize_if(self.form(self.plural, None), uc)
    }

    fn subjective(&self) -> &str {
        // An uninterpreted channel, as in both existing forks: the pronoun spelling is decided in
        // `inflect_pronoun_custom` from `self.entry.class`, not by parsing this string.
        match (self.plural, self.is_feminine()) {
            (true, true) => "هن",
            (true, false) => "هم",
            (false, true) => "هي",
            (false, false) => "هو",
        }
    }

    fn is_plural(&self) -> bool {
        self.plural
    }

    fn inflect(
        &self,
        to_plural: bool,
        uc: bool,
        _case: GrammaticalCase,
        count: Option<PlaceholderCount>,
    ) -> String {
        // ROADMAP.md Phase 7 item 11. Before it, this parameter did not exist and the line below
        // could only choose between two forms, so `{$n kitab}` with `n = 2` rendered the plural
        // `كتب` while every hook that *agrees* with this noun could already see the count and
        // render the dual. `_case` is ignored: Arabic's case endings are a separate hole (3).
        capitalize_if(self.form(to_plural, count.map(|c| c.value)), uc)
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn noun_class(&self) -> NounClass {
        NounClass::new(self.entry.class)
    }

    fn inflect_article_custom(
        &self,
        article: &str,
        _noun_singular: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        _uc: bool,
    ) -> Option<String> {
        match article {
            // `ال` is invariant for gender, number and (in unvocalized script) case — every
            // interesting thing about it happens in `elide_article_custom` below, which is what
            // makes this crate the first real user of that hook.
            "the" | "ال" => Some("ال".to_string()),
            // Arabic has **no indefinite article**: indefiniteness is the bare noun (plus
            // nunation, which unvocalized script does not write). Declining here would let
            // `ranting` render English "a"/"an"; returning an empty string is the honest answer
            // and is what makes `{a kitab}` render `كتاب`.
            "a" | "an" => Some(String::new()),
            _ => None,
        }
    }

    fn elide_article_custom(
        &self,
        article: &str,
        _separator: &str,
        following: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
    ) -> Option<String> {
        // Two separate things, both of which need this hook rather than `inflect_article_custom`:
        //
        // 1. `ال` is written **bound** to its noun, with no space. Only a post-assembly hook can
        //    drop the separator — that is the design note in `docs/EXTENSIBILITY.md` §2.7, and
        //    this is its first real consumer.
        // 2. Sun-letter assimilation: before the fourteen sun letters the `ل` assimilates and the
        //    following consonant doubles (`ال` + `شمس` → `الشّمس`), while moon letters are left
        //    alone (`ال` + `قمر` → `القمر`).
        //
        // The article arrives **already capitalized** where a script has capitals; Arabic has
        // none, so `article` is compared as-is here. See §2.7's trap note — a Latin-script fork
        // must not write this match the way this line does.
        if article != "ال" {
            return None;
        }
        if lexicon::is_sun_initial(following) {
            let mut chars = following.chars();
            let first = chars.next()?;
            return Some(format!("ال{first}{}{}", lexicon::SHADDA, chars.as_str()));
        }
        Some(format!("ال{following}"))
    }

    fn inflect_pronoun_custom(
        &self,
        _subject: &str,
        case: PronounCase,
        class: NounClass,
        as_plural: bool,
        count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        let feminine = class.as_str() == FEMININE;
        let dual = count.map(|c| c.value) == Some(2);
        let word = match case {
            // Arabic's independent pronouns have a dual (`هما`), and the count is what reaches it.
            PronounCase::Subjective if dual => "هما",
            PronounCase::Subjective => match (as_plural, feminine) {
                (true, true) => "هن",
                (true, false) => "هم",
                (false, true) => "هي",
                (false, false) => "هو",
            },
            // Object, possessive and reflexive are all **suffixes** in Arabic (`كتابه` "his
            // book", `رأيته` "I saw him"), not free words. What this hook can return is a free
            // word, so the forms below are the closest honest standalone answers; see README
            // hole 4.
            PronounCase::Objective if dual => "هما",
            PronounCase::Objective => match (as_plural, feminine) {
                (true, true) => "هن",
                (true, false) => "هم",
                (false, true) => "ها",
                (false, false) => "ه",
            },
            PronounCase::PossessiveDeterminer | PronounCase::PossessivePronoun => {
                if feminine {
                    "ها"
                } else {
                    "ه"
                }
            }
            PronounCase::Reflexive => "نفسه",
        };
        Some(capitalize_if(word, uc))
    }

    fn inflect_verb_custom(
        &self,
        subject: &str,
        verb: &str,
        as_plural: bool,
        count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        // The dual reaches the verb through `count`, which Phase 6 item 14 added — and which was
        // sufficient here before item 11, while the noun beside it was not. That asymmetry is
        // what the Arabic spike found.
        let person = Person::from_subject_and_count(subject, as_plural, count.map(|c| c.value));
        lexicon::conjugate(verb, person).map(|form| capitalize_if(form, uc))
    }

    fn inflect_numeral_custom(
        &self,
        numeral: &str,
        count: Option<i64>,
        style: NumeralStyle,
        _case: GrammaticalCase,
        class: NounClass,
        _as_plural: bool,
    ) -> Option<String> {
        match style {
            // `#n`: spell it, with Arabic's gender polarity — see `lexicon::spell`.
            NumeralStyle::Words => lexicon::spell(count?, class.as_str()),
            // `$n`: Arabic-Indic digits, transcribed from whatever English rendered so a `:fmt`
            // width spec survives.
            NumeralStyle::Digits => Some(lexicon::to_arabic_digits(numeral)),
            // `##n`: ROADMAP.md Phase 8 item 4's "second constituency" — ordinals agree normally
            // (not polarity-reversed) with the counted noun's gender, unlike the cardinals above.
            NumeralStyle::Ordinal => lexicon::ordinal(count?, class.as_str()),
            // `$$n`: no Arabic digit-ordinal notation is modeled by this closed lexicon.
            NumeralStyle::OrdinalDigits => None,
        }
    }

    // `capitalize` is deliberately *not* overridden: the Arabic script is caseless, so the
    // English default (uppercase the first character exactly when `uc` says so) is a no-op on
    // every string this crate produces. Item 6's own customer list — Turkish `i`/`İ`, caseless
    // scripts — names this as the *expected* outcome for a caseless script rather than a gap.
    //
    // `inflect_adjective_custom` is deliberately *not* overridden either, and that is a scoping
    // choice rather than a gap: Arabic attributive adjectives are post-nominal like Spanish's, so
    // the `!` slot is in the right position and `ranting_es` already proves the hook works there.
    // Repeating it would add a second working example, not a falsification. See the README.
    //
    // `inflect_preposition_custom` is not overridden: Arabic prepositions do not fuse with `ال`
    // orthographically (`في الكتاب`, two words). `ranting_i18n` and `ranting_es` both exercise
    // that hook already.
}
