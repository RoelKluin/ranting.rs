//! `SpanishNoun` — the `Ranting` implementation. Everything grammatical lives in
//! [`crate::lexicon`]; what is here is only the trait seam, mirroring `ranting_i18n::noun`.

use crate::lexicon::{self, FEMININE, GATO, NounEntry, Person};
use ranting::*;
use std::fmt;

/// One Spanish noun from the closed vocabulary.
///
/// Unlike `ranting_i18n::GermanNoun`, this carries **no** case override: Spanish nouns don't
/// decline by case at all, so there is nothing here for `GrammaticalCase`'s missing dative
/// variant to block — see the crate README's "no case hole" section. `plural` is the only
/// per-occurrence entity state, exactly like an English `Noun`.
#[derive(Copy, Clone)]
pub struct SpanishNoun {
    entry: &'static NounEntry,
    plural: bool,
}

impl SpanishNoun {
    const fn new(entry: &'static NounEntry) -> Self {
        SpanishNoun {
            entry,
            plural: false,
        }
    }

    /// `el gato` (masculine, regular).
    pub const fn gato() -> Self {
        Self::new(&GATO)
    }

    /// `la casa` (feminine, regular).
    pub const fn casa() -> Self {
        Self::new(&lexicon::CASA)
    }

    /// `el problema` (masculine despite the `-a` ending).
    pub const fn problema() -> Self {
        Self::new(&lexicon::PROBLEMA)
    }

    /// `el agua` / `las aguas` (feminine, euphonic singular article).
    pub const fn agua() -> Self {
        Self::new(&lexicon::AGUA)
    }

    /// Make this reference plural (`los gatos`). A placeholder's `+`/`-`/`#n` markers override
    /// it per occurrence, exactly as for an English `Noun`.
    pub const fn plural(mut self) -> Self {
        self.plural = true;
        self
    }

    fn form(&self, plural: bool) -> &'static str {
        if plural {
            self.entry.plural
        } else {
            self.entry.singular
        }
    }

    fn is_feminine(&self) -> bool {
        self.entry.class == FEMININE
    }
}

impl fmt::Display for SpanishNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.form(self.plural))
    }
}

impl Ranting for SpanishNoun {
    fn name(&self, uc: bool) -> String {
        uc_1st_if(self.form(self.plural), uc)
    }

    fn subjective(&self) -> &str {
        // An uninterpreted channel, same as `ranting_i18n::GermanNoun::subjective` — the actual
        // gendered pronoun spelling lives in `inflect_pronoun_custom` below, which reads
        // `self.entry.class` directly rather than parsing this string.
        match (self.plural, self.is_feminine()) {
            (true, true) => "ellas",
            (true, false) => "ellos",
            (false, true) => "ella",
            (false, false) => "él",
        }
    }

    fn is_plural(&self) -> bool {
        self.plural
    }

    fn inflect(&self, to_plural: bool, uc: bool, _case: GrammaticalCase) -> String {
        // No case to honor here — see the struct doc comment and the README's "no case hole".
        uc_1st_if(self.form(to_plural), uc)
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
        class: NounClass,
        as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        // `euphonic_el` is entity-carried (self.entry), not a hook parameter — it is a property
        // of *which noun* this is, not of the placeholder's marker, exactly the way
        // `GermanNoun::definiteness` is entity-carried for a different reason (README hole 4b).
        let form = match article {
            "the" => lexicon::definite_article(class.as_str(), as_plural, self.entry.euphonic_el),
            "a" | "an" => {
                lexicon::indefinite_article(class.as_str(), as_plural, self.entry.euphonic_el)
            }
            // `some`/`these`/`those` are English quantifier/demonstrative slots this lexicon
            // does not model — same stance `ranting_i18n::GermanNoun` takes.
            _ => return None,
        };
        Some(uc_1st_if(form, uc))
    }

    fn inflect_pronoun_custom(
        &self,
        _subject: &str,
        case: PronounCase,
        class: NounClass,
        as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        let feminine = class.as_str() == FEMININE;
        let word = match case {
            PronounCase::Subjective => match (as_plural, feminine) {
                (true, true) => "ellas",
                (true, false) => "ellos",
                (false, true) => "ella",
                (false, false) => "él",
            },
            PronounCase::Objective => match (as_plural, feminine) {
                (true, true) => "las",
                (true, false) => "los",
                (false, true) => "la",
                (false, false) => "lo",
            },
            // Spanish `su`/`suyo` agree with the *possessed* noun, not with this entity — not
            // reachable from here regardless (there is no possessed-noun signal in this hook),
            // exactly the shape `ranting_i18n::GermanNoun::possessive` documents for `sein`/
            // `ihr`. `su` (determiner) and invariant `suyo` (pronoun) are the closest honest
            // single answer.
            PronounCase::PossessiveDeterminer => "su",
            PronounCase::PossessivePronoun => "suyo",
            // Third-person reflexive is `se` for every gender and number here.
            PronounCase::Reflexive => "se",
        };
        Some(uc_1st_if(word, uc))
    }

    fn inflect_adjective_custom(
        &self,
        adjective: &str,
        _degree: AdjectiveDegree,
        _case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        // Same decline-rather-than-guess contract as the verb hook. `_degree` is ignored on
        // purpose: this lexicon's adjectives agree, they don't compare — see
        // `docs/EXTENSIBILITY.md` §2.5's note that a agreement-only fork writes `!` for the
        // plain/positive case too.
        let feminine = class.as_str() == FEMININE;
        lexicon::adjective_form(adjective, feminine, as_plural).map(|form| uc_1st_if(&form, uc))
    }

    fn inflect_numeral_custom(
        &self,
        _numeral: &str,
        count: Option<i64>,
        style: NumeralStyle,
        _case: GrammaticalCase,
        class: NounClass,
        _as_plural: bool,
    ) -> Option<String> {
        match style {
            // `#n`: spell it in Spanish, with `1` agreeing like the indefinite article.
            NumeralStyle::Words => lexicon::spell(count?, class.as_str(), self.entry.euphonic_el),
            // `$n`: Spanish writes the same digits as English, so nothing to do.
            NumeralStyle::Digits => None,
        }
    }

    // `elide_article_custom` is deliberately *not* overridden: Spanish `el`/`la`/`un`/`una`
    // never elide before a following vowel the way French `le`/`la` do (there is no `l'agua`),
    // and `el agua`'s euphonic article is a straight article-selection rule reachable from
    // `inflect_article_custom` alone — see the README. The one real Spanish fusion,
    // preposition + article (`de` + `el` → `del`, `a` + `el` → `al`), is `inflect_preposition_custom`
    // below, not this hook — same split `ranting_i18n::GermanNoun` makes for its own fusions.

    fn inflect_preposition_custom(
        &self,
        preposition: &str,
        article: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        // Formerly README hole 1: "de"/"a" are template literal text before the placeholder, out
        // of every hook's reach until `inflect_preposition_custom` (ROADMAP.md Phase 6 item 26)
        // started receiving that literal word directly. Spanish has exactly two obligatory
        // fusions, both with the masculine singular article "el" — "de la"/"a la" (feminine),
        // "de los"/"a los"/"de las"/"a las" (plural) are already correct unfused, so every other
        // combination declines.
        let fused = match (preposition, article) {
            ("de", "el") => "del",
            ("a", "el") => "al",
            _ => return None,
        };
        Some(uc_1st_if(fused, uc))
    }

    fn inflect_verb_custom(
        &self,
        subject: &str,
        verb: &str,
        as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        let person = Person::from_subject(subject, as_plural);
        lexicon::conjugate(verb, person).map(|form| uc_1st_if(form, uc))
    }

    // `capitalize` is deliberately *not* overridden. Spanish has nothing like German's
    // always-capitalize-the-noun rule (item 6's own customer list — Turkish `i`/`İ`, caseless
    // scripts — doesn't include Spanish either): the English default, uppercase the first
    // rendered character exactly when `uc` says to, is already correct Spanish orthography. See
    // the README's "capitalize has nothing to do" note, and `tests/spanish.rs`'s `¿`/`¡`
    // sentence-start tests, which exercise item 17's *already-closed* widening rather than
    // anything new this crate had to add.
}
