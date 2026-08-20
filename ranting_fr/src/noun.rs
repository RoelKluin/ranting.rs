//! `FrenchNoun` — the `Ranting` implementation. Everything grammatical lives in
//! [`crate::lexicon`]; what is here is only the trait seam, mirroring `ranting_es::noun`.

use crate::lexicon::{self, ARBRE, FEMININE, NounEntry, Person};
use ranting::*;
use std::fmt;

/// One French noun from the closed vocabulary.
///
/// Like `ranting_es::SpanishNoun`, this carries no case override: French nouns don't decline by
/// case either. `mass`/`h_aspire` are entity-carried (`self.entry`), the same pattern
/// `SpanishNoun::euphonic_el` uses — they are properties of *which noun* this is, not of the
/// placeholder's marker.
#[derive(Copy, Clone)]
pub struct FrenchNoun {
    entry: &'static NounEntry,
    plural: bool,
}

impl FrenchNoun {
    const fn new(entry: &'static NounEntry) -> Self {
        FrenchNoun {
            entry,
            plural: false,
        }
    }

    /// `le chat` (masculine, regular, consonant-initial).
    pub const fn chat() -> Self {
        Self::new(&lexicon::CHAT)
    }

    /// `la maison` (feminine, regular, consonant-initial).
    pub const fn maison() -> Self {
        Self::new(&lexicon::MAISON)
    }

    /// `l'arbre` (masculine, vowel-initial: elides).
    pub const fn arbre() -> Self {
        Self::new(&ARBRE)
    }

    /// `l'école` (feminine, vowel-initial: elides).
    pub const fn ecole() -> Self {
        Self::new(&lexicon::ECOLE)
    }

    /// `l'homme` (masculine, `h muet`: elides).
    pub const fn homme() -> Self {
        Self::new(&lexicon::HOMME)
    }

    /// `le héros` (masculine, `h aspiré`: does not elide).
    pub const fn heros() -> Self {
        Self::new(&lexicon::HEROS)
    }

    /// `de l'eau` (feminine, mass, vowel-initial: partitive + elision).
    pub const fn eau() -> Self {
        Self::new(&lexicon::EAU)
    }

    /// Make this reference plural (`les chats`). A placeholder's `+`/`-`/`#n` markers override
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

impl fmt::Display for FrenchNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.form(self.plural))
    }
}

impl Ranting for FrenchNoun {
    fn name(&self, uc: bool) -> String {
        capitalize_if(self.form(self.plural), uc)
    }

    fn subjective(&self) -> &str {
        // An uninterpreted channel, same as `SpanishNoun::subjective` — the gendered pronoun
        // spelling lives in `inflect_pronoun_custom` below, which reads `self.entry.class`
        // directly rather than parsing this string.
        match (self.plural, self.is_feminine()) {
            (true, true) => "elles",
            (true, false) => "ils",
            (false, true) => "elle",
            (false, false) => "il",
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
        _count: Option<PlaceholderCount>,
    ) -> String {
        // No grammatical case to honor here, same as Spanish.
        capitalize_if(self.form(to_plural), uc)
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn is_mass(&self) -> bool {
        // The first fork to override this hook at all (see the crate README) — French's
        // partitive article (`du`/`de la`, see `inflect_article_custom` below) is what it's
        // for.
        self.entry.mass
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
        let form = match article {
            // French's own articles alongside English's, reachable the same way
            // `SpanishNoun::inflect_article_custom` reaches `el`/`la` — see
            // docs/superpowers/specs/2026-08-14-language-modularity.md.
            "the" | "le" | "la" | "les" => lexicon::definite_article(class.as_str(), as_plural),
            "a" | "an" | "some" | "un" | "une" | "des" => {
                // ROADMAP.md Phase 8 item 3's mass/count axis, resolved here rather than by the
                // main crate's a/an guess: a singular mass noun takes the partitive (`du`/`de
                // la`), never `un`/`une`. `get_article_or_so` already resolves a mass noun's
                // article slot to `some` before falling back to English, but this hook fires
                // first and answers directly for every written form of the slot (`a`/`an`/
                // written `some` all land here as `article`).
                if self.entry.mass && !as_plural {
                    lexicon::partitive_article(class.as_str())
                } else {
                    lexicon::indefinite_article(class.as_str(), as_plural)
                }
            }
            _ => return None,
        };
        Some(capitalize_if(form, uc))
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
        // This is `elide_article_custom`'s documented motivating example (see the `Ranting`
        // trait doc's own `le`/`la` → `l'` illustration) — but `ranting_ar` already exercises the
        // hook first, for sun/moon-letter assimilation. What's genuinely new here is the
        // *negative* case: `h_aspire` lets this hook correctly decline to elide (`le héros`, not
        // `l'héros`) even though the surface string looks identical in shape to `l'homme`'s. No
        // existing `elide_article_custom` implementation has had to represent that before.
        if !lexicon::starts_elidable(following, self.entry.h_aspire) {
            return None;
        }
        // Matches a trailing "le"/"la" as a whole word (case-insensitively, since `article`
        // arrives already capitalized at a sentence start) so the partitive "de la" elides to
        // "de l'" the same way the bare definite article does, without a separate code path.
        let lower = article.to_lowercase();
        if lower != "le" && lower != "la" && !lower.ends_with(" le") && !lower.ends_with(" la") {
            return None;
        }
        let prefix = &article[..article.len() - 2];
        // Only the very first letter of the whole placeholder output is ever capitalized, so
        // the apostrophe-`l` is uppercased only when it *is* that first letter — never when
        // it's the tail of "de la"/"à la".
        let apostrophe = if prefix.is_empty() && article.starts_with(|c: char| c.is_uppercase()) {
            "L'"
        } else {
            "l'"
        };
        Some(format!("{prefix}{apostrophe}{following}"))
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
                (true, true) => "elles",
                (true, false) => "ils",
                (false, true) => "elle",
                (false, false) => "il",
            },
            PronounCase::Objective => match (as_plural, feminine) {
                (true, _) => "les",
                (false, true) => "la",
                (false, false) => "le",
            },
            // French `son`/`sa`/`ses` agree with the *possessed* noun, not with this entity —
            // not reachable from here regardless, the same shape `SpanishNoun::su` documents.
            // `son` is the closest honest single answer (French even prefers `son` over `sa`
            // before a vowel-initial feminine noun for euphony, a second layer this closed
            // lexicon doesn't attempt either).
            PronounCase::PossessiveDeterminer => "son",
            PronounCase::PossessivePronoun => "le sien",
            PronounCase::Reflexive => "se",
        };
        Some(capitalize_if(word, uc))
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
        // The hole: agreement is correct for every word in the lexicon, prenominal or not — see
        // `lexicon::AdjectiveForms`'s doc comment. The `!`/`!!` degree slot this hook is called
        // from only ever renders post-noun, so a prenominal word's correctly agreed form (e.g.
        // `grande`) still comes out in the wrong position (`la maison grande`, not
        // `la grande maison`): the mechanism works, the position doesn't, exactly the shape
        // German's adjectives take for the whole language rather than a closed subset. See the
        // crate README and `tests/holes.rs::hole_1_*`.
        let feminine = class.as_str() == FEMININE;
        lexicon::adjective_form(adjective, feminine, as_plural).map(|form| capitalize_if(&form, uc))
    }

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
        // Not a new finding: structurally identical to `SpanishNoun`'s already-closed hole 1
        // (`de`+`el`→`del`, `a`+`el`→`al`) — see the crate README's "holes that do not reproduce
        // here". Included for a correct, complete showcase rather than for falsification.
        // French also fuses the *plural* article (`de`+`les`→`des`, `à`+`les`→`aux`), which this
        // closed lexicon doesn't model, the same scope choice Spanish's two-pair table makes.
        let fused = match (preposition, article) {
            ("de", "le") => "du",
            ("à", "le") => "au",
            _ => return None,
        };
        Some(capitalize_if(fused, uc))
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
            // `#n`: spell it in French, with `1` agreeing like the indefinite article.
            NumeralStyle::Words => lexicon::spell(count?, class.as_str()),
            // `$n`: French writes the same digits as English, so nothing to do.
            NumeralStyle::Digits => None,
            // `##n`: agrees in gender only at `1` (`premier`/`première`) — see
            // `lexicon::ordinal`'s doc comment for how that differs from Spanish's ordinals.
            NumeralStyle::Ordinal => lexicon::ordinal(count?, class.as_str()),
            // `$$n`: no French digit-ordinal notation is modeled by this closed lexicon.
            NumeralStyle::OrdinalDigits => None,
        }
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
        lexicon::conjugate(verb, person).map(|form| capitalize_if(form, uc))
    }

    // `capitalize` is deliberately *not* overridden — French orthography is capitalize-at-
    // sentence-start, exactly like Spanish and exactly what the English default already does.
}
