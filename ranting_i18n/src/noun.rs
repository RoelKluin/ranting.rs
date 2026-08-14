//! `GermanNoun` — the `Ranting` implementation. Everything grammatical lives in
//! [`crate::lexicon`]; what is here is only the trait seam, so that each hook body reads as an
//! answer to "can `ranting` carry this signal?".

use crate::lexicon::{
    self, Case, Definiteness, HAUS, HUND, KATZE, NounEntry, Person, adjective_ending,
    definite_article, indefinite_article,
};
use ranting::*;
use std::fmt;

/// One German noun from the closed vocabulary, with the grammatical state `ranting`'s hooks
/// cannot carry attached to the entity instead.
///
/// `case` and `definiteness` are the recorded holes, not design preferences: `GrammaticalCase`
/// has no dative and no hook reports which article was written. `plural` is ordinary entity
/// state.
///
/// Case-marking and name-vs-pronoun display used to be inseparable (README hole 5, closed by
/// ROADMAP.md Phase 6 item 19): `inflect_pronoun_custom` below always returns a real pronoun now,
/// and a template that wants a case-correct article with the *name* shown writes the fused
/// `*=`/`*@`/`` *` ``/`*~` marker (`` {the *=hund} `` → "Der Hund") instead of the bare one
/// (`` {the =hund} `` → "Er"/"Ihn"/etc.) — no entity-carried flag needed any more.
#[derive(Copy, Clone)]
pub struct GermanNoun {
    entry: &'static NounEntry,
    /// `None` means "take the case from the placeholder's own marker" — which can only ever
    /// yield nominative or accusative. `Some` overrides it, and is the only way to reach dative
    /// or genitive.
    case: Option<Case>,
    definiteness: Definiteness,
    plural: bool,
}

impl GermanNoun {
    const fn new(entry: &'static NounEntry) -> Self {
        GermanNoun {
            entry,
            case: None,
            definiteness: Definiteness::Definite,
            plural: false,
        }
    }

    /// `der Hund` (masculine).
    pub const fn hund() -> Self {
        Self::new(&HUND)
    }

    /// `die Katze` (feminine).
    pub const fn katze() -> Self {
        Self::new(&KATZE)
    }

    /// `das Haus` (neuter).
    pub const fn haus() -> Self {
        Self::new(&HAUS)
    }

    /// Fix the grammatical case on the entity, overriding whatever the placeholder's marker
    /// says. Required for dative and genitive; see README hole 3.
    pub const fn in_case(mut self, case: Case) -> Self {
        self.case = Some(case);
        self
    }

    /// Declare which kind of article will precede the noun, so attributive adjectives can pick
    /// the weak/mixed/strong ending. See README hole 4 — no hook reports this.
    pub const fn with_article(mut self, definiteness: Definiteness) -> Self {
        self.definiteness = definiteness;
        self
    }

    /// Make this reference plural (`die Hunde`). A placeholder's `+`/`-`/`#n` markers override
    /// it per occurrence, exactly as for an English `Noun`.
    pub const fn plural(mut self) -> Self {
        self.plural = true;
        self
    }

    /// The case actually in force: the entity's own, or — when it declares none — whatever the
    /// placeholder's marker could express.
    fn case_for(&self, marker: GrammaticalCase) -> Case {
        self.case.unwrap_or(match marker {
            GrammaticalCase::Objective => Case::Accusative,
            GrammaticalCase::PossessiveDeterminer | GrammaticalCase::PossessivePronoun => {
                Case::Genitive
            }
            _ => Case::Nominative,
        })
    }

    /// The noun's own declined form.
    pub fn form(&self, plural: bool) -> &'static str {
        self.entry
            .form(self.case.unwrap_or(Case::Nominative), plural)
    }

    /// The German personal pronoun for a case and number.
    fn pronoun(&self, case: Case, plural: bool) -> &'static str {
        if plural {
            return match case {
                Case::Nominative | Case::Accusative => "sie",
                Case::Dative => "ihnen",
                Case::Genitive => "ihrer",
            };
        }
        match (case, self.entry.class) {
            (Case::Nominative, lexicon::MASCULINE) => "er",
            (Case::Nominative, lexicon::FEMININE) => "sie",
            (Case::Nominative, _) => "es",
            (Case::Accusative, lexicon::MASCULINE) => "ihn",
            (Case::Accusative, lexicon::FEMININE) => "sie",
            (Case::Accusative, _) => "es",
            (Case::Dative, lexicon::FEMININE) => "ihr",
            (Case::Dative, _) => "ihm",
            (Case::Genitive, lexicon::FEMININE) => "ihrer",
            (Case::Genitive, _) => "seiner",
        }
    }

    /// The possessive determiner (`sein Haus` / `ihr Haus`), undeclined — the noun it modifies is
    /// not this entity, so its own case/gender is unreachable from here anyway.
    fn possessive(&self, plural: bool) -> &'static str {
        if plural || self.entry.class == lexicon::FEMININE {
            "ihr"
        } else {
            "sein"
        }
    }
}

impl fmt::Display for GermanNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.form(self.plural))
    }
}

impl Ranting for GermanNoun {
    fn name(&self, _uc: bool) -> String {
        // German nouns are capitalized wherever they stand, so `uc` has nothing to add.
        self.form(self.plural).to_string()
    }

    fn subjective(&self) -> &str {
        // An uninterpreted channel (ROADMAP "SubjectPronoun is a closed English enum" decision):
        // the value never has to be an English pronoun as long as every hook that consumes it is
        // overridden. `crate::lexicon::Person::from_subject` is the consumer.
        if self.plural {
            "sie"
        } else {
            match self.entry.class {
                lexicon::MASCULINE => "er",
                lexicon::FEMININE => "sie",
                _ => "es",
            }
        }
    }

    fn is_plural(&self) -> bool {
        self.plural
    }

    fn inflect(&self, to_plural: bool, _uc: bool, case: GrammaticalCase) -> String {
        // Hole 2 is still open, just narrower now (item 14). `inflect` gained a `case`
        // parameter, but the only call site that reaches it is bare-placeholder rendering
        // (`{the 0}`/`{?the 0}`), which is always `GrammaticalCase::Name`/`Hidden` — any
        // marker that would carry a real case (`=`/`@`/`` ` ``/`~`) routes to
        // `inflect_pronoun_custom` instead, never here. `case_for` treats `Name`/`Hidden`
        // as nominative, so this is identical to the pre-item-14 behavior in practice; only
        // the entity's own `case` override (`GermanNoun::in_case`) can still reach dative or
        // genitive on the noun form itself.
        self.entry.form(self.case_for(case), to_plural).to_string()
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn noun_class(&self) -> NounClass {
        NounClass::new(match self.entry.class {
            lexicon::MASCULINE => lexicon::MASCULINE,
            lexicon::FEMININE => lexicon::FEMININE,
            _ => lexicon::NEUTER,
        })
    }

    fn inflect_article_custom(
        &self,
        article: &str,
        _noun_singular: &str,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        let case = self.case_for(case);
        let form = match article {
            // German's own definite article, in every form this lexicon declines it into.
            // Reachable because `ranting` now hands an unrecognized pre-noun word to this
            // hook instead of rendering it as literal text, which is what lets a German
            // template be written in German (`{der *=hund}`) rather than with the English
            // keyword (`{the *=hund}`). Which form is *written* selects only the paradigm;
            // `case`/`class`/`as_plural` still pick the form, so `{den *=hund}` on a
            // nominative placeholder correctly renders "der".
            "the" | "der" | "die" | "das" | "den" | "dem" | "des" => {
                definite_article(case, class.as_str(), as_plural)
            }
            "a" | "an" | "ein" | "eine" | "einen" | "einem" | "eines" | "einer" => {
                indefinite_article(case, class.as_str(), as_plural)
            }
            // `some`/`these`/`those` are English quantifier/demonstrative slots this lexicon
            // does not model; let them fall through rather than mistranslate.
            _ => return None,
        };
        Some(uc_1st_if(form, uc))
    }

    fn inflect_pronoun_custom(
        &self,
        _subject: &str,
        case: PronounCase,
        _class: NounClass,
        as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        // German has no dual/paucal pronoun forms, so `count` adds nothing beyond
        // `as_plural` here. Always a real pronoun now (ROADMAP.md Phase 6 item 19) — a template
        // that wants the name instead writes the fused `*=`/`*@`/etc. marker, which never
        // reaches this hook at all (`handle_placeholder_impl` routes it to `inflect()` instead).
        let word = match case {
            PronounCase::Subjective => {
                self.pronoun(self.case_for(GrammaticalCase::Name), as_plural)
            }
            PronounCase::Objective => {
                self.pronoun(self.case_for(GrammaticalCase::Objective), as_plural)
            }
            PronounCase::PossessiveDeterminer | PronounCase::PossessivePronoun => {
                self.possessive(as_plural)
            }
            // German's third-person reflexive is `sich` for every gender, number and case that
            // occurs here.
            PronounCase::Reflexive => "sich",
        };
        Some(uc_1st_if(word, uc))
    }

    fn inflect_adjective_custom(
        &self,
        adjective: &str,
        _degree: AdjectiveDegree,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        // Outside the closed vocabulary, decline rather than guess — the same contract as the
        // verb hook, and the reason `say!()`'s English output survives a partial fork.
        if !lexicon::ADJECTIVES.contains(&adjective) {
            return None;
        }
        // The ending is fully determined; the *position* is not reachable (README hole 4).
        let case = self.case_for(case);
        let ending = adjective_ending(self.definiteness, case, class.as_str(), as_plural);
        Some(uc_1st_if(&format!("{adjective}{ending}"), uc))
    }

    fn inflect_numeral_custom(
        &self,
        _numeral: &str,
        count: Option<i64>,
        style: NumeralStyle,
        case: GrammaticalCase,
        class: NounClass,
        _as_plural: bool,
    ) -> Option<String> {
        match style {
            // `#n`: spell it in German, with `1` agreeing like an article.
            NumeralStyle::Words => lexicon::spell(count?, self.case_for(case), class.as_str()),
            // `$n`: German writes the same digits as English, so nothing to do.
            NumeralStyle::Digits => None,
        }
    }

    // `elide_article_custom` is deliberately *not* overridden. German's real fusions —
    // `in dem` → `im`, `zu dem` → `zum`, `an das` → `ans` — are all preposition+article, and
    // that gap is `inflect_preposition_custom` below, not this hook (see ROADMAP.md Phase 6 item
    // 26 — previously recorded here as README hole 7). The other thing a fork might want
    // `elide_article_custom` for, splicing away the separator left behind by the empty
    // indefinite plural article, is out of reach too: the hook is skipped when the article
    // renders empty (README hole 6). Overriding it would be dead code.

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
        // Formerly README hole 7: the pre-noun placeholder slot is a closed English word list, so
        // a preposition can only ever be template literal text, outside `elide_article_custom`'s
        // own reach (its span starts at the article). `inflect_preposition_custom` is fed that
        // literal word directly instead, closing the gap. Only the well-known, universally used
        // contractions are listed — German has others that are colloquial/optional (`aufs`,
        // `übers`) rather than obligatory, and this lexicon only speaks for the obligatory ones.
        let fused = match (preposition, article) {
            ("zu", "dem") => "zum",
            ("bei", "dem") => "beim",
            ("von", "dem") => "vom",
            ("in", "dem") => "im",
            ("in", "das") => "ins",
            ("an", "dem") => "am",
            ("an", "das") => "ans",
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

    fn capitalize(
        &self,
        word: &str,
        role: OrthographyRole,
        uc: bool,
        _sentence_start: bool,
    ) -> String {
        match role {
            // German capitalizes nouns wherever they stand. `inflect`/`name` already return them
            // capitalized, so this only has to promise not to undo it.
            OrthographyRole::Noun => word.to_string(),
            _ => uc_1st_if(word, uc),
        }
    }
}
