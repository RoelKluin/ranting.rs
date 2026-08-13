// (c) Roel Kluin 2022 MIT
//! Recursive [`Ranting`] impls for `Vec<T>`, `Option<T>`, and `Box<T>` — Phase 3 item 7.
//!
//! `Box<T>` gets a direct blanket impl: `Ranting` is local to this crate, so implementing it for
//! the foreign `Box<T>` is allowed by the orphan rules, and `std` already provides
//! `impl<T: Display> Display for Box<T>` so the `Ranting: Display` supertrait bound is satisfied
//! for free.
//!
//! `Vec<T>` and `Option<T>` cannot get the same treatment: `Ranting: Display` requires `Vec<T>`/
//! `Option<T>` to implement `Display`, but `Display` is a foreign trait and neither `Vec` nor
//! `Option` is `#[fundamental]` (unlike `Box`/`&`), so `impl Display for Vec<T>` (or `Option<T>`)
//! is rejected by the orphan rules (E0117) regardless of what `T` is. There is no way around this
//! short of relaxing the `Display` supertrait, which is a breaking public-API change out of scope
//! here (see ROADMAP.md Phase 4). Instead this module provides local newtype wrappers, [`Many`]
//! and [`Maybe`], which *are* local types and so can freely implement both `Display` and
//! `Ranting`.

use crate::{GrammaticalCase, NarrationContext, NounClass, PronounCase, Ranting, uc_1st_if};
use std::fmt;

/// Wraps a `Vec<T>` so it can be used as a `say!()` placeholder subject/argument.
///
/// Grammatically a `Many<T>` behaves as a single collective noun phrase: its rendered name is
/// the items' own names joined as `"a, b and c"` (Oxford-comma-free), pluralized to `they`/`them`
/// grammar whenever the collection doesn't hold exactly one item.
///
/// - **Empty** (`Many(vec![])`): renders as the empty string, is treated as plural ("there are no
///   items", not "there is no item"), and [`skip_article`](Ranting::skip_article) returns `true`.
/// - **One item**: subject pronoun, plurality, and custom-hook overrides all delegate straight
///   through to that item, so `Many(vec![noun])` behaves like `noun` alone.
/// - **Several items**: subject pronoun is `"they"`/plural agreement; custom hooks (which are
///   per-item, not per-collection) fall back to the built-in English rules, since there's no
///   single item to delegate to.
///
/// Placeholder verbs are always written in their base plural form (e.g. `are`, not `is`) — see
/// the crate-level docs on `say!()` — so a `Many` with exactly one item still reads `{=solo are}`
/// in the template even though it renders singular ("is").
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, Many, say};
/// let heroes = Many(vec![Noun::new("Alice", "she"), Noun::new("Bob", "he")]);
/// assert_eq!(say!("{=heroes are} ready."), "They are ready.".to_string());
/// assert_eq!(say!("{heroes}"), "Alice and Bob".to_string());
///
/// let solo = Many(vec![Noun::new("Alice", "she")]);
/// assert_eq!(say!("{=solo are} ready."), "She is ready.".to_string());
///
/// let none: Many<Noun> = Many(vec![]);
/// assert_eq!(say!("There {=none are} no items."), "There they are no items.".to_string());
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Many<T: Ranting>(pub Vec<T>);

// Hand-written rather than `#[derive(Default)]`: derive would add a `T: Default` bound, but an
// empty `Vec` (the sensible default for a collection) doesn't need `T` to have a default value
// of its own.
impl<T: Ranting> Default for Many<T> {
    fn default() -> Self {
        Many(Vec::new())
    }
}

impl<T: Ranting> Many<T> {
    fn joined_names(&self, uc: bool) -> String {
        let mut names: Vec<String> = self.0.iter().map(|item| item.name(false)).collect();
        match names.len() {
            0 => String::new(),
            1 => uc_1st_if(&names.remove(0), uc),
            _ => {
                let last = names.pop().expect("len > 1 checked above");
                let joined = format!("{} and {}", names.join(", "), last);
                uc_1st_if(&joined, uc)
            }
        }
    }
}

impl<T: Ranting> fmt::Display for Many<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{}", self.name(false))
    }
}

impl<T: Ranting> Ranting for Many<T> {
    fn name(&self, uc: bool) -> String {
        self.joined_names(uc)
    }

    fn subjective(&self) -> &str {
        match self.0.len() {
            1 => self.0[0].subjective(),
            _ => "they",
        }
    }

    fn is_plural(&self) -> bool {
        self.0.len() != 1
    }

    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        // A collection of distinct named items has no separate singular/plural word form of its
        // own to choose between (unlike a single countable noun), except in the one-item case
        // where it just forwards to that item.
        match self.0.len() {
            1 => self.0[0].inflect(to_plural, uc),
            _ => self.joined_names(uc),
        }
    }

    fn skip_article(&self) -> bool {
        match self.0.len() {
            0 => true,
            1 => self.0[0].skip_article(),
            _ => false,
        }
    }

    fn noun_class(&self) -> NounClass {
        // Only a one-item `Many` has an unambiguous class to report; a multi-item phrase mixes
        // whatever classes its members carry, and an empty one has no member at all.
        match self.0.len() {
            1 => self.0[0].noun_class(),
            _ => NounClass::UNSET,
        }
    }

    fn inflect_verb_custom(
        &self,
        subject: &str,
        verb: &str,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        match self.0.len() {
            1 => self.0[0].inflect_verb_custom(subject, verb, as_plural, uc),
            _ => None,
        }
    }

    fn inflect_verb_custom_with_context(
        &self,
        subject: &str,
        verb: &str,
        as_plural: bool,
        uc: bool,
        ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        match self.0.len() {
            1 => self.0[0].inflect_verb_custom_with_context(subject, verb, as_plural, uc, ctx),
            _ => None,
        }
    }

    fn inflect_pronoun_custom(
        &self,
        subject: &str,
        case: PronounCase,
        class: NounClass,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        match self.0.len() {
            1 => self.0[0].inflect_pronoun_custom(subject, case, class, as_plural, uc),
            _ => None,
        }
    }

    fn inflect_pronoun_custom_with_context(
        &self,
        subject: &str,
        case: PronounCase,
        class: NounClass,
        as_plural: bool,
        uc: bool,
        ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        match self.0.len() {
            1 => self.0[0]
                .inflect_pronoun_custom_with_context(subject, case, class, as_plural, uc, ctx),
            _ => None,
        }
    }

    fn inflect_article_custom(
        &self,
        article: &str,
        noun_singular: &str,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        match self.0.len() {
            1 => {
                self.0[0].inflect_article_custom(article, noun_singular, case, class, as_plural, uc)
            }
            _ => None,
        }
    }

    fn inflect_article_custom_with_context(
        &self,
        article: &str,
        noun_singular: &str,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        uc: bool,
        ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        match self.0.len() {
            1 => self.0[0].inflect_article_custom_with_context(
                article,
                noun_singular,
                case,
                class,
                as_plural,
                uc,
                ctx,
            ),
            _ => None,
        }
    }
}

/// Wraps an `Option<T>` so it can be used as a `say!()` placeholder subject/argument.
///
/// `Maybe(Some(x))` delegates every `Ranting` method straight through to `x` — it behaves exactly
/// like `x` alone. `Maybe(None)` renders as the empty string, is singular, subject `"it"`, and
/// [`skip_article`](Ranting::skip_article) returns `true` so `{a maybe}` doesn't leave a
/// dangling article when there's nothing to name.
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, Maybe, say};
/// let some = Maybe(Some(Noun::new("Alex", "they")));
/// assert_eq!(say!("{=some are} here."), "They are here.".to_string());
///
/// let none: Maybe<Noun> = Maybe(None);
/// assert_eq!(say!("There {=none are} nothing."), "There it is nothing.".to_string());
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Maybe<T: Ranting>(pub Option<T>);

// Hand-written for the same reason as `Many`'s: `None` is a sensible default for any `T`, but
// `#[derive(Default)]` would add a `T: Default` bound that isn't actually needed.
impl<T: Ranting> Default for Maybe<T> {
    fn default() -> Self {
        Maybe(None)
    }
}

impl<T: Ranting> fmt::Display for Maybe<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{}", self.name(false))
    }
}

impl<T: Ranting> Ranting for Maybe<T> {
    fn name(&self, uc: bool) -> String {
        match &self.0 {
            Some(item) => item.name(uc),
            None => String::new(),
        }
    }

    fn subjective(&self) -> &str {
        match &self.0 {
            Some(item) => item.subjective(),
            None => "it",
        }
    }

    fn is_plural(&self) -> bool {
        match &self.0 {
            Some(item) => item.is_plural(),
            None => false,
        }
    }

    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        match &self.0 {
            Some(item) => item.inflect(to_plural, uc),
            None => String::new(),
        }
    }

    fn skip_article(&self) -> bool {
        match &self.0 {
            Some(item) => item.skip_article(),
            None => true,
        }
    }

    fn noun_class(&self) -> NounClass {
        match &self.0 {
            Some(item) => item.noun_class(),
            None => NounClass::UNSET,
        }
    }

    fn inflect_verb_custom(
        &self,
        subject: &str,
        verb: &str,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        self.0
            .as_ref()
            .and_then(|item| item.inflect_verb_custom(subject, verb, as_plural, uc))
    }

    fn inflect_verb_custom_with_context(
        &self,
        subject: &str,
        verb: &str,
        as_plural: bool,
        uc: bool,
        ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        self.0.as_ref().and_then(|item| {
            item.inflect_verb_custom_with_context(subject, verb, as_plural, uc, ctx)
        })
    }

    fn inflect_pronoun_custom(
        &self,
        subject: &str,
        case: PronounCase,
        class: NounClass,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        self.0
            .as_ref()
            .and_then(|item| item.inflect_pronoun_custom(subject, case, class, as_plural, uc))
    }

    fn inflect_pronoun_custom_with_context(
        &self,
        subject: &str,
        case: PronounCase,
        class: NounClass,
        as_plural: bool,
        uc: bool,
        ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        self.0.as_ref().and_then(|item| {
            item.inflect_pronoun_custom_with_context(subject, case, class, as_plural, uc, ctx)
        })
    }

    fn inflect_article_custom(
        &self,
        article: &str,
        noun_singular: &str,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        self.0.as_ref().and_then(|item| {
            item.inflect_article_custom(article, noun_singular, case, class, as_plural, uc)
        })
    }

    fn inflect_article_custom_with_context(
        &self,
        article: &str,
        noun_singular: &str,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        uc: bool,
        ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        self.0.as_ref().and_then(|item| {
            item.inflect_article_custom_with_context(
                article,
                noun_singular,
                case,
                class,
                as_plural,
                uc,
                ctx,
            )
        })
    }
}

impl<T: Ranting> Ranting for Box<T> {
    fn name(&self, uc: bool) -> String {
        (**self).name(uc)
    }

    fn subjective(&self) -> &str {
        (**self).subjective()
    }

    fn is_plural(&self) -> bool {
        (**self).is_plural()
    }

    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        (**self).inflect(to_plural, uc)
    }

    fn skip_article(&self) -> bool {
        (**self).skip_article()
    }

    fn noun_class(&self) -> NounClass {
        (**self).noun_class()
    }

    fn inflect_verb_custom(
        &self,
        subject: &str,
        verb: &str,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        (**self).inflect_verb_custom(subject, verb, as_plural, uc)
    }

    fn inflect_verb_custom_with_context(
        &self,
        subject: &str,
        verb: &str,
        as_plural: bool,
        uc: bool,
        ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        (**self).inflect_verb_custom_with_context(subject, verb, as_plural, uc, ctx)
    }

    fn inflect_pronoun_custom(
        &self,
        subject: &str,
        case: PronounCase,
        class: NounClass,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        (**self).inflect_pronoun_custom(subject, case, class, as_plural, uc)
    }

    fn inflect_pronoun_custom_with_context(
        &self,
        subject: &str,
        case: PronounCase,
        class: NounClass,
        as_plural: bool,
        uc: bool,
        ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        (**self).inflect_pronoun_custom_with_context(subject, case, class, as_plural, uc, ctx)
    }

    fn inflect_article_custom(
        &self,
        article: &str,
        noun_singular: &str,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        (**self).inflect_article_custom(article, noun_singular, case, class, as_plural, uc)
    }

    fn inflect_article_custom_with_context(
        &self,
        article: &str,
        noun_singular: &str,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        uc: bool,
        ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        (**self).inflect_article_custom_with_context(
            article,
            noun_singular,
            case,
            class,
            as_plural,
            uc,
            ctx,
        )
    }
}
