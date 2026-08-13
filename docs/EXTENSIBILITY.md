# Trait-Based Inflection Extensibility

Ranting v1.1 enables custom grammar rules via trait method overrides, allowing ecosystem forks (ranting-spanish, ranting-pirate, ranting-elvish) to implement dialect-specific inflection rules. By implementing trait methods on your custom noun types, you can extend Ranting to support any language, dialect, or specialized grammar system.

## Quick Start

Here's a simple example: a Pirate dialect that uses "be" for all forms of the verb "to be":

```rust
use ranting::*;
use std::fmt;

#[derive(Clone, Copy)]
struct PirateNoun;

impl fmt::Display for PirateNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "pirate")
    }
}

impl Ranting for PirateNoun {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("pirate", uc)
    }

    fn subjective(&self) -> &str {
        "ye"
    }

    fn is_plural(&self) -> bool {
        true
    }

    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        if to_plural {
            uc_1st_if("pirates", uc)
        } else {
            uc_1st_if("pirate", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_verb_custom(
        &self,
        _subject: &str,
        verb: &str,
        _as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        match verb {
            "be" | "is" | "am" | "are" => Some(uc_1st_if("be", uc)),
            _ => None,
        }
    }
}

// Usage:
fn main() {
    let pirate = PirateNoun;
    assert_eq!(
        say!("{=0 be} fearless!", pirate),
        "Ye be fearless!".to_string()
    );
}
```

## Extension Points (API Reference)

### 2.0 Story-Wide Context: the `_with_context` Variants (v1.1)

Every `_custom` hook below (`inflect_verb_custom`, `inflect_pronoun_custom` and
`inflect_article_custom` since v1.1; `inflect_adjective_custom`,
`elide_article_custom` and `inflect_numeral_custom` since v1.3 — six pairs,
twelve methods) has a `_with_context` counterpart —
`inflect_verb_custom_with_context` and so on, and `capitalize_with_context` for
the orthography hook in §2.6 — that takes one extra parameter,
`ctx: Option<&NarrationContext>`, and is what every call site in the crate
actually invokes. The default implementation of each `_with_context` method
ignores `ctx` and delegates to the plain hook, so everything above still
works unchanged — override the plain hook if you don't need story-wide
context, and only reach for the `_with_context` variant when you do.

`say!()` calls the `_with_context` hooks with `ctx: None`. `say_with!(context, ...)`
calls them with `ctx: Some(&context)`, where `context: NarrationContext` carries
`tense`/`narration_person` (resolved by the crate itself, see the runtime tense/viewpoint
sections above) plus `register: Option<Register>` (`Formal`/`Neutral`/`Casual`) and
`dialect: Option<&'static str>`, which the crate never interprets — they exist purely
for your hook to branch on:

```rust
fn inflect_verb_custom_with_context(
    &self,
    subject: &str,
    verb: &str,
    as_plural: bool,
    uc: bool,
    ctx: Option<&NarrationContext>,
) -> Option<String> {
    match (verb, ctx.and_then(|c| c.register)) {
        ("greet", Some(Register::Formal)) => Some(uc_1st_if("bows before", uc)),
        _ => self.inflect_verb_custom(subject, verb, as_plural, uc),
    }
}
```

`ctx` always arrives as a parameter, never read off `self` — an entity's own `subject`
stays a property of the entity, while `register`/`dialect`/`narration_person` are
story-wide settings that can differ per `say_with!()` call for the same noun.

### 2.1 Verb Inflection: `inflect_verb_custom()`

Customize verb conjugation for any tense, plurality, or person.

**Signature:**
```rust
fn inflect_verb_custom(
    &self,
    subject: &str,
    verb: &str,
    as_plural: bool,
    uc: bool,
) -> Option<String>
```

**Parameters:**
- `subject` (&str): The subject pronoun (e.g., "I", "you", "he", "she", "it", "we", "they")
- `verb` (&str): The verb to inflect (e.g., "be", "have", "walk")
- `as_plural` (bool): Whether to conjugate for plural form
- `uc` (bool): Whether to uppercase the first character (handle contractions with `uc_1st_if`)

**Return Values:**
- `Some(String)`: A custom verb form (used by the macro)
- `None`: Use English default conjugation (no overhead)

**Example: Pirate English**

```rust
fn inflect_verb_custom(
    &self,
    _subject: &str,
    verb: &str,
    _as_plural: bool,
    uc: bool,
) -> Option<String> {
    match verb {
        "be" | "is" | "am" | "are" => Some(uc_1st_if("be", uc)),
        "have" | "has" => Some(uc_1st_if("have", uc)),
        "do" | "does" => Some(uc_1st_if("do", uc)),
        _ => None,  // Fall back to English for other verbs
    }
}
```

**Best Practice:** Return `None` for verbs you don't customize. This automatically triggers English fallback with zero overhead.

### 2.2 Pronoun Inflection: `inflect_pronoun_custom()`

Customize pronoun forms (subject, object, possessive).

**Signature:**
```rust
fn inflect_pronoun_custom(
    &self,
    subject: &str,
    case: PronounCase,
    class: NounClass,
    as_plural: bool,
    uc: bool,
) -> Option<String>
```

**Parameters:**
- `subject` (&str): The subject pronoun (e.g., "I", "you", "he", "she", "it", "we", "they")
- `case` (PronounCase): Which pronoun form is requested (see enum below)
- `class` (`NounClass`): The noun's own lexical gender / noun class, or `NounClass::UNSET` when
  it declares none — see §2.4
- `as_plural` (bool): Whether to pluralize the pronoun
- `uc` (bool): Whether to uppercase the first character

**`PronounCase` Enum:**
```rust
pub enum PronounCase {
    /// Subject pronouns: I, you, he, she, it, we, they
    Subjective,
    /// Object pronouns: me, you, him, her, it, us, them
    Objective,
    /// Possessive determiners: my, your, his, her, its, our, their
    PossessiveDeterminer,
    /// Possessive pronouns: mine, yours, his, hers, its, ours, theirs
    PossessivePronoun,
}
```

**Example: Formal French (vous for plural you)**

```rust
fn inflect_pronoun_custom(
    &self,
    subject: &str,
    case: PronounCase,
    _class: NounClass,
    _as_plural: bool,
    uc: bool,
) -> Option<String> {
    if subject == "you" && case == PronounCase::Subjective {
        // Formal French uses "vous" for plural "you"
        return Some(uc_1st_if("vous", uc));
    }
    None  // Fall back to English for other pronouns
}
```

**Best Practice:** Use case routing (match on `PronounCase`) to handle specific pronoun forms independently. This allows you to customize only the forms you need (e.g., subjective only) while falling back to English for others.

### 2.3 Article Inflection: `inflect_article_custom()`

Customize article forms (a/an/the/some/demonstratives).

**Signature:**
```rust
fn inflect_article_custom(
    &self,
    article: &str,
    noun_singular: &str,
    case: GrammaticalCase,
    class: NounClass,
    as_plural: bool,
    uc: bool,
) -> Option<String>
```

**Parameters:**
- `article` (&str): The requested article form ("a", "an", "the", "some", "these", "those")
- `noun_singular` (&str): The singular inflected form of the noun (useful for vowel/gender detection)
- `case` (`GrammaticalCase`): The noun's own grammatical role, taken from its case marker if the
  template gave it one (`` {the =noun} `` → `Subjective`, `` {the @noun} `` → `Objective`, etc.);
  `GrammaticalCase::Name` for a bare `` {the noun} `` with no marker at all — English gives
  nothing more specific to report in that form, so neither does this. Exists for case-declining
  languages (German `der`/`den`/`dem`) where the article's own form depends on more than gender
  and number; English forks can ignore it.
- `class` (`NounClass`): The noun's own lexical gender / noun class, carried by the entity rather
  than inferred from `noun_singular`, or `NounClass::UNSET` when it declares none — see §2.4.
- `as_plural` (bool): Whether the noun is plural
- `uc` (bool): Whether to uppercase the first character

**Return Values:**
- `Some(String)`: A custom article form (returned with no trailing space; caller adds spacing)
- `None`: Use English default article logic (no overhead)

**Example: Spanish Gendered Articles**

```rust
fn inflect_article_custom(
    &self,
    article: &str,
    noun_singular: &str,
    _case: GrammaticalCase,
    _class: NounClass,
    as_plural: bool,
    uc: bool,
) -> Option<String> {
    match article {
        "the" => {
            // Spanish gendered articles: la/el/los/las based on noun ending
            let form = if noun_singular.ends_with('a') {
                if as_plural { "las" } else { "la" }
            } else {
                if as_plural { "los" } else { "el" }
            };
            Some(uc_1st_if(form, uc))
        }
        _ => None,  // Fall back to English for a/an/some
    }
}
```

**Best Practice:** Examine `noun_singular` for vowel/gender patterns. This parameter is the singularized form of the noun, allowing you to make decisions based on linguistic properties (e.g., French uses "un" for masculine, "une" for feminine). Spanish's own gender doesn't need `case` — its gap is on the pronoun side, not the article side — but a case-declining language's article hook should route on it the same way pronoun hooks already route on `PronounCase`. Where gender itself is what you need, prefer the `class` parameter (§2.4) over inferring it from `noun_singular`'s spelling — the Spanish example above is a spelling heuristic, and it is wrong for `el problema`/`la mano`.

### 2.4 Lexical Gender / Noun Class: `noun_class()` and the `class` parameter (v1.3)

`NounClass` is an open-ended label carried **by the entity**, handed to
`inflect_article_custom`, `inflect_pronoun_custom` and `inflect_adjective_custom` (§2.5) — and
their `_with_context` twins — as the `class` parameter. It is the channel that lets a non-English implementation stop keying gender
off the display string.

```rust
pub struct NounClass(&'static str);      // a newtype, not an enum

impl NounClass {
    pub const UNSET: NounClass;                       // == NounClass::new("")
    pub const fn new(label: &'static str) -> Self;
    pub const fn as_str(&self) -> &'static str;       // "" when unset
    pub const fn is_unset(&self) -> bool;
}
```

**Why a newtype over `&'static str`, not `enum { Masculine, Feminine, Neuter }`.** Bantu
languages have a dozen-plus noun classes and Danish has common/neuter, so an English-adjacent
closed enum would be wrong on arrival. `ranting` attaches no meaning to the label at all — it
carries it from the noun to your hook, exactly like `NarrationContext::dialect`. What the classes
*are* is your language module's business, and adding one costs nothing in this crate.

**What `&'static str` does and doesn't make static.** The *set of labels* a program uses must be
known at compile time (or leaked). Which label a given entity carries is ordinary per-value data,
so a `Noun` built at runtime picks its class at runtime — that's what fixes the homograph problem
below. It is not a promise of runtime-*computed* label strings.

**Declaring a class.** Three ways, all optional:

```rust
// 1. On a derived struct or enum:
#[derive_ranting]
#[ranting(subject = "he", name = "Hund", gender = "masculine")]
struct Hund {}

// 2. On a `Noun`, at construction:
let katze = Noun::new("Katze", "she").with_noun_class(NounClass::new("feminine"));

// 3. In a hand-written `Ranting` impl:
fn noun_class(&self) -> NounClass { self.class }
```

`#[ranting(gender = "$")]` reads the class from a `gender: ranting::NounClass` field on the
struct, following the same attribute-name-is-field-name rule as `name = "$"`/`subject = "$"`.
(`Noun` itself uses this form.)

**Why not just look at `noun_singular`?** Because a gender table keyed by the display string
breaks on homographs, on names, and on nouns built at runtime:

```rust
let music_band = GermanNoun::new("Band", "feminine");  // die Band (a music group)
let ribbon     = GermanNoun::new("Band", "neuter");    // das Band (a ribbon)
```

Both spell `Band`. A `HashMap<&str, Gender>` has one entry for them; the entity has two. Gender
is a property *of the entity*, exactly like `subject`, so that is where it lives.

**Worked example: `der Hund` / `die Katze` / `das Haus` from one code path.** `class` picks the
column, `case` (§2.3) picks the row — nothing here looks at the noun's spelling:

```rust
fn inflect_article_custom(
    &self,
    article: &str,
    _noun_singular: &str,
    case: GrammaticalCase,
    class: NounClass,
    as_plural: bool,
    uc: bool,
) -> Option<String> {
    if article != "the" {
        return None;
    }
    let form = match (class.as_str(), case) {
        (_, _) if as_plural => "die",
        ("masculine", GrammaticalCase::Objective) => "den",
        ("masculine", _) => "der",
        ("feminine", _) => "die",
        ("neuter", _) => "das",
        _ => return None,   // no class declared: let English through
    };
    Some(uc_1st_if(form, uc))
}
```

See `tests/ranting/noun_class.rs` for the runnable version, including the accusative
`den Hund`/`die Katze` contrast and the `Band` homograph pair.

**Additive by construction.** A noun that declares no class reports `NounClass::UNSET`, which is
what a hook would have received before this channel existed; `ranting` itself never reads the
value, so English rendering is byte-identical whether or not a class is set. `Box<T>` forwards
its inner value's class; `Maybe(None)` and a `Many` that doesn't hold exactly one item report
`UNSET`, since neither has one unambiguous class to report.

**Not threaded into `inflect_verb_custom`.** Verb agreement in the languages this targets is
driven by person/number, not by noun class; the verb hook's signature is unchanged. The adjective
hook (§2.5) does receive `class` — adjective *agreement* is exactly where a class label is needed.

### 2.5 Adjective Agreement: `inflect_adjective_custom()` (v1.3)

```rust
fn inflect_adjective_custom(
    &self,
    adjective: &str,          // as written in the placeholder, e.g. "noir"
    degree: AdjectiveDegree,  // Comparative (`!`) or Superlative (`!!`)
    case: GrammaticalCase,    // as §2.3
    class: NounClass,         // as §2.4
    as_plural: bool,
    uc: bool,
) -> Option<String>

fn inflect_adjective_custom_with_context(/* the same, plus */ ctx: Option<&NarrationContext>)
    -> Option<String>
```

Called for the post-noun degree slot, `{noun !adj}` / `{noun !!adj}`. Return `Some` to render your
own form; return `None` (the default) to keep the English comparative/superlative the macro
resolved at compile time — which is why English output is unchanged by this hook's existence.

**Why it exists.** English degree needs no agreement, so `ranting_derive` resolves `!good` to
`better` at compile time and `ranting` had no runtime adjective path at all. Romance and Germanic
adjectives agree with their noun in gender, number and (German) case — none of which is knowable
when the macro runs.

**You get the adjective as written, not English's degree form.** `{a chat !noir}` hands your hook
`"noir"`, never `"noirer"`: the resolved form is not reversible back into the base, and parsing it
back out would be the string-sniffing this API exists to avoid. The macro bakes both (the base for
you, the English form for the fallback), the same way `say_with!()` bakes the uninflected base verb
for runtime tense resolution.

**Worked example: `un chat noir` / `une robe noire` / `des chats noirs` from one template.**

```rust
fn inflect_adjective_custom(
    &self,
    adjective: &str,
    _degree: AdjectiveDegree,
    _case: GrammaticalCase,
    class: NounClass,
    as_plural: bool,
    uc: bool,
) -> Option<String> {
    let mut form = adjective.to_string();
    if class.as_str() == "feminine" {
        form.push('e');
    }
    if as_plural {
        form.push('s');
    }
    Some(uc_1st_if(&form, uc))
}
```

With `un`/`une`/`des` coming from `inflect_article_custom` (§2.3) off the same `class`, the single
template `say!("J'ai vu {a 0 !noir}.", noun)` renders all three. `tests/ranting/adjective_agreement.rs`
is the runnable version, including a French superlative (`le plus noir`) that uses `degree`.

**Known limitation: `!` is the only adjective slot there is.** The placeholder grammar has no
positive-degree adjective marker — an unmarked post-noun word is parsed as a *verb*, and an
adjective written outside the placeholder is literal template text no hook can reach. So a fork
whose adjectives merely agree (rather than compare) writes `!` and ignores `degree`, as above.
That is a real wart, recorded rather than papered over; widening the grammar would add surface for
every English user, the shape of point fix ROADMAP.md Phase 6 item 1 rejected for German word
order. Note also that agreement is *form*, never *position*: French post-nominal vs. prenominal
adjective placement is word order, which item 1 established stays with the caller's template.

**`uc` is yours to apply.** As with the article and pronoun hooks, the caller's
uppercase-first-character pass runs only on the fallback path, so a custom form should call
`uc_1st_if` itself.

**Wrappers.** `Box<T>` forwards to its inner value; `Many`/`Maybe` forward only when they hold
exactly one item, and otherwise decline (there is no single entity whose gender could agree).

### 2.6 Orthography: `capitalize()` (v1.3, `sentence_start` added in Phase 6 item 17)

```rust
fn capitalize(
    &self,
    word: &str,               // the rendered text, uncapitalized (but see the Noun caveat)
    role: OrthographyRole,    // Article | Verb | Pronoun | Noun | Adjective
    uc: bool,                 // what English would do: uppercase the first character
    sentence_start: bool,     // was this placeholder actually at the start of a sentence?
) -> String

fn capitalize_with_context(/* the same, plus */ ctx: Option<&NarrationContext>) -> String
```

Unlike every other hook on this page this returns a `String`, not an `Option<String>` — it *is*
the fallback, not a chance to decline one, which is why it isn't named `_custom`. Its default is
exactly `uc_1st_if(word, uc)`, so overriding nothing leaves `say!()`'s output byte-identical.

**Why it exists.** Sentence-start uppercasing is an English orthographic assumption that used to be
compiled into `ranting` at every call site. German capitalizes every noun wherever it stands;
Japanese, Chinese, Arabic and Hebrew have no letter case, so `uc` is meaningless and the honest
implementation returns `word` unchanged; Turkish needs `i` → `İ` and `ı` → `I`, which
`char::to_uppercase` gets wrong for a Turkish locale. All of that is now one override.

**What it does not decide.** The hook decides what is *done* with `uc`, not what `uc` *is*. Whether
a placeholder sits at a sentence start, and the `,`/`^` markers that force lower/uppercase, are
resolved by the macro at compile time and arrive here as the bool. Nor is this case *preservation*:
`apply_case`, which keeps an irregular plural's ALL-CAPS/Title/lowercase pattern, sits behind the
`self`-less free function `inflect_noun_irregular` and is not routed through any hook.

**`sentence_start`, separate from `uc` (Phase 6 item 17).** `uc` conflates two things: "this
placeholder is at a sentence start" and "something forces uppercase regardless of position" — a
`` {The 0} `` pre-text word forces `uc == true` even mid-sentence, and a `` {,noun} `` marker forces
`uc == false` even right after a period. `sentence_start` is the first signal alone, computed once
at compile time by the same check that already feeds `uc`
(`ranting_core::grammar::PH_START`/`SENTENCE_TRIGGER_CHARS`), and threaded through
`ranting_core::placeholder::PlaceholderSpec` so it costs nothing at runtime. Most forks that only
care about letter case can keep ignoring it and use `uc` exactly as before — it exists for a
caseless-script fork that still wants sentence boundaries for its own punctuation, or a downstream
word-order/reordering layer (see `docs/superpowers/specs/2026-08-13-word-order-feasibility.md`,
open question 2, which this parameter closes). `tests/ranting/orthography.rs`'s
`forced_lowercase_marker_keeps_sentence_start_true` and
`uppercase_pre_word_does_not_imply_sentence_start` pin the two signals disagreeing in both
directions.

**Sentence detection beyond ASCII (Phase 6 item 17).** `PH_START` used to recognize only an ASCII
`.`/`?`/`!` followed by whitespace as putting the next placeholder at a sentence start — missing
Greek's question mark (a distinct Unicode codepoint that looks like an ASCII semicolon), Japanese/
Chinese full-width terminators (which take no following space at all), Urdu's full stop, and
Spanish's opening `¿`/`¡` (which mark sentence-initial from *before* the placeholder, not after a
prior sentence). `PH_START` now recognizes all of these, each with the shape its script actually
uses: ASCII/Greek/Urdu terminators still require `\s+` after them (those scripts space-separate
words); the CJK full-width terminators need no following whitespace; Spanish's opening marks are
optionally followed by whitespace (`\s*+`) since they attach to the sentence they open rather than
close one. `ranting_core::grammar::SENTENCE_TRIGGER_CHARS` is the single list both `PH_START`'s
regex and `ranting_derive`'s `at_sentence_start` check read from, so the two can't drift apart. See
`tests/ranting/sentence_detection.rs` for the Greek/Japanese/Spanish cases and a byte-identical
ASCII regression guard.

**Worked example: German nouns, capitalized wherever they stand.**

```rust
fn capitalize(&self, word: &str, role: OrthographyRole, uc: bool, _sentence_start: bool) -> String {
    match role {
        OrthographyRole::Noun => uc_1st_if(word, true),  // German: always
        _ => uc_1st_if(word, uc),                        // everything else: as English
    }
}
```

With `der`/`den` coming from `inflect_article_custom` (§2.3), `say!("Heute bellt {the 0}.", hund)`
renders `"Heute bellt der Hund."` from a noun whose own `name()` is the lowercase `"hund"` — the
capital can only have come from the hook. `tests/ranting/orthography.rs` is the runnable version,
including a caseless-script no-op and a Turkish `capitalize_with_context` keyed on
`NarrationContext::dialect`.

**The one asymmetry: `OrthographyRole::Noun` gets `uc: false`.** Four of the five roles hand the
hook an uncapitalized word and a truthful `uc`. The noun's name does not: it has already been
through `inflect()`, which takes `uc` itself and is user-implementable, so English capitalization
is spent by the time the hook runs — and it is not simply `uc_1st_if`, since a derive-generated
`name()` for `#[ranting(name = "designer")]` reads `uc == true` as "as written", not "force
uppercase". Routing `uc` through the hook there would silently start capitalizing such names, so
the call site passes `false` instead. An always-capitalize fork ignores `uc` and is unaffected; a
fork needing *position-sensitive* noun casing overrides `name`/`inflect` instead.

**Custom forms are not routed here.** As everywhere else on this page, the hook runs on the
fallback path only. An `inflect_*_custom` that returns `Some` owns its own `uc` and never reaches
`capitalize`.

**Wrappers.** `Box<T>` forwards to its inner value. `Many` forwards only when it holds exactly one
item — the same rule as `noun_class()` (§2.4) and for the same reason: a multi-item phrase is one
joined string whose members may disagree, so it keeps the English default. `Maybe(Some(x))`
forwards to `x`; `Maybe(None)` keeps the default.

### 2.7 Elision & Contraction: `elide_article_custom()` (v1.3)

```rust
fn elide_article_custom(
    &self,
    article: &str,          // the article as rendered, capitalization included
    separator: &str,        // the whitespace between it and what follows (usually " ")
    following: &str,        // the rendered text adjacent to it (see below)
    case: GrammaticalCase,
    class: NounClass,
    as_plural: bool,
) -> Option<String>         // Some(fused) replaces all three; None keeps them as rendered

fn elide_article_custom_with_context(/* the same, plus */ ctx: Option<&NarrationContext>) -> Option<String>
```

**Why it exists, and why it is not a parameter on `inflect_article_custom`.** English `a`/`an` is
the crate's one article choice that depends on the word *after* the article — and it is hard-coded
English phonology. No fork can express its equivalent through §2.3's hook, and the reason is
structural rather than a missing parameter: `inflect_article_custom` returns its string *before*
the following text has been rendered. French `l'homme` vs `le chien`, Italian `lo`/`il`/`l'` and
Portuguese article fusion all need the two words side by side, so this hook runs *after* assembly,
when they are.

**Worked example: French elision.** Gender picks `le`/`la` in §2.3's hook, which cannot yet see the
noun; elision then rewrites both alike.

```rust
fn elide_article_custom(
    &self, article: &str, _separator: &str, following: &str,
    _case: GrammaticalCase, _class: NounClass, _as_plural: bool,
) -> Option<String> {
    if !self.elides { return None; }          // aspirate h (le héros) declines, per-noun
    match article {
        "le" | "la" => Some(format!("l'{following}")),
        "Le" | "La" => Some(format!("L'{following}")),
        _ => None,
    }
}
```

`say!("Voici {the 0}.", homme)` → `"Voici l'homme."`, `say!("Voici {the 0}.", chien)` → `"Voici le
chien."`. Note the elision decision is *lexical*, carried by the entity (mute vs. aspirate h is not
derivable from spelling), exactly as `NounClass` is in §2.4. `tests/ranting/elision.rs` is the
runnable version, including an Italian `lo`/`il`/`l'` body.

**What `following` contains.** Whatever is actually adjacent to the article: any words the
placeholder's own pre-text carried after it (`` {a set of $n chiens} `` gives `"set of 2 chiens"`),
then the number when there is one, then the noun name or case-selected pronoun. Rendered text, not
dictionary forms — that is the point of running after assembly.

**No `uc` parameter.** The `article` handed to this hook is already rendered *and* capitalized —
whether by §2.3's hook or the English fallback — so there is nothing left for `uc` to decide. A
fork that re-cases its fused form inspects the first character, or calls `capitalize` (§2.6)
itself.

**What is not reachable from here.** Preposition-article fusion *across a placeholder boundary* —
French `de` + `le` → `du`, Italian `di` + `il` → `del` — is out of scope: the preposition lives in
the template's literal text, outside the placeholder, and `` {de le chien} `` parses `de` as a
pre-noun verb. Reaching it would take a post-pass over the whole rendered `say!()` output. A hidden
noun (`` {?the noun} ``) also renders nothing to elide against, so the hook is not called there.

**English is untouched.** `a`/`an` is chosen from the singular noun inside `get_article_or_so` and
never routes through this hook; the default returns `None`, which keeps the article, separator and
following text exactly as rendered.

**Wrappers.** `Box<T>` forwards to its inner value. `Many` forwards only when it holds exactly one
item — the same rule as `noun_class()` (§2.4) and `capitalize()` (§2.6), and for the same reason:
for 2+ items `following` is the joined phrase, whose members may elide differently.
`Maybe(Some(x))` forwards to `x`; `Maybe(None)` declines.

### 2.8 Numerals: `inflect_numeral_custom()` (v1.3)

```rust
fn inflect_numeral_custom(
    &self,
    numeral: &str,          // the number as English renders it — the fallback if this declines
    count: Option<i64>,     // the number itself, when available (see below)
    style: NumeralStyle,    // Words for `#var`, Digits for `$var`
    case: GrammaticalCase,
    class: NounClass,
    as_plural: bool,
) -> Option<String>         // Some(numeral) replaces the rendering; None keeps English's

fn inflect_numeral_custom_with_context(/* the same, plus */ ctx: Option<&NarrationContext>) -> Option<String>
```

**Why it exists.** A placeholder can write its number two ways, and before this hook both were
hard-coded: `` {#n boots} `` spelled it in English words via the `english-numbers` crate, and
`` {$n boots} `` printed the argument's own `Display`, i.e. ASCII digits. Every other language
needs its own speller (`zwei`, `deux`, `два`), several agree the numeral itself with the noun's
gender and case — Russian `два стола` but `две книги` — and several scripts have digits of their
own (Devanagari `२`, Arabic-Indic `٢`).

**What changed to make it possible.** `#var` used to be spelled by the *macro*, baked into the
`format!()` argument as a finished English word. It is spelled at runtime now, from a count the
macro bakes instead, with `rant_convert_numbers` — the same speller — as the fallback. That is why
English output is unchanged and why a fork can replace the speller outright rather than
post-processing its output. The number's leading space moved along with it, out of the baked string
and into `placeholder::NumeralSpec`, so the text handed to this hook is the numeral alone.

**Worked example: Russian gender agreement.**

```rust
fn inflect_numeral_custom(
    &self, numeral: &str, count: Option<i64>, style: NumeralStyle,
    _case: GrammaticalCase, class: NounClass, _as_plural: bool,
) -> Option<String> {
    match style {
        NumeralStyle::Words => Some(match (count?, class.as_str()) {
            (1, "feminine") => "одна".to_string(),
            (1, _)          => "один".to_string(),
            (2, "feminine") => "две".to_string(),
            (2, _)          => "два".to_string(),
            (n, _)          => n.to_string(),
        }),
        // Devanagari digits: a transcription of what English rendered.
        NumeralStyle::Digits => Some(numeral.chars().map(|c| match c {
            '0'..='9' => char::from_u32(c as u32 - '0' as u32 + 0x966).unwrap_or(c),
            other => other,
        }).collect()),
    }
}
```

One template, one hook body, gender off the entity (§2.4): `say!("есть {#0 1}", 2, stol)` →
`"есть два стола"`, `say!("есть {#0 1}", 2, kniga)` → `"есть две книги"`.
`tests/ranting/numeral.rs` is the runnable version.

**When `count` is `Some`.** Always for `NumeralStyle::Words`: the macro bakes the same `as i64`
cast it always applied before spelling. For `NumeralStyle::Digits` it is recovered by parsing
`numeral`, because a `$var` argument need not be an integer at all — anything `Display` will do —
so it is `None` for a float, a width-padded or otherwise formatted number, and a non-numeric
argument. A digit-transcribing fork wants `numeral` anyway; a spelling fork wants `count`.

**This is not item 4's count channel.** The count here is local to the numeral. It says nothing to
the other five `_custom` pairs, which still receive `as_plural: bool` alone — so Arabic dual, Slavic paucal
and CLDR-style categories on the *noun, article and verb* remain out of reach. See the
`as_plural` discussion in CLAUDE.md and ROADMAP.md Phase 6 item 4.

**Number agreement is decided before this runs.** `as_plural` comes from the count, not from the
rendered word, so a custom numeral can never flip it. (It used to come from the word: the old test
compared the rendering against the literal English `"one"`, which would have made a fork's `"один"`
plural. That was the prerequisite ROADMAP.md item 8 called out, and it is fixed rather than
documented-around.)

**No `uc` parameter.** `handle_placeholder` never capitalizes the numeral — a placeholder that
starts a sentence spends its `uc` on the article, verb or noun — so there is nothing for the hook
to decide. Note also that a returned string replaces the rendering outright, so a `:fmt` width/fill
spec on `$var` is not re-applied to it; a fork that wants padding pads its own output.

**When it is not called.** A placeholder with no `#var`/`$var` marker, and a hidden one
(`` {?$n boots} ``, where the number governs agreement but is not written) — the same
"nothing rendered, nothing to customize" rule as §2.7. `heed!()`/`ask!()`'s `{$name}` is input
parsing, the inverse direction, and does not route here either.

**English is untouched.** The default returns `None`, which keeps `rant_convert_numbers` for `#var`
and the argument's own `Display` for `$var`.

**Wrappers.** `Box<T>` forwards to its inner value; `Many` forwards only when it holds exactly one
item, as in §2.4/§2.6/§2.7; `Maybe(Some(x))` forwards to `x`, `Maybe(None)` declines.

### 2.9 `Many` Supplying Its Own Length as the Count (v1.3, ROADMAP.md Phase 6 item 15)

`inflect_verb_custom`, `inflect_pronoun_custom`, `inflect_article_custom`,
`elide_article_custom` and `inflect_adjective_custom` (and their five `_with_context` twins) each
take a `count: Option<PlaceholderCount>` parameter (item 14) sourced from the placeholder's own
`#var`/`$var` marker — `None` for a bare placeholder (`` {noun} ``, `` {+noun} ``, `` {-noun} ``).
`Many<T>` (`src/collections.rs`) is the one wrapper that genuinely knows a count with no numeral
in sight: its own `Vec`'s length. When `Many` delegates one of these five hook pairs to its single
item (the `len() == 1` case — see §2.4/§2.6/§2.7's wrapper notes), it substitutes its own length
for a `None` count before forwarding, so a fork's hook sees `Some(PlaceholderCount { value: 1, .. })`
even though the placeholder carried no numeral at all. If the placeholder *did* carry a numeral,
that value is left untouched — `Many` only fills in the gap, never overrides an explicit count.

This does not extend to `Many` holding zero or 2+ items: those arms have no single item to
delegate a hook call to in the first place (`tests/ranting/elision.rs`'s
`many_with_two_items_does_not_elide` pins that a 2+ item `Many` keeps the English default
rendering untouched), so there is no hook invocation for a substituted count to accompany. `Maybe`
and `Box` are unaffected — `Maybe(Some(x))`/`Box<T>` always hold exactly one value with no
alternative count to offer, and `Maybe(None)` has none at all — so both keep forwarding whatever
`count` they were handed, `None` included. `tests/ranting/many_count.rs` is the runnable version,
covering empty, single-item and multi-item `Many`.

### 2.10 First-Person Narration Viewpoint: `is_first_person_subject_custom()` (v1.3, ROADMAP.md Phase 6 item 16)

`say_with!()`'s `NarrationContext.narration_person` override (§2.0, and the README's Gender-Neutral
Pronouns / narration sections) only retells a noun declared first-person — the narrator or
narrator-group — into second or third person. Whether a given `subject` label counts as
first-person used to be a hard-coded `matches!(subject, "I" | "we")` in
`ranting_core::grammar::is_first_person_subject`, with no way for a fork to widen it. A fork whose
first-person labels are `ich`/`wir`, `je`/`nous`, etc. got a silent no-op: `narration_person` was
set, but `resolve_viewpoint` never recognized the noun as first-person, so nothing changed.

`is_first_person_subject_custom(&self, subject: &str) -> bool` closes that gap. It defaults to
exactly the old hard-coded check (`ranting_core::grammar::is_first_person_subject`), so English
output — and every implementation that doesn't override it — is unaffected. A fork overrides it to
recognize its own first-person labels:

```rust
impl Ranting for GermanNarrator {
    // ... required methods ...

    fn is_first_person_subject_custom(&self, subject: &str) -> bool {
        matches!(subject, "ich" | "wir")
    }
}
```

With that override in place, `say_with!(ctx, ...)` with `ctx.narration_person = Some(Person::Third)`
retells a `subject = "ich"` narrator the same way it retells an English `subject = "I"` one — the
rendered pronoun itself is still the crate's fixed `"you"`/`"they"` (§2.0's "no gender data on a
first-person-declared noun" caveat applies here too), only *whether the override fires at all* is
what the hook controls. `subject` is passed as a parameter rather than read off `self.subjective()`
for the same reason `inflect_verb_custom` does: `Many`/`Maybe`/`Box` delegate this hook to an inner
value the same way they delegate `noun_class()`, and it's the caller, not the callee, that decides
which entity's declared subject is in play. See `tests/ranting/first_person_hook.rs`.

## Partial Customization

You don't need to implement every `_custom` method. If you only need verb customization, implement `inflect_verb_custom()` and leave the rest as default (returning `None`). The trait provides a default for all of them:

```rust
impl Ranting for MyNoun {
    // ... required methods (name, subjective, is_plural, inflect, skip_article) ...

    fn inflect_verb_custom(
        &self,
        subject: &str,
        verb: &str,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        // Custom verb logic here
        match verb {
            "be" => Some(uc_1st_if("am", uc)),
            _ => None,
        }
    }

    // inflect_pronoun_custom and inflect_article_custom are not overridden
    // They return None by default, triggering English fallback
}
```

This is perfectly valid. Returning `None` automatically triggers English fallback with zero overhead.

## Full Examples

### 4.1 Pirate Dialect

A complete pirate dialect implementation that customizes verb conjugation:

```rust
use ranting::*;
use std::fmt;

#[derive(Clone, Copy)]
struct PirateNoun;

impl fmt::Display for PirateNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "pirate")
    }
}

impl Ranting for PirateNoun {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("pirate", uc)
    }

    fn subjective(&self) -> &str {
        "ye"
    }

    fn is_plural(&self) -> bool {
        true
    }

    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        if to_plural {
            uc_1st_if("pirates", uc)
        } else {
            uc_1st_if("pirate", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_verb_custom(
        &self,
        _subject: &str,
        verb: &str,
        _as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        match verb {
            "be" | "is" | "am" | "are" => Some(uc_1st_if("be", uc)),
            "have" | "has" => Some(uc_1st_if("have", uc)),
            "do" | "does" => Some(uc_1st_if("do", uc)),
            _ => None,
        }
    }
}

fn main() {
    let pirate = PirateNoun;
    
    // Usage examples:
    let result1 = say!("{=0 be} fearless!", pirate);
    assert_eq!(result1, "Ye be fearless!".to_string());
    
    let result2 = say!("{=0 have} {the 0}?", pirate);
    assert_eq!(result2, "Ye have the pirate?".to_string());
}
```

**Output:**
- `"{=0 be} fearless!"` → `"Ye be fearless!"`
- `"{=0 have} treasure"` → `"Ye have treasure"`

### 4.2 Scottish Highland English

A dialect combining verb and pronoun customization:

```rust
use ranting::*;
use std::fmt;

#[derive(Clone, Copy)]
struct ScottishHighlander;

impl fmt::Display for ScottishHighlander {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "highlander")
    }
}

impl Ranting for ScottishHighlander {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("highlander", uc)
    }

    fn subjective(&self) -> &str {
        "he"
    }

    fn is_plural(&self) -> bool {
        false
    }

    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        if to_plural {
            uc_1st_if("highlanders", uc)
        } else {
            uc_1st_if("highlander", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_verb_custom(
        &self,
        _subject: &str,
        verb: &str,
        _as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        match verb {
            "be" | "is" | "am" | "are" => Some(uc_1st_if("be", uc)),
            _ => None,
        }
    }

    fn inflect_pronoun_custom(
        &self,
        subject: &str,
        case: PronounCase,
        _as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        if subject == "he" && case == PronounCase::Subjective {
            return Some(uc_1st_if("he lad", uc));
        }
        None
    }
}

fn main() {
    let highlander = ScottishHighlander;
    
    // Case routing demonstration:
    let result1 = say!("{=0 be} brave.", highlander);
    // Subjective case uses custom "he lad"
    assert_eq!(result1, "He lad be brave.".to_string());
    
    let result2 = say!("I see {@0}.", highlander);
    // Objective case falls back to English "him"
    assert_eq!(result2, "I see him.".to_string());
}
```

**Output:**
- `"{=0 be} brave"` → `"He lad be brave"` (custom pronoun + custom verb)
- `"I see {@0}"` → `"I see him"` (objective case falls back to English)

### 4.3 Spanish with Gendered Articles and Verbs

A complete Spanish implementation with both article and verb customization:

```rust
use ranting::*;
use std::fmt;

#[derive(Clone, Copy)]
struct SpanishNoun;

impl fmt::Display for SpanishNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "cosa")
    }
}

impl Ranting for SpanishNoun {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("cosa", uc)
    }

    fn subjective(&self) -> &str {
        "it"
    }

    fn is_plural(&self) -> bool {
        false
    }

    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        if to_plural {
            uc_1st_if("cosas", uc)
        } else {
            uc_1st_if("cosa", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_verb_custom(
        &self,
        _subject: &str,
        verb: &str,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        match verb {
            "be" => {
                // Spanish "ser": "es" (singular) or "son" (plural)
                let form = if as_plural { "son" } else { "es" };
                Some(uc_1st_if(form, uc))
            }
            _ => None,
        }
    }

    fn inflect_article_custom(
        &self,
        article: &str,
        noun_singular: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        if article == "the" {
            // Spanish gendered articles based on noun ending
            let form = if noun_singular.ends_with('a') {
                if as_plural { "las" } else { "la" }
            } else {
                if as_plural { "los" } else { "el" }
            };
            return Some(uc_1st_if(form, uc));
        }
        None
    }
}

fn main() {
    let cosa = SpanishNoun;
    
    // Article and verb customization working together:
    let result1 = say!("{the 0 be} hermosa", cosa);
    // Feminine ending 'a' triggers "la", verb "be" becomes "es"
    assert_eq!(result1, "La cosa es hermosa".to_string());
    
    let result2 = say!("{the +0 be} hermosas", cosa);
    // Plural form triggers "las"
    assert_eq!(result2, "Las cosas son hermosas".to_string());
}
```

**Output:**
- `"{the 0 be} hermosa"` → `"La cosa es hermosa"` (feminine article + Spanish verb)
- `"{the +0 be} hermosas"` → `"Las cosas son hermosas"` (plural feminine + plural verb)

Note this example predates `NounClass` (§2.4) and still infers gender from the noun's ending —
a heuristic that gets `el problema` and `la mano` wrong. A `ranting-spanish` written today should
declare `#[ranting(gender = "feminine")]` (or set it per-`Noun`) and match on the `class`
parameter instead of on `noun_singular`'s last character.

## Best Practices

1. **Partial customization is fine** — Return `None` for any inflection you don't customize. This immediately triggers English default behavior with zero overhead, avoiding code duplication.

2. **Use `uc_1st_if()` for capitalization** — This helper function correctly handles both regular words and contractions (e.g., "'tis" → "'Tis"). Always use it when your custom method needs to apply uppercase logic.

3. **Test your overrides with integration tests** — Create tests similar to `tests/ranting/custom_inflection.rs` that exercise your custom methods with real `say!()` calls. Include edge cases like empty forms, contractions, and plural/singular transitions.

4. **Document your dialect/language in your ecosystem fork's README** — If you create a new crate (e.g., `ranting-french`), include examples of the customizations you've implemented and any cultural/linguistic notes that help users understand the rules.

5. **Keep custom methods fast** — These methods are called for every placeholder that requires inflection. Avoid complex lookups or allocations; prefer simple match statements on the verb/article/case enums and return early for non-matching cases.

## Performance Notes

Custom method dispatch is a **zero-cost abstraction** in Ranting:

- **If your method returns `None`:** The English fallback is used without any function-call overhead beyond the Option check itself.
- **If your method returns `Some(String)`:** You pay the cost of string creation (unavoidable) and returning the custom form.
- **No additional function-call overhead:** Unlike virtual dispatch, the method call is inlined by the compiler, and the Option check is a single branch.

Example: A pirate verb method that matches and returns a custom form costs one match statement and one Option wrap. This is negligible compared to the cost of string handling in the macro itself.

## Contributing Custom Rules

The Ranting ecosystem grows when users and contributors build dialect-specific forks. Here's how to contribute:

### For English Inflection Bugs
Open a GitHub issue at [RoelKluin/ranting.rs](https://github.com/RoelKluin/ranting.rs) with:
- Your test case (a `say!()` expression and expected output)
- The actual output you got
- A description of which English rule is broken

### For New Language Modules
Create a companion crate following the naming convention `ranting-<language>` or `ranting-<dialect>`:

```toml
[package]
name = "ranting-french"
version = "0.1.0"

[dependencies]
ranting = "0.2"
```

In your crate:
1. Define custom noun types with `impl Ranting` blocks for your language
2. Pre-build common patterns (e.g., `FrenchMasculineNoun`, `FrenchFeminineNoun`)
3. Export them from your `lib.rs` for users to import
4. Include examples and documentation in your README

Example use:
```rust
use ranting::say;
use ranting_french::FrenchNoun;

fn main() {
    let word = FrenchNoun::masculine("chat");  // "cat"
    assert_eq!(
        say!("{the 0 be} noir.", word),
        "Le chat est noir.".to_string()
    );
}
```

By creating these forks, you help the Ranting ecosystem support more languages and dialects while keeping the core library lean and focused on English.
