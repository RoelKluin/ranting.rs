Ranting&ensp;╰(°Д°)/
==========================

[<img alt="github" src="https://img.shields.io/badge/github-RoelKluin/ranting-8da0cb?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/RoelKluin/ranting.rs)
[<img alt="crates.io" src="https://img.shields.io/crates/v/ranting.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/ranting)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-ranting-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs" height="20">](https://docs.rs/ranting/0.2.1/ranting/)

This library provides [`Ranting`](https://docs.rs/ranting/0.2.1/ranting/trait.Ranting.html), a trait for inflection within [`say!()`](https://docs.rs/ranting_derive/0.2.1/ranting_derive/macro.say.html) litteral string placeholders.

**New to Ranting?** Start with the [**Tutorial**](docs/TUTORIAL.md) (30-40 min read) or jump to the [**Cookbook**](docs/COOKBOOK.md) (10 practical recipes).

```toml
[dependencies]
ranting = "0.2"
```

<br>

## Details

- A `say!()` macro produces a String similar to `format!()`, but with placeholder markers a pronouns can be
  received. A verb alongside, always specified in plural, inflects accordingly.

```rust
use ranting::*;
use ranting_derive::*;

fn say_this(who: Noun, title: &Noun) -> String {
    say!("{=who do} say {`who title are} {who}.")
}


fn main() {
    let title = Noun::new("name", "it");
    assert_eq!(
        say_this(Noun::new("Jane", "I"), &title),
        "I do say my name is Jane.".to_string()
    );
    assert_eq!(
        say_this(Noun::new("Tarzan", "he"), &title),
        "He does say his name is Tarzan.".to_string()
    );
}
```

## Gender-Neutral Pronouns (Singular They)

The library fully supports singular they/them pronouns for individuals who prefer gender-neutral language:

```rust
use ranting::*;
use ranting_derive::*;

fn main() {
    let alex = Noun::new("Alex", "they");
    let jordan = Noun::new("Jordan", "they");
    
    assert_eq!(
        say!("{=alex have} shared {`alex} pronouns: {=alex use} they/them."),
        "They have shared their pronouns: They use they/them.".to_string()
    );
    
    assert_eq!(
        say!("{=jordan are} a talented engineer. I admire {`jordan} work."),
        "They are a talented engineer. I admire their work.".to_string()
    );
}
```

Singular "they" conjugates as plural in form (they are, they have, they do) while referring to a single individual.
All pronouns inflect correctly: subject (they), object (them), possessive determiner (their), possessive pronoun (theirs).

- `Noun` is a struct with the `Ranting` trait. You can use `#[derive(Ranting)]` on a struct or enum for similar
  behavior. To specify the subject use I .. they, thou or ye.

- A placeholder to display a Ranting variable has a structure like:
<br>
  ``{[,^]?(verb )?(article |`noun )?([+-]|([#$]|\?$)var )?[`=@~?*]?noun( verb):fmt}``
<br>

- With `,` and `^` lower- and uppercase are enforced, but a placeholder at sentence start is uppercase by default.
  Also an article or verb with an uppercase causess an uppercase for the first character.

```rust
fn state<T: Ranting>(who: T, liberty: &str) -> String {
    say!("{haven't =who} a {liberty} to say {a who's} land is {~who}?")
}

#[derive_ranting]
#[ranting(subject = "he")]
struct Earl {}

#[derive_ranting]
#[ranting(subject = "they")]
struct Farmers {}

fn main() {
    assert_eq!(
        state(Earl {}, "right"),
        "Hasn't he a right to say an earl's land is his?".to_string()
    );
    assert_eq!(
        state(Farmers {}, "right"),
        "Haven't they a right to say some farmers' land is theirs?".to_string()
    );
}
```

- An article, possesive `'s` or verbs before the noun are also adapted. Normal placeholders just follow their Display or
  Debug traits within `say!()`.

- A given Ranting Enum or Struct can also be inflected to plural or singular. To force plurality use `+`, for a singular
  use `-`. If prependeded by `$var` or `#var`, plurality of the noun is adapted to the numeric variable var. Which is
  displayed, unless prepended with a '?'. The number is converted to a word for `#var`. Other words within the
  placeholder are adapted accordingly as well.

- A Noun or pronoun is displayed dependent on its leading character or string marker.
  * `?` - subject in inflection, but neither variable nor its space is displayed.
  * `=` - subject
  * `@` - object
  * `` ` `` - possesive
  * `~` - adjective
  * `*` - display the name (as is the default) but also mark this word as the Ranting element in the placeholder.
          "A {*can can} contain water."
  (removed the mutname variant)

- If a Noun or numeric plurality has a leading question mark, it is hidden but its inferred inflection does apply.

- An 'article' can be one of `a`, `an`, `some`, `the`, `those` or `these`. These and those are converted to
  this and that if the pronoun is singular. A question mark indicates its display dependends (see no_article).

- `ack!()` and `nay!()` provide an Ok() / Err() return with a `say!()` formatted string included. Intended for allow or
  deny ranting responses. Not for error handling, because true errors should be easy to search in code.

- A struct can receive via attributes. **Core attributes** determine how the noun functions grammatically:
  * **subject** ["it"] - the subject pronoun; if "$", the struct must contain a `subject: String` field
  * **name** [Struct or Enum name] - the display name; if "$", the struct must contain a `name: String` field
  * **singular_end** [""] - suffix to strip when singularizing (for inflect() method)
  * **plural_end** ["s"] - suffix to add when pluralizing (for inflect() method)
  
  **Cosmetic attributes** (optional) adjust formatting and display behavior:
  * plural_you [false] - if subject is "you", whether it refers to plural (affects verb conjugation)
  * uc [false] - whether the name should always start with uppercase (advanced)
  * no_article [false] - whether to skip articles in most contexts (e.g., for proper nouns or meals; advanced)
    Example: `say!("{?the 0} was great!", activity)` with `no_article=true` omits "the"

Positional arguments and numeric references are supported, as well as named arguments:
```
fn main() {
    let thing = Noun::new("thing", "it");

    assert_eq!(say!("this is {=thing}."), "this is it.".to_string());
    assert_eq!(say!("this is {=0}.", thing), "this is it.".to_string());
    
    // Named arguments also work:
    assert_eq!(say!("this is {=x}.", x = thing), "this is it.".to_string());
}
```

## Parsing input with `heed!()`

`heed!()` is the reverse direction from `say!()` — matching input text against a template to extract values, in the spirit of C's `scanf`:

```rust
use ranting::heed;

fn main() {
    assert_eq!(
        heed!("take {item}", "take sword"),
        Some("sword".to_string())
    );
    assert_eq!(
        heed!("give {item} to {target}", "give sword to guard"),
        Some(("sword".to_string(), "guard".to_string()))
    );
    assert_eq!(heed!("take {item}", "drop sword"), None);
}
```

- `{name}` captures a single word; `{name...}` captures greedily (multiple words) up to the next literal word or the end of input; `{$name}` captures digits and parses them as a `u64`.
- Returns `None` if the input doesn't match the template.
- Two placeholders directly adjacent, with no text at all between them (`{a}{b}`), is a compile-time error — there would be no way to know where one capture ends and the next begins. Captures separated only by whitespace (`{a} {b}`) are fine.
- `heed!()` doesn't understand `say!()`'s grammar markers (`=`, `@`, `` ` ``, `~`, tense markers, articles) — it matches plain input text against literal words and named captures only.
