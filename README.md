Ranting&ensp;╰(°Д°)/
==========================

[<img alt="github" src="https://img.shields.io/badge/github-RoelKluin/ranting-8da0cb?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/RoelKluin/ranting)
[<img alt="crates.io" src="https://img.shields.io/crates/v/ranting.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/ranting)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-ranting-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs" height="20">](https://docs.rs/ranting/0.2.0/ranting/)

This library provides [`Ranting`](https://docs.rs/ranting/0.2.0/ranting/trait.Ranting.html), a trait for inflection within [`say!()`](https://docs.rs/ranting_derive/0.2.0/ranting_derive/macro.say.html) litteral string placeholders.

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

fn say_name(who: Noun) -> String {
    say!("{=who do} say {`who} name is {who}.")
}

fn main() {
    assert_eq!(
        say_name(Noun::new("Jane", "I")),
        "I do say my name is Jane.".to_string()
    );
    assert_eq!(
        say_name(Noun::new("Tarzan", "he")),
        "He does say his name is Tarzan.".to_string()
    );
}
```

- `Noun` is a struct with the `Ranting` trait. You can use `#[derive(Ranting)]` on a struct or enum fo similar
  behavior. A struct should also hve a name and a subject String variable. Use I .. they, thou or ye.

- A placeholder to display a Ranting variable has the structure:
<br>
  `{[,^]?(article |verb )?([+-]|#var )?['=@~?*]noun( verb):fmt}`
<br>

- With `,` and `^` lower- and uppercase are enforced, but a placeholder at sentence start is assumed to be
  uppercase. Also an article or verb with an uppercase enforces using an uppercase.

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

- An article, possesive `'s` or verbs before the noun are also adapted. Normal variables just follow their Display or
  Debug traits.

- With the "inflector" feature, a given Ranting trait can also be inflected to plural or singular.

- To force plurality use `+`, for a singular use `-`. If prependeded by `#var`, plurality of the noun is adapted to the
  numeric variable var. Which is displayed, unless prepended with a '?'. Other words within the placeholder are adapted
  as well.

- A Noun or pronoun is displayed dependent on its leading character or string marker.
  * `?` - subject in inflection, but neither variable nor its space is displayed.
  * `=` - subject
  * `@` - object
  * `` ` `` - possesive
  * `~` - adjective
  * `*` - display the name, and mark that this is the Ranting element in the placeholder.
  * `<word>` - similarly, but passes `"word"` as argument to a  function that mutates the Ranting element. However, to
    have any effect `fn mutname(&mut self, command: &str) -> String` must be overridden in the struct implementation.

- If a Noun or numeric plurality has a leading question mark, it is hidden but its inferred inflection does apply.

- An 'article' can be one of `a`, `an`, `some`, `the`, `those` or `these`. These and those are converted to
  this and that if the pronoun is singular.

- `ack!()` and `nay!()` provide an Ok() / Err() return with a `say!()` formatted string included. Intended for allow or
  deny ranting responses. Not for error handling, because true errors should be easy to search in code.

Positional argument and numeric references are supported, but named arguments or empty arguments are not, currecntly.
```
fn main() {
    let thing = Noun::new("thing", "it");

    assert_eq!(say!("this is {=thing}."), "this is it.".to_string());
    assert_eq!(say!("this is {=0}.", thing), "this is it.".to_string());

    // won't work:
    //assert_eq!("{}", say!("this is {=x}.", x = thing), "this is it.".to_string());
    //assert_eq!("{}", say!("this is {=}.", thing), "this is it.".to_string());
}
```
