Ranting&ensp;╰(°Д°)/
==========================

[<img alt="github" src="https://img.shields.io/badge/github-RoelKluin/ranting-8da0cb?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/RoelKluin/ranting)
[<img alt="crates.io" src="https://img.shields.io/crates/v/ranting.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/ranting)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-ranting-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs" height="20">](https://docs.rs/ranting/0.1.0/ranting/)

This library provides [`Ranting`][Ranting], a trait for extended `say!()` placeholders.

```toml
[dependencies]
ranting = "0.1"
```

<br>

## Details

- A `say!()` macro allows you to produce a String with pronouns or verbs inflected accordingly.
  The stuct noun has the Ranting trait. 

```rust
use ranting::*;

fn name(who: Noun) -> String {
    say!("{:who do} say {`who} name is {who}.")
}

fn main() {
    assert_eq!(
        name(Noun::new("Jane", "I")),
        "I do say my name is Jane.".to_string()
    );
    assert_eq!(
        name(Noun::new("Tarzan", "he")),
        "He does say his name is Tarzan.".to_string()
    );
}
```
- It also can adapt an article, possesive `'s` or inflect verbs before the noun, or display normal variables.

```rust
use ranting::*;

fn state(who: Noun, liberty: &str) -> String {
    say!("{haven't :who} a {liberty} to say {a who's} land is {~who}?")
}

fn main() {
    assert_eq!(
        state(Noun::new("earl", "he"), "right"),
        "Hasn't he a right to say an earl's land is his?".to_string()
    );
    assert_eq!(
        state(Noun::new("farmers", "they"), "right"),
        "Haven't they a right to say some farmers' land is theirs?".to_string()
    );
}
```

- `ack!()` and `nay!()` provide an Ok() / Err() return with similar formatting.



Ranting trait objects as arguments to say!() are displayed either as name (by default)
or as pronoun and/or inflected, dependent on provided markers. Articles and verbs
within the curly braces are inflected accordingly. Verbs should be specified in the
plural form.

Any struct with the Ranting trait should have a name and subject variable. The name
should preferrably just be a noun. The subject variable determines the default case,
use I .. they, thou or ye.

Beside ranting trait variables, also variables can be included as normal, that have a
Display or Debug trait. Then just Normal formatting specifiers apply, but for ranting
trait arguments normal formatting specifiers apply to specific parts of the content.

A placeholder to display a Ranting variable has the structure:

`{[,^]?(article |verb )?([+-]|#var )?[':@~?*]noun( verb):fmt}`

With `,` and `^` lower- and uppercase are enforced, but a sentence start is assumed
to be uppercase - the start of a string or after `. ` - also an article or verb with
an uppercase enforces uppercase.

To force plurality use `+`, for a singular use `-`. If prependeded by a `#var` where
`var` is an integer in scope, plurality is adapted to the count, singular if 1,
otherwise plural. A verb or article is inflected along with the specified or default
plurality.

By default the name of a variable is displayed, but a pronoun with formatting markes:
subject with `:`, or `@` for object, `` ` ``: possesive, `~`: adjective. If a verb is
included in the placeholder, the noun is assumed to be subject.

Both a var and the noun can be prepended with a question mark, such as `#?var`,
to indicates that inflection rules occur accordingly, but the var or noun is not
displayed. The inflection may apply to the article and / or verb alongside.

The article can be one of `a`, `an`, `some` `the` `those` or `these`. If capitalized
this is retained. These and those are converted to this and that if the pronoun is
singular.

Positional argument and numeric references are supported, but not named arguments.

ack!() and nay!() are convenience wrappers respectively for 

```rust
return Ok(say!())
// and
return Err(say!())
```

ack!() and nay!() are not intended for normal error handling; placeholders obfuscate
where an error originates from, rendering a simple string search less effective.
Instead use these as a allow / deny response where multiple actors could be involved.
