// (c) Roel Kluin 2026 MIT
// A non-English template may write its *own* article keyword, not just
// English's. Before this change the pre-noun slot accepted only a closed
// English vocabulary (the articles, plus a fixed set of modals/auxiliaries),
// so a Spanish template had to be written `{the *=gato}` and rely on
// inflect_article_custom to turn the English keyword into Spanish output.
//
// Two changes make `{el *=gato}` work instead: ph_ext::parse retries with an
// open pre-word slot for templates the English pass rejects, and
// get_article_or_so now offers an unrecognized pre-word to
// inflect_article_custom rather than returning None. Both are needed --
// without the second, the native word renders as inert literal text and gets
// no agreement (`{el +*=gato}` would give "el gatos", not "los gatos").
//
// See docs/superpowers/specs/2026-08-14-language-modularity.md.
use ranting::*;
use std::fmt;

/// A Spanish noun that accepts Spanish article keywords as well as English's.
/// `ranting` itself knows none of these words -- the vocabulary lives here.
#[derive(Clone, Copy)]
struct SpanishNoun {
    singular: &'static str,
    plural: &'static str,
    feminine: bool,
}

const GATO: SpanishNoun = SpanishNoun {
    singular: "gato",
    plural: "gatos",
    feminine: false,
};
const CASA: SpanishNoun = SpanishNoun {
    singular: "casa",
    plural: "casas",
    feminine: true,
};

impl fmt::Display for SpanishNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.singular)
    }
}

impl Ranting for SpanishNoun {
    fn name(&self, uc: bool) -> String {
        capitalize_if(self.singular, uc)
    }
    fn subjective(&self) -> &str {
        if self.feminine { "she" } else { "he" }
    }
    fn is_plural(&self) -> bool {
        false
    }
    fn inflect(
        &self,
        to_plural: bool,
        uc: bool,
        _case: GrammaticalCase,
        _count: Option<PlaceholderCount>,
    ) -> String {
        capitalize_if(
            if to_plural {
                self.plural
            } else {
                self.singular
            },
            uc,
        )
    }
    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_article_custom(
        &self,
        article: &str,
        _noun_singular: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        let definite = match article {
            // English keyword, still accepted -- this is the pre-existing path.
            "the" => true,
            // Spanish's own keywords, which only reach this hook because an
            // unrecognized pre-word is now offered to it.
            "el" | "la" | "los" | "las" => true,
            "a" | "an" | "un" | "una" => false,
            _ => return None,
        };
        let form = match (definite, as_plural, self.feminine) {
            (true, false, false) => "el",
            (true, false, true) => "la",
            (true, true, false) => "los",
            (true, true, true) => "las",
            (false, false, false) => "un",
            (false, false, true) => "una",
            (false, true, false) => "unos",
            (false, true, true) => "unas",
        };
        Some(capitalize_if(form, uc))
    }
}

#[test]
fn native_keyword_renders_like_the_english_one() {
    assert_eq!(say!("Veo {el *=0}.", GATO), "Veo el gato.");
    assert_eq!(say!("Veo {the *=0}.", GATO), "Veo el gato.");
}

/// The point of routing the word to the hook rather than letting it render as
/// literal text: the module, not the template, picks the form, so the article
/// agrees with number and gender.
#[test]
fn native_keyword_agrees_with_number_and_gender() {
    assert_eq!(say!("Veo {el +*=0}.", GATO), "Veo los gatos.");
    assert_eq!(say!("Veo {la *=0}.", CASA), "Veo la casa.");
    assert_eq!(say!("Veo {la +*=0}.", CASA), "Veo las casas.");
    // A native keyword that disagrees with the entity is corrected by the hook,
    // exactly as the English one would be -- the written word selects the
    // *paradigm*, not the form.
    assert_eq!(say!("Veo {los *=0}.", GATO), "Veo el gato.");
}

#[test]
fn native_indefinite_keyword() {
    assert_eq!(say!("Veo {un *=0}.", GATO), "Veo un gato.");
    assert_eq!(say!("Veo {una *=0}.", CASA), "Veo una casa.");
}

/// English output must be byte-identical: an English impl returns `None` from
/// the hook, so an unrecognized pre-word renders exactly as written, which is
/// what it did before this change.
#[test]
fn english_output_is_unchanged() {
    let noun = Noun::new("cat", "it");
    assert_eq!(say!("I see {the 0}.", noun), "I see the cat.");
    assert_eq!(say!("I see {a 0}.", noun), "I see a cat.");
    assert_eq!(say!("I see {these +0}.", noun), "I see these cats.");
    assert_eq!(say!("{haven't =0} a right?", noun), "Hasn't it a right?");
}

/// The open pass only runs when the English pass *fails*, so an unmarked
/// two-word placeholder keeps its English noun+post-verb reading. This is the
/// documented limitation: a native article needs a case marker on the noun to
/// be read as an article rather than as the noun itself.
#[test]
fn unmarked_two_word_placeholder_keeps_the_english_reading() {
    let noun = Noun::new("cat", "it");
    // `walk` is the post-noun verb here, not a pre-noun word.
    assert_eq!(say!("The {=0 walk}.", noun), "The it walks.");
}
