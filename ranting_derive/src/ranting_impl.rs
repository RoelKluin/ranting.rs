// (c) RoelKluin 2022 MIT

use darling::FromDeriveInput;
use proc_macro2::TokenStream;
use ranting_core::grammar as language;
use syn::{Ident, parse_quote};

fn string_it() -> String {
    String::from("it")
}

#[derive(FromDeriveInput, Default)]
#[darling(default, attributes(ranting))]
pub(crate) struct RantingOptions {
    // === CORE ATTRIBUTES (Required for full functionality) ===

    // The subject pronoun: "I", "you", "he", "she", "it", "we", "ye", or "they".
    // If "$", the struct must contain a `subject: String` field.
    // Default: "it"
    #[darling(default = "string_it")]
    pub(crate) subject: String,

    // The display name for the noun. Defaults to struct/enum name.
    // If "$", the struct must contain a `name: String` field.
    pub(crate) name: String,

    // Suffix to strip when singularizing. Empty string means no singularization.
    // Used in the `inflect()` method to convert plural names to singular form.
    //
    // `Option`, not `String`: whether the attribute was *written* is what selects between
    // English's orthographic rules and a literal strip-and-append, so "absent" and "written
    // as the value that happens to be the default" must stay distinguishable. Testing the
    // value instead made `#[ranting(plural_end = "s")]` — the exact opt-out a German or Dutch
    // loanword plural needs — indistinguishable from the default, i.e. no opt-out at all.
    pub(crate) singular_end: Option<String>,

    // Suffix to append when pluralizing. Absent means English's regular rules; see
    // `singular_end` above for why this is an `Option`. Defaulted to "s" only where the
    // literal path actually needs a value, in `ranting::inflect_noun_regular`.
    pub(crate) plural_end: Option<String>,

    // The lexical gender / noun class label, e.g. "masculine", "feminine", "neuter", "common",
    // or any label a non-English fork wants (Bantu class numbers, etc.) — `ranting` never
    // interprets it, it only hands it to the article/pronoun hooks.
    // If "$", the struct must contain a `gender: ranting::NounClass` field.
    // Default: "" (unset), in which case no `noun_class()` is generated at all and the trait's
    // own `NounClass::UNSET` default applies — this is what keeps the attribute purely additive.
    pub(crate) gender: String,

    // === COSMETIC ATTRIBUTES (Optional, affect formatting only) ===

    // If the subject is "you", whether it refers to a plural (default: false, singular).
    // Determines verb conjugation: "you do" (plural) vs. "you does" (singular, archaic).
    pub(crate) plural_you: bool,

    // Whether the name should always start with uppercase.
    // Default: false (respects provided case or applies grammar rules).
    pub(crate) uc: bool,

    // Whether to skip articles in most contexts (e.g., for proper nouns, sports, meals).
    // If true, `skip_article()` returns true in the Ranting impl.
    // Default: false (articles are displayed).
    pub(crate) no_article: bool,
}

fn get_namefn_for(mut opt: RantingOptions, is_enum: bool) -> TokenStream {
    let get_name: TokenStream = if is_enum {
        parse_quote!(self.to_string())
    } else if !opt.name.is_empty() {
        match opt.name.as_str() {
            "$" => {
                return parse_quote! {
                    fn name(&self, uc: bool) -> String {
                        ranting::uc_1st_if(self.name.as_str(), uc)
                    }
                };
            }
            n => {
                opt.uc = n.starts_with(|c: char| c.is_uppercase());
                parse_quote!(#n)
            }
        }
    } else {
        parse_quote!(
            std::any::type_name::<Self>()
                .rsplit("::")
                .next()
                .expect("type path has at least one component")
        )
    };
    let mut first_char: TokenStream = parse_quote!(chrs.next());
    if !opt.uc {
        first_char = parse_quote! {
            if uc {
                #first_char
            } else {
                #first_char.map(|c| c.to_lowercase().next().unwrap_or(c))
            }
        }
    };
    parse_quote! {
        fn name(&self, uc: bool) -> String {
            let name = #get_name;
            let mut chrs = name.chars();
            let oulc = #first_char;
            let mut lc_spaced = String::from(oulc.expect("name should not be empty"));
            for c in chrs {
                if c.is_uppercase() {
                    lc_spaced.push(' ');
                    lc_spaced.push(c.to_lowercase().next().unwrap_or(c));
                } else {
                    lc_spaced.push(c);
                }
            }
            lc_spaced
        }
    }
}

/// The `Option<&str>` a `singular_end`/`plural_end` attribute becomes in the generated
/// `inflect()` call: `None` if unwritten, the struct's field via [`ranting::DeclaredEnding`]
/// for `"$"`, and `Some(literal)` otherwise.
fn declared_ending(attr: Option<&str>, field: TokenStream) -> TokenStream {
    match attr {
        None => parse_quote!(None),
        Some("$") => parse_quote!(ranting::DeclaredEnding::declared(&self.#field)),
        Some(n) => parse_quote!(Some(#n)),
    }
}

fn get_plurality_fns(opt: &RantingOptions) -> TokenStream {
    // `None` when the attribute is absent, which is what selects English's regular rules —
    // an attribute written as `"s"` is a declaration and takes the literal path, exactly as
    // `"e"` or `"en"` would. `"$"` reads the struct's own field and goes through
    // `ranting::DeclaredEnding`, so a field typed `Option<String>` can say "unset" at runtime
    // too (that is how `ranting::Noun` offers `with_plural_end`), while the documented
    // `String` field keeps meaning "declared".
    let singular_end = declared_ending(opt.singular_end.as_deref(), quote::quote!(singular_end));
    let plural_end = declared_ending(opt.plural_end.as_deref(), quote::quote!(plural_end));
    let subject_str = opt.subject.as_str();
    if subject_str == "$" {
        parse_quote! {
            fn subjective(&self) -> &str {
                self.subject.as_str()
            }
            fn is_plural(&self) -> bool {
                ranting::is_subjective_plural(self.subject.as_str())
            }
            fn inflect(&self, as_plural: bool, uc: bool, _case: ranting::GrammaticalCase) -> String {
                let name = self.name(uc);
                if as_plural == self.is_plural() {
                    name
                } else {
                    if as_plural {
                        // Try irregular plural lookup first
                        if let Some(plural) = ranting::inflect_noun_irregular(&name, true) {
                            ranting::uc_1st_if(&plural, uc)
                        } else {
                            // Fall back to the regular rules. They apply English
                            // orthography only while both attributes are at their
                            // defaults; a struct that set either one keeps the
                            // literal strip-and-append it declared.
                            ranting::inflect_noun_regular(&name, true, #singular_end, #plural_end)
                        }
                    } else {
                        // Try irregular singular lookup first
                        if let Some(singular) = ranting::inflect_noun_irregular(&name, false) {
                            ranting::uc_1st_if(&singular, uc)
                        } else {
                            ranting::inflect_noun_regular(&name, false, #singular_end, #plural_end)
                        }
                    }
                }
            }
        }
    } else {
        let is_plural = match subject_str {
            "you" => opt.plural_you,
            _ => language::is_subjective_plural(subject_str),
        };
        parse_quote! {
            fn is_plural(&self) -> bool { #is_plural }
            fn subjective(&self) -> &str {
                #subject_str
            }
            fn inflect(&self, as_plural: bool, uc: bool, _case: ranting::GrammaticalCase) -> String {
                let name = self.name(uc);
                if as_plural == self.is_plural() {
                    name
                } else {
                    if as_plural {
                        // Try irregular plural lookup first
                        if let Some(plural) = ranting::inflect_noun_irregular(&name, true) {
                            ranting::uc_1st_if(&plural, uc)
                        } else {
                            // See the `subject = "$"` branch above: same rules,
                            // same attribute-defaults contract.
                            ranting::inflect_noun_regular(&name, true, #singular_end, #plural_end)
                        }
                    } else {
                        // Try irregular singular lookup first
                        if let Some(singular) = ranting::inflect_noun_irregular(&name, false) {
                            ranting::uc_1st_if(&singular, uc)
                        } else {
                            ranting::inflect_noun_regular(&name, false, #singular_end, #plural_end)
                        }
                    }
                }
            }
        }
    }
}

/// The `noun_class()` override for `#[ranting(gender = "...")]`, or nothing at all when the
/// attribute is absent — an unset gender must leave the generated impl byte-identical to what it
/// was before the attribute existed, so the trait's `NounClass::UNSET` default is used instead of
/// emitting an equivalent method.
fn get_noun_class_fn(opt: &RantingOptions) -> TokenStream {
    match opt.gender.as_str() {
        "" => TokenStream::new(),
        "$" => parse_quote! {
            fn noun_class(&self) -> ranting::NounClass {
                self.gender
            }
        },
        label => parse_quote! {
            fn noun_class(&self) -> ranting::NounClass {
                ranting::NounClass::new(#label)
            }
        },
    }
}

/// An abstract thing, which may be a person and have a gender
pub(crate) fn ranting_q(opt: RantingOptions, is_enum: bool, ident: &Ident) -> TokenStream {
    // TODO: span of attribute?

    let plurality_action: TokenStream = get_plurality_fns(&opt);
    let noun_class_fn: TokenStream = get_noun_class_fn(&opt);

    let no_article = opt.no_article.to_owned();
    let name_fn_q = get_namefn_for(opt, is_enum);

    let display_impl = if is_enum {
        TokenStream::new() // no-op, strum already provided it.
    } else {
        parse_quote! {
            impl std::fmt::Display for #ident {
                fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                    write!(formatter, "{}", self.name(false))
                }
            }
        }
    };
    let ranting_functions: TokenStream = parse_quote! {
        #name_fn_q
        #plurality_action
        fn skip_article(&self) -> bool {
            #no_article
        }
        #noun_class_fn
    };
    parse_quote! {
        impl Ranting for #ident {
            #ranting_functions
        }
        impl Ranting for &#ident {
            #ranting_functions
        }
        impl Ranting for &mut #ident {
            #ranting_functions
        }
        #display_impl
    }
}
