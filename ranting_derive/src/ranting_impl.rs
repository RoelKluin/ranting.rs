// (c) RoelKluin 2022 GPL v3

use crate::language::english_shared as language;
use darling::FromDeriveInput;
use proc_macro2::TokenStream;
use syn::{parse_quote, Ident};

fn string_it() -> String {
    String::from("it")
}
fn string_s() -> String {
    String::from("s")
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
    pub(crate) singular_end: String,

    // Suffix to append when pluralizing.
    // Used in the `inflect()` method to convert singular names to plural form.
    // Default: "s"
    #[darling(default = "string_s")]
    pub(crate) plural_end: String,

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
        parse_quote!(std::any::type_name::<Self>().rsplit("::").next().expect("type path has at least one component"))
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

fn get_plurality_fns(opt: &RantingOptions) -> TokenStream {
    let singular_end: TokenStream = match opt.singular_end.as_str() {
        "$" => parse_quote!(self.singular_end.as_str()),
        n => parse_quote!(#n),
    };
    let plural_end: TokenStream = match opt.plural_end.as_str() {
        "$" => parse_quote!(self.plural_end.as_str()),
        n => parse_quote!(#n),
    };
    let subject_str = opt.subject.as_str();
    if subject_str == "$" {
        parse_quote! {
            fn subjective(&self) -> &str {
                self.subject.as_str()
            }
            fn is_plural(&self) -> bool {
                ranting::is_subjective_plural(self.subject.as_str())
            }
            fn inflect(&self, as_plural: bool, uc: bool) -> String {
                let name = self.name(uc);
                if as_plural == self.is_plural() {
                    name
                } else {
                    if as_plural {
                        // Try irregular plural lookup first
                        if let Some(plural) = ranting::inflect_noun_irregular(&name, true) {
                            ranting::uc_1st_if(&plural, uc)
                        } else {
                            // Fall back to regular plural formation
                            name.strip_suffix(#singular_end)
                                .expect("Ranting implementation error: name is not singular or does not match singular_end attribute").to_string()
                                + #plural_end
                        }
                    } else {
                        // Try irregular singular lookup first
                        if let Some(singular) = ranting::inflect_noun_irregular(&name, false) {
                            ranting::uc_1st_if(&singular, uc)
                        } else {
                            // Fall back to regular singular formation
                            name.strip_suffix(#plural_end)
                                .expect("Ranting implementation error: name is not plural or does not match plural_end attribute").to_string()
                                + #singular_end
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
            fn inflect(&self, as_plural: bool, uc: bool) -> String {
                let name = self.name(uc);
                if as_plural == self.is_plural() {
                    name
                } else {
                    if as_plural {
                        // Try irregular plural lookup first
                        if let Some(plural) = ranting::inflect_noun_irregular(&name, true) {
                            ranting::uc_1st_if(&plural, uc)
                        } else {
                            // Fall back to regular plural formation
                            name.strip_suffix(#singular_end)
                                .expect("singular extension mismatch").to_string()
                                + #plural_end
                        }
                    } else {
                        // Try irregular singular lookup first
                        if let Some(singular) = ranting::inflect_noun_irregular(&name, false) {
                            ranting::uc_1st_if(&singular, uc)
                        } else {
                            // Fall back to regular singular formation
                            name.strip_suffix(#plural_end)
                                .expect("plural extension mismatch").to_string()
                                + #singular_end
                        }
                    }
                }
            }
        }
    }
}

/// An abstract thing, which may be a person and have a gender
pub(crate) fn ranting_q(opt: RantingOptions, is_enum: bool, ident: &Ident) -> TokenStream {
    // TODO: span of attribute?

    let plurality_action: TokenStream = get_plurality_fns(&opt);

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
