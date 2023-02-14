// (c) RoelKluin 2022 GPL v3

use crate::language::english_shared as language;
use darling::FromDeriveInput;
use proc_macro2::TokenStream;
use std::str::FromStr;
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
    // If empty, the struct must contain the subject: SubjectPronoun.
    #[darling(default = "string_it")]
    pub(crate) subject: String,
    // defaults to struct / enum variant name. If empty, the struct must contain a name: String.
    pub(crate) name: Option<String>,
    // pluralize: (singular extension to remove, plural ext to add). Singularize with the inverse.
    pub(crate) singular_end: String,
    #[darling(default = "string_s")]
    pub(crate) plural_end: String,
    // whether the subject form is plural - if subject is 'you'; default false.
    pub(crate) is_plural: Option<bool>,
    // set if name should always be with an upper case. Not required if name is given or in struct.
    pub(crate) uc: bool,
    // set if no article should be displayed in most cases (e.g. names, sports or meals)
    pub(crate) no_article: bool,
}

fn get_namefn_for(mut opt: RantingOptions, is_enum: bool) -> TokenStream {
    let get_name: TokenStream = if is_enum {
        parse_quote!(self.to_string())
    } else if let Some(name) = opt.name {
        match name.as_str() {
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
        parse_quote!(std::any::type_name::<Self>().rsplit("::").next().unwrap())
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
            let mut chrs = #get_name.chars();
            let oulc = #first_char;
            let mut lc_spaced = String::from(oulc.expect("name is empty?"));
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

fn get_plurality_fns(
    subject_str: &str,
    singular_end: TokenStream,
    plural_end: TokenStream,
    is_plural: bool,
) -> TokenStream {
    if subject_str == "$" {
        parse_quote! {
            fn subjective(&self) -> ranting::SubjectPronoun {
                self.subject
            }
            fn is_plural(&self) -> bool {
                ranting::is_subjective_plural(self.subjective())
            }
            fn inflect(&self, as_plural: bool, uc: bool) -> String {
                let mut name = self.name(uc);
                if as_plural == self.is_plural() {
                    name
                } else if as_plural {
                    name.strip_suffix(#singular_end).expect("singular extension mismatch").to_string() + #plural_end
                } else {
                    name.strip_suffix(#plural_end).expect("plural extension mismatch").to_string() + #singular_end
                }
            }
        }
    } else {
        let shared_plural_fns: TokenStream = parse_quote! {
            fn is_plural(&self) -> bool { #is_plural }
            fn subjective(&self) -> ranting::SubjectPronoun {
                use std::str::FromStr;
                ranting::SubjectPronoun::from_str(#subject_str).unwrap()
            }
        };

        if is_plural {
            // action: SayAction, to have a single function that returns strings?
            parse_quote! {
                #shared_plural_fns
                fn inflect(&self, as_plural: bool, uc: bool) -> String {
                    let mut name = self.name(uc);
                    if as_plural {
                        name
                    } else  {
                        name.strip_suffix(#plural_end).expect("plural extension mismatch").to_string() + #singular_end
                    }
                }
            }
        } else {
            parse_quote! {
                #shared_plural_fns
                fn inflect(&self, as_plural: bool, uc: bool) -> String {
                    let mut name = self.name(uc);
                    if as_plural {
                        name.strip_suffix(#singular_end).expect("singular extension mismatch").to_string() + #plural_end
                    } else {
                        name
                    }
                }
            }
        }
    }
}

/// An abstract thing, which may be a person and have a gender
pub(crate) fn ranting_q(opt: RantingOptions, is_enum: bool, ident: &Ident) -> TokenStream {
    // TODO: span of attribute?
    let subject_string = opt.subject.to_owned();
    let subject_str = subject_string.as_str();
    let singular_end: TokenStream = match opt.singular_end.as_str() {
        "$" => parse_quote!(self.singular_end.as_str()),
        n => parse_quote!(#n),
    };
    let plural_end: TokenStream = match opt.plural_end.as_str() {
        "$" => parse_quote!(self.plural_end.as_str()),
        n => parse_quote!(#n),
    };
    let plurality_action: TokenStream = if subject_str == "$" {
        get_plurality_fns(subject_str, singular_end, plural_end, false)
    } else {
        let subject = language::SubjectPronoun::from_str(subject_str).expect("not a subject");

        let is_pl = opt
            .is_plural
            .unwrap_or_else(|| language::is_subjective_plural(subject));
        get_plurality_fns(subject_str, singular_end, plural_end, is_pl)
    };

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
