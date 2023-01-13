// (c) RoelKluin 2022 GPL v3

use darling::FromDeriveInput;
use proc_macro2::TokenStream;
use syn::{parse_quote, Ident};

use std::str::FromStr;

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
    // indicate whether the name should always start with an upper case
    pub(crate) lc: bool,
}

fn get_namefn_for(opt: RantingOptions, is_enum: bool) -> TokenStream {
    let get_name: TokenStream = if is_enum {
        parse_quote!(self)
    } else if let Some(name) = opt.name {
        match name.is_empty() {
            true => {
                return parse_quote! {
                    fn name(&self, uc: bool) -> String {
                        self.name.to_owned()
                    }
                }
            }
            false => parse_quote!(#name),
        }
    } else {
        parse_quote!(std::any::type_name::<Self>().rsplit("::").next().unwrap())
    };
    let first_char_uc: TokenStream;
    let first_char_lc: TokenStream;
    if opt.lc {
        first_char_lc = parse_quote!(chrs.next());
        first_char_uc = parse_quote!(chrs.next().map(|c| c.to_uppercase().next().unwrap_or(c)));
    } else {
        first_char_lc = parse_quote!(chrs.next().map(|c| c.to_lowercase().next().unwrap_or(c)));
        first_char_uc = parse_quote!(chrs.next());
    }
    parse_quote! {
        fn name(&self, uc: bool) -> String {
            let mut chrs = #get_name.chars();
            let oulc = if uc {#first_char_uc} else {#first_char_lc};
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

fn get_plurality_fns(opt: &RantingOptions, subject_str: &str) -> TokenStream {
    let singular_end = opt.singular_end.to_owned();
    let plural_end = opt.plural_end.to_owned();
    if subject_str.is_empty() {
        parse_quote! {
            fn subjective(&self) -> ranting::SubjectPronoun {
                self.subject
            }
            fn is_plural(&self) -> bool {
                ranting::is_subjective_plural(self.subjective())
            }
            fn inflect_noun(&self, as_plural: bool, uc: bool) -> String {
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
        let subject =
            crate::language::SubjectPronoun::from_str(subject_str).expect("not a subject");

        let is_plural = opt
            .is_plural
            .unwrap_or_else(|| crate::language::is_subjective_plural(subject));

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
                fn inflect_noun(&self, as_plural: bool, uc: bool) -> String {
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
                fn inflect_noun(&self, as_plural: bool, uc: bool) -> String {
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
    let subject = opt.subject.to_owned();

    let plurality_action: TokenStream = get_plurality_fns(&opt, subject.as_str());
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
        fn mut_name(&mut self, _word: &str) -> String {
            self.name(false)
        }
        fn requires_article(&self) -> bool {
            true
        }
        fn indefinite_article(&self, uc: bool) -> String {
            if self.is_plural() {
                return if uc { "Some" } else { "some" }.to_string();
            }
            let name = self.name(false);
            match ranting::in_definite::get_a_or_an(name.as_str()) {
                "a" if uc => "A".to_string(),
                "an" if uc => "An".to_string(),
                lc => lc.to_string(),
            }
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
