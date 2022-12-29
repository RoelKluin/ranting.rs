// (c) RoelKluin 2022 GPL v3

use darling::FromDeriveInput;
use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

#[derive(FromDeriveInput, Default)]
#[darling(default, attributes(ranting))]
pub(crate) struct RantingOptions {
    pub(crate) subject: Option<String>,
    pub(crate) is_plural: Option<bool>,
    pub(crate) is_enum: bool,
}

/// An abstract thing, which may be a person and have a gender
pub(crate) fn ranting_q(opt: RantingOptions, ident: &Ident) -> TokenStream {
    let is_plural = opt.is_plural.unwrap_or(false);
    let name_fn_q = if opt.is_enum {
        quote! {
            fn name(&self, mut uc: bool) -> String {
                let mut lc_spaced = String::new();

                for c in self.to_string().chars() {
                    if c.is_lowercase() {
                        lc_spaced.push(c);
                        uc = false;
                    } else {
                        lc_spaced.push(c);
                    } else if let Some(l) = c.to_lowercase().next() {
                        lc_spaced.push(' ');
                        lc_spaced.push(l);
                    }

                }
                lc_spaced
            }
        }
    } else {
        quote! {
            fn name(&self, uc: bool) -> String {
                ranting::uc_1st_if(self.name.as_str(), uc)
            }
        }
    };
    let dismplay_impl = if opt.is_enum {
        quote! {
            // no-op
        }
    } else {
        quote! {
            impl std::fmt::Display for #ident {
                fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                    write!(formatter, "{}", self.name(false))
                }
            }
        }
    };
    quote! {
        impl Ranting for #ident {
            #name_fn_q
            fn subjective(&self) -> &str {
                self.subject.as_str()
            }
            fn is_plural(&self) -> bool {
                ranting::is_subjective_plural(self.subjective()).unwrap_or(#is_plural)
            }
            fn mut_name(&mut self, _word: &str) -> String {
                self.name(false)
            }
            fn requires_article(&self) -> bool {
                true
            }
            fn indefinite_article(&self, uc: bool) -> &str {
                if self.is_plural() {
                    return if uc { "Some" } else { "some" };
                }
                match ranting::in_definite::get_a_or_an(self.name.as_str()) {
                    "a" if uc => "A",
                    "an" if uc => "An",
                    lc => lc,
                }
            }
        }
        #dismplay_impl
    }
}
