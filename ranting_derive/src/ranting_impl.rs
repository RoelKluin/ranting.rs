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
}

/// An abstract thing, which may be a person and have a gender
pub(crate) fn ranting_q(opt: RantingOptions, ident: &Ident) -> TokenStream {
    let is_plural = opt.is_plural.unwrap_or(false);
    quote! {
        impl Ranting for #ident {
            fn subjective(&self) -> &str {
                self.subject.as_str()
            }
            fn is_plural(&self) -> bool {
                ranting::is_subjective_plural(self.subjective()).unwrap_or(#is_plural)
            }
            fn name(&self, uc: bool) -> String {
                let subject = self.subjective();
                match subject {
                    "he" | "she" | "it" | "they" => {
                        ranting::inflect_noun(self.name.as_str(), true, true, uc)
                    },
                    "I" => format!("I, {},", self.name),
                    "you" | "we" => {
                        format!("{}, {},", ranting::subjective(subject, uc), self.name)
                    },
                    p => panic!("Unimplemented: subject for '{}'", p),
                }
            }
            fn requires_article(&self) -> bool {
                true
            }
            fn a_or_an(&self, uc: bool) -> &str {
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
        impl std::fmt::Display for #ident {
            fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "{}", self.name)
            }
        }
    }
}
