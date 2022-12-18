// (c) RoelKluin 2022 GPL v3

use darling::FromDeriveInput;
use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

#[derive(FromDeriveInput, Default)]
#[darling(default, attributes(object))]
pub(crate) struct RantingOptions {
    pub(crate) subject: Option<String>,
    pub(crate) is_plural_you: Option<bool>,
}

/// An abstract thing, which may be a person and have a gender
pub(crate) fn ranting_q(opt: RantingOptions, ident: &Ident) -> TokenStream {
    let subject = opt.subject.unwrap_or("it".to_string());
    let is_plural_you = opt.is_plural_you.unwrap_or(false);
    quote! {
        impl Ranting for #ident {
            fn subjective(&self) -> &str {
                #subject
            }
            fn is_plural(&self) -> bool {
                ranting::is_subjective_plural(#subject).unwrap_or(#is_plural_you)
            }
            fn name(&self, uc: bool) -> String {
                match #subject {
                    "he" | "she" | "it" | "they" => {
                        if uc {
                            ranting::inflector::cases::sentencecase::to_sentence_case(self.name.as_str())
                        } else {
                            self.name.to_string()
                        }
                    },
                    "I" => format!("I, {},", self.name),
                    "you" | "we" => {
                        format!("{}, {},", ranting::subjective(#subject, uc), self.name)
                    },
                    p => panic!("Unimplemented: subject for '{}'", p),
                }
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
