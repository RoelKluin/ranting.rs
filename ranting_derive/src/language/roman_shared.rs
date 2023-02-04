// (c) Roel Kluin 2022 GPL v3

use std::fmt;

/// A word that may be uppercased and has a base and separate extension. (e.g. plural for a verb)
pub struct ExtCased<'a> {
    pub(super) s: &'a str,
    pub(super) uc: bool,
    pub(super) ext: &'a str,
}

impl<'a> fmt::Display for ExtCased<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let cased = Cased {
            s: self.s,
            uc: self.uc,
        };
        write!(f, "{}{}", cased, self.ext)
    }
}

/// A word that may be uppercased - without a string copy.
pub struct Cased<'a> {
    pub(super) s: &'a str,
    pub(super) uc: bool,
}

impl<'a> fmt::Display for Cased<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.uc {
            let mut it = self.s.chars();
            let fst = it.next().ok_or(fmt::Error)?;
            if fst.is_ascii_punctuation() {
                // in case of a contraction, lowercase the next character.
                let snd = it.next().ok_or(fmt::Error)?;
                let offs = fst.len_utf8() + snd.len_utf8();
                let etc = self.s.get(offs..).unwrap_or("");
                write!(f, "{fst}{}{etc}", snd.to_uppercase())
            } else {
                let etc = self.s.get(fst.len_utf8()..).unwrap_or("");
                write!(f, "{}{etc}", fst.to_uppercase())
            }
        } else {
            write!(f, "{}", self.s)
        }
    }
}

/// upper cases first character if uc is true, or second in a contraction.
pub fn uc_1st_if(s: &str, uc: bool) -> String {
    if uc {
        let mut c = s.chars();
        c.next()
            .map(|t| match t {
                '\'' => {
                    t.to_string()
                        + &c.next()
                            .map(|c| c.to_uppercase().collect::<String>())
                            .unwrap_or_default()
                }
                _ => t.to_uppercase().collect::<String>(),
            })
            .unwrap_or_default()
            + c.as_str()
    } else {
        s.to_string()
    }
}
