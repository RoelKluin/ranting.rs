//! The grammar tables: a closed noun vocabulary with numeral classifiers, and verb forms across
//! the plain/polite register split.
//!
//! Nothing here touches `ranting` — [`crate::noun`] is the only file that does.

/// One noun from the closed vocabulary.
///
/// There is no plural column. Japanese nouns do not inflect for number at all, which is why
/// `Ranting::inflect` is an identity function in this crate — see the README.
pub struct NounEntry {
    pub word: &'static str,
    /// The numeral classifier (counter) this noun takes: 匹 for small animals, 人 for people,
    /// 本 for long thin things. **Read off the noun, not off `NounClass`** — see the README.
    pub counter: Counter,
    /// Whether this referent is honored, which selects sonkeigo (respectful) verb forms at
    /// [`ranting::Register::Formal`]. Entity-carried, because whether to honor someone is a
    /// property of who they are, not of how formal the scene is.
    pub honorific: bool,
}

/// The three counters this vocabulary uses. Each is its own irregular table rather than a
/// suffix: `一匹` is *ippiki*, `三匹` is *sanbiki*, `一人` is *hitori*.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Counter {
    /// 匹 — small animals.
    Hiki,
    /// 人 — people.
    Nin,
    /// 本 — long thin objects.
    Hon,
}

impl Counter {
    /// The counted form for `n`, or `None` past the end of this closed table.
    ///
    /// The sound changes are the point: nothing about `一匹`/`二匹`/`三匹` is derivable by
    /// suffixing a numeral to a counter, so this is a table exactly as an irregular plural is.
    pub fn count(self, n: i64) -> Option<&'static str> {
        let row: [&'static str; 6] = match self {
            Counter::Hiki => ["〇匹", "一匹", "二匹", "三匹", "四匹", "五匹"],
            Counter::Nin => ["〇人", "一人", "二人", "三人", "四人", "五人"],
            Counter::Hon => ["〇本", "一本", "二本", "三本", "四本", "五本"],
        };
        usize::try_from(n).ok().and_then(|i| row.get(i)).copied()
    }
}

/// 猫 "cat" — counted with 匹.
pub const NEKO: NounEntry = NounEntry {
    word: "猫",
    counter: Counter::Hiki,
    honorific: false,
};

/// 人 "person" — counted with 人.
pub const HITO: NounEntry = NounEntry {
    word: "人",
    counter: Counter::Nin,
    honorific: false,
};

/// 本 "book" — counted with 本, which is also its own word.
pub const HON: NounEntry = NounEntry {
    word: "本",
    counter: Counter::Hon,
    honorific: false,
};

/// 先生 "teacher" — counted with 人, and **honored**, so verbs about them take sonkeigo forms
/// when the register is formal.
pub const SENSEI: NounEntry = NounEntry {
    word: "先生",
    counter: Counter::Nin,
    honorific: true,
};

/// How polite the verb form should be. Mapped from [`ranting::Register`] by
/// [`crate::noun::JapaneseNoun`]; kept as its own type so the lexicon does not depend on
/// `ranting`.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Politeness {
    /// 常体 — plain form (だ, 食べる). The default when no register is set, so `say!()` and a
    /// context-free `say_with!()` agree.
    Plain,
    /// 丁寧語 — teineigo, the polite form (です, 食べます).
    Polite,
}

/// Conjugate one of the four verbs in this vocabulary.
///
/// `honored` selects **sonkeigo** — respectful *substitution* of a different verb (食べる →
/// 召し上がる), not a suffix — which is why it is a lexical lookup keyed by verb, politeness and
/// who the subject is, all three of which reach `inflect_verb_custom_with_context`.
pub fn conjugate(verb: &str, politeness: Politeness, honored: bool) -> Option<&'static str> {
    let (plain, polite, sonkeigo_plain, sonkeigo_polite) = match verb {
        // The copula. English writes `are`/`is`; both map to the same Japanese form.
        "are" | "is" | "be" => ("だ", "です", "だ", "です"),
        "eat" | "食べる" => ("食べる", "食べます", "召し上がる", "召し上がります"),
        "go" | "行く" => ("行く", "行きます", "いらっしゃる", "いらっしゃいます"),
        "see" | "見る" => ("見る", "見ます", "ご覧になる", "ご覧になります"),
        _ => return None,
    };
    Some(match (politeness, honored) {
        (Politeness::Plain, false) => plain,
        (Politeness::Polite, false) => polite,
        (Politeness::Plain, true) => sonkeigo_plain,
        (Politeness::Polite, true) => sonkeigo_polite,
    })
}
