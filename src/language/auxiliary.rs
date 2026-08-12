// (c) Roel Kluin 2024 GPL v3
//! Auxiliary verb conjugation for tense expressions.
//! Handles insertion and conjugation of helping verbs (will, is/are, was/were, have, had).

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AuxiliaryVerb {
    Will,      // Future tense marker: `>walk` → "will walk"
    IsAre,     // Continuous present: `=walk` → "is walking"
    WasWere,   // Continuous past: `<=walk` → "was walking"
}

/// Return the correctly conjugated auxiliary verb for the given subject pronoun.
pub(crate) fn conjugate_auxiliary(aux: AuxiliaryVerb, subject: &str) -> &'static str {
    let subject_lower = subject.to_lowercase();
    match (aux, subject_lower.as_str()) {
        (AuxiliaryVerb::Will, _) => "will",  // Same for all persons
        (AuxiliaryVerb::IsAre, "i") => "am",
        (AuxiliaryVerb::IsAre, "you" | "we" | "they") => "are",
        (AuxiliaryVerb::IsAre, "he" | "she" | "it") => "is",
        (AuxiliaryVerb::IsAre, _) => "are",  // Default for unrecognized pronouns
        (AuxiliaryVerb::WasWere, "i" | "he" | "she" | "it") => "was",
        (AuxiliaryVerb::WasWere, "you" | "we" | "they") => "were",
        (AuxiliaryVerb::WasWere, _) => "were",  // Default for unrecognized pronouns
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn will_same_for_all_persons() {
        let pronouns = vec!["I", "you", "he", "she", "it", "we", "they"];
        for pronoun in pronouns {
            assert_eq!(conjugate_auxiliary(AuxiliaryVerb::Will, pronoun), "will");
        }
    }

    #[test]
    fn is_are_conjugation() {
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::IsAre, "I"), "am");
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::IsAre, "you"), "are");
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::IsAre, "he"), "is");
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::IsAre, "she"), "is");
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::IsAre, "it"), "is");
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::IsAre, "we"), "are");
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::IsAre, "they"), "are");
    }

    #[test]
    fn was_were_conjugation() {
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::WasWere, "I"), "was");
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::WasWere, "you"), "were");
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::WasWere, "he"), "was");
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::WasWere, "she"), "was");
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::WasWere, "it"), "was");
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::WasWere, "we"), "were");
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::WasWere, "they"), "were");
    }

    #[test]
    fn case_insensitive_subject() {
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::IsAre, "HE"), "is");
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::WasWere, "THEY"), "were");
        assert_eq!(conjugate_auxiliary(AuxiliaryVerb::Will, "She"), "will");
    }
}
