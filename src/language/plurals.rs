// (c) Roel Kluin 2024 GPL v3
//! Irregular noun pluralization support.
//! Generated table from data/irregular_plurals.txt via build.rs.

// Include the generated irregular plurals table at compile time
include!(concat!(env!("OUT_DIR"), "/irregular_plurals_generated.rs"));

/// Look up the plural form of a noun, preserving the case of the original.
/// Returns Some(plural_form) if found in irregular table, None otherwise (fallback to regular rules).
pub(crate) fn get_plural(singular: &str) -> Option<String> {
    IRREGULAR_PLURALS
        .iter()
        .find(|(s, _)| s.eq_ignore_ascii_case(singular))
        .map(|(_, p)| apply_case(singular, p))
}

/// Look up the singular form of a noun, preserving the case of the original.
/// Returns Some(singular_form) if found in irregular table, None otherwise (fallback to regular rules).
pub(crate) fn get_singular(plural: &str) -> Option<String> {
    IRREGULAR_PLURALS
        .iter()
        .find(|(_, p)| p.eq_ignore_ascii_case(plural))
        .map(|(s, _)| apply_case(plural, s))
}

/// Apply the case pattern from original to target string.
/// If original is all uppercase, return target uppercase.
/// If original starts with uppercase, return target with first char uppercase.
/// Otherwise return target lowercase.
fn apply_case(original: &str, target: &str) -> String {
    if original.chars().all(|c| !c.is_alphabetic() || c.is_uppercase()) {
        // All uppercase or no letters
        target.to_uppercase()
    } else if original.chars().next().map_or(false, |c| c.is_uppercase()) {
        // First character is uppercase
        let mut chars = target.chars();
        match chars.next() {
            None => String::new(),
            Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        }
    } else {
        // Lowercase
        target.to_lowercase()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_irregular_plurals_lookup() {
        assert_eq!(get_plural("child"), Some("children".to_string()));
        assert_eq!(get_plural("person"), Some("people".to_string()));
        assert_eq!(get_plural("mouse"), Some("mice".to_string()));
    }

    #[test]
    fn test_irregular_singulars_lookup() {
        assert_eq!(get_singular("children"), Some("child".to_string()));
        assert_eq!(get_singular("people"), Some("person".to_string()));
        assert_eq!(get_singular("mice"), Some("mouse".to_string()));
    }

    #[test]
    fn test_not_found_returns_none() {
        assert_eq!(get_plural("dog"), None);
        assert_eq!(get_singular("dogs"), None);
    }

    #[test]
    fn test_case_preservation_uppercase() {
        assert_eq!(get_plural("CHILD"), Some("CHILDREN".to_string()));
    }

    #[test]
    fn test_case_preservation_title() {
        assert_eq!(get_plural("Child"), Some("Children".to_string()));
    }
}
