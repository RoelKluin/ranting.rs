# Ranting Cookbook: 10 Practical Recipes

*[🇪🇸 Leer en español](es/COOKBOOK.md)*

Real-world examples for pronoun-aware text generation. Each recipe includes a code snippet, explanation, and test reference.

> All examples are backed by compiled tests in `tests/ranting/cookbook.rs`, so they'll be checked on every `cargo test`.

---

## Recipe 1: Game NPC Dialogue

**Use case**: NPCs that speak naturally about their actions, regardless of gender or pronoun.

```rust
use ranting::*;

let merchant = Noun::new("Merchant", "he");
let dialogue = say!("{=merchant walk} wares. {=merchant <sell} yesterday.");
// "He walks wares. He sold yesterday."
```

**Why it matters**: In games with diverse characters, writing dialogue branches for each pronoun is tedious. Ranting lets you write once and support all pronouns automatically.

**Tested**: `tests/ranting/cookbook.rs::recipe_1_game_npc_dialogue`

---

## Recipe 2: Chatbot Responses

**Use case**: A bot acknowledges users with grammatically correct responses regardless of their pronouns.

```rust
use ranting::*;

fn bot_acknowledge(who: Noun) -> String {
    say!("{=who have} registered!")
}

let singular = Noun::new("User", "you");
bot_acknowledge(singular)  // "You have registered!"

let plural = Noun::new("team", "they");
bot_acknowledge(plural)    // "They have registered!"
```

**Why it matters**: User-facing bots need to adapt to diverse pronouns without conditional branching.

**Tested**: `tests/ranting/cookbook.rs::recipe_2_chatbot_singular_plural`

---

## Recipe 3: Interactive Fiction Branching

**Use case**: Different narrative branches in story text based on past/present/future tense, all grammatically correct.

```rust
use ranting::*;

let protagonist = Noun::new("Hero", "I");

// Past branch: "I discovered a chamber..."
let past = say!("{=protagonist <discover} a chamber. {=protagonist =search} inside.");
// "I discovered a chamber. I am searching inside."

// Future branch: "I will discover a chamber..."
let future = say!("{=protagonist >discover} a chamber. {=protagonist =search} inside.");
// "I will discover a chamber. I am searching inside."
```

**Why it matters**: Interactive fiction often branches on game state (did the player find the treasure?). Ranting lets you write tense-aware text without duplicating the narrative.

**Tested**: `tests/ranting/cookbook.rs::recipe_3_interactive_fiction_branching`

---

## Recipe 4: User Profile Generation

**Use case**: Auto-generate profile bios that are grammatically correct for any pronoun.

```rust
use ranting::*;

let alice = Noun::new("Alice", "she");
let bob = Noun::new("Bob", "he");
let jordan = Noun::new("Jordan", "they");

say!("{=alice walk}.");     // "She walks."
say!("{=bob walk}.");       // "He walks."
say!("{=jordan walk}.");    // "They walk."
```

**Why it matters**: User bios should respect the person's pronouns and be grammatically correct in one pass.

**Tested**: `tests/ranting/cookbook.rs::recipe_4_user_profile_generation`

---

## Recipe 5: Plural Handling (Singular/Plural Forms)

**Use case**: Toggle between singular and plural forms in descriptions.

```rust
use ranting::*;

let cat = Noun::new("cat", "it");

say!("{=cat walk}");       // "It walks" (singular)
say!("{+=cat walk}");      // "They walk" (plural forced with +)
```

**Why it matters**: Many descriptions need both singular and plural versions (e.g., "1 cat walks" vs "3 cats walk"). Ranting handles this automatically.

**Tested**: `tests/ranting/cookbook.rs::recipe_5_plural_handling_singulars`

---

## Recipe 6: Gender-Neutral Pronouns (Singular They)

**Use case**: Respectfully support people who use singular they/them pronouns.

```rust
use ranting::*;

let alex = Noun::new("Alex", "they");
say!("{=alex have} a voice.");    // "They have a voice."
say!("{=alex walk} fast.");       // "They walk fast."
```

**Why it matters**: Singular "they" is now widely accepted in English. Ranting supports it natively without special cases.

**Key insight**: Singular "they" conjugates as plural in form ("they walk", not "they walks"), even though it refers to one person.

**Tested**: `tests/ranting/cookbook.rs::recipe_6_gender_neutral_pronouns`

---

## Recipe 7: Verb Tense Forms

**Use case**: Describe actions in different tenses with correct auxiliary verbs.

```rust
use ranting::*;

let friend = Noun::new("Chris", "they");

say!("{=friend walk}");        // "They walk"       (present)
say!("{=friend <walk}");       // "They walked"     (past)
say!("{=friend =walk}");       // "They are walking" (continuous)
```

**Why it matters**: Narratives need multiple tenses. Ranting conjugates and inserts auxiliaries automatically.

**Tested**: `tests/ranting/cookbook.rs::recipe_7_verb_forms_tense`

---

## Recipe 8: Mixed Tense Narrative

**Use case**: Write multi-tense narratives that remain grammatically correct.

```rust
use ranting::*;

let protagonist = Noun::new("Sam", "she");

let story = say!(
    "{=protagonist <arrive} at the gates. "
    "{=protagonist =search} treasure. "
    "{=protagonist >find} it."
);
// "She arrived at the gates. She is searching treasure. She will find it."
```

**Why it matters**: Stories often mix tenses (describe what happened, what's happening now, what will happen). Ranting keeps everything grammatical.

**Tested**: `tests/ranting/cookbook.rs::recipe_8_mixed_tense_narrative`

---

## Recipe 9: Using Nouns with Custom Data Structures

**Use case**: Embed pronouns in larger data structures (characters, users, items).

```rust
use ranting::*;

struct Character {
    noun: Noun,
}

let merlin = Character {
    noun: Noun::new("Merlin", "he"),
};

let text = say!("{=0 walk} slowly.", merlin.noun);
// "He walks slowly."
```

**Why it matters**: Real apps don't use bare Nouns; they're embedded in Characters, Users, Entities. Ranting plays well with Rust's type system.

**Tested**: `tests/ranting/cookbook.rs::recipe_9_custom_data_with_noun`

---

## Recipe 10: Clarity with Pronouns

**Use case**: Use pronouns naturally while keeping references clear and unambiguous.

```rust
use ranting::*;

let alice = Noun::new("Alice", "she");

// Pronouns work naturally
let text = say!("{=alice walk} fast.");
// "She walks fast."

// Use the display name (*) to disambiguate when needed
let text2 = say!("{*alice walk} fast.");
// "Alice walks fast."  (shows name explicitly, verb still agrees)
```

**Why it matters**: Pronouns make text flow naturally, but can cause ambiguity when multiple people are mentioned. Ranting lets you switch between pronouns and explicit names without repeating code.

**Tested**: `tests/ranting/cookbook.rs::recipe_10_clarity_with_pronouns`

---

## Common Patterns

### Looping over pronouns

Want to test your code with all supported pronouns?

```rust
use ranting::*;

let pronouns = vec!["I", "you", "he", "she", "it", "we", "they"];
for pronoun in pronouns {
    let person = Noun::new("person", pronoun);
    println!("{}", say!("{=0 walk}.", person));
}
```

### Conditional tense

Store tense markers in variables (well, sort of):

```rust
use ranting::*;

fn describe(person: Noun, tense_marker: &str) -> String {
    // Note: macros don't support runtime markers directly.
    // This pattern works at code-generation level, not runtime.
    match tense_marker {
        "past" => say!("{=0 <walk}", person),
        "future" => say!("{=0 >walk}", person),
        _ => say!("{=0 walk}", person),
    }
}
```

### Error handling with ack!() and nay!()

Return success/failure with Ranting text:

```rust
use ranting::*;
use ranting_derive::{ack, nay};

fn register_user(person: Noun) -> Result<String, String> {
    if person.name.is_empty() {
        nay!("{=person} can't register without a name.")
    } else {
        ack!("{=person} registered successfully!")
    }
}
```

---

## Troubleshooting

### "My text isn't capitalizing correctly"

Placeholders at sentence start auto-capitalize. Mid-sentence placeholders don't. This is intentional:

```rust
say!("{=person walk}.");          // Starts sentence: "She walks."
say!("I think {=person walk}.");  // Mid-sentence: "I think she walks."
```

### "Irregular verbs aren't working"

Make sure you're using the base form. Ranting handles conjugation:

```rust
say!("{=person go}");      // ✓ "He goes" (base verb)
say!("{=person goes}");    // ✗ "He goeses" (already conjugated—don't do this)
say!("{=person <go}");     // ✓ "He went" (base + past marker)
```

### "Articles aren't showing"

Articles only appear if you explicitly include them in the placeholder. Also note that a
placeholder with no case marker (`=`, `@`, `` ` ``, `~`, `*`) displays the noun's **name**,
not its pronoun:

```rust
let person = Noun::new("person", "he");
say!("{person walk}");     // "Person walks"     (no article; shows the name)
say!("{the person walk}"); // "The person walks" (article included; still the name)
say!("{=person walk}");    // "He walks"          (= shows the pronoun instead)
```

---

## What's Next?

Read the [Tutorial](TUTORIAL.md) for a deeper dive into syntax and tense markers, see [Advanced Examples](ADVANCED.md) for composed, multi-feature passages, or explore the [API docs](https://docs.rs/ranting/) for the full public surface.

---

**All examples compile and run**: `cargo test --test ranting`
