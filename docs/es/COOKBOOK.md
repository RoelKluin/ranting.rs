# Recetario de Ranting: 10 recetas prácticas

*Traducción al español de [`docs/COOKBOOK.md`](../COOKBOOK.md). Ranting genera texto en
**inglés**: los ejemplos de código y las salidas mostradas no cambian. Solo se ha traducido el
texto explicativo. Si buscas una implementación en español, consulta
[`ranting_es`](../../ranting_es/README.md).*

Ejemplos del mundo real para generar texto consciente de pronombres. Cada receta incluye un
fragmento de código, una explicación y una referencia a su prueba.

> Todos los ejemplos están respaldados por pruebas compiladas en `tests/ranting/cookbook.rs`, así
> que se verifican en cada `cargo test`.

---

## Receta 1: diálogo de PNJ en un videojuego

**Caso de uso**: PNJ que hablan con naturalidad sobre sus acciones, sin importar su género o
pronombre.

```rust
use ranting::*;

let merchant = Noun::new("Merchant", "he");
let dialogue = say!("{=merchant walk} wares. {=merchant <sell} yesterday.");
// "He walks wares. He sold yesterday."
```

**Por qué importa**: en juegos con personajes diversos, escribir ramas de diálogo para cada
pronombre es tedioso. Ranting te permite escribir una sola vez y soportar todos los pronombres
automáticamente.

**Verificado en**: `tests/ranting/cookbook.rs::recipe_1_game_npc_dialogue`

---

## Receta 2: respuestas de chatbot

**Caso de uso**: un bot reconoce a los usuarios con respuestas gramaticalmente correctas sin
importar sus pronombres.

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

**Por qué importa**: los bots de cara al usuario necesitan adaptarse a pronombres diversos sin
ramificación condicional.

**Verificado en**: `tests/ranting/cookbook.rs::recipe_2_chatbot_singular_plural`

---

## Receta 3: ramificación en ficción interactiva

**Caso de uso**: distintas ramas narrativas en el texto de la historia según el tiempo verbal
(pasado/presente/futuro), todas gramaticalmente correctas.

```rust
use ranting::*;

let protagonist = Noun::new("Hero", "I");

// Rama en pasado: "I discovered a chamber..."
let past = say!("{=protagonist <discover} chamber. {=protagonist =search} inside.");
// "I discovered chamber. I am searching inside."

// Rama en futuro: "I will discover a chamber..."
let future = say!("{=protagonist >discover} chamber. {=protagonist =search} inside.");
// "I will discover chamber. I am searching inside."
```

**Por qué importa**: la ficción interactiva suele ramificarse según el estado del juego (¿encontró
el jugador el tesoro?). Ranting te permite escribir texto consciente del tiempo verbal sin
duplicar la narrativa.

**Verificado en**: `tests/ranting/cookbook.rs::recipe_3_interactive_fiction_branching`

---

## Receta 4: generación de perfiles de usuario

**Caso de uso**: generar automáticamente biografías de perfil gramaticalmente correctas para
cualquier pronombre.

```rust
use ranting::*;

let alice = Noun::new("Alice", "she");
let bob = Noun::new("Bob", "he");
let jordan = Noun::new("Jordan", "they");

say!("{=alice walk}.");     // "She walks."
say!("{=bob walk}.");       // "He walks."
say!("{=jordan walk}.");    // "They walk."
```

**Por qué importa**: las biografías de usuario deben respetar los pronombres de la persona y ser
gramaticalmente correctas en una sola pasada.

**Verificado en**: `tests/ranting/cookbook.rs::recipe_4_user_profile_generation`

---

## Receta 5: manejo de plurales (formas singular/plural)

**Caso de uso**: alternar entre formas singular y plural en descripciones.

```rust
use ranting::*;

let cat = Noun::new("cat", "it");

say!("{=cat walk}");       // "It walks" (singular)
say!("{+=cat walk}");      // "They walk" (plural forzado con +)
```

**Por qué importa**: muchas descripciones necesitan tanto la versión singular como la plural (p.
ej., "1 cat walks" frente a "3 cats walk"). Ranting lo gestiona automáticamente.

**Verificado en**: `tests/ranting/cookbook.rs::recipe_5_plural_handling_singulars`

---

## Receta 6: pronombres neutros en cuanto al género (they singular)

**Caso de uso**: apoyar respetuosamente a las personas que usan el pronombre singular they/them.

```rust
use ranting::*;

let alex = Noun::new("Alex", "they");
say!("{=alex have} voice.");      // "They have voice."
say!("{=alex walk} fast.");       // "They walk fast."
```

**Por qué importa**: el "they" singular está ahora ampliamente aceptado en inglés. Ranting lo
soporta de forma nativa, sin casos especiales.

**Detalle clave**: el "they" singular conjuga en forma plural ("they walk", no "they walks"),
aunque se refiera a una sola persona.

**Verificado en**: `tests/ranting/cookbook.rs::recipe_6_gender_neutral_pronouns`

---

## Receta 7: formas de tiempo verbal

**Caso de uso**: describir acciones en distintos tiempos verbales con los auxiliares correctos.

```rust
use ranting::*;

let friend = Noun::new("Chris", "they");

say!("{=friend walk}");        // "They walk"       (presente)
say!("{=friend <walk}");       // "They walked"     (pasado)
say!("{=friend =walk}");       // "They are walking" (continuo)
```

**Por qué importa**: las narrativas necesitan varios tiempos verbales. Ranting conjuga e inserta
auxiliares automáticamente.

**Verificado en**: `tests/ranting/cookbook.rs::recipe_7_verb_forms_tense`

---

## Receta 8: narrativa con tiempos verbales mixtos

**Caso de uso**: escribir narrativas con varios tiempos verbales que permanezcan gramaticalmente
correctas.

```rust
use ranting::*;

let protagonist = Noun::new("Sam", "she");

let story = say!(
    "{=protagonist <arrive} gates. "
    "{=protagonist =search} treasure. "
    "{=protagonist >find} it."
);
// "She arrived gates. She is searching treasure. She will find it."
```

**Por qué importa**: las historias a menudo mezclan tiempos verbales (describir lo que pasó, lo
que está pasando ahora, lo que pasará). Ranting mantiene todo gramaticalmente correcto.

**Verificado en**: `tests/ranting/cookbook.rs::recipe_8_mixed_tense_narrative`

---

## Receta 9: usar sustantivos con estructuras de datos personalizadas

**Caso de uso**: incrustar pronombres en estructuras de datos más grandes (personajes, usuarios,
objetos).

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

**Por qué importa**: las aplicaciones reales no usan `Noun` sueltos; van incrustados en
Characters, Users, Entities. Ranting convive bien con el sistema de tipos de Rust.

**Verificado en**: `tests/ranting/cookbook.rs::recipe_9_custom_data_with_noun`

---

## Receta 10: claridad con pronombres

**Caso de uso**: usar pronombres con naturalidad manteniendo las referencias claras y sin
ambigüedad.

```rust
use ranting::*;

let alice = Noun::new("Alice", "she");

// Los pronombres funcionan con naturalidad
let text = say!("{=alice walk} fast.");
// "She walks fast."

// Usa el nombre visible (*) para desambiguar cuando haga falta
let text2 = say!("{*alice walk} fast.");
// "Alice walks fast."  (muestra el nombre explícitamente, el verbo sigue concordando)
```

**Por qué importa**: los pronombres hacen que el texto fluya con naturalidad, pero pueden causar
ambigüedad cuando se menciona a varias personas. Ranting te permite alternar entre pronombres y
nombres explícitos sin repetir código.

**Verificado en**: `tests/ranting/cookbook.rs::recipe_10_clarity_with_pronouns`

---

## Patrones comunes

### Recorrer todos los pronombres

¿Quieres probar tu código con todos los pronombres soportados?

```rust
use ranting::*;

let pronouns = vec!["I", "you", "he", "she", "it", "we", "they"];
for pronoun in pronouns {
    let person = Noun::new("person", pronoun);
    println!("{}", say!("{=0 walk}.", person));
}
```

### Tiempo verbal condicional

Guardar marcadores de tiempo en variables (bueno, más o menos):

```rust
use ranting::*;

fn describe(person: Noun, tense_marker: &str) -> String {
    // Nota: las macros no admiten marcadores en tiempo de ejecución directamente.
    // Este patrón funciona a nivel de generación de código, no en tiempo de ejecución.
    match tense_marker {
        "past" => say!("{=0 <walk}", person),
        "future" => say!("{=0 >walk}", person),
        _ => say!("{=0 walk}", person),
    }
}
```

### Manejo de errores con ack!() y nay!()

Devolver éxito/fallo con texto de Ranting:

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

## Resolución de problemas

### "Mi texto no se pone en mayúscula correctamente"

Los placeholders al inicio de frase se ponen en mayúscula automáticamente. Los placeholders a
mitad de frase no. Esto es intencional:

```rust
say!("{=person walk}.");          // Inicia frase: "She walks."
say!("I think {=person walk}.");  // A mitad de frase: "I think she walks."
```

### "Los verbos irregulares no funcionan"

Asegúrate de usar la forma base. Ranting se encarga de la conjugación:

```rust
say!("{=person go}");      // ✓ "He goes" (verbo base)
say!("{=person goes}");    // ✗ "He goeses" (ya conjugado — no hagas esto)
say!("{=person <go}");     // ✓ "He went" (base + marcador de pasado)
```

### "Los artículos no aparecen"

Los artículos solo aparecen si los incluyes explícitamente en el placeholder. Ten en cuenta
también que un placeholder sin marcador de caso (`=`, `@`, `` ` ``, `~`, `*`) muestra el
**nombre** del sustantivo, no su pronombre:

```rust
let person = Noun::new("person", "he");
say!("{person walk}");     // "Person walks"     (sin artículo; muestra el nombre)
say!("{the person walk}"); // "The person walks" (artículo incluido; sigue siendo el nombre)
say!("{=person walk}");    // "He walks"          (= muestra el pronombre en su lugar)
```

---

## ¿Qué sigue?

Lee el [Tutorial](TUTORIAL.md) para profundizar en la sintaxis y los marcadores de tiempo, o
explora la [documentación de la API](https://docs.rs/ranting/) (en inglés) para funcionalidades
avanzadas.

---

**Todos los ejemplos compilan y se ejecutan**: `cargo test --test ranting`

Ver también: [Tutorial en español](TUTORIAL.md) · [Hoja de referencia en español](CHEATSHEET.md)
