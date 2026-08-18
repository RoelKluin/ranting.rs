# Primeros pasos con Ranting

*Traducción al español de [`docs/TUTORIAL.md`](../TUTORIAL.md). Ranting genera texto en
**inglés**: los ejemplos de código no cambian, y las salidas mostradas (`// "She walks"`, etc.)
son cadenas reales en inglés, tal como las produce la librería. Solo se ha traducido el texto
explicativo. Si buscas una implementación en español construida sobre la API pública de Ranting,
consulta el crate [`ranting_es`](../../ranting_es/README.md).*

Una guía práctica para generar texto consciente de pronombres con la macro `say!()`. Tiempo de
lectura estimado: 30-40 minutos.

---

## 1. ¿Qué es Ranting? ¿Por qué `say!()` en lugar de `format!()`?

Ranting resuelve un problema fundamental del formateo de cadenas integrado en Rust: **los
pronombres y los verbos deben concordar entre sí, pero `format!()` no tiene ninguna noción de
gramática.**

Compara:

```rust
// Con format!() — la plantilla está fija para un solo pronombre ("they"), así que
// produce silenciosamente una gramática incorrecta para cualquier otro caso:
format!("{} do say their name is Jordan.", "he")
// Salida: "he do say their name is Jordan." ✗ Forma verbal y posesivo incorrectos

// Con say!() y Ranting — la misma plantilla se adapta al pronombre del sustantivo
use ranting::*;
fn say_this(who: Noun, title: &Noun) -> String {
    say!("{=who do} say {`who title are} {who}.")
}
let title = Noun::new("name", "it");
say_this(Noun::new("Jordan", "he"), &title)
// Salida: "He does say his name is Jordan." ✓ Correcto
say_this(Noun::new("Jordan", "she"), &title)
// Salida: "She does say her name is Jordan." ✓ También correcto, sin cambiar código
say_this(Noun::new("Jordan", "they"), &title)
// Salida: "They do say their name is Jordan." ✓ "They" singular, sigue siendo correcto
```

La macro `say!()` **conjuga verbos y adapta artículos automáticamente según el pronombre**, de
modo que tu texto sea gramaticalmente correcto sin importar de quién se trate.

### Por qué esto importa

- **Diseño inclusivo**: soporta todos los pronombres (he, she, they, I, you, etc.) sin
  ramificaciones condicionales.
- **Menos código repetitivo**: nada de cadenas `if subject == "they" { ... } else { ... }`.
- **Seguro en tipos**: los pronombres se validan en tiempo de compilación mediante el trait
  `Ranting`.
- **De dominio específico**: diseñado específicamente para las reglas gramaticales del inglés, no
  es un formateador de propósito general.

### Cuándo usar Ranting

- **Diálogos de videojuegos**: falas de PNJ, descripciones de objetos, retroalimentación de
  combate.
- **Chatbots**: respuestas que se adaptan a cualquier pronombre/nombre.
- **Texto de cara al usuario**: biografías de perfil, notificaciones, narrativas generadas.

### Cuándo evitarlo

- Texto muy dependiente de plantillas (usa un motor de plantillas en su lugar).
- Idiomas distintos del inglés. `ranting` flexiona en inglés por defecto, pero cada decisión de
  artículo, pronombre, verbo, adjetivo, numeral, preposición y capitalización se puede sobrescribir
  mediante los hooks `_custom` del trait `Ranting` — consulta `docs/EXTENSIBILITY.md` (en inglés),
  y `ranting_i18n`/`ranting_es` para implementaciones completas en alemán y español construidas
  únicamente sobre la API pública. El *orden* de las palabras sigue perteneciendo a tu plantilla:
  una aplicación no inglesa provee una plantilla por idioma.

---

## 2. Tu primera macro `say!()`: pronombres y sujetos

El núcleo de Ranting es la estructura `Noun`, que empareja un nombre con un pronombre:

```rust
use ranting::*;

let jane = Noun::new("Jane", "I");
let tarzan = Noun::new("Tarzan", "he");
let pat = Noun::new("Pat", "they");
let jeffersons = Noun::new("The Jeffersons", "they");
```

### Sintaxis de los placeholders: mostrar pronombres

Los placeholders dentro de `say!()` usan **marcadores de caso** para controlar qué forma del
pronombre se muestra:

| Marcador | Nombre | Ejemplo | Salida |
|--------|------|---------|--------|
| `=` | Sujeto | `{=jane}` | `I` |
| `@` | Objeto | `{@jane}` | `Me` |
| `` ` `` | Determinante posesivo | `{`jane}` | `My` |
| `~` | Pronombre posesivo | `{~jane}` | `Mine` |
| `*` | Mostrar el nombre | `{*jeffersons who have}` | `The Jeffersons who have` |
| `*=`, `*@`, `` *` ``, `*~`, `*%` | Fusionado: marca el caso igual que el marcador simple (un `inflect_article_custom` personalizado ve el `GrammaticalCase` real), pero sigue mostrando el nombre del sustantivo en lugar de cambiar a un pronombre | `{the *=noun}` | el artículo se renderiza con el caso correcto, se muestra el nombre |

Ejemplos verificados de `tests/ranting/tutorial.rs::section_2_first_say_pronouns`:

```rust
say!("{=jane}")        // "I"
say!("{=tarzan}")      // "He"
say!("{=pat}")         // "They"

say!("{`jane}")        // "My"
say!("{`tarzan}")      // "His"
say!("{`pat}")         // "Their"

say!("{*jeffersons who have} a book.")       // "The Jeffersons who have a book."
say!("{=jane}, {*jane}, who have a book.")   // "I, Jane, who have a book."
```

El texto posterior a un sustantivo con `*` (o sin marcador) sigue pasando por la conjugación
verbal — su primera palabra se trata como el verbo (así, `say!("{*tarzan walk}")` da
`"Tarzan walks"`). `who` no es sintaxis especial; aquí permanece invariable solo porque el
pronombre declarado de `jeffersons` (`"they"`) es plural, lo que no cambia la ortografía de la
palabra siguiente. Un sustantivo en tercera persona del singular sí conjugaría (incorrectamente)
un `who` colocado justo después del marcador de caso, por lo que el ejemplo
`{=jane}, {*jane}, who have a book.` coloca `who` en el texto literal de la propia frase, tras dos
placeholders separados, en lugar de dentro del slot verbal de un único sustantivo.

### Argumentos posicionales

Puedes referenciar argumentos por posición:

```rust
let noun = Noun::new("Alice", "she");
say!("{=0}", noun)     // "She"
say!("{@0}", noun)     // "her"
```

O por nombre:

```rust
say!("{=person}", person = noun)   // "She"
```

---

## 3. Marcadores de tiempo verbal: pasado, presente, continuo y futuro

Ranting admite **seis tiempos verbales distintos** mediante marcadores de prefijo en la posición
posterior al sustantivo (`{...verbo}`):

| Marcador | Tiempo | Ejemplo | Salida |
|--------|-------|---------|--------|
| (ninguno) | Presente | `{=kate walk}` | `She walks` |
| `<` | Pasado | `{=kate <walk}` | `She walked` |
| `=` | Presente continuo | `{=kate =run}` | `She is running` |
| `>` | Futuro | `{=kate >paint}` | `She will paint` |
| `<=` | Pasado continuo | `{=kate <=dance}` | `She were dancing` |
| `%` | Presente perfecto | `{=kate %finish}` | `She has finished` |
| `<%` | Pasado perfecto | `{=kate <%leave}` | `She had left` |

### Cómo funcionan los marcadores de tiempo

- **Conjugación en tiempo de compilación**: el crate `ranting_derive` conjuga el verbo base (p.
  ej., "walk" → "walked", "run" → "running").
- **Inserción de auxiliar en tiempo de ejecución**: el crate `ranting` inserta el verbo auxiliar
  correcto ("is", "have", "had", "will").
- **Concordancia automática**: la forma verbal más el auxiliar siempre concuerdan con el pronombre
  sujeto.

Ejemplos verificados de `tests/ranting/tutorial.rs::section_3_*`:

```rust
// Pasado
say!("{=kate <walk}");  // "She walked"

// Continuo (presente o pasado)
say!("{=luis =run}");   // "He is running"
say!("{=alex <=dance}"); // "They were dancing"

// Futuro
say!("{=sophia >paint}");  // "She will paint"

// Perfecto (presente o pasado)
say!("{=morgan %finish}");  // "She has finished"
say!("{=jordan <%leave}");  // "He had left"
```

### Verbos irregulares

Los verbos irregulares (go → went, see → saw, be → been, etc.) se gestionan automáticamente
mediante una tabla integrada de más de 118 formas irregulares. No hace falta ninguna sintaxis
especial:

```rust
say!("{=hero <go} into dungeon.");  // "He went..." (no "go'd")
```

---

## 4. Errores comunes y la sintaxis completa del placeholder

### Mayúscula automática al inicio de frase

Los placeholders al comienzo de una frase ponen en mayúscula su primer carácter automáticamente:

```rust
say!("{=avery walk} quickly.");       // "She walks quickly." (inicio de frase)
say!("When {=avery walk}, she..."); // "When she runs, she..." (a mitad de frase)
```

### Gramática completa del placeholder

Un placeholder completo puede incluir artículos, marcadores de plural y marcadores de caso:

```
{[,^]?(artículo)?([+-]|#var)?(\*[`=@~%]|[`=@~?*])?sustantivo( verbo)}
```

- **Modificadores de caso** (opcionales): `,` (forzar minúscula) o `^` (forzar mayúscula).
- **Artículo** (opcional): `a`, `an`, `some`, `the`, `these`, `those`.
- **Pluralidad** (opcional): `+` (forzar plural), `-` (forzar singular), `$var`/`#var`
  (impulsado por un número).
- **Marcador de caso** (opcional): `` ` `` (determinante posesivo), `=` (sujeto), `@` (objeto), `~`
  (pronombre posesivo), `*` (mostrar nombre), `?` (oculto); las formas fusionadas de dos
  caracteres `*=`/`*@`/`` *` ``/`*~`/`*%` marcan el caso del placeholder igual que el marcador
  simple, pero siguen mostrando el nombre del sustantivo.
- **Sustantivo**: un nombre de variable o un índice posicional.
- **Verbo** (opcional): un verbo base, opcionalmente prefijado con un marcador de tiempo (`<`,
  `=`, `>`, etc.).

### Adaptación de artículos

Los artículos se adaptan automáticamente según la pluralidad del sustantivo — `a`/`an` se
convierte en `some` para un sujeto plural, mientras que `the` permanece igual:

```rust
let dog = Noun::new("dog", "it");
say!("{a dog}")      // "A dog"
say!("{the dog}")    // "The dog"

let dogs = Noun::new("dog", "they");
say!("{a dogs}")     // "Some dog"
say!("{the dogs}")   // "The dog"
```

### Forzar singular/plural

```rust
let person = Noun::new("person", "it");
say!("{-=0 do}", person)   // "It does"   (singular forzado)
say!("{+=0 do}", person)   // "They do"   (plural forzado)
```

---

## 5. Depuración con `--features debug`

Durante el desarrollo, puedes ver cómo se compila la macro `say!()` mediante un indicador de
funcionalidad de depuración:

```bash
cargo test --features debug
```

Esto imprime la expansión en tiempo de compilación de cada placeholder de `say!()` a la llamada
`format!()` que se generará. Úsalo para verificar que la macro genera el código esperado.

Ejemplo (salida teórica):

```
// En tiempo de compilación:
say!("{=kate walk}");

// Se expande aproximadamente a:
format!("{}...", handle_placeholder(kate, ...))
```

Ejemplo verificado de `tests/ranting/tutorial.rs::section_5_debug_feature`:

```rust
let morgan = Noun::new("Morgan", "she");
let result = say!("{=morgan walk}");
assert_eq!(result, "She walks");
```

---

## Próximos pasos

Ahora que entiendes lo básico, explora el **Recetario** (Cookbook) para ver 10 recetas prácticas
sobre diálogos de videojuegos, chatbots, ficción interactiva y más.

### Referencia rápida

- **Pronombres**: I, you, he, she, it, we, ye, they
- **Marcadores de tiempo**: `<` (pasado), `=` (continuo), `>` (futuro), `%` (perfecto), `<=`
  (pasado continuo), `<%` (pasado perfecto)
- **Marcadores de caso**: `=` (sujeto), `@` (objeto), `` ` `` (determinante posesivo), `~`
  (pronombre posesivo)
- **Pluralidad**: `+` (plural), `-` (singular), `$var`/`#var` (impulsado por un número)

### Recursos

- [Ranting en crates.io](https://crates.io/crates/ranting)
- [Documentación de la API en docs.rs](https://docs.rs/ranting/)
- Código fuente: https://github.com/RoelKluin/ranting.rs
- [Recetario en español](COOKBOOK.md) · [Hoja de referencia en español](CHEATSHEET.md)
