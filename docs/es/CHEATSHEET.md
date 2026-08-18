# Hoja de referencia de Ranting

*Traducción al español de [`docs/CHEATSHEET.md`](../CHEATSHEET.md). Ranting genera texto en
**inglés**: los ejemplos de código y las salidas mostradas no cambian. Solo se ha traducido el
texto explicativo. Si buscas una implementación en español, consulta
[`ranting_es`](../../ranting_es/README.md).*

Una referencia rápida de una sola página para la sintaxis de placeholders de `say!()` y las
macros/tipos públicos del crate. Para una introducción guiada consulta el
[Tutorial](TUTORIAL.md); para ejemplos elaborados consulta el [Recetario](COOKBOOK.md); para la
API pública completa consulta [API.md](../API.md) (en inglés) o
[docs.rs](https://docs.rs/ranting).

Cada ejemplo de esta página se ejecutó contra el código actual — las salidas están copiadas de
llamadas reales a `say!()`, no escritas a mano.

## Anatomía del placeholder

```
{[,^]?(artículo)?([+-]|[$#]?var)?(\*[`=@~%]|[`=@~%*?])?sustantivo( verbo)( !palabra | !!palabra)}
```

- **Modificador de caso** (opcional, primero): `,` fuerza minúscula, `^` fuerza mayúscula,
  sobrescribiendo el comportamiento por defecto de inicio de frase.
- **Artículo** (opcional): `a`, `an`, `some`, `the`, `these`, `those`.
- **Pluralidad** (opcional): `+` fuerza plural, `-` fuerza singular, `$var`/`#var` impulsado por
  un número.
- **Marcador de caso** (opcional): qué forma gramatical del sustantivo renderizar — ver la tabla
  siguiente. Sin ningún marcador se renderiza el **nombre** del sustantivo, no un pronombre. Las
  formas fusionadas de dos caracteres (`*=`, `*@`, `` *` ``, `*~`, `*%`) marcan el caso del
  placeholder exactamente igual que el marcador simple, pero siguen renderizando el **nombre**
  del sustantivo en lugar de cambiar a un pronombre.
- **Sustantivo**: un nombre de variable o un índice posicional (`0`, `1`, ...).
- **Verbo** (opcional): un verbo base, opcionalmente prefijado con un marcador de tiempo.
- **Grado** (opcional, tras una palabra posterior al sustantivo): `!palabra` comparativo,
  `!!palabra` superlativo.

## Marcadores de caso

Sin ningún marcador se muestra el **nombre** del sustantivo, no un pronombre — esta es la
sorpresa más común, ver la primera fila.

| Marcador | Significado | Ejemplo | Salida |
|---|---|---|---|
| *(ninguno)* | Nombre (por defecto) | `say!("{person walk}")` con `Noun::new("person","he")` | `"Person walks"` |
| `=` | Sujeto | `say!("{=person walk}")` (mismo sustantivo) | `"He walks"` |
| `@` | Objeto | `say!("{@0}", noun)` | `"her"` |
| `` ` `` | Determinante posesivo | ``say!("{`jane}")`` con `Noun::new("Jane","I")` | `"My"` |
| `~` | Pronombre posesivo | ``say!("{~tarzan}")`` | `"His"` |
| `%` | Reflexivo | `say!("{%alex} can decide that.")` con `alex = Noun::new("Alex","she")` | `"Herself can decide that."` |
| `*` | Mostrar nombre, el verbo sigue concordando | `say!("{*tarzan who have} book")` | `"Tarzan who has book"` |
| `*=`, `*@`, `` *` ``, `*~`, `*%` | Fusionado: marca el caso del placeholder como el marcador simple (así `inflect_article_custom` ve el `GrammaticalCase` real), pero sigue mostrando el **nombre** del sustantivo en lugar de cambiar a un pronombre | `say!("{the *=noun}")` | el artículo se renderiza con el caso correcto, se muestra el nombre |
| `?` | Oculto — flexiona pero no se muestra | `say!("There {are no ?$n item}.")` con `n = 0i64` y un `Noun::new("item","it")` | `"There are no items."` |

`?` se combina con otros marcadores, p. ej. `{?the noun}` oculta el sustantivo (y su artículo)
mientras sigue impulsando la concordancia en el resto de la frase — ver el ejemplo de
`no_article` en `README.md`.

## Artículos

`a`/`an`/`some` se adaptan a la pluralidad del sustantivo; `the` es invariable; `these`/`those`
se convierten en `this`/`that` para un sustantivo singular.

| Placeholder | Sustantivo | Salida |
|---|---|---|
| `{a dog}` | `Noun::new("dog","it")` | `"A dog"` |
| `{the dog}` | igual | `"The dog"` |
| `{a dogs}` | `Noun::new("dog","they")` | `"Some dog"` |
| `{the dogs}` | igual | `"The dog"` |
| `{these dog}` | `Noun::new("dog","it")` (singular) | `"This dog"` |
| `{those dog}` | igual | `"That dog"` |
| `{these 0}` | `Noun::new("cat","they")` (plural) | `"These cat"` |
| `{those 0}` | igual | `"Those cat"` |

Nota que el artículo se adapta a la pluralidad por sí solo, pero el nombre propio del sustantivo
no — `{a dogs}` da `"Some dog"`, no `"Some dogs"`; añade `+` (ver abajo) para pluralizar también
el nombre.

## Forzar pluralidad

| Placeholder | Ejemplo | Salida |
|---|---|---|
| `{+0}` | `say!("{+0}", book)` con `book = Noun::new("book","it")` | `"Books"` |
| `{-=0 do}` | `say!("{-=0 do}", person)` con `person` declarado en plural | `"It does"` |
| `{+=0 do}` | mismo `person` | `"They do"` |
| `{$count noun}` | `say!("I see {$count apple}.", count = 3, apple = ...)` | `"I see 3 apples."` |
| `{#count noun}` | igual, pero el número se escribe con letras | `"I see three apples."` |
| `{#count ?noun}` | se muestra la palabra del número, se oculta el sustantivo | `"I count three."` |
| `{$one noun}` | `count = 1` | `"I see 1 apple."` |

## Modificadores de forzado de caso (`,` / `^`)

Solo son necesarios a mitad de frase — un placeholder al inicio de frase pone mayúscula
automáticamente por defecto.

| Placeholder | Contexto | Salida |
|---|---|---|
| `{,+0}` | `"The person is actually {,+0}."` (plural irregular, minúscula forzada) | `"The person is actually people."` |
| `{^%alex}` | `"Only {^%alex} can decide that."` (mayúscula forzada a mitad de frase) | `"Only Herself can decide that."` |

## Tiempos verbales

Escribe el verbo en su forma **base**; un marcador de tiempo lo prefija. Nunca pases un verbo ya
conjugado (`{=person goes}` → `"He goeses"` ✗) — el marcador de tiempo se encarga de la
conjugación.

| Tiempo | Marcador | Ejemplo | Salida |
|---|---|---|---|
| Presente | *(ninguno)* | `{=person walk}` | "He walks" |
| Pasado | `<` | `{=person <walk}` | "He walked" (irregular: "He went") |
| Presente continuo | `=` | `{=person =walk}` | "He is walking" |
| Futuro | `>` | `{=person >walk}` | "He will walk" |
| Pasado continuo | `<=` | `{=person <=walk}` | "He was walking" |
| Presente perfecto | `%` | `{=person %walk}` | "He has walked" (irregular: "He has gone") |
| Pasado perfecto | `<%` | `{=person <%walk}` | "He had walked" (irregular: "He had gone") |

`say_with!(context, "...", args...)` resuelve estos marcadores contra un `NarrationContext.tense`
en tiempo de ejecución en su lugar, recurriendo al valor por defecto del propio marcador cuando el
contexto no lo sobrescribe. La salida de `say!()` no se ve afectada en ningún caso.

## Comparativo / superlativo (marcadores de grado)

```rust
say!("{?w !good} than that.", w)   // "better than that."
say!("{?w !!good} in class", w)    // "best in class"
```
Tabla de irregulares más `-er`/`-est` regular (con duplicación CVC, y→i) y perifrástico
`more`/`most` para adjetivos más largos. Se resuelve enteramente en tiempo de compilación — no
hace falta `NarrationContext` ni concordancia con el sujeto.

## Macros

| Macro | Firma | Comportamiento |
|---|---|---|
| `say!()` | `say!(fmt, args...)` | Construye un `String`, como `format!()` pero con gramática de placeholders. |
| `say_with!()` | `say_with!(context, fmt, args...)` | Como `say!()`, pero resuelve los marcadores de tiempo/punto de vista contra un `NarrationContext` en tiempo de ejecución. |
| `ack!()` | `ack!(fmt, args...)` | Se expande a `Ok(say!(fmt, args...))` — una expresión normal, no un `return` oculto. |
| `nay!()` | `nay!(fmt, args...)` | Se expande a `Err(say!(fmt, args...))` — igual, una expresión normal. |
| `heed!()` | `heed!(template, input)` | Dirección inversa: compara el texto `input` contra `template` y devuelve las capturas. |
| `ask!()` | `ask!(speaker, audience, template, input)` | Analiza `input` contra `template` como `heed!()`, y reenvía las capturas al `Answerable::answer` de `audience`. Devuelve `Option<String>`. |

```rust
let result: Result<String, String> = ack!("{=p are} welcome.");
assert_eq!(result, Ok("She is welcome.".to_string()));

let result: Result<String, String> = nay!("{=p can't} get in {`p} house.");
assert_eq!(result, Err("She can't get in her house.".to_string()));
```

### `heed!()`

```rust
heed!("take {item}", "take sword")                    // Some("sword".to_string())
heed!("give {item} to {target}", "give sword to guard") // Some(("sword".to_string(), "guard".to_string()))
heed!("take {item}", "drop sword")                    // None
```
`{name}` — un solo token. `{name...}` — voraz (greedy), hasta el siguiente literal o el final de
la entrada. `{$name}` — dígitos, analizados como `u64`. Dos capturas adyacentes sin texto literal
entre ellas es un error **en tiempo de compilación** (ambiguo).

### `#[derive(Heed)]`

```rust
#[derive(Heed)]
#[heed(template = "give {item} to {target}")]
struct Give { item: String, target: String }

Give::heed("give sword to guard") // Some(Give { item: "sword".into(), target: "guard".into() })
Give::heed("drop sword")          // None
```
Versión de `heed!()` para estructuras — misma gramática de plantilla, pero enlaza las capturas a
campos con el mismo nombre en lugar de devolver una tupla posicional. Cada captura necesita un
campo correspondiente y viceversa; el tipo del campo debe coincidir con el tipo de captura
(`String` frente a `u64`). Consulta API.md (en inglés) para las reglas completas.

### `ask!()`

```rust
struct Dog;
impl Answerable for Dog {
    type Captures = ();
    fn answer(&self, _speaker: &dyn Ranting, _: ()) -> String {
        "Woof!".to_string()
    }
}
let player = Noun::new("Jo", "she");
ask!(player, Dog, "pet dog", "pet dog") // Some("Woof!".to_string())
ask!(player, Dog, "pet dog", "kick dog") // None
```
Misma gramática de plantilla que `heed!()`, pero las capturas se reenvían al
[`Answerable::answer`](../API.md#the-answerable-trait-asks-audience) de `audience` en lugar de
devolverse directamente — consulta API.md (en inglés) para la forma completa y un ejemplo guiado
por capturas.

## Tipos principales

| Tipo | Propósito |
|---|---|
| `Noun` | El implementador concreto de `Ranting`. `Noun::new(name, subject)` entra en pánico ante un sujeto inválido; `Noun::try_new` devuelve `Result<Noun, InvalidSubjectError>`. |
| `Many<T: Ranting>` | Envuelve un `Vec<T>` — una frase nominal colectiva (`"a, b and c"`), plural siempre que `len() != 1` (incluyendo cero). |
| `Maybe<T: Ranting>` | Envuelve un `Option<T>` — `Some(x)` delega en `x`; `None` no renderiza nada, es singular, con sujeto `"it"`. |
| `Box<T: Ranting>` | Delega cada método de `Ranting` directamente al valor contenido. |
| `NarrationContext` | Builder: `.tense(Tense)`, `.narration_person(Person)`, `.register(Register)`, `.dialect(&'static str)`. |
| `Answerable` | El contrato de audiencia de `ask!()`: `fn answer(&self, speaker: &dyn Ranting, captures: Self::Captures) -> String`. |

Consulta [API.md](../API.md) (en inglés) para la superficie completa, o
[docs.rs](https://docs.rs/ranting) para la documentación de referencia generada.

---

Ver también: [Tutorial en español](TUTORIAL.md) · [Recetario en español](COOKBOOK.md)
