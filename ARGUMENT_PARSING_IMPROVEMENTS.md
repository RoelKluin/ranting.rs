# Argument Parsing Improvements

> **Partially superseded (2026-08-15).** Kept as a historical record of this change;
> do not read the "Named Argument Missing" example as current behavior. The
> positional-argument error message in §1/§"Error Message Improvements" still matches
> `ranting_derive/src/lib.rs::get_opt_num_ph_expr` verbatim. But the named-argument
> case does not: a name absent from `given` is never reported as
> `"named argument 'missing_name' not found in provided arguments"` — it falls
> through to `check_ident_path`/`path_from` and is treated as a variable from the
> local scope instead (the same fallback §2 of this doc itself describes), so it
> either resolves there or surfaces as rustc's own "cannot find value" error, not a
> `ranting_derive`-authored message. `tests/ranting/error_messages.rs`'s "Expected
> error" comments describe this same superseded named-argument message; per
> `CLAUDE.md`'s testing conventions, this repo has no compile-fail harness, so
> nothing ever executed that comment to catch the drift.

## Summary

Improved error messages for argument parsing in macros (`say!()`, `ask!()`, `ack!()`, `nay!()`) and fixed implicit variable name lookup to work correctly with the local scope.

## Changes Made

### 1. Enhanced Error Messages

**File:** `ranting_derive/src/lib.rs` - `get_opt_num_ph_expr()` function

#### Before
```rust
Ok(u) => Err(format!("No positional argument {u}")),
Err(_) => Err(format!("named argument {s} not found")),
```

#### After
```rust
Ok(u) => Err(format!(
    "positional argument at index {u} not provided (only {} argument(s) given)",
    given.iter().filter(|(k, _)| k.parse::<usize>().is_ok()).count()
)),
Err(_) => {
    // Not a number and not in given arguments - assume it's a variable from local scope
    Ok(path_from(&s))
},
```

### 2. Fixed Implicit Variable Lookup

The macro now correctly handles variables from the local scope. Instead of immediately erroring when a variable name isn't in the provided arguments, it now:
1. Checks if it's in the provided arguments
2. If not found and it's a positional index, returns a clear error
3. If not found and it's a name, treats it as a variable from the local scope

This enables the intended behavior shown in the README:
```rust
fn say_this(who: Noun, title: &Noun) -> String {
    say!("{=who do} say {`who title are} {who}.")  // No arguments passed!
}
```

### 3. Comprehensive Test Coverage

#### New Test File: `tests/ranting/argument_parsing.rs` (19 tests)
Tests for various argument combinations:
- Single/multiple positional arguments
- Single/multiple named arguments  
- Mixed positional and named arguments
- Variable name inference from local scope
- Complex placeholders with articles, verbs, possessives
- `ack!()` and `nay!()` macros with various argument types

Example tests:
- `test_positional_single_argument` - Verify positional args work
- `test_named_single_argument` - Verify named args work
- `test_variable_name_inference` - Verify implicit local scope lookup
- `test_mixed_positional_and_named` - Verify mixing both types

#### New Test File: `tests/ranting/error_messages.rs` (10 tests)
Tests documenting expected error behavior and best practices:
- Valid positional/named/implicit variable usage
- Complex placeholder combinations
- Documentation of what error messages should say for missing args

### 4. Test Results

All tests pass:
```
running 33 tests
...
test result: ok. 33 passed; 0 failed
```

## Error Message Improvements

### Positional Argument Missing
**Before:** `"No positional argument 2"`
**After:** `"positional argument at index 2 not provided (only 2 argument(s) given)"`

Benefits:
- Clearly states the index needed
- Shows how many arguments were actually provided
- Easier to debug - immediately see the mismatch

### Named Argument Missing  
**Before:** `"named argument missing not found"`
**After:** `"named argument 'missing_name' not found in provided arguments"`

Benefits:
- Clearly identifies the exact name that's missing
- Quotes the name for clarity
- Explicitly states it wasn't found in provided arguments

## Usage Examples

### Positional Arguments
```rust
let alice = Noun::new("Alice", "I");
let bob = Noun::new("Bob", "he");
say!("{=0} and {=1}", alice, bob)
```

### Named Arguments
```rust
let alice = Noun::new("Alice", "I");
say!("{=person}", person = alice)
```

### Implicit Variable Lookup (Local Scope)
```rust
fn greet(person: Noun) -> String {
    say!("{=person do} say hello")  // No arguments passed!
}
```

### Mixed Arguments
```rust
let alice = Noun::new("Alice", "I");
let bob = Noun::new("Bob", "he");
say!("{=0} meets {=bob}", alice, bob = bob)
```

## Implementation Notes

1. **Shared Grammar Code:** Both `ranting` and `ranting_derive` crates have `src/language/english_shared.rs`. Keep them in sync when modifying placeholder parsing.

2. **Error Location Tracking:** Errors include precise spans showing exactly where in the format string the problem occurs.

3. **Compile-Time Checking:** All argument validation happens at compile-time via the proc macro, providing immediate feedback.

## Future Improvements

Potential enhancements (not implemented):
- Better suggestions for misspelled variable names
- Support for more complex expressions in placeholders
- Better error recovery to allow multiple error reporting in one string
