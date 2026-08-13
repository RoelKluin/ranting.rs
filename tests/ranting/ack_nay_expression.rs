// (c) Roel Kluin 2022 GPL v3
//
// Phase 4 item 5: ack!()/nay!() no longer expand to a hidden `return` --
// they expand to plain `Ok(say!(...))` / `Err(say!(...))` expressions, so
// they can be used anywhere an expression is valid, not just as an implicit
// early-return statement. These tests prove that: the value is bound with
// `let` and used in match-arm tail position, not returned.

use ranting::*;

#[test]
fn ack_as_plain_let_bound_expression() {
    let p = Noun::new("Jo", "she");
    // No `return` here -- ack!() is a plain expression bound to a `let`.
    let result: Result<String, String> = ack!("{=p are} welcome.");
    assert_eq!(result, Ok("She is welcome.".to_string()));
}

#[test]
fn nay_as_plain_let_bound_expression() {
    let p = Noun::new("Jo", "she");
    let result: Result<String, String> = nay!("{=p can't} get in {`p} house.");
    assert_eq!(result, Err("She can't get in her house.".to_string()));
}

#[test]
fn ack_and_nay_as_match_arm_tail_expressions() {
    fn classify(ok: bool, p: Noun) -> Result<String, String> {
        // Tail expression of the function -- no explicit `return` needed,
        // since ack!()/nay!() are now ordinary expressions.
        match ok {
            true => ack!("{=p are} accepted."),
            false => nay!("{=p are} rejected."),
        }
    }

    let jo = Noun::new("Jo", "she");
    assert_eq!(classify(true, jo), Ok("She is accepted.".to_string()));
    let al = Noun::new("Al", "he");
    assert_eq!(classify(false, al), Err("He is rejected.".to_string()));
}

#[test]
fn ack_and_nay_still_work_with_explicit_return() {
    fn respond(ok: bool, p: Noun) -> Result<String, String> {
        if ok {
            return ack!("{=p agree}.");
        }
        nay!("{=p disagree}.")
    }

    assert_eq!(
        respond(true, Noun::new("Sam", "they")),
        Ok("They agree.".to_string())
    );
    assert_eq!(
        respond(false, Noun::new("Sam", "they")),
        Err("They disagree.".to_string())
    );
}
