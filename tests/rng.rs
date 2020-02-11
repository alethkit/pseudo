mod common;
use common::evaluate_expression;
use pseudocode::{Literal, PseudocodeError, RuntimeError};

#[test]
fn random_integer_normal() {
    let val = evaluate_expression("RANDOM_INT(3,5)").unwrap();
    match val {
        Literal::Integer(i) => assert!(3 <= i && i <= 5),
        _ => unreachable!(),
    }
}

#[test]
fn random_integer_bound_error() {
    let err = evaluate_expression("RANDOM_INT(5,3)").unwrap_err();
    assert_eq!(
        err,
        PseudocodeError::Runtime(RuntimeError::InvalidRangeBound)
    )
}
