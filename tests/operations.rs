mod common;
use common::evaluate_expression;
use pseudocode::{Literal, PseudocodeError, Type, TypeError};

#[test]
fn integer_addition_success() {
    assert_eq!(evaluate_expression("1 + 1").unwrap(), Literal::Integer(2));
}

#[test]
fn real_addition_success() {
    assert_eq!(
        evaluate_expression("1.5 + 1.5").unwrap(),
        Literal::Real(3.0)
    )
}

#[test]
fn string_concatenation_success() {
    assert_eq!(
        evaluate_expression("\"foo\" + \"bar\"").unwrap(),
        Literal::Str("foobar".to_owned())
    )
}

#[test]
fn integer_real_addition_fail() {
    assert_eq!(
        evaluate_expression("1 + 1.5").unwrap_err(),
        PseudocodeError::Type(TypeError::DoubleExpectedOneOf(
            vec![
                (Type::Str, Type::Str),
                (Type::Integer, Type::Integer),
                (Type::Real, Type::Real)
            ],
            (Type::Integer, Type::Real),
        ))
    )
}
