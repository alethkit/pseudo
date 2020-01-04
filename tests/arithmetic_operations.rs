mod common;
use common::evaluate_expression;
use pseudocode::{Literal, PseudocodeError, RuntimeError, Type, TypeError};

#[test]
fn integer_addition_normal() {
    assert_eq!(evaluate_expression("1 + 1").unwrap(), Literal::Integer(2));
}

#[test]
fn real_addition_normal() {
    assert_eq!(
        evaluate_expression("1.5 + 1.5").unwrap(),
        Literal::Real(3.0)
    )
}

#[test]
fn string_concatenation_normal() {
    assert_eq!(
        evaluate_expression("\"foo\" + \"bar\"").unwrap(),
        Literal::Str("foobar".to_owned())
    )
}

#[test]
fn integer_real_addition_error() {
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

#[test]
fn string_integer_concatenation_error() {
    assert_eq!(
        evaluate_expression("\"1\" + 1").unwrap_err(),
        PseudocodeError::Type(TypeError::DoubleExpectedOneOf(
            vec![
                (Type::Str, Type::Str),
                (Type::Integer, Type::Integer),
                (Type::Real, Type::Real)
            ],
            (Type::Str, Type::Integer),
        ))
    )
}

#[test]
fn integer_subtraction_normal() {
    assert_eq!(evaluate_expression("2 - 1").unwrap(), Literal::Integer(1));
}

#[test]
fn real_subtraction_normal() {
    assert_eq!(
        evaluate_expression("2.0 - 1.5").unwrap(),
        Literal::Real(0.5)
    )
}

#[test]
fn integer_real_subtraction_error() {
    assert_eq!(
        evaluate_expression("2 - 1.0").unwrap_err(),
        PseudocodeError::Type(TypeError::DoubleExpectedOneOf(
            vec![(Type::Integer, Type::Integer), (Type::Real, Type::Real)],
            (Type::Integer, Type::Real),
        ))
    )
}

#[test]
fn integer_multiplication_normal() {
    assert_eq!(evaluate_expression("2 * 3").unwrap(), Literal::Integer(6));
}

#[test]
fn real_multiplication_normal() {
    assert_eq!(
        evaluate_expression("1.5 * 3.0").unwrap(),
        Literal::Real(4.5)
    )
}

#[test]
fn integer_real_multiplication_error() {
    assert_eq!(
        evaluate_expression("2 * 3.0").unwrap_err(),
        PseudocodeError::Type(TypeError::DoubleExpectedOneOf(
            vec![(Type::Integer, Type::Integer), (Type::Real, Type::Real)],
            (Type::Integer, Type::Real),
        ))
    )
}

#[test]
fn integer_division_normal() {
    assert_eq!(evaluate_expression("4 / 2").unwrap(), Literal::Real(2.0));
}

#[test]
fn real_division_normal() {
    assert_eq!(
        evaluate_expression("3.0 / 0.75").unwrap(),
        Literal::Real(4.0)
    )
}

#[test]
fn integer_zero_division_error() {
    assert_eq!(
        evaluate_expression("4 / 0").unwrap_err(),
        PseudocodeError::Runtime(RuntimeError::DivisionByZero)
    )
}

#[test]
fn real_zero_division_error() {
    assert_eq!(
        evaluate_expression("3.0 / 0.0").unwrap_err(),
        PseudocodeError::Runtime(RuntimeError::DivisionByZero)
    )
}

#[test]
fn integer_real_division_error() {
    assert_eq!(
        evaluate_expression("5 / 2.0").unwrap_err(),
        PseudocodeError::Type(TypeError::DoubleExpectedOneOf(
            vec![(Type::Integer, Type::Integer), (Type::Real, Type::Real)],
            (Type::Integer, Type::Real),
        ))
    )
}

#[test]
fn integer_div_normal() {
    assert_eq!(evaluate_expression("5 DIV 3").unwrap(), Literal::Integer(1));
}

#[test]
fn integer_zero_div_error() {
    assert_eq!(
        evaluate_expression("5 DIV 0").unwrap_err(),
        PseudocodeError::Runtime(RuntimeError::DivisionByZero)
    )
}

#[test]
fn real_div_error() {
    assert_eq!(
        evaluate_expression("5.0 DIV 3.0").unwrap_err(),
        PseudocodeError::Type(TypeError::DoubleExpected(
            (Type::Integer, Type::Integer),
            (Type::Real, Type::Real),
        ))
    )
}

#[test]
fn integer_mod_normal() {
    assert_eq!(evaluate_expression("5 MOD 3").unwrap(), Literal::Integer(2));
}

#[test]
fn integer_zero_mod_error() {
    assert_eq!(
        evaluate_expression("5 MOD 0").unwrap_err(),
        PseudocodeError::Runtime(RuntimeError::DivisionByZero)
    )
}

#[test]
fn real_mod_error() {
    assert_eq!(
        evaluate_expression("5.0 MOD 3.0").unwrap_err(),
        PseudocodeError::Type(TypeError::DoubleExpected(
            (Type::Integer, Type::Integer),
            (Type::Real, Type::Real),
        ))
    )
}
