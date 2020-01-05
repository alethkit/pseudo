mod common;
use common::evaluate_expression;
use pseudocode::{Literal, PseudocodeError, Type, TypeError};

#[test]
fn true_true_and_normal() {
    assert_eq!(
        evaluate_expression("3 == 3 AND 3 <= 4").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn false_true_and_normal() {
    assert_eq!(
        evaluate_expression("3 != 3 AND 3 <= 4").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn true_false_and_normal() {
    assert_eq!(
        evaluate_expression("3 == 3 AND 3 >= 4").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn false_false_and_normal() {
    assert_eq!(
        evaluate_expression("3 != 3 AND 3 >= 4").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn boolean_integer_and_error() {
    assert_eq!(
        evaluate_expression("3 != 3 AND 1").unwrap_err(),
        PseudocodeError::Type(TypeError::DoubleExpected((Type::Boolean, Type::Boolean), (Type::Boolean, Type::Integer)))
    )
}

#[test]
fn true_true_or_normal() {
    assert_eq!(
        evaluate_expression("3 == 3 OR 3 <= 4").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn false_true_or_normal() {
    assert_eq!(
        evaluate_expression("3 != 3 OR 3 <= 4").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn true_false_or_normal() {
    assert_eq!(
        evaluate_expression("3 == 3 OR 3 >= 4").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn false_false_or_normal() {
    assert_eq!(
        evaluate_expression("3 != 3 OR 3 >= 4").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn boolean_integer_or_error() {
    assert_eq!(
        evaluate_expression("3 != 3 OR 1").unwrap_err(),
        PseudocodeError::Type(TypeError::DoubleExpected((Type::Boolean, Type::Boolean), (Type::Boolean, Type::Integer)))
    )
}

#[test]
fn true_not_normal() {
    assert_eq!(
        evaluate_expression("NOT True").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn false_not_normal() {
    assert_eq!(
        evaluate_expression("NOT False").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn string_not_error() {
    assert_eq!(
        evaluate_expression("NOT \"foo\"").unwrap_err(),
        PseudocodeError::Type(TypeError::SingleExpected(Type::Boolean, Type::Str))
    )
}
