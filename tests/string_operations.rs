mod common;
use common::evaluate_expression;
use pseudocode::{Literal, PseudocodeError, Type, TypeError};

#[test]
fn string_length_normal() {
    assert_eq!(
        evaluate_expression("LEN(\"foobar\")").unwrap(),
        Literal::Integer(6)
    )
}

#[test]
fn type_mismatch_length_error() {
    assert_eq!(
        evaluate_expression("LEN(123)").unwrap_err(),
        PseudocodeError::Type(TypeError::SingleExpectedOneOf(
            vec![Type::Str, Type::List(Box::new(Type::Any))],
            Type::Integer
        ))
    )
}

#[test]
fn position_normal() {
    assert_eq!(
        evaluate_expression("POSITION(\"computer science\", 'm')").unwrap(),
        Literal::Integer(2)
    )
}

#[test]
fn substring_normal() {
    assert_eq!(
        evaluate_expression("SUBSTRING(2, 9,\"computer science\")").unwrap(),
        Literal::Str("mputer s".to_owned())
    )
}
