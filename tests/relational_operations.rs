mod common;
use common::evaluate_expression;
use pseudocode::{Literal, PseudocodeError, Type, TypeError};

#[test]
fn integer_less_than_less_normal() {
    assert_eq!(
        evaluate_expression("4 < 6").unwrap(),
        Literal::Boolean(true)
    );
}

#[test]
fn integer_less_than_greater_normal() {
    assert_eq!(
        evaluate_expression("6 < 4").unwrap(),
        Literal::Boolean(false)
    );
}

#[test]
fn integer_less_than_equal_extreme() {
    assert_eq!(
        evaluate_expression("4 < 4").unwrap(),
        Literal::Boolean(false)
    );
}

#[test]
fn real_less_than_less_normal() {
    assert_eq!(
        evaluate_expression("3.5 < 6.5").unwrap(),
        Literal::Boolean(true)
    );
}

#[test]
fn real_less_than_greater_normal() {
    assert_eq!(
        evaluate_expression("6.5 < 3.5").unwrap(),
        Literal::Boolean(false)
    );
}

#[test]
fn real_less_than_slightly_less_extreme() {
    assert_eq!(
        evaluate_expression("3.5 < 3.50001").unwrap(),
        Literal::Boolean(true)
    );
}

#[test]
fn real_less_than_slightly_greater_extreme() {
    assert_eq!(
        evaluate_expression("3.50001 < 3.5").unwrap(),
        Literal::Boolean(false)
    );
}

#[test]
fn integer_real_less_than_error() {
    assert_eq!(
        evaluate_expression("4 < 6.5").unwrap_err(),
        PseudocodeError::Type(TypeError::DoubleExpectedOneOf(
            vec![(Type::Integer, Type::Integer), (Type::Real, Type::Real)],
            (Type::Integer, Type::Real),
        ))
    )
}

#[test]
fn integer_greater_than_less_normal() {
    assert_eq!(
        evaluate_expression("6 > 4").unwrap(),
        Literal::Boolean(true)
    );
}

#[test]
fn integer_greater_than_greater_normal() {
    assert_eq!(
        evaluate_expression("4 > 6").unwrap(),
        Literal::Boolean(false)
    );
}

#[test]
fn integer_greater_than_equal_extreme() {
    assert_eq!(
        evaluate_expression("4 > 4").unwrap(),
        Literal::Boolean(false)
    );
}

#[test]
fn real_greater_than_less_normal() {
    assert_eq!(
        evaluate_expression("6.5 > 3.5").unwrap(),
        Literal::Boolean(true)
    );
}

#[test]
fn real_greater_than_greater_normal() {
    assert_eq!(
        evaluate_expression("3.5 > 6.5").unwrap(),
        Literal::Boolean(false)
    );
}

#[test]
fn real_greater_than_slightly_less_extreme() {
    assert_eq!(
        evaluate_expression("3.50001 > 3.5").unwrap(),
        Literal::Boolean(true)
    );
}

#[test]
fn real_greater_than_slightly_greater_extreme() {
    assert_eq!(
        evaluate_expression("3.5 > 3.50001").unwrap(),
        Literal::Boolean(false)
    );
}

#[test]
fn integer_real_greater_than_error() {
    assert_eq!(
        evaluate_expression("4 > 6.5").unwrap_err(),
        PseudocodeError::Type(TypeError::DoubleExpectedOneOf(
            vec![(Type::Integer, Type::Integer), (Type::Real, Type::Real)],
            (Type::Integer, Type::Real),
        ))
    )
}

#[test]
fn integer_equality_true_normal() {
    assert_eq!(
        evaluate_expression("3 == 3").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn integer_equality_false_normal() {
    assert_eq!(
        evaluate_expression("3 == 4").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn real_equality_true_normal() {
    assert_eq!(
        evaluate_expression("3.0 == 3.0").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn real_equality_false_normal() {
    assert_eq!(
        evaluate_expression("3.0 == 4.0").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn boolean_equality_true_normal() {
    assert_eq!(
        evaluate_expression("True == True").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn boolean_equality_false_normal() {
    assert_eq!(
        evaluate_expression("True == False").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn character_equality_true_normal() {
    assert_eq!(
        evaluate_expression("'a' == 'a'").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn character_equality_false_normal() {
    assert_eq!(
        evaluate_expression("'a' == 'b'").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn string_equality_true_normal() {
    assert_eq!(
        evaluate_expression("\"foo\" == \"foo\"").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn string_equality_false_normal() {
    assert_eq!(
        evaluate_expression("\"foo\" == \"bar\"").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn array_equality_true_normal() {
    assert_eq!(
        evaluate_expression("[\"foo\",\"bar\"] == [\"foo\",\"bar\"]").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn array_equality_false_normal() {
    assert_eq!(
        evaluate_expression("[\"foo\",\"bar\"] == [\"bar\",\"foo\"]").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn different_type_equality_error() {
    assert_eq!(
        evaluate_expression("'a' == \"a\"").unwrap_err(),
        PseudocodeError::Type(TypeError::UnequalTypes(Type::Character, Type::Str))
    )
}

#[test]
fn integer_inequality_true_normal() {
    assert_eq!(
        evaluate_expression("3 != 4").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn integer_inequality_false_normal() {
    assert_eq!(
        evaluate_expression("3 != 3").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn real_inequality_true_normal() {
    assert_eq!(
        evaluate_expression("3.0 != 4.0").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn real_inequality_false_normal() {
    assert_eq!(
        evaluate_expression("3.0 != 3.0").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn boolean_inequality_true_normal() {
    assert_eq!(
        evaluate_expression("True != False").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn boolean_inequality_false_normal() {
    assert_eq!(
        evaluate_expression("True != True").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn character_inequality_true_normal() {
    assert_eq!(
        evaluate_expression("'a' != 'b'").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn character_inequality_false_normal() {
    assert_eq!(
        evaluate_expression("'a' != 'a'").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn string_inequality_true_normal() {
    assert_eq!(
        evaluate_expression("\"foo\" != \"bar\"").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn string_inequality_false_normal() {
    assert_eq!(
        evaluate_expression("\"foo\" != \"foo\"").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn array_inequality_true_normal() {
    assert_eq!(
        evaluate_expression("[\"foo\",\"bar\"] != [\"bar\",\"foo\"]").unwrap(),
        Literal::Boolean(true)
    )
}

#[test]
fn array_inequality_false_normal() {
    assert_eq!(
        evaluate_expression("[\"foo\",\"bar\"] != [\"foo\",\"bar\"]").unwrap(),
        Literal::Boolean(false)
    )
}

#[test]
fn different_type_inequality_error() {
    assert_eq!(
        evaluate_expression("'a' != \"a\"").unwrap_err(),
        PseudocodeError::Type(TypeError::UnequalTypes(Type::Character, Type::Str))
    )
}

#[test]
fn integer_less_equal_less_normal() {
    assert_eq!(
        evaluate_expression("3 <= 4").unwrap(),
        Literal::Boolean(true)
    );
}

#[test]
fn integer_less_equal_equal_normal() {
    assert_eq!(
        evaluate_expression("4 <= 4").unwrap(),
        Literal::Boolean(true)
    );
}

#[test]
fn integer_less_equal_greater_normal() {
    assert_eq!(
        evaluate_expression("5 <= 4").unwrap(),
        Literal::Boolean(false)
    );
}


#[test]
fn real_less_equal_less_normal() {
    assert_eq!(
        evaluate_expression("3.0 <= 4.0").unwrap(),
        Literal::Boolean(true)
    );
}

#[test]
fn real_less_equal_equal_normal() {
    assert_eq!(
        evaluate_expression("4.0 <= 4.0").unwrap(),
        Literal::Boolean(true)
    );
}

#[test]
fn real_less_equal_greater_normal() {
    assert_eq!(
        evaluate_expression("5.0 <= 4.0").unwrap(),
        Literal::Boolean(false)
    );
}

#[test]
fn integer_real_less_equal_error() {
    assert_eq!(
        evaluate_expression("3 <= 4.0").unwrap_err(),
        PseudocodeError::Type(TypeError::DoubleExpectedOneOf(
            vec![(Type::Integer, Type::Integer), (Type::Real, Type::Real)],
            (Type::Integer, Type::Real),
        ))
    )
}

#[test]
fn integer_greater_equal_less_normal() {
    assert_eq!(
        evaluate_expression("5 >= 4").unwrap(),
        Literal::Boolean(true)
    );
}

#[test]
fn integer_greater_equal_equal_normal() {
    assert_eq!(
        evaluate_expression("4 >= 4").unwrap(),
        Literal::Boolean(true)
    );
}

#[test]
fn integer_greater_equal_greater_normal() {
    assert_eq!(
        evaluate_expression("3 >= 4").unwrap(),
        Literal::Boolean(false)
    );
}


#[test]
fn real_greater_equal_less_normal() {
    assert_eq!(
        evaluate_expression("5.0 >= 4.0").unwrap(),
        Literal::Boolean(true)
    );
}

#[test]
fn real_greater_equal_equal_normal() {
    assert_eq!(
        evaluate_expression("4.0 >= 4.0").unwrap(),
        Literal::Boolean(true)
    );
}

#[test]
fn real_greater_equal_greater_normal() {
    assert_eq!(
        evaluate_expression("3.0 >= 4.0").unwrap(),
        Literal::Boolean(false)
    );
}

#[test]
fn integer_real_greater_equal_error() {
    assert_eq!(
        evaluate_expression("5 >= 4.0").unwrap_err(),
        PseudocodeError::Type(TypeError::DoubleExpectedOneOf(
            vec![(Type::Integer, Type::Integer), (Type::Real, Type::Real)],
            (Type::Integer, Type::Real),
        ))
    )
}

