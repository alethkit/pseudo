mod common;
use common::evaluate_expression;
use pseudocode::Literal;

#[test]
fn string_to_int_normal() {
    assert_eq!(
        evaluate_expression("STRING_TO_INT(\"16\")").unwrap(),
        Literal::Integer(16)
    )
}

#[test]
fn string_to_real_normal() {
    assert_eq!(
        evaluate_expression("STRING_TO_REAL(\"16.3\")").unwrap(),
        Literal::Real(16.3)
    )
}

#[test]
fn int_to_string_normal() {
    assert_eq!(
        evaluate_expression("INT_TO_STRING(16)").unwrap(),
        Literal::Str("16".to_owned())
    )
}

#[test]
fn real_to_string_normal() {
    assert_eq!(
        evaluate_expression("REAL_TO_STRING(16.3)").unwrap(),
        Literal::Str("16.3".to_owned())
    )
}

#[test]
fn char_to_code_normal() {
    assert_eq!(
        evaluate_expression("CHAR_TO_CODE('a')").unwrap(),
        Literal::Integer(97)
    )
}

#[test]
fn code_to_char_normal() {
    assert_eq!(
        evaluate_expression("CODE_TO_CHAR(97)").unwrap(),
        Literal::Character('a')
    )
}
