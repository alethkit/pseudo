mod common;
use common::check_output_stream;
use pseudocode::{PseudocodeError, Type, TypeError};

#[test]
fn while_loop_normal() {
    assert!(check_output_stream(
        "VAR: Int a = 1;
        WHILE a < 4 DO
        OUTPUT(INT_TO_STRING(a));
        a = a + 1;
        ENDWHILE",
        vec!["1", "2", "3"]
    )
    .unwrap())
}

#[test]
fn boolean_integer_while_loop_error() {
    assert_eq!(
        check_output_stream(
            "WHILE 1 DO
            OUTPUT(\"1\");
            ENDWHILE",
            vec!["1"]
        )
        .unwrap_err(),
        PseudocodeError::Type(TypeError::SingleExpected(Type::Boolean, Type::Integer))
    )
}

#[test]
fn do_while_loop_normal() {
    assert!(check_output_stream(
        "VAR: Int a = 1;
        DO
        OUTPUT(INT_TO_STRING(a));
        a = a + 1;
        WHILE a != 4
        ENDDOWHILE",
        vec!["1", "2", "3"]
    )
    .unwrap())
}

#[test]
fn boolean_integer_do_while_loop_error() {
    assert_eq!(
        check_output_stream(
            "DO 
            OUTPUT(\"1\");
            WHILE 1
            ENDDOWHILE",
            vec!["1"]
        )
        .unwrap_err(),
        PseudocodeError::Type(TypeError::SingleExpected(Type::Boolean, Type::Integer))
    )
}
