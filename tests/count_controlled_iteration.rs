mod common;
use common::check_output_stream;
use pseudocode::{PseudocodeError, RuntimeError, ParserError, Token, TypeError, Type};

#[test]
fn for_loop_normal() {
    assert!(
        check_output_stream(
            "FOR i = 1 TO 3 DO
            OUTPUT(
            INT_TO_STRING(i));
            ENDFOR",
            vec!["1", "2", "3"],
        )
        .unwrap(),
    )
}

#[test]
fn for_loop_step_normal() {
    assert!(
        check_output_stream(
            "FOR i = 1 TO 3 STEP 2 DO
            OUTPUT(
            INT_TO_STRING(i));
            ENDFOR",
            vec!["1", "3"],
        )
        .unwrap(),
    )
}

#[test]
fn for_loop_decreasing_normal() {
    assert!(
        check_output_stream(
            "FOR i = 3 TO 1 STEP -1 DO
            OUTPUT(
            INT_TO_STRING(i));
            ENDFOR",
            vec!["3", "2", "1"],
        )
        .unwrap(),
    )
}


#[test]
fn for_loop_negative_step_increasing_range_error() {
    assert_eq!(
        check_output_stream(
            "FOR i = 1 TO 3 STEP -1 DO
            OUTPUT(
            INT_TO_STRING(i));
            ENDFOR",
            vec!["1", "2", "3"],
        )
        .unwrap_err(),
        PseudocodeError::Runtime(RuntimeError::InvalidRangeBound)
    )
}

#[test]
fn for_loop_postitive_step_decreasing_range_error() {
    assert_eq!(
        check_output_stream(
            "FOR i = 3 TO 1  DO
            OUTPUT(
            INT_TO_STRING(i));
            ENDFOR",
            vec!["3", "2", "1"],
        )
        .unwrap_err(),
        PseudocodeError::Runtime(RuntimeError::InvalidRangeBound)
    )
}

#[test]
fn for_loop_zero_step_error() {
    assert_eq!(
        check_output_stream(
            "FOR i = 1 TO 3 STEP 0 DO
            OUTPUT(
            INT_TO_STRING(i));
            ENDFOR",
            vec!["1", "2", "3"],
        )
        .unwrap_err(),
        PseudocodeError::Runtime(RuntimeError::RangeStepCannotBeZero)
    )
}

#[test]
fn for_loop_constant_identifier_error() {
    assert_eq!(
        check_output_stream(
            "FOR I = 1 TO 3 DO
            OUTPUT(
            INT_TO_STRING(i));
            ENDFOR",
            vec!["1", "2", "3"],
        ).unwrap_err(),
        PseudocodeError::Parsing(ParserError::Expected(Token::VarIdentifier(String::new())))
    )
}

#[test]
fn start_value_integer_string_for_loop_error() {
    assert_eq!(
        check_output_stream(
            "FOR i = \"1\" TO 3 DO
            OUTPUT(
            INT_TO_STRING(i));
            ENDFOR",
            vec!["1", "2", "3"],
        )
        .unwrap_err(),
        PseudocodeError::Type(TypeError::SingleExpected(Type::Integer, Type::Str))
    )
}

#[test]
fn end_value_integer_string_for_loop_error() {
    assert_eq!(
        check_output_stream(
            "FOR i = 1 TO \"3\" DO
            OUTPUT(
            INT_TO_STRING(i));
            ENDFOR",
            vec!["1", "2", "3"],
        )
        .unwrap_err(),
        PseudocodeError::Type(TypeError::SingleExpected(Type::Integer, Type::Str))
    )
} 

#[test]
fn step_value_integer_string_for_loop_error() {
    assert_eq!(
        check_output_stream(
            "FOR i = 1 TO 3 STEP \"2\" DO
            OUTPUT(
            INT_TO_STRING(i));
            ENDFOR",
            vec!["1", "3"],
        )
        .unwrap_err(),
        PseudocodeError::Type(TypeError::SingleExpected(Type::Integer, Type::Str))
    )
} 
