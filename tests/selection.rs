mod common;
use common::check_output_stream;
use pseudocode::{PseudocodeError, TypeError, Type};


#[test]
fn if_statement_normal() {
    // When a is even
    assert!(check_output_stream("
    VAR: Int a = 2;
    IF a MOD 2 == 0 THEN
    OUTPUT(\"even\");
    ENDIF", vec!["even"]).unwrap());
    // When a is odd
    assert!(check_output_stream("
    VAR: Int a = 1;
    IF a MOD 2 == 0 THEN
    OUTPUT(\"even\");
    ENDIF", vec![]).unwrap())
}

#[test]
fn if_else_statement_normal() {
    // When a is even
    assert!(check_output_stream("
    VAR: Int a = 2;
    IF a MOD 2 == 0 THEN
    OUTPUT(\"even\");
    ENDIF", vec!["even"]).unwrap());
    // When a is odd
    assert!(check_output_stream("
    VAR: Int a = 1;
    IF a MOD 2 == 0 THEN
    OUTPUT(\"even\");
    ELSE
    OUTPUT(\"odd\");
    ENDIF", vec!["odd"]).unwrap())
}

#[test]
fn else_if_statement_normal() {
    assert!(check_output_stream("
    VAR: Int a = 1;
    IF a MOD 4 == 0 THEN
    OUTPUT(\"multiple of 4\");
    ELSE IF a MOD 4 == 1 THEN
    OUTPUT(\"leaves a remainder of 1\");
    ELSE IF a MOD 4 == 2 THEN
    OUTPUT(\"leaves a remainder of 2\");
    ELSE
    OUTPUT(\"leaves a remainder of 3\");
    ENDIF
    ENDIF ENDIF", vec!["leaves a remainder of 1"]).unwrap());
    assert!(check_output_stream("
    VAR: Int a = 2;
    IF a MOD 4 == 0 THEN
    OUTPUT(\"multiple of 4\");
    ELSE IF a MOD 4 == 1 THEN
    OUTPUT(\"leaves a remainder of 1\");
    ELSE IF a MOD 4 == 2 THEN
    OUTPUT(\"leaves a remainder of 2\");
    ELSE
    OUTPUT(\"leaves a remainder of 3\");
    ENDIF
    ENDIF ENDIF", vec!["leaves a remainder of 2"]).unwrap());
    assert!(check_output_stream("
    VAR: Int a = 3;
    IF a MOD 4 == 0 THEN
    OUTPUT(\"multiple of 4\");
    ELSE IF a MOD 4 == 1 THEN
    OUTPUT(\"leaves a remainder of 1\");
    ELSE IF a MOD 4 == 2 THEN
    OUTPUT(\"leaves a remainder of 2\");
    ELSE
    OUTPUT(\"leaves a remainder of 3\");
    ENDIF
    ENDIF ENDIF", vec!["leaves a remainder of 3"]).unwrap());
    assert!(check_output_stream("
    VAR: Int a = 4;
    IF a MOD 4 == 0 THEN
    OUTPUT(\"multiple of 4\");
    ELSE IF a MOD 4 == 1 THEN
    OUTPUT(\"leaves a remainder of 1\");
    ELSE IF a MOD 4 == 2 THEN
    OUTPUT(\"leaves a remainder of 2\");
    ELSE
    OUTPUT(\"leaves a remainder of 3\");
    ENDIF
    ENDIF ENDIF", vec!["multiple of 4"]).unwrap());
}

#[test]
fn boolean_integer_if_error() {
    assert_eq!(check_output_stream("
    IF 1 THEN
    OUTPUT(\"even\");
    ENDIF", vec!["even"]).unwrap_err(),
        PseudocodeError::Type(TypeError::SingleExpected(Type::Boolean, Type::Integer)));
}

#[test]
fn boolean_integer_if_else_error() {
    assert_eq!(check_output_stream("
    IF 1 THEN
    OUTPUT(\"even\");
    ELSE
    OUTPUT(\"odd\");
    ENDIF", vec!["even"]).unwrap_err(),
        PseudocodeError::Type(TypeError::SingleExpected(Type::Boolean, Type::Integer)));
}
