mod common;
use common::{execute_with_environment, get_value_from_env};
use pseudocode::{Environment, Literal, ParserError, PseudocodeError, Token, Type, TypeError};

use std::rc::Rc;

#[test]
fn integer_variable_declaration_normal() {
    let env_wrap = Environment::new_wrapper();
    let new_env_wrap = execute_with_environment("VAR: Int test_i = 1;", env_wrap).unwrap();
    assert_eq!(
        get_value_from_env(new_env_wrap, "test_i"),
        Literal::Integer(1)
    )
}

#[test]
fn real_variable_declaration_normal() {
    let env_wrap = Environment::new_wrapper();
    let new_env_wrap = execute_with_environment("VAR: Real test_r = 1.5;", env_wrap).unwrap();
    assert_eq!(
        get_value_from_env(new_env_wrap, "test_r"),
        Literal::Real(1.5)
    )
}

#[test]
fn boolean_variable_declaration_normal() {
    let env_wrap = Environment::new_wrapper();
    let new_env_wrap = execute_with_environment("VAR: Bool test_b = True;", env_wrap).unwrap();
    assert_eq!(
        get_value_from_env(new_env_wrap, "test_b"),
        Literal::Boolean(true)
    )
}

#[test]
fn char_variable_declaration_normal() {
    let env_wrap = Environment::new_wrapper();
    let new_env_wrap = execute_with_environment("VAR: Char test_c = 'c';", env_wrap).unwrap();
    assert_eq!(
        get_value_from_env(new_env_wrap, "test_c"),
        Literal::Character('c')
    )
}

#[test]
fn string_variable_declaration_normal() {
    let env_wrap = Environment::new_wrapper();
    let new_env_wrap = execute_with_environment("VAR: String test_s = \"foo\";", env_wrap).unwrap();
    assert_eq!(
        get_value_from_env(new_env_wrap, "test_s"),
        Literal::Str("foo".to_owned())
    )
}

#[test]
fn constant_declaration_normal() {
    let env_wrap = Environment::new_wrapper();
    let new_env_wrap = execute_with_environment("CONSTANT: Int ANSWER = 42;", env_wrap).unwrap();
    assert_eq!(
        get_value_from_env(new_env_wrap, "ANSWER"),
        Literal::Integer(42)
    )
}

#[test]
fn type_mismatch_variable_declaration_error() {
    let env_wrap = Environment::new_wrapper();
    let err = execute_with_environment("VAR: Int not_int = \"bob\";", env_wrap).unwrap_err();
    assert_eq!(
        err,
        PseudocodeError::Type(TypeError::SingleExpected(Type::Integer, Type::Str))
    )
}

#[test]
fn constant_identifier_variable_declaration_error() {
    let env_wrap = Environment::new_wrapper();
    let err = execute_with_environment("VAR: String NOT_VAR = \"bob\";", env_wrap).unwrap_err();
    assert_eq!(
        err,
        PseudocodeError::Parsing(ParserError::Expected(Token::VarIdentifier(String::new())))
    )
}

#[test]
fn variable_identifier_constant_declaration_error() {
    let env_wrap = Environment::new_wrapper();
    let err =
        execute_with_environment("CONSTANT: String not_con = \"bob\";", env_wrap).unwrap_err();
    assert_eq!(
        err,
        PseudocodeError::Parsing(ParserError::Expected(Token::ConstIdentifier(String::new())))
    )
}

#[test]
fn integer_variable_assignment_normal() {
    let env_wrap = Environment::new_wrapper();
    let new_env_wrap =
        execute_with_environment("VAR: Int test_i = 1; test_i = 2;", Rc::clone(&env_wrap)).unwrap();
    assert_eq!(
        get_value_from_env(Rc::clone(&new_env_wrap), "test_i"),
        Literal::Integer(2)
    );
}

#[test]
fn type_mismatch_variable_assignment_error() {
    let env_wrap = Environment::new_wrapper();
    let err = execute_with_environment("VAR: Real test_r = 1.5; test_r = \"bar\";", env_wrap)
        .unwrap_err();
    assert_eq!(
        err,
        PseudocodeError::Type(TypeError::SingleExpected(Type::Real, Type::Str))
    )
}

#[test]
fn constant_assignment_error() {
    let env_wrap = Environment::new_wrapper();
    let err =
        execute_with_environment("CONSTANT: Int ANSWER = 42; ANSWER = 43;", env_wrap).unwrap_err();
    assert_eq!(
        err,
        PseudocodeError::Parsing(ParserError::ConstantsAreConstant)
    )
}

