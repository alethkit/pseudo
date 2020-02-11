mod common;
use common::{evaluate_expression, execute, get_value_from_env};
use pseudocode::{Literal, PseudocodeError, RuntimeError, Type, TypeError};

#[test]
fn array_declaration_normal() {
    let new_env_wrap = execute("VAR: Int[] test_arr = [1,2,3];").unwrap();
    assert_eq!(
        get_value_from_env(new_env_wrap, "test_arr"),
        Literal::List(vec![
            Literal::Integer(1),
            Literal::Integer(2),
            Literal::Integer(3)
        ])
    )
}

#[test]
fn array_assignment_normal() {
    let new_env_wrap = execute("VAR: Int[] test_arr = [1,2,3]; test_arr = [1,2,3,4];").unwrap();
    assert_eq!(
        get_value_from_env(new_env_wrap, "test_arr"),
        Literal::List(vec![
            Literal::Integer(1),
            Literal::Integer(2),
            Literal::Integer(3),
            Literal::Integer(4)
        ])
    )
}

#[test]
fn integer_string_array_assignment_error() {
    let err =
        execute("VAR: Int[] test_arr = [1,2,3]; test_arr = [\"1\",\"2\",\"3\"];").unwrap_err();
    assert_eq!(
        err,
        PseudocodeError::Type(TypeError::SingleExpected(
            Type::List(Box::new(Type::Integer)),
            Type::List(Box::new(Type::Str))
        ))
    )
}

#[test]
fn array_index_normal() {
    assert_eq!(
        evaluate_expression("[1,2,3][0]").unwrap(),
        Literal::Integer(1)
    )
}

#[test]
fn array_index_out_of_range_error() {
    assert_eq!(
        evaluate_expression("[1,2,3][4]").unwrap_err(),
        PseudocodeError::Runtime(RuntimeError::OutOfRange)
    )
}

#[test]
fn array_index_assignment_normal() {
    let new_env_wrap = execute("VAR: Int[] test_arr = [1,2,3]; test_arr[0] = 5;").unwrap();
    assert_eq!(
        get_value_from_env(new_env_wrap, "test_arr"),
        Literal::List(vec![
            Literal::Integer(5),
            Literal::Integer(2),
            Literal::Integer(3)
        ])
    )
}

#[test]
fn two_dim_array_declaration_normal() {
    let new_env_wrap = execute("VAR: Int[][] test_arr = [[1,2,3],[4,5,6]];").unwrap();
    assert_eq!(
        get_value_from_env(new_env_wrap, "test_arr"),
        Literal::List(vec![
            Literal::List(vec![
                Literal::Integer(1),
                Literal::Integer(2),
                Literal::Integer(3)
            ]),
            Literal::List(vec![
                Literal::Integer(4),
                Literal::Integer(5),
                Literal::Integer(6)
            ])
        ])
    )
}

#[test]
fn two_dim_array_index_normal() {
    assert_eq!(
        evaluate_expression("[[1,2,3],[4,5,6]][1][2]").unwrap(),
        Literal::Integer(6)
    )
}

#[test]
fn two_dim_array_index_assignment_normal() {
    let new_env_wrap =
        execute("VAR: Int[][] test_arr = [[1,2,3],[4,5,6]]; test_arr[1][2] = 7;").unwrap();
    assert_eq!(
        get_value_from_env(new_env_wrap, "test_arr"),
        Literal::List(vec![
            Literal::List(vec![
                Literal::Integer(1),
                Literal::Integer(2),
                Literal::Integer(3)
            ]),
            Literal::List(vec![
                Literal::Integer(4),
                Literal::Integer(5),
                Literal::Integer(7)
            ])
        ])
    )
}

#[test]
fn array_len_normal() {
    assert_eq!(
        evaluate_expression("LEN([1,2,3])").unwrap(),
        Literal::Integer(3)
    )
}

#[test]
fn two_dim_array_len_normal() {
    assert_eq!(
        evaluate_expression("LEN([[1,2,3],[4,5,6]])").unwrap(),
        Literal::Integer(2)
    )
}
