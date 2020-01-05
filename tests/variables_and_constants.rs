mod common;
use common::{execute_with_environment, get_value_from_env};
use pseudocode::{Environment, Literal};


#[test]
fn integer_variable_declaration_normal() {
    let env_wrap = Environment::new_wrapper();
    let new_env_wrap = execute_with_environment("VAR: Int test_i = 1", env_wrap)?;
    assert_eq!(get_value_from_env(new_env_wrap, "test_i"), Literal::Integer(1))

}

