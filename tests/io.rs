mod common;
use common::{check_output_stream, check_output_stream_with_input, execute_with_input, get_value_from_env};
use pseudocode::Literal;

#[test]
fn output_normal() {
    assert!(check_output_stream("OUTPUT(\"foobar\");", vec!["foobar"]).unwrap())
}

#[test]
fn output_input_normal() {
    assert!(
        check_output_stream_with_input("OUTPUT(USERINPUT());", vec!["foobar"], vec!["foobar"])
            .unwrap()
    )
}

#[test]
fn user_input_normal() {
    let env_wrap = execute_with_input("VAR: String s = USERINPUT();", vec!["test123"]).unwrap();
    assert_eq!(get_value_from_env(env_wrap, "s"),
        Literal::Str("test123".to_owned()))

}

