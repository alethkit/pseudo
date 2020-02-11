mod common;
use common::{check_output_stream, get_callable};
use pseudocode::{ParserError, PseudocodeError, Subroutine, Type, TypeError};

#[test]
fn subroutine_declaration_normal() {
    let call = get_callable(
        "SUBROUTINE say_hi() OUTPUT(\"hi\"); ENDSUBROUTINE",
        "say_hi",
    )
    .unwrap();
    match call {
        Subroutine(sub) => assert_eq!(sub.name(), "say_hi"),
        _ => unreachable!(),
    }
}

#[test]
fn subroutine_return_value_normal() {
    let call = get_callable(
        "SUBROUTINE answer() -> Int RETURN 42; ENDSUBROUTINE",
        "answer",
    )
    .unwrap();
    match call {
        Subroutine(sub) => {
            assert_eq!(sub.name(), "answer");
            assert_eq!(sub.ret_type(), &Type::Integer)
        }
        _ => unreachable!(),
    }
}

#[test]
fn subroutine_parameters_normal() {
    let call = get_callable(
        "SUBROUTINE show_add(a: Int, b: Int) OUTPUT(INT_TO_STRING(a+b)); ENDSUBROUTINE",
        "show_add",
    )
    .unwrap();
    match call {
        Subroutine(sub) => {
            assert_eq!(sub.name(), "show_add");
            assert_eq!(sub.params(), vec!["a", "b"])
        }
        _ => unreachable!(),
    }
}

#[test]
fn subroutine_parameters_return_value_normal() {
    let call = get_callable(
        "SUBROUTINE add(a: Int, b:Int) -> Int RETURN a+b ; ENDSUBROUTINE",
        "add",
    )
    .unwrap();
    match call {
        Subroutine(sub) => {
            assert_eq!(sub.name(), "add");
            assert_eq!(sub.params(), vec!["a", "b"]);
            assert_eq!(sub.ret_type(), &Type::Integer)
        }
        _ => unreachable!(),
    }
}

#[test]
fn subroutine_early_return_normal() {
    assert!(check_output_stream("SUBROUTINE early_return() OUTPUT(\"Hello\"); RETURN; OUTPUT(\"World\"); ENDSUBROUTINE  early_return();", vec!["Hello"]).unwrap())
}

#[test]
fn subroutine_void_return_value_error() {
    let err = get_callable("SUBROUTINE answer() RETURN 42; ENDSUBROUTINE", "").unwrap_err();
    assert_eq!(
        err,
        PseudocodeError::Type(TypeError::InvalidReturnType(Type::Void, Type::Integer))
    )
}

#[test]
fn subroutine_no_return_error() {
    let err = get_callable("SUBROUTINE answer() ->Int ENDSUBROUTINE", "").unwrap_err();
    assert_eq!(
        err,
        PseudocodeError::Parsing(ParserError::SubroutineRequiresReturn)
    )
}
#[test]
fn subroutine_empty_return_error() {
    let err = get_callable("SUBROUTINE answer() ->Int RETURN; ENDSUBROUTINE", "").unwrap_err();
    assert_eq!(
        err,
        PseudocodeError::Type(TypeError::InvalidReturnType(Type::Integer, Type::Void))
    )
}

#[test]
fn subroutine_invalid_return_value_error() {
    let err = get_callable(
        "SUBROUTINE answer() -> Int RETURN \"42\"; ENDSUBROUTINE",
        "",
    )
    .unwrap_err();
    assert_eq!(
        err,
        PseudocodeError::Type(TypeError::InvalidReturnType(Type::Integer, Type::Str))
    )
}

#[test]
fn subroutine_call_normal() {
    assert!(check_output_stream(
        "SUBROUTINE answer() -> Int RETURN 42; ENDSUBROUTINE OUTPUT(INT_TO_STRING(answer()));",
        vec!["42"]
    )
    .unwrap())
}

#[test]
fn subroutine_call_parameters_normal() {
    assert!(check_output_stream(
            "SUBROUTINE add(a: Int, b:Int) ->Int RETURN a+b; ENDSUBROUTINE OUTPUT(INT_TO_STRING(add(2,3)));",
        vec!["5"]
    )
    .unwrap())
}

#[test]
fn subroutine_call_type_mismatch_error() {
    assert_eq!(check_output_stream(
            "SUBROUTINE add(a: Int, b:Int) ->Int RETURN a+b; ENDSUBROUTINE OUTPUT(INT_TO_STRING(add(\"2\",3)));",
        vec!["5"]
    )
    .unwrap_err(),
    PseudocodeError::Type(TypeError::ArgumentMismatch(vec![Type::Integer, Type::Integer],vec![Type::Str, Type::Integer]))
    )
}
