use super::super::ast::Token;
use super::TypeError;
use std::fmt::{Display, Formatter, Result};
use std::string::ToString;

#[derive(Debug, PartialEq)]
pub enum ParserError {
    Typing(TypeError),
    ExpectedExpression,
    Expected(Token),
    ExpectedOneOf(Vec<Token>),
    UnexpectedEndOfFile,
    UndefinedVariable(String),
    UndefinedConstant(String),
    DifferentArrayTypes,
    ConstantsAreConstant, // A special case is added, if users forget that constants are constant.
    InvalidAssignmentTarget,
    NotACallable,
    IncorrectFunctionArity(usize, usize), // Wrong number of arguments passed
    AlreadyDefinedAsFunction,
    SubroutineRequiresReturn,
    ReturnOutsideSubroutine,
}


impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Typing(e) => e.fmt(f),
            Self::ExpectedExpression => write!(f, "Expected an expression"),
            Self::Expected(t) => write!(f, "Expected the following: {}", t),
            Self::ExpectedOneOf(l1) => write!(f, "Expected one of the following: {}", l1.iter().map(ToString::to_string).collect::<Vec<_>>().join(",")),
            Self::UnexpectedEndOfFile => write!(f, "The end of the file has been reached unexpectedly."),
            Self::UndefinedVariable(s) => write!(f, "The following variable has not been declared: {}", s),
            Self::UndefinedConstant(s) => write!(f, "The following constant has not been declared: {}", s),
            Self::DifferentArrayTypes => write!(f, "An array contains values of different types. Arrays can only contain values of one type."),
            Self::ConstantsAreConstant => write!(f, "You may not assign a new value to a constant."),
            Self::InvalidAssignmentTarget => write!(f, "You may only assign to a variable or a valid index of a variable."),
            Self::NotACallable => write!(f, "The expression given cannot be called, as it is not a subroutine or a native function."),
            Self::IncorrectFunctionArity(n1,n2) => write!(f, "The function you are trying to call expects {} arguments, but {} were provided.",n1,n2),
            Self::AlreadyDefinedAsFunction => write!(f, "The variable or constant you are trying to declare has already been defined as a functon."),
            Self::SubroutineRequiresReturn => write!(f, "This subroutine requires a return statement due to its return type, but none has been encountered."),
            Self::ReturnOutsideSubroutine => write!(f, "A return statement has been encountered outside a subroutine.")



        }
    }
}

impl From<TypeError> for ParserError {
    fn from(err: TypeError) -> Self {
        ParserError::Typing(err)
    }
}
