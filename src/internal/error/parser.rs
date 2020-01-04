use super::super::ast::Token;
use super::TypeError;

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

impl From<TypeError> for ParserError {
    fn from(err: TypeError) -> Self {
        ParserError::Typing(err)
    }
}
