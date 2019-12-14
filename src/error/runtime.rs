use core::num::{ParseFloatError, ParseIntError};
use std::char::CharTryFromError;

#[derive(Debug)]
pub enum RuntimeError {
    DivisionByZero,
    OutOfRange,
    UndefinedVariable,
    MustBeCalled,
    RangeStepCannotBeZero,
    InvalidRangeBound,
    IncorrectReturnExpression,
    InvalidInteger,
    InvalidReal,
    InvalidCharacter
}

impl From<ParseIntError> for RuntimeError {
    fn from(_: ParseIntError) -> Self {
        Self::InvalidInteger
    }
}
impl From<ParseFloatError> for RuntimeError {
    fn from(_: ParseFloatError) -> Self {
        Self::InvalidReal
    }
}
impl From<CharTryFromError> for RuntimeError {
    fn from(_: CharTryFromError) -> Self {
        Self::InvalidCharacter
    }
}
