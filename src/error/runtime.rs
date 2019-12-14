use core::num::{ParseFloatError, ParseIntError};
use std::char::CharTryFromError;
use std::io::Error as IoError;

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
    InvalidCharacter,
    IOError,
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
impl From<IoError> for RuntimeError {
    fn from(_: IoError) -> Self {
        Self::IOError
    }
}
