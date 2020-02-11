/*
RuntimeError represens errors that can only be detected at runtime i.e cannot be detected
with static analysis and hence require evaluation.
*/
use super::IOError;
use core::num::{ParseFloatError, ParseIntError};
use std::char::CharTryFromError;
use std::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq)]
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
    StackOverflow,
    IOError(IOError),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::DivisionByZero => write!(f, "Division by zero is not allowed"),
            Self::OutOfRange => write!(f, "The value specified is outside the valid range"),
            Self::UndefinedVariable => write!(f, "An undefined variable has been referenced. Please define the variable."),
            Self::MustBeCalled => write!(f, "A function cannot be referenced directly. Call the function by using a pair of brackets e.g my_function()"),
            Self::RangeStepCannotBeZero => write!(f, "The step value in a for loop cannot be zero"),
            Self::InvalidRangeBound => write!(f, "The bounds given cannot result in a valid non-empty range"),
            Self::IncorrectReturnExpression => write!(f, "The value returned is not the same type as the declared return type"),
            Self::InvalidInteger => write!(f, "The given string cannot be parsed into an integer"),
            Self::InvalidReal => write!(f, "The given string cannot be parsed into a real"),
            Self::InvalidCharacter => write!(f, "The given integer cannot be converted into a valid character"),
            Self::StackOverflow => write!(f, "The call stack limit has been reached. If using a recursive function, please check it has a base case."),
            Self::IOError(e) => write!(f, "An error occured in user I/O: {}", e)
        }
    }
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

impl From<IOError> for RuntimeError {
    fn from(e: IOError) -> Self {
        Self::IOError(e)
    }
}
