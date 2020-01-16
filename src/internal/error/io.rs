use std::io::Error as ConsoleIoError;
use std::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq)]
// Field left public so IOProvider implementations can construct IOError directly, rather than with
// constructor
pub struct IOError(pub &'static str);

impl Display for IOError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.0.fmt(f)
    }
}

impl From<ConsoleIoError> for IOError {
    fn from(_: ConsoleIoError) -> Self {
        Self("Error with getting user input from console")
    }
}
