use std::io::Error as ConsoleIoError;

#[derive(Debug, PartialEq)]
// Field left public so IOProvider implementations can construct IOError directly, rather than with
// constructor
pub struct IOError(pub &'static str);

impl From<ConsoleIoError> for IOError {
    fn from(_: ConsoleIoError) -> Self {
        Self("Error with getting user input from console")
    }
}
