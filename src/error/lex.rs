use std::fmt;
use std::error::Error;

#[derive(Debug)]
pub enum LexError {
    UnterminatedString,
    UnterminatedChar,
    UnterminatedReal,
    InvalidCharacter,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnterminatedString => "Unterminated string. Did you forget to place a \"?",
                UnterminatedChar => "Unterimated character. Did you forget to place a '?",
                UnterminatedReal => "You have not input a decimal part after the decimal point.",
                InvalidCharacter => "You have entered a character that cannot be recognised.",
            }
        )
    }
}

impl Error for LexError {}

//impl From<Result<_,LexError> for LexError {}
