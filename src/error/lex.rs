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
                _UnterminatedString => "Unterminated string. Did you forget to place a \"?",
                _UnterminatedChar => "Unterimated character. Did you forget to place a '?",
                _UnterminatedReal => "You have not input a decimal part after the decimal point.",
                _InvalidCharacter => "You have entered a character that cannot be recognised.",
            }
        )
    }
}

impl Error for LexError {}
