/*
LexError exists to represent errors that take place during lexical analysis.
*/
use std::error::Error;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum LexError {
    UnterminatedString,
    UnterminatedChar,
    UnterminatedReal,
    InvalidCharacter(char),
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::UnterminatedString =>
                    "Unterminated string. Did you forget to place a \"?".to_owned(),
                Self::UnterminatedChar =>
                    "Unterimated character. Did you forget to place a '?".to_owned(),
                Self::UnterminatedReal =>
                    "You have not input a decimal part after the decimal point.".to_owned(),
                Self::InvalidCharacter(c) =>
                    format!("The following character cannot be recognised: {}", c),
            }
        )
    }
}

impl Error for LexError {}
