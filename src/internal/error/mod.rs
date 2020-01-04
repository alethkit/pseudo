mod io;
mod lex;
mod parser;
mod runtime;
mod types;

pub use io::IOError;
pub use lex::LexError;
pub use parser::ParserError;
pub use runtime::RuntimeError;
pub use types::TypeError;

#[derive(Debug, PartialEq)]
pub enum PseudocodeError {
    // Joint error, used for testing
    Lexing(LexError),
    Type(TypeError),
    Parsing(ParserError),
    Runtime(RuntimeError),
}

impl From<LexError> for PseudocodeError {
    fn from(e: LexError) -> Self {
        Self::Lexing(e)
    }
}
impl From<TypeError> for PseudocodeError {
    fn from(e: TypeError) -> Self {
        Self::Type(e)
    }
}
impl From<ParserError> for PseudocodeError {
    fn from(e: ParserError) -> Self {
        match e {
            ParserError::Typing(type_err) => Self::Type(type_err),
            parse_err => Self::Parsing(parse_err),
        }
    }
}
impl From<RuntimeError> for PseudocodeError {
    fn from(e: RuntimeError) -> Self {
        Self::Runtime(e)
    }
}
