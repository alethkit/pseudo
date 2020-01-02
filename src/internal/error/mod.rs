mod lex;
mod types;
mod parser;
mod runtime;

pub use lex::LexError;
pub use types::TypeError;
pub use parser::ParserError;
pub use runtime::RuntimeError;

#[derive(Debug)]
pub enum PseudocodeError {
    // Joint error, used for testing
    Lexing(LexError),
    Type(TypeError),
    Parsing(ParserError),
    Runtime(RuntimeError)
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
        Self::Parsing(e)
    } 
}
impl From<RuntimeError> for PseudocodeError {
    fn from(e: RuntimeError) -> Self {
        Self::Runtime(e)
    } 
}
