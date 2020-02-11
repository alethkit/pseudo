/*
 The internal module isolates the internals of the interpreter from the GUI frontend.
 This allows for an alternative GUI to be implemented, or for the interpreter
 to be accessed from a console prompt.
 */
mod ast;
mod environment;
mod error;
mod interpreter;
mod lexer;
mod parser;

pub use interpreter::Interpreter;
pub use lexer::{Lexer, LocatableChars};
pub use parser::Parser;

// For implemntations of IOProvider. Is in internal so that RuntimeError can access it.
pub use error::IOError;

// Used for integration tests
pub use ast::{Literal, Type, Token, Callable};
pub use ast::Callable::Subroutine;
pub use environment::{EnvWrapper, Environment, Identifier};

pub use error::{PseudocodeError, RuntimeError, TypeError, ParserError};
