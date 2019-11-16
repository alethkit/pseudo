mod ast;
mod error;
mod lexer;
use lexer::{Lexer, LocatableChars, LocatableTokenResult};
use std::io;

fn main() {
    let mut test = String::new();
    println!("reading line");
    io::stdin().read_line(&mut test).unwrap();
    println!("line read. getting chars");
    let chars = LocatableChars::from(test.trim());
    println!("chars formed, lexing");
    let lex = Lexer::from(chars);
    let (tokens, errors): (Vec<_>, Vec<_>) = lex.partition(|(r, l)| r.is_ok());
    println!("lexing complete");
    if errors.is_empty() {
        println!("Tokens: {:#?}", tokens);
    } else {
        println!("Errors: {:#?}", errors);
    }
}
