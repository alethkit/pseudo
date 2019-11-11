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
    let v: Vec<LocatableTokenResult> = lex.collect();
    println!("lexing complete");
    println!("{:#?}", v)
}
