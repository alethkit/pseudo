mod ast;
mod error;
mod lexer;
mod parser;
use lexer::{Lexer, LocatableChars};
use parser::Parser;
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
        let mut pars = Parser::from(tokens.into_iter().map(|(r, l)| (r.unwrap(), l)));
        match pars.expression() {
            Ok(exp) => {
                println!("Expression: {:#?}", exp);
                println!("Value: {:#?}",exp.0.evaluate() );
            },
            Err(e) => println!("Error: {:#?}", e)
        }
    } else {
        println!("Errors: {:#?}", errors);
    }
}
