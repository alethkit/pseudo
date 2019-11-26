mod ast;
mod environment;
mod error;
mod interpreter;
mod lexer;
mod parser;
use interpreter::Interpreter;
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
    let (tokens, errors): (Vec<_>, Vec<_>) = lex.partition(|(r, _l)| r.is_ok());
    println!("lexing complete");
    if errors.is_empty() {
         println!("Tokens: {:#?}", tokens);
        let pars = Parser::from(tokens.into_iter().map(|(r, l)| (r.unwrap(), l)));
        let program: Result<Vec<_>, _> = pars.collect();
        match program {
            Ok(stmts) => {
                println!("statements : {:#?}", stmts);
                let inter = Interpreter::new();
                if let Err(e) = inter.execute(stmts.into_iter().map(|c| c.0).collect()) {
                    println!("Runtime error: {:#?}", e);
                }
            }
            Err(e) => println!("Error: {:#?}", e),
        }
    } else {
        println!("Errors: {:#?}", errors);
    }
}
