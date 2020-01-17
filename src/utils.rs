use pseudocode::{IOProvider, Interpreter, Lexer, LocatableChars, Parser};

pub fn run_program(contents: &str, mut provider: impl IOProvider + Clone + 'static) {
    //Runs the program given and interacts with the user via IOProvider.
    let chars = LocatableChars::from(contents);
    let lex = Lexer::from(chars);
    let (tokens, errors): (Vec<_>, Vec<_>) = lex.partition(Result::is_ok);
    if errors.is_empty() {
        let pars = Parser::from(tokens.into_iter().map(Result::unwrap));
        let program: Result<Vec<_>, _> = pars.collect();
        match program {
            Ok(stmts) => {
                let mut inter = Interpreter::new(Box::new(provider.clone()));
                if let Err(e) = inter.execute(&stmts.into_iter().map(|a| a.deloc()).collect()) {
                    provider.show_err(&format!("Runtime error: {}", e));
                }
            }
            Err(e) => provider.show_err(&format!("Error: {}", e)),
        }
    } else {
        let err_string = format!(
            "Errors: {}",
            errors
                .into_iter()
                .map(|r| r.unwrap_err().to_string())
                .collect::<Vec<_>>()
                .join(",")
        );
        provider.show_err(&err_string);
    }
}
