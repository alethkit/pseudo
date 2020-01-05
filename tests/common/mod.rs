use pseudocode::{
    IOError, IOProvider, Interpreter, Lexer, Literal, LocatableChars, Parser, PseudocodeError, EnvWrapper, Identifier
};

use std::rc::Rc;

struct TestIOProvider {
    input_stream: Vec<String>,
    output_stream: Vec<String>,
    err_stream: Vec<String>,
}

impl TestIOProvider {
    pub fn new() -> Self {
        Self {
            input_stream: Vec::new(),
            output_stream: Vec::new(),
            err_stream: Vec::new(),
        }
    }

    pub fn add_input_string(&mut self, new_input: &str) {
        self.input_stream.push(new_input.to_owned());
    }

    pub fn get_output_string(&mut self) -> String {
        self.output_stream.remove(0)
    }

    pub fn get_err_string(&mut self) -> String {
        self.err_stream.remove(0)
    }
}

impl IOProvider for TestIOProvider {
    fn get_line(&mut self) -> Result<String, IOError> {
        Ok(self.input_stream.remove(0))
    }

    fn show_line(&mut self, msg: &str) {
        self.output_stream.push(msg.to_owned())
    }

    fn show_err(&mut self, msg: &str) {
        self.err_stream.push(msg.to_owned())
    }
}

pub fn evaluate_expression(expression_string: &str) -> Result<Literal, PseudocodeError> {
    let lexer = Lexer::from(LocatableChars::from(expression_string));
    let tokens = lexer.collect::<Result<Vec<_>, _>>().map_err(|(e, l)| e)?;
    let mut parser = Parser::from(tokens.into_iter());
    let expression_result = parser.expression();
    let expression = expression_result.map(|r| r.0).map_err(|r| r.0)?;
    let mut inter = Interpreter::new(Box::new(TestIOProvider::new()));
    inter
        .evaluate_expression(&expression)
        .map_err(PseudocodeError::from)
}

pub fn execute_with_environment(contents: &str, env: EnvWrapper) -> Result<EnvWrapper, PseudocodeError> {
    let lexer = Lexer::from(LocatableChars::from(contents));
    let tokens = lexer.collect::<Result<Vec<_>, _>>().map_err(|(e, _)| e)?;
    let parser = Parser::from(tokens.into_iter());
    let program_result : Result<Vec<_>,_> = parser.collect();
    match program_result {
        Ok(program) => {
            let mut inter = Interpreter::from_environment(Rc::clone(&env), Box::new(TestIOProvider::new()));
            inter.execute(&program.into_iter().map(|stmt| stmt.0).collect())?;
            Ok(env)
        },
        Err(e) => Err(PseudocodeError::from(e.0))
    }

}

pub fn get_value_from_env(env: EnvWrapper, name: &str) -> Literal {
    env.borrow().get(&Identifier::from(name.to_owned())).unwrap()
}
