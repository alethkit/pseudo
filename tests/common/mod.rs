use pseudocode::{
    Callable, EnvWrapper, Environment, IOError, IOProvider, Identifier, Interpreter, Lexer,
    Literal, LocatableChars, Parser, PseudocodeError,
};

use std::borrow::ToOwned;
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

    pub fn from_input(input: Vec<&str>) -> Self {
        Self {
            input_stream: input.into_iter().map(ToOwned::to_owned).collect(),
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
    fn compare_output_stream(&self, out_stream: Vec<String>) -> bool {
        self.output_stream == out_stream
    }
}

pub fn evaluate_expression(expression_string: &str) -> Result<Literal, PseudocodeError> {
    let lexer = Lexer::from(LocatableChars::from(expression_string));
    let tokens = lexer
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| e.deloc())?;
    let mut parser = Parser::from(tokens.into_iter());
    let expression_result = parser.expression();
    let expression = expression_result
        .map(|r| r.deloc())
        .map_err(|r| r.deloc())?;
    let mut inter = Interpreter::new(Box::new(TestIOProvider::new()));
    inter
        .evaluate_expression(&expression)
        .map_err(PseudocodeError::from)
}

pub fn execute(contents: &str) -> Result<EnvWrapper, PseudocodeError> {
    let lexer = Lexer::from(LocatableChars::from(contents));
    let tokens = lexer
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| e.deloc())?;
    let parser = Parser::from(tokens.into_iter());
    let program_result: Result<Vec<_>, _> = parser.collect();
    let env = Environment::new_wrapper();
    match program_result {
        Ok(program) => {
            let mut inter =
                Interpreter::from_environment(Rc::clone(&env), Box::new(TestIOProvider::new()));
            inter.execute(&program.into_iter().map(|stmt| stmt.deloc()).collect())?;
            Ok(env)
        }
        Err(e) => Err(PseudocodeError::from(e.deloc())),
    }
}

pub fn execute_with_input(contents: &str, input: Vec<&str>) -> Result<EnvWrapper, PseudocodeError> {
    let lexer = Lexer::from(LocatableChars::from(contents));
    let tokens = lexer
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| e.deloc())?;
    let parser = Parser::from(tokens.into_iter());
    let program_result: Result<Vec<_>, _> = parser.collect();
    let env = Environment::new_wrapper();
    match program_result {
        Ok(program) => {
            let mut inter = Interpreter::from_environment(
                Rc::clone(&env),
                Box::new(TestIOProvider::from_input(input)),
            );
            inter.execute(&program.into_iter().map(|stmt| stmt.deloc()).collect())?;
            Ok(env)
        }
        Err(e) => Err(PseudocodeError::from(e.deloc())),
    }
}

pub fn get_callable(contents: &str, name: &str) -> Result<Callable, PseudocodeError> {
    let lexer = Lexer::from(LocatableChars::from(contents));
    let tokens = lexer
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| e.deloc())?;
    let parser = Parser::from(tokens.into_iter());
    let program_result: Result<Vec<_>, _> = parser.collect();
    let env = Environment::new_wrapper();
    match program_result {
        Ok(program) => {
            let mut inter =
                Interpreter::from_environment(Rc::clone(&env), Box::new(TestIOProvider::new()));
            inter.execute(&program.into_iter().map(|stmt| stmt.deloc()).collect())?;
            Ok(inter.get_callable(name))
        }
        Err(e) => Err(PseudocodeError::from(e.deloc())),
    }
}

pub fn get_value_from_env(env: EnvWrapper, name: &str) -> Literal {
    env.borrow()
        .get(&Identifier::from(name.to_owned()))
        .unwrap()
}

pub fn check_output_stream(
    contents: &str,
    expected_output: Vec<&str>,
) -> Result<bool, PseudocodeError> {
    let lexer = Lexer::from(LocatableChars::from(contents));
    let tokens = lexer
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| e.deloc())?;
    let parser = Parser::from(tokens.into_iter());
    let program_result: Result<Vec<_>, _> = parser.collect();
    match program_result {
        Ok(program) => {
            let prov = Box::new(TestIOProvider::new());
            let mut inter = Interpreter::new(prov);
            inter.execute(&program.into_iter().map(|stmt| stmt.deloc()).collect())?;
            Ok(inter.get_prov().compare_output_stream(
                expected_output.into_iter().map(ToOwned::to_owned).collect(),
            ))
        }
        Err(e) => Err(PseudocodeError::from(e.deloc())),
    }
}

pub fn check_output_stream_with_input(
    contents: &str,
    input: Vec<&str>,
    expected_output: Vec<&str>,
) -> Result<bool, PseudocodeError> {
    let lexer = Lexer::from(LocatableChars::from(contents));
    let tokens = lexer
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| e.deloc())?;
    let parser = Parser::from(tokens.into_iter());
    let program_result: Result<Vec<_>, _> = parser.collect();
    match program_result {
        Ok(program) => {
            let prov = Box::new(TestIOProvider::from_input(input));
            let mut inter = Interpreter::new(prov);
            inter.execute(&program.into_iter().map(|stmt| stmt.deloc()).collect())?;
            Ok(inter.get_prov().compare_output_stream(
                expected_output.into_iter().map(ToOwned::to_owned).collect(),
            ))
        }
        Err(e) => Err(PseudocodeError::from(e.deloc())),
    }
}
