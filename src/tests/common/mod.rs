use pseudocode::error::PseudocodeError;
use pseudocode::{Interpreter, Parser, Lexer, LocatableChars};



fn evaluate_expression(expression_string: &str) -> Result<Literal, PseudocodeError> {
   let parser = Parser::from(Lexer::from::(LocatableChars::from(expression_string)).collect()?);
   let (expression,_) = parser.expression()?;
   let inter = Interpreter::new();
   inter.evaluate_expression(expression)
}
