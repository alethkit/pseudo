use super::types::{Type, Typed};
use super::expression::Expression;

#[derive(Debug, PartialEq)]
pub enum Literal {    
    Integer(i64),
    Real(f64),
    Str(String), // Renamed to avoid namespace conflict with std::str::String
    Character(char),
    Boolean(bool) // True and False (Stored as the boolean value)
}

impl Typed for Literal {

    fn get_type(&self) -> Type  {
        match self {
            Literal::Integer(_) => Type::Integer,
            Literal::Real(_) => Type::Real,
            Literal::Str(_) => Type::Str,
            Literal::Character(_) => Type::Character,
            Literal::Boolean(_) => Type::Boolean
        }
    }
}


impl Expression for Literal {
    fn evaluate(self) -> Literal {
        self
    }
}
