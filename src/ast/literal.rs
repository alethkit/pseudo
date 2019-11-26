use super::types::{Type, Typed};

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {    
    Integer(i64),
    Real(f64),
    Str(String), // Renamed to avoid namespace conflict with std::str::String
    Character(char),
    Boolean(bool), // True and False (Stored as the boolean value)
    List(Vec<Literal>)
}

impl From<Literal> for bool { // Should only be called when known if integer
    fn from(lit: Literal) -> Self { 
        match lit {
            Literal::Boolean(val) => val,
            _ => unreachable!() 
        }
    }
}
impl From<Literal> for i64 { // Should only be called when known if integer
    fn from(lit: Literal) -> Self { 
        match lit {
            Literal::Integer(val) => val,
            _ => unreachable!() 
        }
    }
}

impl Typed for Literal {

    fn get_type(&self) -> Type  {
        match self {
            Literal::Integer(_) => Type::Integer,
            Literal::Real(_) => Type::Real,
            Literal::Str(_) => Type::Str,
            Literal::Character(_) => Type::Character,
            Literal::Boolean(_) => Type::Boolean,
            Literal::List(l) => Type::List(Box::new(l.iter().next().unwrap().get_type()))
            // Assumes that array is non empty and that types are all equal.
        }
    }
}


