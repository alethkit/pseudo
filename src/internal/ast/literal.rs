/*
Literal represents the basic values that can be used in the language.
*/
use super::types::{Type, Typed};
use std::fmt::{Display, Formatter, Result};
use std::string::ToString;

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(i64),
    Real(f64),
    Str(String), // Renamed to avoid namespace conflict with std::str::String
    Character(char),
    Boolean(bool), // True and False (Stored as the boolean value)
    List(Vec<Literal>),
    Void, // Is needed to represent the output of subroutines that do not return a literal.
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Integer(l) => l.fmt(f),
            Self::Real(l) => l.fmt(f),
            Self::Str(l) => l.fmt(f),
            Self::Character(l) => l.fmt(f),
            Self::Boolean(l) => l.fmt(f),
            Self::List(l_l) => write!(
                f,
                "[{}]",
                l_l.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            Self::Void => write!(f, "void literal"),
        }
    }
}

impl From<Literal> for bool {
    // Should only be called when known if integer
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::Boolean(val) => val,
            _ => unreachable!(),
        }
    }
}
impl From<Literal> for i64 {
    // Should only be called when known if integer
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::Integer(val) => val,
            _ => unreachable!(),
        }
    }
}

impl Typed for Literal {
    fn get_type(&self) -> Type {
        match self {
            Literal::Integer(_) => Type::Integer,
            Literal::Real(_) => Type::Real,
            Literal::Str(_) => Type::Str,
            Literal::Character(_) => Type::Character,
            Literal::Boolean(_) => Type::Boolean,
            Literal::List(l) => Type::List(Box::new(l.iter().next().unwrap().get_type())),
            Literal::Void => Type::Void, // Assumes that array is non empty and that types are all equal.
        }
    }
}
