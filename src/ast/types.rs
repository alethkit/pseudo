use super::token::Token;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Integer,
    Real,
    Boolean,
    Character,
    Str,
    List(Box<Type>),
    Any, // Used for list errors to avoid a clone.
    Void // Used for functions that do not return. Should not be used anywhere else.
}

impl From<Token> for Type {
    fn from(t: Token) -> Self {
        match t { // Meant to be used for tokens we know to be specifiers
            Token::Int => Self::Integer,
            Token::Real => Self::Real,
            Token::Bool => Self::Boolean,
            Token::Char => Self::Character,
            Token::String => Self::Str,
            _ => unreachable!()
        }
    }
}

#[derive(Debug)]
pub enum TypeError{
    SingleExpected(Type, Type), // Unary operators that accept a single type
    SingleExpectedOneOf(Vec<Type>, Type), // Unary operators that accept multiple types
    DoubleExpected((Type, Type), (Type, Type)),
    DoubleExpectedOneOf(Vec<(Type, Type)>, (Type, Type)),
    UnequalTypes(Type, Type)
}

pub trait Typed {
    fn get_type(&self) -> Type;
}
