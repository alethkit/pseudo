use super::super::ast::Type;
use std::fmt::{Display, Formatter, Result};
use std::string::ToString;

#[derive(Debug, PartialEq)]
pub enum TypeError {
    // Typing erros are in the form:
    // First field: expected type.
    // Second field: actual type.
    SingleExpected(Type, Type), // Unary operators that accept a single type
    SingleExpectedOneOf(Vec<Type>, Type), // Unary operators that accept multiple types
    DoubleExpected((Type, Type), (Type, Type)),
    DoubleExpectedOneOf(Vec<(Type, Type)>, (Type, Type)),
    UnequalTypes(Type, Type),
    ArgumentMismatch(Vec<Type>, Vec<Type>), // When argument lists do not match
    ArgumentMistatchOneOf(Vec<Vec<Type>>, Vec<Type>),
    InvalidReturnType(Type, Type),
}

impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::SingleExpected(t1, t2) => write!(f, "Expected a value of type {}, but instead got a value of type {}", t1, t2),
            Self::SingleExpectedOneOf(l1, t2) => write!(f, "Expected a value of one of the following types: {}, but instead got a value of type {}",
                l1.iter().map(ToString::to_string).collect::<Vec<_>>().join(","), t2),
            Self::DoubleExpected((t1,t2),(t3,t4)) => write!(f, "Expected values of types {} and {}, but instead got values of types {} and {}", t1, t2, t3, t4),
            Self::DoubleExpectedOneOf(l1, (t1, t2)) => write!(f, "Expected a value of any of the following type pairs: {}, but instead got values of types {} and {}",
                l1.iter().map(|(a,b)| a.to_string() + " and " + &b.to_string()).collect::<Vec<_>>().join(","),t1,t2),
            Self::UnequalTypes(t1,t2) => write!(f, "{} and {} are not the same type.", t1, t2),
            Self::ArgumentMismatch(l1,l2) => write!(f,"Expected the following arguments: {}, but instead got: {}",  l1.iter().map(ToString::to_string).collect::<Vec<_>>().join(","),
             l2.iter().map(ToString::to_string).collect::<Vec<_>>().join(",")),
             Self::ArgumentMistatchOneOf(l1, l2) => write!(f, "Expected one of the following argument lists: {}, but instead got: {}", l1.iter().map(|l| l.iter().map(ToString::to_string).collect::<Vec<_>>().join(",")).collect::<Vec<_>>().join(";") ,l2.iter().map(ToString::to_string).collect::<Vec<_>>().join(",")),
             Self::InvalidReturnType(t1, t2) => write!(f, "Expected a return type of {}, but instead got a return type of {}",t1,t2)
                

        }
    }
}
