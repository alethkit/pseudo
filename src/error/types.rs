
#[derive(Debug)]
pub enum TypeError{
    // Typing erros are in the form:
    // First field: expected type.
    // Second field: actual type.
    SingleExpected(Type, Type), // Unary operators that accept a single type
    SingleExpectedOneOf(Vec<Type>, Type), // Unary operators that accept multiple types
    DoubleExpected((Type, Type), (Type, Type)),
    DoubleExpectedOneOf(Vec<(Type, Type)>, (Type, Type)),
    UnequalTypes(Type, Type),
    ArgumentMismatch(Vec<Type>,Vec<Type>), // When argument lists do not match
    ArgumentMistatchOneOf(Vec<Vec<Type>>, Vec<Type>),
    InvalidReturnType(Type, Type)
}

