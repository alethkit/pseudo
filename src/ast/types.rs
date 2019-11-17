#[derive(PartialEq, Eq, Debug)]
pub enum Type {
    Integer,
    Real,
    Boolean,
    Character,
    Str,
//    List(Box<Type>)
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
