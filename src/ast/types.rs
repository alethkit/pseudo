#[derive(PartialEq, Eq)]
pub enum Type {
    Integer,
    Real,
    Boolean,
    Character,
    Str,
//    List(Box<Type>)
}


pub trait Typed {
    fn get_type(&self) -> Type;
}
