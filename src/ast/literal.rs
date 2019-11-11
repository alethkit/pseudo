#[derive(Debug, PartialEq)]
enum Literal {    
    Integer(i64),
    Real(f64),
    Str(String), // Renamed to avoid namespace conflict with std::str::String
    Character(char),
    Boolean(bool)
}
