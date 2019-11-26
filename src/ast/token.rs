use super::literal::Literal;

#[derive(Debug, PartialEq)]
pub enum Token {
    // Literals
    Literal(Literal),
    // Identifiers
    VarIdentifier(String), // Identifier used as postfix to tell apart from respective keywords
    ConstIdentifier(String),
    // Primtive types
    Int,
    Real,
    Bool,
    Char,
    String,
    //Operators (except assignment)
    Or,
    And,
    DoubleEqual,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Plus,
    Minus,
    Star,
    Slash,
    Div,
    Mod,
    Not,
    // Keywords
    Constant,
    Var,
    Subroutine,
    EndSubroutine,
    While,
    Do,
    EndWhile,
    EndDoWhile,
    For,
    To,
    Step,
    EndFor,
    If,
    Then,
    Else,
    EndIf,
    Return,
    // Single-character tokens (and the arrow)
    Colon,
    Comma,
    LeftBracket,
    RightBracket,
    Equals,
    SemiColon,
    LeftParenthesis,
    RightParenthesis,
    Arrow, // ->
}

impl From<char> for Token {
    fn from(c: char) -> Self {
        match c {
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::Slash,
            ':' => Token::Colon,
            ',' => Token::Comma,
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,
            '=' => Token::Equals,
            ';' => Token::SemiColon,
            '(' => Token::LeftParenthesis,
            ')' => Token::RightParenthesis,
            _ => unreachable!()
        }
    }
}

impl From<String> for Token {
    fn from(s: String) -> Self {
        match s.as_str() {
            "True" => Token::Literal(Literal::Boolean(true)),
            "False" => Token::Literal(Literal::Boolean(false)),
            "Int" => Token::Int,
            "Real" => Token::Real,
            "Bool" => Token::Bool,
            "Char" => Token::Char,
            "String" => Token::String,
            "OR" => Token::Or,
            "AND" => Token::And,
            "DIV" => Token::Div,
            "MOD" => Token::Mod,
            "NOT" => Token::Not,
            "CONSTANT" => Token::Constant,
            "VAR" => Token::Var,
            "SUBROUTINE" => Token::Subroutine,
            "ENDSUBROUTINE" => Token::EndSubroutine,
            "WHILE" => Token::While,
            "DO" => Token::Do,
            "ENDWHILE" => Token::EndWhile,
            "ENDDOWHILE" => Token::EndDoWhile,
            "FOR" => Token::For,
            "TO" => Token::To,
            "STEP" => Token::Step,
            "ENDFOR" => Token::EndFor,
            "IF" => Token::If,
            "THEN" => Token::Then,
            "ELSE" => Token::Else,
            "ENDIF" => Token::EndIf,
            "RETURN" => Token::Return,
            _ => {
                if s.chars().all(|c| c.is_ascii_uppercase() || c == '_') {
                    Token::ConstIdentifier(s)
                } else {
                    Token::VarIdentifier(s)
                }
            }
        }
    }
}
