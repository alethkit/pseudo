/*
 Tokens are the fundamental units that the parser interprets
 during syntax analysis. They are produced during the lexical analysis
 phase by the lexer.

 By using tokens, error messages can be specified to requiring a specific keyword,
 rather than to the character level, making them more useful for the users, and reducing
 the workload of the parser.
 */
use super::literal::Literal;
use std::fmt::{Display, Formatter, Result};

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

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Literal(lit) => lit.fmt(f),
            Self::VarIdentifier(s) | Self::ConstIdentifier(s) => s.fmt(f),
            Self::Int => write!(f, "Int keyword"),
            Self::Real => write!(f, "Real keyword"),
            Self::Bool => write!(f, "Bool keyword"),
            Self::Char => write!(f, "Char keyword"),
            Self::String => write!(f, "String keyword"),
            Self::Or => write!(f, "Or operator"),
            Self::And => write!(f, "And operator"),
            Self::DoubleEqual => write!(f, "Equality operator"),
            Self::NotEqual => write!(f, "Inequality operator"),
            Self::LessThan => write!(f, "Less than operator"),
            Self::LessEqual => write!(f, "Less or equal operator"),
            Self::GreaterThan => write!(f, "Greater than operator"),
            Self::GreaterEqual => write!(f, "Greater or equal operator"),
            Self::Plus => write!(f, "Plus"),
            Self::Minus => write!(f, "Minus"),
            Self::Star => write!(f, "Multiplication"),
            Self::Slash => write!(f, "Division"),
            Self::Div => write!(f, "Div"),
            Self::Mod => write!(f, "Mod"),
            Self::Not => write!(f, "Not"),
            Self::Constant => write!(f, "Constant"),
            Self::Var => write!(f, "Var"),
            Self::Subroutine => write!(f, "Subroutine"),
            Self::EndSubroutine => write!(f, "Endsubroutine"),
            Self::While => write!(f, "While"),
            Self::Do => write!(f, "Do"),
            Self::EndWhile => write!(f, "Endwhile"),
            Self::EndDoWhile => write!(f, "Enddowhile"),
            Self::For => write!(f, "For"),
            Self::To => write!(f, "To"),
            Self::Step => write!(f, "Step"),
            Self::EndFor => write!(f, "Endfor"),
            Self::If => write!(f, "If"),
            Self::Then => write!(f, "Then"),
            Self::Else => write!(f, "Else"),
            Self::EndIf => write!(f, "Endif"),
            Self::Return => write!(f, "Return"),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),
            Self::LeftBracket => write!(f, "["),
            Self::RightBracket => write!(f, "]"),
            Self::Equals => write!(f, "="),
            Self::SemiColon => write!(f, ";"),
            Self::LeftParenthesis => write!(f, "("),
            Self::RightParenthesis => write!(f, ")"),
            Self::Arrow => write!(f, "Arrow operator (->)"),
        }
    }
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
            _ => unreachable!(),
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
