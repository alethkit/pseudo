use super::ast::{Literal, Locatable, Location, Token};
use super::error::LexError;
use peeking_take_while::PeekableExt;
use std::iter::Peekable;
use std::str::Chars;


// LocatableChars assigns a location to each character.
// This means that the Lexer struct is only concerned with lexical analysis,
// and not assigning a location for tokens.
pub struct LocatableChars<'a> {
    line: u64,
    column: u64,
    chars: Peekable<Chars<'a>>,
}

impl<'a> From<&'a str> for LocatableChars<'a> {
    fn from(s: &'a str) -> Self {
        LocatableChars {
            line: 1,
            column: 0,
            chars: s.chars().peekable(),
        }
    }
}

impl Iterator for LocatableChars<'_> {
    type Item = (char, Location);

    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.peek()? {
            '\n' => {
                let cur_loc = Location::new(self.line, self.column);
                self.line += 1;
                self.column = 0;
                self.chars.next();
                // Newlines are replaced with a whitespace character so that
                // newlines act as a boundary between tokens.
                Some((' ', cur_loc))
            }
            _ => {
                self.column += 1;
                let c = self.chars.next().unwrap();
                Some((c, Location::new(self.line, self.column)))
            }
        }
    }
}

type TokenResult = Result<Token, LexError>;

pub struct Lexer<'a> {

    chars: Peekable<LocatableChars<'a>>,
}

impl<'a> From<LocatableChars<'a>> for Lexer<'a> {
    fn from(chars: LocatableChars<'a>) -> Self {
        Lexer {
            chars: chars.peekable(),
        }
    }
}

impl<'a> Lexer<'a> {
    fn skip_whitespace_and_comments(&mut self) {
        // Removes whitespace and comment lines from the stream of characters.
        // This ensures that commented lines are not treated as syntax.
        while let Some((c, l)) = self.chars.peek() {
            if c.is_whitespace() {
                self.chars.next();
            } else if *c == '#' {
                let current_line = l.line;
                while let Some((_sub_c, sub_l)) = self.chars.peek() {
                    if sub_l.line == current_line {
                        self.chars.next();
                    } else {
                        return;
                    }
                }
            } else {
                return;
            }
        }
    }

    fn two_char(&mut self, c: char, present: TokenResult, absent: TokenResult) -> TokenResult {
        // Returns present if c is the next character, and absent otherwise.
        self.chars.next();
        match self.chars.peek() {
            Some(&(b, _)) if b == c => {
                self.chars.next();
                present
            }
            _ => absent,
        }
    }

    fn collect_into_str(&mut self, cond: impl FnMut(&(char, Location)) -> bool) -> String
// collects characters from the input stream whilst a condition is met
    {
        self.chars
            .by_ref()
            .peeking_take_while(cond)
            .map(|(c, _)| c)
            .collect()
    }

    fn number(&mut self) -> TokenResult {
    //Parses number string into number token
        let num_str = self.collect_into_str(|(c, _)| c.is_ascii_digit() || *c == '.');
        match num_str.parse::<i64>() {
            Ok(int) => Ok(Token::Literal(Literal::Integer(int))),
            Err(_) => match num_str.parse::<f64>() {
                Ok(float) => Ok(Token::Literal(Literal::Real(float))),
                Err(_) => Err(LexError::UnterminatedReal),
            },
        }
    }
    fn char_literal(&mut self) -> TokenResult {
    // Parses char string into char literal
        self.chars.next();
        let (c, _) = self.chars.next().ok_or(LexError::UnterminatedChar)?;
        match self.chars.next() {
            Some(('\'', _)) => Ok(Token::Literal(Literal::Character(c))),
            _ => Err(LexError::UnterminatedChar),
        }
    }
    fn str_literal(&mut self) -> TokenResult {
        //Parses quoted string into string literal
        self.chars.next();
        let s = self.collect_into_str(|(c, _)| *c != '"');
        match self.chars.next() {
            Some(('"', _)) => Ok(Token::Literal(Literal::Str(s))),
            _ => Err(LexError::UnterminatedString),
        }
    }
}

impl Iterator for Lexer<'_> {
    // This is where the bulk of the lexical analysis takes place.
    // The Iterator trait (interface) specifies how the next token is to be fetched.
    // Helper methods then repeat this process for the entire stream
    // of characters.
    type Item = Result<Locatable<Token>, Locatable<LexError>>;
    fn next(&mut self) -> Option<Self::Item> {
        use LexError::*;
        use Token::*;
        self.skip_whitespace_and_comments();
        let (c, l) = *self.chars.peek()?;
        let val = match c {
            '+' | '*' | '/' | ':' | ',' | '[' | ']' | ';' | '(' | ')' => {
                Ok(Token::from(self.chars.next()?.0))
            }
            '<' => self.two_char('=', Ok(LessEqual), Ok(LessThan)),
            '>' => self.two_char('=', Ok(GreaterEqual), Ok(GreaterThan)),
            '-' => self.two_char('>', Ok(Arrow), Ok(Minus)),
            '!' => self.two_char('=', Ok(NotEqual), Err(InvalidCharacter(c))),
            '=' => self.two_char('=', Ok(DoubleEqual), Ok(Equals)),
            '0'..='9' => self.number(),
            '"' => self.str_literal(),
            '\'' => self.char_literal(),
            _ if c.is_ascii_alphabetic() => {
                Ok(Token::from(self.collect_into_str(|(c, _)| {
                    c.is_ascii_alphabetic() || *c == '_'
                })))
            }
            _ => {
                self.chars.next();
                Err(InvalidCharacter(c))
            }
        };
        Some(match val {
            Ok(t) => Ok(Locatable::new(t, l)),
            Err(e) => Err(Locatable::new(e, l)),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_single_token(s: &str, t: &Token) -> Result<(), LexError>
// Asserts that a string that contains a single token produces the expected token
    {
        let mut lex = Lexer::from(LocatableChars::from(s));
        let val = lex.next().unwrap().map_err(Locatable::deloc).map(Locatable::deloc)?;
        assert_eq!(&val, t);
        Ok(())
    }

    #[test]
    fn token_tests() -> Result<(), LexError> {
        use super::super::ast::Literal as Lit;
        use Token::*;
        let token_strs = vec![
            "12",
            "12.34",
            "\"bob\"",
            "'c'",
            "True",
            "False",
            "test_var",
            "PI",
            "Int",
            "Real",
            "Bool",
            "Char",
            "String",
            "OR",
            "AND",
            "==",
            "!=",
            "<",
            "<=",
            ">",
            ">=",
            "+",
            "-",
            "*",
            "/",
            "DIV",
            "MOD",
            "NOT",
            "CONSTANT",
            "VAR",
            "SUBROUTINE",
            "ENDSUBROUTINE",
            "WHILE",
            "DO",
            "ENDWHILE",
            "ENDDOWHILE",
            "FOR",
            "TO",
            "ENDFOR",
            "IF",
            "THEN",
            "ELSE",
            "ENDIF",
            "RETURN",
            ":",
            ",",
            "[",
            "]",
            "=",
            ";",
            "(",
            ")",
            "->",
        ];
        let token_vals = vec![
            Literal(Lit::Integer(12)),
            Literal(Lit::Real(12.34)),
            Literal(Lit::Str(std::string::String::from("bob"))),
            Literal(Lit::Character('c')),
            Literal(Lit::Boolean(true)),
            Literal(Lit::Boolean(false)),
            // Identifiers
            VarIdentifier(std::string::String::from("test_var")), // Identifier used as postfix to tell apart from respective keywords
            ConstIdentifier(std::string::String::from("PI")),
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
        ];
        let pairs = token_strs.iter().zip(token_vals);
        for (s, val) in pairs {
            assert_single_token(s, &val)?;
            assert_single_token(&("\n\n".to_owned() + s), &val)?;
            assert_single_token(&("  ".to_owned() + s + "\n\n"), &val)?;
            assert_single_token(&("\n\n".to_owned() + s + "\n\n"), &val)?;
        }
        Ok(())
    }

    #[test]
    fn plus() -> Result<(), LexError> {
        assert_single_token("+", &Token::Plus)?;
        assert_single_token("\n\n\n+", &Token::Plus)?;
        Ok(())
    }

    #[test]
    fn minus() -> Result<(), LexError> {
        assert_single_token("-", &Token::Minus)?;
        Ok(())
    }
}
