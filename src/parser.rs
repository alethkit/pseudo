use super::ast::{
    expression::Expression,
    literal::Literal,
    location::Location,
    operator::{BinaryOperator, UnaryOperator},
    token::Token,
    types::TypeError,
};
use std::iter::Peekable;

pub struct Parser<T>
where
    T: Iterator<Item = (Token, Location)>,
{
    tokens: Peekable<T>,
}

impl<T> From<T> for Parser<T>
where
    T: Iterator<Item = (Token, Location)>,
{
    fn from(tokens: T) -> Self {
        Parser {
            tokens: tokens.peekable(),
        }
    }
}

#[derive(Debug)]
pub enum ParserError {
    Typing(TypeError),
    ExpectedExpression,
    Expected(Token),
    UnexpectedEndOfFile,
}

impl From<TypeError> for ParserError {
    fn from(err: TypeError) -> Self {
        ParserError::Typing(err)
    }
}

type LocResult<T> = Result<(T, Location), (ParserError, Location)>;
// The location field is duplicated so results can be handled using standard error operators.
// Had (Result<T< ParserError>, Location) been used, the `?` operator would not work directly.

macro_rules! recursive_descent {
    ($self:ident, $fallback:ident, $($ops:pat) | *) => {{
        let (mut expr, loc) = $self.$fallback()?;
        while let Some((kind, _)) = $self.tokens.peek() {
            match kind {
                $($ops) | * => {
                    let (op_token, loc) = $self.tokens.next().unwrap(); //Safe to unwrap: peek ensures a value exists
                    let right = $self.$fallback()?.0;
                    let op = BinaryOperator::from(op_token);
                    match op.validate(&expr, &right) {
                        Ok(_) => {
                            expr = Expression::Binary {
                                left: Box::new(expr),
                                op,
                                right: Box::new(right),
                            }
                        }
                        Err(e) => return Err((ParserError::from(e), loc)),
                    }
                }

                _ => return Ok((expr, loc)),
            }
        }

        Ok((expr, loc))
    }
}}

impl<T> Parser<T>
where
    T: Iterator<Item = (Token, Location)>,
{
    pub fn expression(&mut self) -> LocResult<Expression> {
        self.logical_or()
    }

    fn logical_or(&mut self) -> LocResult<Expression> {
        recursive_descent!(self, logical_and, Token::Or)
    }

    fn logical_and(&mut self) -> LocResult<Expression> {
        recursive_descent!(self, equality, Token::And)
    }
    fn equality(&mut self) -> LocResult<Expression> {
        recursive_descent!(self, comparison, Token::DoubleEqual | Token::NotEqual)
    }

    fn comparison(&mut self) -> LocResult<Expression> {
        recursive_descent!(
            self,
            sum,
            Token::LessThan | Token::LessEqual | Token::GreaterThan | Token::GreaterEqual
        )
    }

    fn sum(&mut self) -> LocResult<Expression> {
        recursive_descent!(self, factor, Token::Plus | Token::Minus)
    }

    fn factor(&mut self) -> LocResult<Expression> {
        recursive_descent!(
            self,
            unary,
            Token::Star | Token::Slash | Token::Div | Token::Mod
        )
    }

    fn unary(&mut self) -> LocResult<Expression> {
        match self.tokens.peek() {
            Some((Token::Not, _)) | Some((Token::Minus, _)) => {
                let (op_token, loc) = self.tokens.next().unwrap();
                let right = self.unary()?.0;
                let op = UnaryOperator::from(op_token);
                match op.validate(&right) {
                    Ok(_) => Ok((Expression::Unary(op, Box::new(right)), loc)),
                    Err(e) => Err((ParserError::from(e), loc)),
                }
            }
            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> LocResult<Expression> {
        match self.tokens.next() {
            Some((Token::Literal(lit), loc)) => Ok((Expression::Literal(lit), loc)),
            Some((Token::LeftParenthesis, _)) => {
                let (expr, loc) = self.expression()?;
                match self.tokens.peek() {
                    Some((Token::RightParenthesis, _)) => {
                        self.tokens.next();
                        Ok((Expression::Grouping(Box::new(expr)), loc))
                    }
                    _ => Err((ParserError::Expected(Token::RightParenthesis), loc)),
                }
            }
            Some((_, loc)) => Err((ParserError::ExpectedExpression, loc)),
            _ => Err((
                ParserError::UnexpectedEndOfFile,
                Location { line: 0, column: 0 },
            )),
        }
    }
}
