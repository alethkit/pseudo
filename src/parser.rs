use super::ast::{
    expression::Expression,
    location::Location,
    operator::{BinaryOperator, EvalError, UnaryOperator},
    statement::Statement,
    token::Token,
    types::{Type, TypeError, Typed},
};
use std::collections::HashMap;
use std::iter::Peekable;
use std::mem::discriminant;

pub type TypeScope = HashMap<String, Type>;
pub struct Parser<T>
where
    T: Iterator<Item = (Token, Location)>,
{
    tokens: Peekable<T>,
    type_scope: TypeScope,
}

impl<T> From<T> for Parser<T>
where
    T: Iterator<Item = (Token, Location)>,
{
    fn from(tokens: T) -> Self {
        Parser {
            tokens: tokens.peekable(),
            type_scope: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub enum ParserError {
    Typing(TypeError),
    Evaluation(EvalError),
    ExpectedExpression,
    Expected(Token),
    ExpectedOneOf(Vec<Token>),
    UnexpectedEndOfFile,
    UnterminatedArray, //Subset of unexcepted end of file, for clarification
    UndefinedVariable(String),
    UndefinedConstant(String),
    DifferentArrayTypes,
    ConstantsAreConstant, // A special case is added, if users forget that constants are constant.
    InvalidAssignmentTarget,
}

impl From<TypeError> for ParserError {
    fn from(err: TypeError) -> Self {
        ParserError::Typing(err)
    }
}
impl From<EvalError> for ParserError {
    fn from(err: EvalError) -> Self {
        ParserError::Evaluation(err)
    }
}

type LocResult<T> = Result<(T, Location), (ParserError, Location)>;
// The location field is duplicated so results can be handled using standard error operators.
// Had (Result<T< ParserError>, Location) been used, the `?` operator would not work directly.

macro_rules! recursive_descent {
    ($self:ident, $fallback:ident, $($ops:pat) | +, $add:stmt) => {{
        let (mut expr, loc) = $self.$fallback()?;
        while let Some((kind, loc2)) = $self.tokens.peek() {
            match kind {
                $($ops) | + => {
                    let (op_token, loc) = $self.tokens.next().unwrap(); //Safe to unwrap: peek ensures a value exists
                    let right = $self.$fallback()?.0;
                    $add;
                    let op = BinaryOperator::from(op_token);
                    println!("Left: {:#?}", expr);
                    println!("Right: {:#?}", right);
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

                _ => return Ok((expr, *loc2)),
            }
        }

        Ok((expr, loc))
    }
}}

impl<T> Parser<T>
where
    T: Iterator<Item = (Token, Location)>,
{
    const UnexpectedEOF: (ParserError, Location) = (
        ParserError::UnexpectedEndOfFile,
        Location { line: 0, column: 0 },
    );

    fn consume(&mut self, kind: Token) -> LocResult<Token> {
        let (test, loc) = self.tokens.next().ok_or(Parser::<T>::UnexpectedEOF)?;
        if discriminant(&test) == discriminant(&kind) {
            Ok((test, loc))
        } else {
            Err((ParserError::Expected(kind), loc))
        }
    }

    pub fn expression(&mut self) -> LocResult<Expression> {
        self.assignment()
    }

    fn assignment(&mut self) -> LocResult<Expression> {
        let (expr, loc) = self.logical_or()?;
        match self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)? {
            (Token::Equals, _) => {
                let (_, loc2) = self.tokens.next().unwrap();
                let (val, _) = self.assignment()?;
                match expr {
                    Expression::Variable(name, t) => {
                        let val_type = val.get_type();
                        if val.get_type() == t {
                            Ok((Expression::Assignment(name, Box::new(val)), loc2))
                        } else {
                            Err((
                                ParserError::Typing(TypeError::SingleExpected(t, val_type)),
                                loc2,
                            ))
                        }
                    }
                    Expression::Constant(_, _) => Err((ParserError::ConstantsAreConstant, loc2)),
                    _ => Err((ParserError::InvalidAssignmentTarget, loc2)),
                }
            }
            _ => Ok((expr, loc)),
        }
    }

    fn logical_or(&mut self) -> LocResult<Expression> {
        recursive_descent!(self, logical_and, Token::Or, ())
    }

    fn logical_and(&mut self) -> LocResult<Expression> {
        recursive_descent!(self, equality, Token::And, ())
    }
    fn equality(&mut self) -> LocResult<Expression> {
        recursive_descent!(self, comparison, Token::DoubleEqual | Token::NotEqual, ())
    }

    fn comparison(&mut self) -> LocResult<Expression> {
        recursive_descent!(
            self,
            sum,
            Token::LessThan | Token::LessEqual | Token::GreaterThan | Token::GreaterEqual,
            ()
        )
    }

    fn sum(&mut self) -> LocResult<Expression> {
        recursive_descent!(self, factor, Token::Plus | Token::Minus, ())
    }

    fn factor(&mut self) -> LocResult<Expression> {
        recursive_descent!(
            self,
            unary,
            Token::Star | Token::Slash | Token::Div | Token::Mod,
            ()
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
            _ => self.index(),
        }
    }

    fn index(&mut self) -> LocResult<Expression> {
        recursive_descent!(self, primary, Token::LeftBracket, {
            self.consume(Token::RightBracket)?;
        })
    }

    fn primary(&mut self) -> LocResult<Expression> {
        match self.tokens.next().ok_or(Parser::<T>::UnexpectedEOF)? {
            (Token::Literal(lit), loc) => Ok((Expression::Literal(lit), loc)),
            (Token::LeftParenthesis, _) => {
                let (expr, loc) = self.expression()?;
                match self.tokens.peek() {
                    Some((Token::RightParenthesis, _)) => {
                        self.tokens.next();
                        Ok((Expression::Grouping(Box::new(expr)), loc))
                    }
                    _ => Err((ParserError::Expected(Token::RightParenthesis), loc)),
                }
            }
            (Token::LeftBracket, loc) => self.list(loc),
            (Token::ConstIdentifier(name), loc) => match self.type_scope.get(&name) {
                Some(t) => Ok((Expression::Constant(name, t.clone()), loc)),
                None => Err((ParserError::UndefinedConstant(name), loc)),
            },
            (Token::VarIdentifier(name), loc) => match self.type_scope.get(&name) {
                Some(t) => Ok((Expression::Variable(name, t.clone()), loc)),
                None => Err((ParserError::UndefinedVariable(name), loc)),
            },

            (_, loc) => Err((ParserError::ExpectedExpression, loc)),
        }
    }

    fn list(&mut self, loc: Location) -> LocResult<Expression> {
        let mut prospective_list = Vec::new();
        prospective_list.push(self.expression()?);
        while let Some((kind, loc2)) = self.tokens.next() {
            match kind {
                Token::Comma => {
                    prospective_list.push(self.expression()?);
                }
                Token::RightBracket => {
                    let mut non_loc_list = prospective_list.iter().map(|c| &c.0); // This list is for checking types
                    let first_lit_type = non_loc_list
                        .next()
                        .ok_or((ParserError::UnterminatedArray, loc2))?
                        .get_type();
                    if non_loc_list.all(|lit| lit.get_type() == first_lit_type) {
                        return Ok((
                            Expression::Array(prospective_list.into_iter().map(|c| c.0).collect()),
                            loc2,
                        ));
                    } else {
                        return Err((ParserError::DifferentArrayTypes, loc2));
                    }
                }
                _ => {
                    return Err((
                        ParserError::ExpectedOneOf(vec![Token::Comma, Token::RightBracket]),
                        loc2,
                    ))
                }
            }
        }
        Err((ParserError::UnterminatedArray, loc))
    }

    fn expression_statement(&mut self) -> LocResult<Statement> {
        let (expr, loc) = self.expression()?;
        self.consume(Token::SemiColon)?;
        Ok((Statement::Expression(expr), loc))
    }

    fn var_declaration(&mut self) -> LocResult<Statement> {
        self.consume(Token::Colon)?;
        let (var_type, _) = self.type_hint()?;
        let (var_name, loc) = match self.consume(Token::VarIdentifier("".to_string()))? {
            (Token::VarIdentifier(var_name), loc) => (var_name, loc),
            _ => unreachable!("consume should have checked type"),
        };
        self.consume(Token::Equals)?;
        let (var_val, _) = self.expression()?;
        self.consume(Token::SemiColon)?;
        println!("{:#?}", var_val);
        let val_type = var_val.get_type();
        if val_type == var_type {
            self.type_scope.insert(var_name.clone(), var_type);
            Ok((Statement::VarDeclaration(var_name, var_val), loc))
        } else {
            Err((
                ParserError::Typing(TypeError::SingleExpected(var_type, val_type)),
                loc,
            ))
        }
    }
    fn constant_declaration(&mut self) -> LocResult<Statement> {
        self.consume(Token::Colon)?;
        let (const_type, _) = self.type_hint()?;
        let (const_name, loc) = match self.consume(Token::ConstIdentifier("".to_string()))? {
            (Token::ConstIdentifier(const_name), loc) => (const_name, loc),
            _ => unreachable!("consume should have checked type"),
        };
        self.consume(Token::Equals)?;
        let (const_val, _) = self.expression()?;
        self.consume(Token::SemiColon)?;
        let val_type = const_val.get_type();
        if val_type == const_type {
            self.type_scope.insert(const_name.clone(), const_type);
            Ok((Statement::ConstDeclaraction(const_name, const_val), loc))
        } else {
            Err((
                ParserError::Typing(TypeError::SingleExpected(const_type, val_type)),
                loc,
            ))
        }
    }

    fn block_statement(
        &mut self,
        end_tok: &[Token],
        fallback: fn(&mut Self) -> LocResult<Statement>,
    ) -> LocResult<Vec<Statement>> {
        let mut block = Vec::new();
        while let Some((kind, loc)) = self.tokens.peek() {
            println!("token: {:#?}", kind);
            if end_tok
                .iter()
                .any(|t| discriminant(t) == discriminant(kind))
            {
                // End token kept in stream
                return Ok((block, *loc));
            } else {
                let stmt = fallback(self)?;
                block.push(stmt.0)
            }
        }
        Err(Parser::<T>::UnexpectedEOF)
    }

    fn type_hint(&mut self) -> LocResult<Type> {
        // What allows us to identify type errors
        match self.tokens.next().ok_or(Parser::<T>::UnexpectedEOF)? {
            (t @ Token::Int, loc)
            | (t @ Token::Real, loc)
            | (t @ Token::Bool, loc)
            | (t @ Token::Char, loc)
            | (t @ Token::String, loc) => {
                let (mut cur_type, loc) = (Type::from(t), loc);
                while let (Token::LeftBracket, _) =
                    self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)?
                {
                    self.tokens.next();
                    self.consume(Token::RightBracket)?;
                    cur_type = Type::List(Box::new(cur_type));
                }
                Ok((cur_type, loc))
            }
            (_, loc) => Err((
                ParserError::ExpectedOneOf(vec![
                    Token::Int,
                    Token::Real,
                    Token::Bool,
                    Token::Char,
                    Token::String,
                ]),
                loc,
            )),
        }
    }

    fn if_statement(&mut self) -> LocResult<Statement> {
        let (condition, loc) = self.expression()?;
        let cond_type = condition.get_type();
        if cond_type != Type::Boolean {
            return Err((
                ParserError::Typing(TypeError::SingleExpected(Type::Boolean, cond_type)),
                loc,
            ));
        }
        //println!("expr parsed");
        self.consume(Token::Then)?;
        let (body, _) = self.block_statement(&[Token::Else, Token::EndIf], Parser::statement)?;
        let alternative =
            if let (Token::Else, _) = self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)? {
                println!("boop");
                self.tokens.next();
                Some(self.block_statement(&[Token::EndIf], Parser::statement)?)
            } else {
                None
            };
        self.consume(Token::EndIf)?;
        Ok((
            Statement::If {
                condition,
                body,
                alternative: alternative.map(|t| t.0),
            },
            loc,
        ))
    }

    fn while_statement(&mut self) -> LocResult<Statement> {
        let (condition, loc) = self.expression()?;
        let cond_type = condition.get_type();
        if cond_type != Type::Boolean {
            return Err((
                ParserError::Typing(TypeError::SingleExpected(Type::Boolean, cond_type)),
                loc,
            ));
        }
        self.consume(Token::Do)?;
        let (body, _) = self.block_statement(&[Token::EndWhile], Parser::statement)?;
        self.consume(Token::EndWhile)?;
        Ok((Statement::While { condition, body }, loc))
    }

    fn statement(&mut self) -> LocResult<Statement> {
        match self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)? {
            (Token::If, _) => {
                self.tokens.next();
                self.if_statement()
            }
            (Token::While, _) => {
                self.tokens.next();
                self.while_statement()
            }
            _ => self.expression_statement(),
        }
    }

    fn declaration(&mut self) -> LocResult<Statement> {
        match self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)? {
            (Token::Var, _) => {
                self.tokens.next();
                self.var_declaration()
            }
            (Token::Constant, _) => {
                self.tokens.next();
                self.constant_declaration()
            }
            _ => self.statement(),
        }
    }
}

impl<T> Iterator for Parser<T>
where
    T: Iterator<Item = (Token, Location)>,
{
    type Item = LocResult<Statement>;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.peek()?; // Checks if there are any more tokens. If there are none, returns none.
        Some(self.declaration())
    }
}
