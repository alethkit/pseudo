use super::expression::Expression;
use super::literal::Literal;
use super::token::Token;
use super::types::{Type, Typed};
use crate::internal::environment::EnvWrapper;
use crate::internal::interpreter::Interpreter;
use super::super::error::{RuntimeError, TypeError};

use std::rc::Rc;

#[derive(Debug, Copy, Clone)]
pub enum UnaryOperator {
    Not,
   Minus,
}

impl From<Token> for UnaryOperator {
    fn from(t: Token) -> Self {
        match t {
            Token::Minus => Self::Minus,
            Token::Not => Self::Not,
            _ => unreachable!(),
        }
    }
}

impl UnaryOperator {
    pub fn validate(&self, expr: &Expression) -> Result<Type, TypeError> {
        use Type::*;
        match self {
            UnaryOperator::Not => match expr.get_type() {
                Boolean => Ok(Boolean),
                t => Err(TypeError::SingleExpected(Boolean, t)),
            },
            UnaryOperator::Minus => match expr.get_type() {
                Integer => Ok(Integer),
                Real => Ok(Real),
                t => Err(TypeError::SingleExpectedOneOf(vec![Integer, Real], t)),
            },
        }
    }
   pub fn evaluate(&self, expr: &Expression, env: EnvWrapper, interpreter: &mut Interpreter) -> Result<Literal, RuntimeError> {
        match self {
            UnaryOperator::Not => match expr.evaluate(env, interpreter)? {
                Literal::Boolean(b) => Ok(Literal::Boolean(!b)),
                _ => unreachable!(),
            },
            UnaryOperator::Minus => match expr.evaluate(env, interpreter)? {
                Literal::Integer(int) => Ok(Literal::Integer(-int)),
                Literal::Real(rl) => Ok(Literal::Real(-rl)),
                _ => unreachable!(),
            },
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOperator {
    Or,
    And,
    Equality,
    Inequality,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    Add,
    Subtract,
    Multiply,
    Divide,
    IntDivide,
    Mod,
    Index
}

impl From<Token> for BinaryOperator {
    fn from(t: Token) -> Self {
        match t {
            Token::Or => Self::Or,
            Token::And => Self::And,
            Token::DoubleEqual => Self::Equality,
            Token::NotEqual => Self::Inequality,
            Token::LessThan => Self::LessThan,
            Token::GreaterThan => Self::GreaterThan,
            Token::LessEqual => Self::LessEqual,
            Token::GreaterEqual => Self::GreaterEqual,
            Token::Plus => Self::Add,
            Token::Minus => Self::Subtract,
            Token::Star => Self::Multiply,
            Token::Slash => Self::Divide,
            Token::Div => Self::IntDivide,
            Token::Mod => Self::Mod,
            Token::LeftBracket => Self::Index,
            _ => unreachable!()
        }
    }
}

impl BinaryOperator {
    pub fn validate(&self, expr1: &Expression, expr2: &Expression) -> Result<Type, TypeError> {
        // Although the expressions on both sides must be the same type in the language,
        // the implementation means that a binary expression and a literal are considered to be
        // different types, even though they may evaluate to the same type in the language.
        use Type::*;
        match self {
            // Although there only needs to be one level of match statements, there are two levels
            // to make the validation conditions for the code clearer.
            BinaryOperator::Or | BinaryOperator::And => {
                match (expr1.get_type(), expr2.get_type()) {
                    (Boolean, Boolean) => Ok(Boolean),
                    (t1, t2) => Err(TypeError::DoubleExpected((Boolean, Boolean), (t1, t2))),
                }
            }
            BinaryOperator::Equality | BinaryOperator::Inequality => {
                let (type1, type2) = (expr1.get_type(), expr2.get_type());
                if expr1.get_type() == expr2.get_type() {
                    Ok(Boolean)
                } else {
                    Err(TypeError::UnequalTypes(type1, type2))
                }
            }
            BinaryOperator::LessThan
            | BinaryOperator::GreaterThan
            | BinaryOperator::LessEqual
            | BinaryOperator::GreaterEqual => match (expr1.get_type(), expr2.get_type()) {
                (Integer, Integer) | (Real, Real) => Ok(Boolean),
                (t1, t2) => Err(TypeError::DoubleExpectedOneOf(
                    vec![(Integer, Integer), (Real, Real)],
                    (t1, t2),
                )),
            },
            BinaryOperator::Add => match (expr1.get_type(), expr2.get_type()) {
                (Str, Str) => Ok(Str),
                (Integer, Integer) => Ok(Integer),
                (Real, Real) => Ok(Real),
                (t1, t2) => Err(TypeError::DoubleExpectedOneOf(
                    vec![(Str, Str), (Integer, Integer), (Real, Real)],
                    (t1, t2),
                )),
            },
            BinaryOperator::Subtract | BinaryOperator::Multiply | BinaryOperator::Mod => {
                match (expr1.get_type(), expr2.get_type()) {
                    (Integer, Integer) => Ok(Integer),
                    (Real, Real) => Ok(Real),
                    (t1, t2) => Err(TypeError::DoubleExpectedOneOf(
                        vec![(Integer, Integer), (Real, Real)],
                        (t1, t2),
                    )),
                }
            }
            BinaryOperator::Divide => match (expr1.get_type(), expr2.get_type()) {
                (Integer, Integer) | (Real, Real) => Ok(Real),
                (t1, t2) => Err(TypeError::DoubleExpectedOneOf(
                    vec![(Integer, Integer), (Real, Real)],
                    (t1, t2),
                )),
            },
            BinaryOperator::IntDivide => match (expr1.get_type(), expr2.get_type()) {
                (Integer, Integer) | (Real, Real) => Ok(Integer),
                (t1, t2) => Err(TypeError::DoubleExpectedOneOf(
                    vec![(Integer, Integer), (Real, Real)],
                    (t1, t2),
                )),
            },
            BinaryOperator::Index => match (expr1.get_type(), expr2.get_type()) {
                (List(t), Integer) => Ok(*t),
                (t1, t2) => Err(TypeError::DoubleExpected((List(Box::new(Any)), Integer),(t1,t2)))
            }
        }
    }

    pub fn evaluate(&self, expr1: &Expression, expr2: &Expression, env: EnvWrapper, interpreter: &mut Interpreter) -> Result<Literal, RuntimeError> {
        let (val1, val2) = (expr1.evaluate(Rc::clone(&env), interpreter)?, expr2.evaluate(Rc::clone(&env), interpreter)?);
        match self {
            BinaryOperator::Equality => Ok(Literal::Boolean(val1 == val2)),
            BinaryOperator::Inequality => Ok(Literal::Boolean(val1 != val2)),
            BinaryOperator::Or => match (val1, val2) {
                (Literal::Boolean(a), Literal::Boolean(b)) => Ok(Literal::Boolean(a || b)),
                _ => unreachable!(),
            },
            BinaryOperator::And => match (val1, val2) {
                (Literal::Boolean(a), Literal::Boolean(b)) => Ok(Literal::Boolean(a && b)),
                _ => unreachable!(),
            },
            BinaryOperator::LessThan => match (val1, val2) {
                (Literal::Integer(a), Literal::Integer(b)) => Ok(Literal::Boolean(a < b)),
                (Literal::Real(a), Literal::Real(b)) => Ok(Literal::Boolean(a < b)),
                _ => unreachable!(),
            },
            BinaryOperator::GreaterThan => match (val1, val2) {
                (Literal::Integer(a), Literal::Integer(b)) => Ok(Literal::Boolean(a > b)),
                (Literal::Real(a), Literal::Real(b)) => Ok(Literal::Boolean(a > b)),
                _ => unreachable!(),
            },
            BinaryOperator::LessEqual => match (val1, val2) {
                (Literal::Integer(a), Literal::Integer(b)) => Ok(Literal::Boolean(a <= b)),
                (Literal::Real(a), Literal::Real(b)) => Ok(Literal::Boolean(a <= b)),
                _ => unreachable!(),
            },
            BinaryOperator::GreaterEqual => match (val1, val2) {
                (Literal::Integer(a), Literal::Integer(b)) => Ok(Literal::Boolean(a >= b)),
                (Literal::Real(a), Literal::Real(b)) => Ok(Literal::Boolean(a >= b)),
                _ => unreachable!(),
            },
            BinaryOperator::Add => match (val1, val2) {
                (Literal::Str(a), Literal::Str(b)) => Ok(Literal::Str(a + &b)),
                (Literal::Integer(a), Literal::Integer(b)) => Ok(Literal::Integer(a + b)),
                (Literal::Real(a), Literal::Real(b)) => Ok(Literal::Real(a + b)),
                _ => unreachable!(),
            },
            BinaryOperator::Subtract => match (val1, val2) {
                (Literal::Integer(a), Literal::Integer(b)) => Ok(Literal::Integer(a - b)),
                (Literal::Real(a), Literal::Real(b)) => Ok(Literal::Real(a - b)),
                _ => unreachable!(),
            },
            BinaryOperator::Multiply => match (val1, val2) {
                (Literal::Integer(a), Literal::Integer(b)) => Ok(Literal::Integer(a * b)),
                (Literal::Real(a), Literal::Real(b)) => Ok(Literal::Real(a * b)),
                _ => unreachable!(),
            },
            BinaryOperator::Divide => match (val1, val2) {
                (Literal::Integer(_a), Literal::Integer(0)) => Err(RuntimeError::DivisionByZero),
                (Literal::Real(_a), Literal::Real(0.0)) => Err(RuntimeError::DivisionByZero),
                (Literal::Integer(a), Literal::Integer(b)) => Ok(Literal::Real((a / b) as f64)),
                (Literal::Real(a), Literal::Real(b)) => Ok(Literal::Real(a / b)),
                _ => unreachable!(),
            },
            BinaryOperator::IntDivide => match (val1, val2) {
                (Literal::Integer(_a), Literal::Integer(0)) => Err(RuntimeError::DivisionByZero),
                (Literal::Real(_a), Literal::Real(0.0)) => Err(RuntimeError::DivisionByZero),
                (Literal::Integer(a), Literal::Integer(b)) => Ok(Literal::Integer(a / b)),
                (Literal::Real(a), Literal::Real(b)) => Ok(Literal::Integer((a / b) as i64)),
                _ => unreachable!(),
            },
            BinaryOperator::Mod => match (val1, val2) {
                (Literal::Integer(_a), Literal::Integer(0)) => Err(RuntimeError::DivisionByZero),
                (Literal::Real(_a), Literal::Real(0.0)) => Err(RuntimeError::DivisionByZero),
                (Literal::Integer(a), Literal::Integer(b)) => Ok(Literal::Integer(a % b)),
                (Literal::Real(a), Literal::Real(b)) => Ok(Literal::Real(a % b)),
                _ => unreachable!(),
            },
            BinaryOperator::Index => match (val1, val2) {
                (Literal::List(a), Literal::Integer(b)) => match a.get(b as usize) {
                    Some(v) => Ok(v.clone()),
                    None => Err(RuntimeError::OutOfRange)
                }
                _ => unreachable!()
            }
        }
    }
}
