use super::literal::Literal;
use super::operator::{BinaryOperator, EvalError, UnaryOperator};
use super::types::{Type, Typed};
use crate::environment::EnvWrapper;
use std::rc::Rc;

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Binary {
        left: Box<Expression>,
        op: BinaryOperator,
        right: Box<Expression>,
    },
    Unary(UnaryOperator, Box<Expression>),
    Variable(String, Type),
    Constant(String, Type),
    Assignment(String, Box<Expression>),
    Grouping(Box<Expression>),
    Array(Vec<Expression>),
}

impl Typed for Expression {
    fn get_type(&self) -> Type {
        match self {
            Expression::Literal(lit) => lit.get_type(),
            Expression::Unary(op, exp) => op.validate(exp).unwrap(), //Assume that the created value is sound
            Expression::Binary { left, op, right } => op.validate(left, right).unwrap(), // Assume that the created value is sound
            Expression::Grouping(exp) => exp.get_type(),
            Expression::Variable(_, t) | Expression::Constant(_, t) => t.clone(),
            Expression::Array(v) => Type::List(Box::new(v.iter().next().unwrap().get_type())), // Assumes array is non empty
            Expression::Assignment(_,exp) => exp.get_type(),
        }
    }
}

impl Expression {
    pub fn evaluate(&self, env: EnvWrapper) -> Result<Literal, EvalError> {
        match self {
            Expression::Literal(lit) => Ok(lit.clone()),
            Expression::Binary { left, op, right } => op.evaluate(left, right, env),
            Expression::Unary(op, exp) => op.evaluate(exp, env),
            Expression::Grouping(expr) => expr.evaluate(env),
            Expression::Variable(name, _) | Expression::Constant(name, _) => {
                Ok(env.borrow().get(&name))
            }
            Expression::Array(v) => Ok(Literal::List(
                v.into_iter()
                    .map(|l| l.evaluate(Rc::clone(&env)))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Expression::Assignment(name, exp) => {
                let val = exp.evaluate(Rc::clone(&env))?;
                env.borrow_mut().assign(&name, val.clone())?;
                Ok(val)
            }
        }
    }
}
