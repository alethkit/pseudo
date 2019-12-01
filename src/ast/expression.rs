use super::literal::Literal;
use super::operator::{BinaryOperator, EvalError, UnaryOperator};
use super::types::{Type, Typed};
use crate::environment::{EnvWrapper, Identifier};
use std::rc::Rc;
use std::convert::TryFrom;

#[derive(Debug)]
pub enum ExprIdentifier {
    Variable(String),
    Index(Box<ExprIdentifier>, Box<Expression>) // Created from index expression, so is integer
}

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
    Assignment(ExprIdentifier, Box<Expression>),
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
                env.borrow().get(&Identifier::from(name.to_string()))
            }
            Expression::Array(v) => Ok(Literal::List(
                v.into_iter()
                    .map(|l| l.evaluate(Rc::clone(&env)))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Expression::Assignment(ident, exp) => {
                let val = exp.evaluate(Rc::clone(&env))?;
                let evaluated_ident = Identifier::try_from((ident, Rc::clone(&env)))?;
                env.borrow_mut().assign(&evaluated_ident, val.clone())?;
                Ok(val)
            }
        }
    }
}
