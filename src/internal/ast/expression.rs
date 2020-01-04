use super::literal::Literal;
use super::operator::{BinaryOperator, UnaryOperator};
use super::types::{Type, Typed};
use crate::internal::environment::{EnvWrapper, Identifier};
use crate::internal::error::RuntimeError;
use crate::internal::Interpreter;
use std::convert::TryFrom;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum ExprIdentifier {
    Variable(String),
    Index(Box<ExprIdentifier>, Box<Expression>), // Created from index expression, so is integer
}

#[derive(Debug, Clone)]
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
    Call {
        callee: String,
        args: Vec<Expression>,
        return_type: Type,
    },
    // Not expected to be evaluated directly
    Subroutine(String, Type),     // User Defined
    NativeFunction(String, Type), // Built-in
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
            Expression::Assignment(_, exp) => exp.get_type(),
            Expression::Subroutine(_, _) | Expression::NativeFunction(_, _) => Type::NotTyped,
            Expression::Call { return_type, .. } => return_type.clone(),
        }
    }
}

impl Expression {
    pub fn evaluate(
        &self,
        env: EnvWrapper,
        interpreter: &mut Interpreter,
    ) -> Result<Literal, RuntimeError> {
        match self {
            Expression::Literal(lit) => Ok(lit.clone()),
            Expression::Binary { left, op, right } => op.evaluate(left, right, env, interpreter),
            Expression::Unary(op, exp) => op.evaluate(exp, env, interpreter),
            Expression::Grouping(expr) => expr.evaluate(env, interpreter),
            Expression::Variable(name, _) | Expression::Constant(name, _) => {
                env.borrow().get(&Identifier::from(name.to_string()))
            }
            Expression::Array(v) => Ok(Literal::List(
                v.into_iter()
                    .map(|l| l.evaluate(Rc::clone(&env), interpreter))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Expression::Assignment(ident, exp) => {
                let val = exp.evaluate(Rc::clone(&env), interpreter)?;
                let evaluated_ident = Identifier::try_from((ident, Rc::clone(&env), interpreter))?;
                env.borrow_mut().assign(&evaluated_ident, val.clone())?;
                Ok(val)
            }
            Expression::Subroutine(_, _) | Expression::NativeFunction(_, _) => {
                Err(RuntimeError::MustBeCalled)
            }
            Expression::Call { callee, args, .. } => {
                let evaluated_args: Vec<Literal> = args
                    .iter()
                    .map(|expr| expr.evaluate(Rc::clone(&env), interpreter))
                    .collect::<Result<_, _>>()?;
                interpreter
                    .get_callable(callee)
                    .call(evaluated_args, interpreter)
            }
        }
    }
}
