use super::literal::Literal;
use super::operator::{BinaryOperator, UnaryOperator};
use super::types::{Type, Typed};

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Binary {
        left: Box<Expression>,
        op: BinaryOperator,
        right: Box<Expression>,
    },
    Unary(UnaryOperator, Box<Expression>),
    //Assignment(Token, Box<Expression>),
    Grouping(Box<Expression>),
}

impl Typed for Expression {
    fn get_type(&self) -> Type {
        match self {
            Expression::Literal(lit) => lit.get_type(),
            Expression::Unary(op, exp) => op.validate(exp).unwrap(), //Assume that the created value is sound
            Expression::Binary { left, op, right } => op.validate(left, right).unwrap(), // Assume that the created value is sound
            Expression::Grouping(exp) => exp.get_type(),
        }
    }
}

impl Expression {
    pub fn evaluate(self) -> Literal {
        match self {
            Expression::Literal(lit) => lit,
            Expression::Binary{left, op, right} => op.evaluate(*left, *right),
            Expression::Unary(op, exp) => op.evaluate(*exp),
            Expression::Grouping(expr) => expr.evaluate()

        }
    }
}

