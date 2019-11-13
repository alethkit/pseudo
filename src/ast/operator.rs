use super::expression::Expression;
use super::literal::Literal;
use super::types::{Type, Typed};

//pub enum Operator {
//   Not, // Boolean -> Boolean
//    Or,
//    And,
//    Equality,
//    Inequality,
//    LessThan,
//    GreaterThan,
//    LessEqual,
//    GreaterEqual,
//    Add,
//    Minus, // Both
//    Multiply,
//    Divide,
//    IntDivide, // Integer division
//    Mod, // Remainder
//}
//
//
//

pub trait UnaryOperator {
    fn validate(&self, expr: &impl Expression) -> Option<Type>;

    fn evaluate(&self, expr: impl Expression) -> Literal;
}

pub trait BinaryOperator {
    fn validate(&self, expr1: &impl Expression, expr2: &impl Expression) -> Option<Type>;

    fn evaluate(&self, expr1: impl Expression, expr2: impl Expression) -> Literal;
}

struct Not {}

impl UnaryOperator for Not {
    fn validate(&self, expr: &impl Expression) -> Option<Type> {
        if expr.get_type() == Type::Boolean {
            Some(Type::Boolean)
        } else {
            None
        }
    }

    fn evaluate(&self, expr: impl Expression) -> Literal {
        match expr.evaluate() {
            Literal::Boolean(b) => Literal::Boolean(!b),
            _ => unreachable!("This should have been type checked"),
        }
    }
}


struct Or {}

impl Typed for Or {
    fn get_type(&self) -> Type {
        Type::Boolean
    }
}

impl BinaryOperator for Not {
    fn validate(&self, expr1: &impl Expression, expr2: &impl Expression) -> Option<Type> {
        if expr1.get_type() == expr2
    }

    fn evaluate(&self, expr1: impl Expression, expr2: impl Expression) -> Literal {

    }
}
