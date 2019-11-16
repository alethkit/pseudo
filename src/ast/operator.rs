use super::expression::Expression;
use super::literal::Literal;
use super::types::Type;

pub enum UnaryOperator {
    Not,
    Minus,
}

impl UnaryOperator {
    pub fn validate(&self, expr: &impl Expression) -> Option<Type> {
        match self {
            UnaryOperator::Not => match expr.get_type() {
                Type::Boolean => Some(Type::Boolean),
                _ => None,
            },
            UnaryOperator::Minus => match expr.get_type() {
                Type::Integer => Some(Type::Integer),
                Type::Real => Some(Type::Real),
                _ => None,
            },
        }
    }
    pub fn evaluate(&self, expr: impl Expression) -> Literal {
        match self {
            UnaryOperator::Not => match expr.evaluate() {
                Literal::Boolean(b) => Literal::Boolean(!b),
                _ => unreachable!(),
            },
            UnaryOperator::Minus => match expr.evaluate() {
                Literal::Integer(int) => Literal::Integer(-int),
                Literal::Real(rl) => Literal::Real(-rl),
                _ => unreachable!(),
            },
        }
    }
}

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
    Minus,
    Multiply,
    Divide,
    IntDivide,
    Mod,
}

impl BinaryOperator {
    pub fn validate(&self, expr1: &impl Expression, expr2: &impl Expression) -> Option<Type> {
        // Although the expressions on both sides must be the same type,
        // the option to allow for operations on different types is allowed, should
        // preferences change in the future e.g adding reals and integers together.
        match self {
            // Although there only needs to be one level of match statements, there are two levels
            // to make the validation conditions for the code clearer.
            BinaryOperator::Or | BinaryOperator::And => {
                match (expr1.get_type(), expr2.get_type()) {
                    (Type::Boolean, Type::Boolean) => Some(Type::Boolean),
                    _ => None,
                }
            }
            BinaryOperator::Equality | BinaryOperator::Inequality => {
                if expr1.get_type() == expr2.get_type() {
                    Some(Type::Boolean)
                } else {
                    None
                }
            }
            BinaryOperator::LessThan
            | BinaryOperator::GreaterThan
            | BinaryOperator::LessEqual
            | BinaryOperator::GreaterEqual => match (expr1.get_type(), expr2.get_type()) {
                (Type::Integer, Type::Integer) | (Type::Real, Type::Real) => Some(Type::Boolean),
                _ => None,
            },
            BinaryOperator::Add => match (expr1.get_type(), expr2.get_type()) {
                (Type::Str, Type::Str) => Some(Type::Str),
                (Type::Integer, Type::Integer) => Some(Type::Integer),
                (Type::Real, Type::Real) => Some(Type::Real),
                _ => None,
            },
            BinaryOperator::Minus | BinaryOperator::Multiply | BinaryOperator::Mod => {
                match (expr1.get_type(), expr2.get_type()) {
                    (Type::Integer, Type::Integer) => Some(Type::Integer),
                    (Type::Real, Type::Real) => Some(Type::Real),
                    _ => None,
                }
            }
            BinaryOperator::Divide => match (expr1.get_type(), expr2.get_type()) {
                (Type::Integer, Type::Integer) | (Type::Real, Type::Real) => Some(Type::Real),
                _ => None,
            },
            BinaryOperator::IntDivide => match (expr1.get_type(), expr2.get_type()) {
                (Type::Integer, Type::Integer) | (Type::Real, Type::Real) => Some(Type::Integer),
                _ => None,
            },
        }
    }

    pub fn evaluate(&self, expr1: impl Expression, expr2: impl Expression) -> Literal {
        match self {
            BinaryOperator::Equality => Literal::Boolean(expr1.evaluate() == expr2.evaluate()),
            BinaryOperator::Inequality => Literal::Boolean(expr1.evaluate() != expr2.evaluate()),
            BinaryOperator::Or => match (expr1.evaluate(), expr2.evaluate()) {
                (Literal::Boolean(a), Literal::Boolean(b)) => Literal::Boolean(a || b),
                _ => unreachable!(),
            },
            BinaryOperator::And => match (expr1.evaluate(), expr2.evaluate()) {
                (Literal::Boolean(a), Literal::Boolean(b)) => Literal::Boolean(a && b),
                _ => unreachable!(),
            },
            BinaryOperator::LessThan => match (expr1.evaluate(), expr2.evaluate()) {
                (Literal::Integer(a), Literal::Integer(b)) => Literal::Boolean(a < b),
                (Literal::Real(a), Literal::Real(b)) => Literal::Boolean(a < b),
                _ => unreachable!(),
            },
            BinaryOperator::GreaterThan => match (expr1.evaluate(), expr2.evaluate()) {
                (Literal::Integer(a), Literal::Integer(b)) => Literal::Boolean(a > b),
                (Literal::Real(a), Literal::Real(b)) => Literal::Boolean(a > b),
                _ => unreachable!(),
            },
            BinaryOperator::LessEqual => match (expr1.evaluate(), expr2.evaluate()) {
                (Literal::Integer(a), Literal::Integer(b)) => Literal::Boolean(a <= b),
                (Literal::Real(a), Literal::Real(b)) => Literal::Boolean(a <= b),
                _ => unreachable!(),
            },
            BinaryOperator::GreaterEqual => match (expr1.evaluate(), expr2.evaluate()) {
                (Literal::Integer(a), Literal::Integer(b)) => Literal::Boolean(a >= b),
                (Literal::Real(a), Literal::Real(b)) => Literal::Boolean(a >= b),
                _ => unreachable!(),
            },
            BinaryOperator::Add => match (expr1.evaluate(), expr2.evaluate()) {
                (Literal::Str(a), Literal::Str(b)) => Literal::Str(a + &b),
                (Literal::Integer(a), Literal::Integer(b)) => Literal::Integer(a + b),
                (Literal::Real(a), Literal::Real(b)) => Literal::Real(a + b),
                _ => unreachable!(),
            },
            BinaryOperator::Minus => match (expr1.evaluate(), expr2.evaluate()) {
                (Literal::Integer(a), Literal::Integer(b)) => Literal::Integer(a - b),
                (Literal::Real(a), Literal::Real(b)) => Literal::Real(a - b),
                _ => unreachable!(),
            },
            BinaryOperator::Multiply => match (expr1.evaluate(), expr2.evaluate()) {
                (Literal::Integer(a), Literal::Integer(b)) => Literal::Integer(a * b),
                (Literal::Real(a), Literal::Real(b)) => Literal::Real(a * b),
                _ => unreachable!(),
            },
            BinaryOperator::Divide => match (expr1.evaluate(), expr2.evaluate()) {
                (Literal::Integer(a), Literal::Integer(b)) => Literal::Real((a / b) as f64),
                (Literal::Real(a), Literal::Real(b)) => Literal::Real(a / b),
                _ => unreachable!(),
            },
            BinaryOperator::IntDivide => match (expr1.evaluate(), expr2.evaluate()) {
                (Literal::Integer(a), Literal::Integer(b)) => Literal::Integer(a / b),
                (Literal::Real(a), Literal::Real(b)) => Literal::Integer((a / b) as i64),
                _ => unreachable!(),
            },
            BinaryOperator::Mod => match (expr1.evaluate(), expr2.evaluate()) {
                (Literal::Integer(a), Literal::Integer(b)) => Literal::Integer(a % b),
                (Literal::Real(a), Literal::Real(b)) => Literal::Real(a % b),
                _ => unreachable!(),
            },
        }
    }
}

//pub trait UnaryOperator {
//
//    fn validate(&self, expr: &impl Expression) -> Option<Type>;
//
//    fn evaluate(&self, expr: impl Expression) -> Literal;
//}
//
//pub trait BinaryOperator {
//    fn validate(&self, expr1: &impl Expression, expr2: &impl Expression) -> Option<Type>;
//
//    fn evaluate(&self, expr1: impl Expression, expr2: impl Expression) -> Literal;
//}
//
//struct Not {}
//
//impl UnaryOperator for Not {
//    fn validate(&self, expr: &impl Expression) -> Option<Type> {
//        if expr.get_type() == Type::Boolean {
//            Some(Type::Boolean)
//        } else {
//            None
//        }
//    }
//
//    fn evaluate(&self, expr: impl Expression) -> Literal {
//        match expr.evaluate() {
//            Literal::Boolean(b) => Literal::Boolean(!b),
//            _ => unreachable!(),
//        }
//    }
//}
//
//struct Or {}
//
//impl BinaryOperator for Or {
//    fn validate(&self, expr1: &impl Expression, expr2: &impl Expression) -> Option<Type> {
//        if expr1.get_type() == Type::Boolean && expr2.get_type() == Type::Boolean {
//            Some(Type::Boolean)
//        } else {
//            None
//        }
//    }
//
//    fn evaluate(&self, expr1: impl Expression, expr2: impl Expression) -> Literal {
//        match (expr1.evaluate(), expr2.evaluate()) {
//            (Literal::Boolean(a), Literal::Boolean(b)) => Literal::Boolean(a || b),
//            _ => unreachable!(),
//        }
//    }
//}
//
//struct And {}
//
//impl BinaryOperator for And {
//    fn validate(&self, expr1: &impl Expression, expr2: &impl Expression) -> Option<Type> {
//        if expr1.get_type() == Type::Boolean && expr2.get_type() == Type::Boolean {
//            Some(Type::Boolean)
//        } else {
//            None
//        }
//    }
//
//    fn evaluate(&self, expr1: impl Expression, expr2: impl Expression) -> Literal {
//        match (expr1.evaluate(), expr2.evaluate()) {
//            (Literal::Boolean(a), Literal::Boolean(b)) => Literal::Boolean(a && b),
//            _ => unreachable!(),
//        }
//    }
//}
//
//struct Equality {}
//
//impl BinaryOperator for Equality {
//    fn validate(&self, expr1: &impl Expression, expr2: &impl Expression) -> Option<Type> {
//        if expr1.get_type() == expr2.get_type() {
//            Some(Type::Boolean)
//        } else {
//            None
//        }
//    }
//
//    fn evaluate(&self, expr1: impl Expression, expr2: impl Expression) -> Literal {
//        Literal::Boolean(expr1.evaluate() == expr2.evaluate())
//    }
//}
//
//struct Inequality {}
//
//impl BinaryOperator for Inequality {
//    fn validate(&self, expr1: &impl Expression, expr2: &impl Expression) -> Option<Type> {
//        if expr1.get_type() == expr2.get_type() {
//            Some(Type::Boolean)
//        } else {
//            None
//        }
//    }
//
//    fn evaluate(&self, expr1: impl Expression, expr2: impl Expression) -> Literal {
//        Literal::Boolean(expr1.evaluate() != expr2.evaluate())
//    }
//}
//
//struct LessThan {}
//
//impl BinaryOperator for LessThan {
//    fn validate(&self, expr1: &impl Expression, expr2: &impl Expression) -> Option<Type> {
//        match (expr1.get_type(), expr2.get_type()) {
//            (Type::Integer, Type::Integer) | (Type::Real, Type::Real) => Some(Type::Boolean),
//            _ => None,
//        }
//    }
//
//    fn evaluate(&self, expr1: impl Expression, expr2: impl Expression) -> Literal {
//        match (expr1.evaluate(), expr2.evaluate()) {
//            (Literal::Integer(int_a), Literal::Integer(int_b)) => Literal::Boolean(int_a < int_b),
//            (Literal::Real(rl_a), Literal::Real(rl_b)) => Literal::Boolean(rl_a < rl_b),
//            _ => unreachable!(),
//        }
//    }
//}
//
//struct GreaterThan {}
//
//impl BinaryOperator for GreaterThan {
//    fn validate(&self, expr1: &impl Expression, expr2: &impl Expression) -> Option<Type> {
//        match (expr1.get_type(), expr2.get_type()) {
//            (Type::Integer, Type::Integer) | (Type::Real, Type::Real) => Some(Type::Boolean),
//            _ => None,
//        }
//    }
//
//    fn evaluate(&self, expr1: impl Expression, expr2: impl Expression) -> Literal {
//        match (expr1.evaluate(), expr2.evaluate()) {
//            (Literal::Integer(int_a), Literal::Integer(int_b)) => Literal::Boolean(int_a > int_b),
//            (Literal::Real(rl_a), Literal::Real(rl_b)) => Literal::Boolean(rl_a > rl_b),
//            _ => unreachable!(),
//        }
//    }
//}
//
//struct LessEqual {}
//
//impl BinaryOperator for LessEqual {
//    fn validate(&self, expr1: &impl Expression, expr2: &impl Expression) -> Option<Type> {
//        match (expr1.get_type(), expr2.get_type()) {
//            (Type::Integer, Type::Integer) | (Type::Real, Type::Real) => Some(Type::Boolean),
//            _ => None,
//        }
//    }
//
//    fn evaluate(&self, expr1: impl Expression, expr2: impl Expression) -> Literal {
//        match (expr1.evaluate(), expr2.evaluate()) {
//            (Literal::Integer(int_a), Literal::Integer(int_b)) => Literal::Boolean(int_a <= int_b),
//            (Literal::Real(rl_a), Literal::Real(rl_b)) => Literal::Boolean(rl_a <= rl_b),
//            _ => unreachable!(),
//        }
//    }
//}
//
//struct GreaterEqual {}
//
//impl BinaryOperator for GreaterEqual {
//    fn validate(&self, expr1: &impl Expression, expr2: &impl Expression) -> Option<Type> {
//        match (expr1.get_type(), expr2.get_type()) {
//            (Type::Integer, Type::Integer) | (Type::Real, Type::Real) => Some(Type::Boolean),
//            _ => None,
//        }
//    }
//    fn evaluate(&self, expr1: impl Expression, expr2: impl Expression) -> Literal {
//
//    }
//}
