use super::literal::Literal;
use super::operator::{BinaryOperator, UnaryOperator};
use super::token::Token;
use super::types::{Type, Typed};

//enum Expression {
//    Literal(Literal),
//    Binary {
//        left: Box<Expression>,
//        op: BinaryOperator,
//        right: Box<Expression>,
//    },
//    Unary(UnaryOperator, Box<Expression>),
//    //Assignment(Token, Box<Expression>),
//    Grouping(Box<Expression>),
//}

//impl Typed for Expression {
//    fn get_type(&self) -> Type {
//        match self {
//            Expression::Literal(lit) => lit.get_type(),
//            Expression::Binary{left, op, right} => ,
//            Expression::Grouping(*exp) => exp.get_type(),
//        }
//    }
//}
//

pub trait Expression: Typed {
    fn evaluate(self) -> Literal;
}

struct UnaryExpression<Exp: Expression> {
    exp: Exp,
    op: UnaryOperator,
}

impl<Exp: Expression> Typed for UnaryExpression<Exp> {
    fn get_type(&self) -> Type {
        self.op.validate(&self.exp).unwrap() //Assume that the created value is sound
    }
}

impl <Exp: Expression> Expression for UnaryExpression<Exp> {
    fn evaluate(self) -> Literal {
        self.op.evaluate(self.exp)
    }
}

struct BinaryExpression<A: Expression, B: Expression> {
    left: A,
    op: BinaryOperator,
    right: B,
}

impl<A: Expression, B: Expression> Typed for BinaryExpression<A, B> {
    fn get_type(&self) -> Type {
        self.op.validate(&self.left, &self.right).unwrap() // Assume that the created value is sound
    }
}

struct Grouping<A: Expression> (A) ;

impl <Exp: Expression> Typed for Grouping<Exp> {
    fn get_type(&self) -> Type {
       self.0.get_type() 
    }

}
