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

struct UnaryExpression<Exp: Expression, UnOp: UnaryOperator> {
    exp: Exp,
    op: UnOp,
}

impl<Exp: Expression, UnOp: UnaryOperator> Typed for UnaryExpression<Exp, UnOp> {
    fn get_type(&self) -> Type {
        self.op.validate(&self.exp).unwrap()
    }
}

impl <Exp: Expression, UnOp: UnaryOperator> Expression for UnaryExpression<Exp,UnOp> {
    fn evaluate(self) -> Literal {
        self.op.evaluate(self.exp)
    }
}

struct BinaryExpression<A: Expression, B: Expression, BinOp: BinaryOperator> {
    left: A,
    op: BinOp,
    right: B,
}

impl<A: Expression, B: Expression, BinOp: BinaryOperator> Typed for BinaryExpression<A, B, BinOp> {
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
