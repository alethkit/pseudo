use super::expression::Expression;

pub enum Statement {
    Expression(Expression),
    VarDeclaration(String, Expression),
    ConstDeclaraction(String, Expression)
}
