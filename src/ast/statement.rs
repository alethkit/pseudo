use super::expression::Expression;

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    VarDeclaration(String, Expression),
    ConstDeclaraction(String, Expression),
    IfStatement {
        condition: Expression,
        body: Box<Statement>,
        alternative: Option<Box<Statement>>
    },
    Block(Vec<Statement>)
}
