use super::expression::Expression;

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    VarDeclaration(String, Expression),
    ConstDeclaraction(String, Expression),
    If {
        condition: Expression,
        body: Vec<Statement>,
        alternative: Option<Vec<Statement>>
    },
   // While {
    //    condition: Expression
   // },
}
