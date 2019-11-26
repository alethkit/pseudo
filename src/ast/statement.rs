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
    While {
        condition: Expression,
        body: Vec<Statement>
    },
    DoWhile {
        condition: Expression,
        body: Vec<Statement>
    },
    For { //For loop is INCLUSIVE
        loop_var: String,
        initial_val: Expression,
        end_val: Expression,
        step_val: Expression, // default value is 1
        body: Vec<Statement>
    }
}
