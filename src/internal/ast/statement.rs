/*
Unlike expressions, which represent a value, statements
have no inherent value.

Statements are used to specify actions to take,
with the notable exception of assignment, which is
an expression.
*/
use super::expression::Expression;
use super::types::Type;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    VarDeclaration(String, Expression),
    ConstDeclaraction(String, Expression),
    If {
        condition: Expression,
        body: Vec<Statement>,
        alternative: Option<Vec<Statement>>,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
    DoWhile {
        condition: Expression,
        body: Vec<Statement>,
    },
    For {
        //For loop is INCLUSIVE
        loop_var: String,
        initial_val: Expression,
        end_val: Expression,
        step_val: Expression, // default value is 1
        body: Vec<Statement>,
    },
    SubroutineDeclaration {
        name: String,
        parameters: Vec<(String, Type)>,
        return_type: Type,
        body: Vec<Statement>,
    },
    Return(Expression),
}
