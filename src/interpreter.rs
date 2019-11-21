use super::ast::statement::Statement;
use super::environment::Environment;
use super::error::runtime::RuntimeError;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn execute(&self, statements: Vec<Statement>) -> Result<(), RuntimeError> {
        for statement in statements {
            self.execute_statement(statement)?;
        }
        Ok(())
    }

    fn execute_statement(&self, statement: Statement) -> Result<(), RuntimeError> {
        match statement {
            Statement::Expression(expr) => {
                let val = expr.evaluate(Rc::clone(&self.env))?;
                println!("{:#?}", val);
                Ok(())
            }
            Statement::ConstDeclaraction(name, expr) => {
                let val = expr.evaluate(Rc::clone(&self.env))?;
                println!("Declare constant {:#?} with {:#?}", name, val);
                self.env.borrow_mut().define(name, val);
                Ok(())
            }
            Statement::VarDeclaration(name, expr) => {
                let val = expr.evaluate(Rc::clone(&self.env))?;
                println!("Declare variable {:#?} with {:#?}", name, val);
                self.env.borrow_mut().define(name, val);
                Ok(())
            }
        }
    }
}
