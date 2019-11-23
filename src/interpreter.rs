use super::ast::literal::Literal;
use super::ast::statement::Statement;
use super::environment::{EnvWrapper, Environment};
use super::error::runtime::RuntimeError;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Interpreter {
    env: EnvWrapper,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn execute(&self, statements: Vec<Statement>) -> Result<(), RuntimeError> {
        for statement in statements {
            self.execute_statement(statement, Rc::clone(&self.env))?;
        }
        Ok(())
    }

    fn execute_statement(&self, statement: Statement, env: EnvWrapper) -> Result<(), RuntimeError> {
        match statement {
            Statement::Expression(expr) => {
                let val = expr.evaluate(env)?;
                println!("{:#?}", val);
                Ok(())
            }
            Statement::ConstDeclaraction(name, expr) => {
                let val = expr.evaluate(Rc::clone(&env))?;
                println!("Declare constant {:#?} with {:#?}", name, val);
                env.borrow_mut().define(name, val);
                Ok(())
            }
            Statement::VarDeclaration(name, expr) => {
                let val = expr.evaluate(Rc::clone(&env))?;
                println!("Declare variable {:#?} with {:#?}", name, val);
                env.borrow_mut().define(name, val);
                Ok(())
            }
            Statement::If {
                condition,
                body,
                alternative,
            } => match condition.evaluate(Rc::clone(&env))? {
                Literal::Boolean(b) => {
                    if b {
                        println!("true");
                        self.execute_block(body, env)
                    } else {
                        println!("false");
                        match alternative {
                            Some(alt) =>  self.execute_block(alt, env),
                            None => Ok(()),
                        }
                    }
                }
                _ => unreachable!(),
            },
        }
    }

    fn execute_block(&self, block: Vec<Statement>, env: EnvWrapper) -> Result<(), RuntimeError> {
        for statement in block {
            self.execute_statement(statement, Rc::clone(&env))?;
        }
        Ok(())
    }
}
