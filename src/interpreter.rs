use super::ast::callable::{Callable, NativeFunction};
use super::ast::literal::Literal;
use super::ast::statement::Statement;
use super::environment::{EnvWrapper, Environment};
use super::error::runtime::RuntimeError;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type FunctionScope = HashMap<String, Callable>;
pub struct Interpreter {
    env: EnvWrapper,
    functions: FunctionScope,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = vec![("LEN", NativeFunction::Len)];
        let mut functions = HashMap::new();
        for (name, func) in globals {
            functions.insert(name.to_owned(), Callable::Native(func));
        }
        Interpreter {
            functions,
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn get_callable(&self, name: &String) -> &Callable {
        self.functions.get(name).expect("Should have already been present in function scope when parsing")
    }
    pub fn execute(&self, statements: &Vec<Statement>) -> Result<(), RuntimeError> {
        for statement in statements {
            self.execute_statement(&statement, Rc::clone(&self.env))?;
        }
        Ok(())
    }

    fn execute_statement(
        &self,
        statement: &Statement,
        env: EnvWrapper,
    ) -> Result<(), RuntimeError> {
        match statement {
            Statement::Expression(expr) => {
                let val = expr.evaluate(env, self)?;
                println!("{:#?}", val);
                Ok(())
            }
            Statement::ConstDeclaraction(name, expr) => {
                let val = expr.evaluate(Rc::clone(&env), self)?;
                println!("Declare constant {:#?} with {:#?}", name, val);
                env.borrow_mut().define(name.to_string(), val);
                Ok(())
            }
            Statement::VarDeclaration(name, expr) => {
                let val = expr.evaluate(Rc::clone(&env), self)?;
                println!("Declare variable {:#?} with {:#?}", name, val);
                env.borrow_mut().define(name.to_string(), val);
                Ok(())
            }
            Statement::If {
                condition,
                body,
                alternative,
            } => match condition.evaluate(Rc::clone(&env), self)? {
                Literal::Boolean(b) => {
                    if b {
                        println!("true");
                        self.execute_block(&body, env)
                    } else {
                        println!("false");
                        match alternative {
                            Some(alt) => self.execute_block(&alt, env),
                            None => Ok(()),
                        }
                    }
                }
                _ => unreachable!(),
            },
            Statement::While { condition, body } => {
                while let true = bool::from(condition.evaluate(Rc::clone(&env), self)?) {
                    self.execute_block(&body, Rc::clone(&env))?;
                }
                Ok(())
            }
            Statement::DoWhile { condition, body } => {
                self.execute_block(&body, Rc::clone(&env))?;
                while let true = bool::from(condition.evaluate(Rc::clone(&env), self)?) {
                    self.execute_block(&body, Rc::clone(&env))?;
                }
                Ok(())
            }
            Statement::For {
                loop_var,
                initial_val,
                end_val,
                step_val,
                body,
            } => {
                let start_val = i64::from(initial_val.evaluate(Rc::clone(&env), self)?);
                let end_val = i64::from(end_val.evaluate(Rc::clone(&env), self)?);
                let step_val = i64::from(step_val.evaluate(Rc::clone(&env), self)?);
                if start_val >= end_val && step_val > 0 || start_val <= end_val && step_val < 0 {
                    return Err(RuntimeError::InvalidRangeBound);
                }
                let loop_var_env = Rc::new(RefCell::new(Environment::from_enclosing(env)));
                let range: Vec<_> = match step_val {
                    step if step_val > 0 => {
                        Ok((start_val..=end_val).step_by(step as usize).collect())
                    }
                    step if step_val < 0 => Ok((end_val..=start_val)
                        .rev()
                        .step_by(-step as usize)
                        .collect()),
                    _ => Err(RuntimeError::RangeStepCannotBeZero),
                }?;
                for loop_val in range.into_iter() {
                    loop_var_env
                        .borrow_mut()
                        .define(loop_var.to_string(), Literal::Integer(loop_val));
                    self.execute_block(body, Rc::clone(&loop_var_env))?;
                }
                Ok(())
            }
        }
    }

    pub fn execute_block(
        &self,
        block: &Vec<Statement>,
        env: EnvWrapper,
    ) -> Result<(), RuntimeError> {
        for statement in block {
            self.execute_statement(statement, Rc::clone(&env))?;
        }
        Ok(())
    }
}
