use super::ast::callable::{Callable, Subroutine, GLOBALS};
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
        let functions = GLOBALS.iter().cloned().map(|(name, f)| (name.to_string(), Callable::Native(f))).collect();
        Interpreter {
            functions,
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn get_callable(&self, name: &str) -> Callable {
        self.functions
            .get(name)
            .expect("Should have already been present in function scope when parsing")
            .clone()
    }
    pub fn execute(&mut self, statements: &Vec<Statement>) -> Result<(), RuntimeError> {
        for statement in statements {
            self.execute_statement(&statement, Rc::clone(&self.env))?;
        }
        Ok(())
    }

    fn execute_statement(
        &mut self,
        statement: &Statement,
        env: EnvWrapper,
    ) -> Result<Option<Literal>, RuntimeError> {
        match statement {
            Statement::Expression(expr) => {
                let val = expr.evaluate(env, self)?;
                println!("{:#?}", val);
                Ok(None)
            }
            Statement::Return(expr) => {
                let val = expr.evaluate(env, self)?;
                println!("{:#?}", val);
                Ok(Some(val))
            }
            Statement::ConstDeclaraction(name, expr) => {
                let val = expr.evaluate(Rc::clone(&env), self)?;
                println!("Declare constant {:#?} with {:#?}", name, val);
                env.borrow_mut().define(name.to_string(), val);
                Ok(None)
            }
            Statement::VarDeclaration(name, expr) => {
                let val = expr.evaluate(Rc::clone(&env), self)?;
                println!("Declare variable {:#?} with {:#?}", name, val);
                env.borrow_mut().define(name.to_string(), val);
                Ok(None)
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
                            None => Ok(None),
                        }
                    }
                }
                _ => unreachable!(),
            },
            Statement::While { condition, body } => {
                while let true = bool::from(condition.evaluate(Rc::clone(&env), self)?) {
                    match self.execute_block(&body, Rc::clone(&env))? {
                        Some(val) => return Ok(Some(val)),
                        None => continue
                    }
                }
                Ok(None)
            }
            Statement::DoWhile { condition, body } => {
                self.execute_block(&body, Rc::clone(&env))?;
                while let true = bool::from(condition.evaluate(Rc::clone(&env), self)?) {
                    match self.execute_block(&body, Rc::clone(&env))? {
                        Some(val) => return Ok(Some(val)),
                        None => continue
                    }
                }
                Ok(None)
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
                    match self.execute_block(&body, Rc::clone(&loop_var_env))? {
                        Some(val) => return Ok(Some(val)),
                        None => continue
                    }
                }
                Ok(None)
            }
            Statement::SubroutineDeclaration {
                name,
                parameters,
                body,
                return_type,
                ..
            } => {
                self.functions.insert(
                    name.to_string(),
                    Callable::Subroutine(Subroutine::new(
                        name.clone(),
                        parameters.iter().map(|(name, _)| name).cloned().collect(),
                        body.clone(),
                        return_type.clone()
                    )),
                );
                Ok(None)
            }
        }
    }

    pub fn execute_block(
        &mut self,
        block: &Vec<Statement>,
        env: EnvWrapper,
    ) -> Result<Option<Literal>, RuntimeError> {
        for statement in block {
            if let Some(val) = self.execute_statement(statement, Rc::clone(&env))? {
                return Ok(Some(val));
            }
        }
        Ok(None)
    }
}
