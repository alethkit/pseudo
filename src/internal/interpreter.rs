use super::ast::callable::{Callable, Subroutine, GLOBALS};
use super::ast::{Expression, Literal, Statement};
use super::environment::{EnvWrapper, Environment};
use super::error::{IOError, RuntimeError};
use crate::io_provider::IOProvider;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type FunctionScope = HashMap<String, Callable>;
pub struct Interpreter {
    env: EnvWrapper,
    functions: FunctionScope,
    io_provider: Box<dyn IOProvider>,
    stack_count: u64
}

impl Interpreter {
    pub fn new(io_provider: Box<dyn IOProvider>) -> Self {
        let functions = GLOBALS
            .iter()
            .cloned()
            .map(|(name, f)| (name.to_string(), Callable::Native(f)))
            .collect();
        Interpreter {
            functions,
            env: Rc::new(RefCell::new(Environment::new())),
            io_provider,
            stack_count: 0
        }
    }

    pub fn from_environment(env: EnvWrapper, io_provider: Box<dyn IOProvider>) -> Self {
        let functions = GLOBALS
            .iter()
            .cloned()
            .map(|(name, f)| (name.to_string(), Callable::Native(f)))
            .collect();
        Interpreter {
            functions,
            env,
            io_provider,
            stack_count: 0
        }
    
    }

    pub fn increment_stack_count(&mut self) -> Result<(), RuntimeError> {
        if self.stack_count >= 255 {
            Err(RuntimeError::StackOverflow)
        }
        else {
            self.stack_count += 1;
            Ok(())
        }
    }

    pub fn decrement_stack_count(&mut self)  {
        //Stack count should not decrement beyond 0, so any exceptions should be unhandled to
        //detect an error
        self.stack_count -= 1;

    }
    
    pub fn get_prov(&mut self) -> &mut Box<dyn IOProvider> {
        &mut self.io_provider
    }

    pub fn show_line(&mut self, line_to_show: &str) {
        self.io_provider.show_line(line_to_show)
    }

    pub fn get_line(&mut self) -> Result<String, IOError> {
        self.io_provider.get_line()
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

    pub fn evaluate_expression(&mut self, expr: &Expression) -> Result<Literal, RuntimeError> {
        expr.evaluate(Rc::clone(&self.env), self)
    }

    fn execute_statement(
        &mut self,
        statement: &Statement,
        env: EnvWrapper,
    ) -> Result<Option<Literal>, RuntimeError> {
        match statement {
            Statement::Expression(expr) => {
                expr.evaluate(env, self)?;
                Ok(None)
            }
            Statement::Return(expr) => {
                let val = expr.evaluate(env, self)?;
                Ok(Some(val))
            }
            Statement::ConstDeclaraction(name, expr) => {
                let val = expr.evaluate(Rc::clone(&env), self)?;
                env.borrow_mut().define(name.to_string(), val);
                Ok(None)
            }
            Statement::VarDeclaration(name, expr) => {
                let val = expr.evaluate(Rc::clone(&env), self)?;
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
                        self.execute_block(&body, env)
                    } else {
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
                        None => continue,
                    }
                }
                Ok(None)
            }
            Statement::DoWhile { condition, body } => {
                self.execute_block(&body, Rc::clone(&env))?;
                while let true = bool::from(condition.evaluate(Rc::clone(&env), self)?) {
                    match self.execute_block(&body, Rc::clone(&env))? {
                        Some(val) => return Ok(Some(val)),
                        None => continue,
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
                if start_val > end_val && step_val > 0 || start_val < end_val && step_val < 0 {
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
                        None => continue,
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
                        return_type.clone(),
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
