use super::statement::Statement;
use super::types::{Type, TypeError, Typed};
use super::literal::Literal;
use crate::parser::ParserError;
use crate::environment::Environment;
use crate::interpreter::Interpreter;
use crate::error::runtime::RuntimeError;

#[derive(Debug, Clone)]
pub enum Callable { 
// The struct representing subroutines for the interpreter 
    Subroutine(Subroutine),
    Native(NativeFunction),
}

impl Callable {
    pub fn call(&self, args: Vec<Literal>, interpreter: &mut Interpreter) -> Result<Literal, RuntimeError> {
        match self {
            Self::Subroutine(func) => func.call(args, interpreter),
            Self::Native(nat_func) => nat_func.call(args) // A native function does not need to call other functions to work
        }

    }
}

#[derive(Debug, Clone)]
pub struct Subroutine {
    name: String,
    parameters: Vec<String>,
    body: Vec<Statement>,
    return_type: Type
}

impl Subroutine {
    

    pub fn new(name: String, parameters: Vec<String>, body: Vec<Statement>, return_type: Type) -> Self {
        Subroutine {
            name,
            parameters,
            body,
            return_type
        }
    }

    fn call(&self, args: Vec<Literal>, interpreter: &mut Interpreter) -> Result<Literal, RuntimeError> {
        let env = Environment::new_wrapper();
        let parameter_arg_pairs = self.parameters.iter().zip(args.into_iter());
        for (name, value) in parameter_arg_pairs {
            env.borrow_mut().define(name.to_owned(),value)
        }
        let val = interpreter.execute_block(&self.body, env)?.unwrap_or(Literal::Void);
        if val.get_type() == self.return_type {
            Ok(val)
        }
        else {
            Err(RuntimeError::IncorrectReturnExpression)
        }
    }
}


#[derive(Debug, Clone, Copy)]
pub enum NativeFunction {
    Len,
}


impl NativeFunction {
    pub fn validate(&self, args_list: &Vec<Type>) -> Result<(), ParserError> {
        // Helper parser method
        match self {
            Self::Len => {
                if args_list.len() != 1 {
                    Err(ParserError::IncorrectFunctionArity(1, args_list.len()))
                } else {
                    match args_list.iter().next().expect("There should be 1").clone() {
                        Type::Str | Type::List(_) => Ok(()),
                        t => Err(ParserError::Typing(TypeError::SingleExpectedOneOf(
                            vec![Type::Str, Type::List(Box::new(Type::Any))],
                            t,
                        ))),
                    }
                }
            }
        }
    }
    pub fn call(&self, arguments: Vec<Literal>) -> Result<Literal, RuntimeError> {
        match self {
            Self::Len => {
                match arguments.get(0).unwrap() {
                    Literal::Str(val)  => Ok(Literal::Integer(val.len() as i64)),
                    Literal::List(val) => Ok(Literal::Integer(val.len() as i64)),
                    _ => unreachable!("Should have been type checked")
                }
            }
        }
    }
}

impl Typed for NativeFunction {
    fn get_type(&self) -> Type {
        match self {
            Self::Len => Type::Integer
        }
    }
}

