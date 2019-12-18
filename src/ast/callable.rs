use super::literal::Literal;
use super::statement::Statement;
use super::types::{Type, TypeError, Typed};
use crate::environment::Environment;
use crate::error::runtime::RuntimeError;
use crate::interpreter::Interpreter;
use crate::parser::ParserError;

use std::convert::TryFrom;
use std::mem::discriminant;

use rand::{thread_rng, Rng};

#[derive(Debug, Clone)]
pub enum Callable {
    // The struct representing subroutines for the interpreter
    Subroutine(Subroutine),
    Native(NativeFunction),
}

impl Callable {
    pub fn call(
        &self,
        args: Vec<Literal>,
        interpreter: &mut Interpreter,
    ) -> Result<Literal, RuntimeError> {
        match self {
            Self::Subroutine(func) => func.call(args, interpreter),
            Self::Native(nat_func) => nat_func.call(args, interpreter),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Subroutine {
    name: String,
    parameters: Vec<String>,
    body: Vec<Statement>,
    return_type: Type,
}

impl Subroutine {
    pub fn new(
        name: String,
        parameters: Vec<String>,
        body: Vec<Statement>,
        return_type: Type,
    ) -> Self {
        Subroutine {
            name,
            parameters,
            body,
            return_type,
        }
    }

    fn call(
        &self,
        args: Vec<Literal>,
        interpreter: &mut Interpreter,
    ) -> Result<Literal, RuntimeError> {
        let env = Environment::new_wrapper();
        let parameter_arg_pairs = self.parameters.iter().zip(args.into_iter());
        for (name, value) in parameter_arg_pairs {
            env.borrow_mut().define(name.to_owned(), value)
        }
        let val = interpreter
            .execute_block(&self.body, env)?
            .unwrap_or(Literal::Void);
        if val.get_type() == self.return_type {
            Ok(val)
        } else {
            Err(RuntimeError::IncorrectReturnExpression)
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum NativeFunction {
    Len,
    Position,
    Substring,
    StringToInt,
    StringToReal,
    IntToString,
    RealToString,
    IntToReal,
    RealToInt,
    CharToCode,
    CodeToChar,
    RandomInt,
    UserInput,
    Output,
}

pub const GLOBALS: [(&str, NativeFunction); 14] = [
    ("LEN", NativeFunction::Len),
    ("POSITION", NativeFunction::Position),
    ("SUBSTRING", NativeFunction::Substring),
    ("STRING_TO_INT", NativeFunction::StringToInt),
    ("STRING_TO_REAL", NativeFunction::StringToReal),
    ("INT_TO_STRING", NativeFunction::IntToString),
    ("REAL_TO_STRING", NativeFunction::RealToString),
    ("INT_TO_REAL", NativeFunction::IntToReal),
    ("REAL_TO_INT", NativeFunction::RealToInt),
    ("CHAR_TO_CODE", NativeFunction::CharToCode),
    ("CODE_TO_CHAR", NativeFunction::CodeToChar),
    ("RANDOM_INT", NativeFunction::RandomInt),
    ("USERINPUT", NativeFunction::UserInput),
    ("OUTPUT", NativeFunction::Output),
];

impl NativeFunction {
    fn expected_args(&self) -> Vec<Type> {
        match self {
            Self::Len => unreachable!("Has special validation"),
            Self::Position => vec![Type::Str, Type::Character],
            Self::Substring => vec![Type::Integer, Type::Integer, Type::Str],
            Self::StringToInt | Self::StringToReal | Self::Output => vec![Type::Str],
            Self::IntToString | Self::IntToReal | Self::CodeToChar => vec![Type::Integer],
            Self::RealToString | Self::RealToInt => vec![Type::Real],
            Self::CharToCode => vec![Type::Character],
            Self::RandomInt => vec![Type::Integer, Type::Integer],
            Self::UserInput => vec![],
        }
    }
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
            _ => {
                let expected = self.expected_args();
                if args_list.len() != expected.len() {
                    Err(ParserError::IncorrectFunctionArity(
                        expected.len(),
                        args_list.len(),
                    ))
                } else if expected
                    .iter()
                    .map(discriminant)
                    .zip(args_list.iter().map(discriminant))
                    .all(|(a, b)| a == b)
                {
                    Ok(())
                } else {
                    Err(ParserError::Typing(TypeError::ArgumentMismatch(
                        expected,
                        args_list.clone(),
                    )))
                }
            }
        }
    }
    pub fn call(
        &self,
        arguments: Vec<Literal>,
        interpreter: &mut Interpreter,
    ) -> Result<Literal, RuntimeError> {
        match self {
            Self::Len => match &arguments[0] {
                Literal::Str(val) => Ok(Literal::Integer(val.len() as i64)),
                Literal::List(val) => Ok(Literal::Integer(val.len() as i64)),
                _ => unreachable!("Should have been type checked"),
            },
            Self::Position => match (&arguments[0], &arguments[1]) {
                (Literal::Str(string), &Literal::Character(character)) => {
                    match string.find(character) {
                        Some(val) => Ok(Literal::Integer(val as i64)),
                        None => Ok(Literal::Integer(-1)),
                    }
                }
                _ => unreachable!("Should have been type checked"),
            },
            Self::Substring => match (&arguments[0], &arguments[1], &arguments[2]) {
                (&Literal::Integer(start), &Literal::Integer(end), Literal::Str(string)) => {
                    if start < 0
                        || start >= string.len() as i64
                        || end < 0
                        || end >= string.len() as i64
                        || start > end
                    {
                        Err(RuntimeError::InvalidRangeBound)
                    } else {
                        Ok(Literal::Str(
                            string[start as usize..=end as usize].to_string(),
                        ))
                    }
                }
                _ => unreachable!("Should have been type checked"),
            },
            Self::RandomInt => match (&arguments[0], &arguments[1]) {
                (&Literal::Integer(lower), &Literal::Integer(upper)) => {
                    if upper < lower {
                        Err(RuntimeError::InvalidRangeBound)
                    } else {
                        Ok(Literal::Integer(thread_rng().gen_range(lower, upper + 1)))
                    }
                }
                _ => unreachable!("Should have been type checked"),
            },
            Self::UserInput => {
                Ok(Literal::Str(interpreter.get_line()?.to_string()))
            }
            _ => match (self, &arguments[0]) {
                (Self::StringToInt, Literal::Str(string)) => string
                    .parse::<i64>()
                    .map_err(RuntimeError::from)
                    .map(Literal::Integer),
                (Self::StringToReal, Literal::Str(string)) => string
                    .parse::<f64>()
                    .map_err(RuntimeError::from)
                    .map(Literal::Real),
                (Self::IntToString, &Literal::Integer(int)) => Ok(Literal::Str(int.to_string())),
                (Self::RealToString, &Literal::Real(real)) => Ok(Literal::Str(real.to_string())),
                (Self::IntToReal, &Literal::Integer(int)) => Ok(Literal::Real(int as f64)),
                (Self::RealToInt, &Literal::Real(real)) => Ok(Literal::Integer(real as i64)),
                (Self::CharToCode, &Literal::Character(character)) => {
                    Ok(Literal::Integer(character as i64))
                }
                (Self::CodeToChar, &Literal::Integer(int)) => {
                    let unsigned = u32::try_from(int).or(Err(RuntimeError::InvalidCharacter))?;
                    char::try_from(unsigned)
                        .map_err(RuntimeError::from)
                        .map(Literal::Character)
                }
                (Self::Output, Literal::Str(string)) => {
                    interpreter.show_line(string);
                    Ok(Literal::Void)
                }
                _ => unreachable!("Should have been type checked"),
            },
        }
    }
}

impl Typed for NativeFunction {
    fn get_type(&self) -> Type {
        match self {
            Self::Len
            | Self::Position
            | Self::StringToInt
            | Self::RealToInt
            | Self::CharToCode
            | Self::RandomInt => Type::Integer,
            Self::Substring | Self::IntToString | Self::RealToString => Type::Str,
            Self::StringToReal | Self::IntToReal => Type::Real,
            Self::CodeToChar => Type::Character,
            Self::UserInput => Type::Str,
            Self::Output => Type::Void,
        }
    }
}
