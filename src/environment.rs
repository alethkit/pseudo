use crate::ast::expression::ExprIdentifier;
use crate::ast::literal::Literal;
use crate::interpreter::Interpreter;
use crate::error::runtime::RuntimeError;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;

pub struct Environment {
    values: HashMap<String, Literal>,
    parent: Option<EnvWrapper>,
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Identifier {
    Variable(String),
    Index(Box<Identifier>, usize),
}

impl From<String> for Identifier {
    fn from(value: String) -> Self {
        Self::Variable(value)
    }
}

impl TryFrom<(&ExprIdentifier, EnvWrapper, &mut Interpreter)> for Identifier {
    type Error = RuntimeError;

    fn try_from((value, env,interpreter): (&ExprIdentifier, EnvWrapper, &mut Interpreter)) -> Result<Self, Self::Error> {
        match value {
            ExprIdentifier::Variable(name) => Ok(Self::Variable(name.to_string())),
            ExprIdentifier::Index(ident, index) => match index.evaluate(Rc::clone(&env), interpreter)? {
                Literal::Integer(int) => Ok(Self::Index(
                    Self::try_from((&**ident, env, interpreter)).map(Box::new)?,
                    int as usize,
                )),
                _ => unreachable!("Already type checked"),
            },
        }
    }
}

pub type EnvWrapper = Rc<RefCell<Environment>>;

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_wrapper() -> EnvWrapper {
        Rc::new(RefCell::new(Self::new()))
    }

    pub fn from_enclosing(env: EnvWrapper) -> Self {
        Environment {
            values: HashMap::new(),
            parent: Some(env),
        }
    }

    pub fn define(&mut self, name: String, value: Literal) {
        self.values.insert(name, value);
    }

    pub fn get(&self, ident: &Identifier) -> Result<Literal, RuntimeError> {
        match ident {
            Identifier::Variable(name) => match self.values.get(name) {
                Some(v) => Ok(v.clone()),
                None => self
                    .parent
                    .as_ref()
                    .ok_or(RuntimeError::UndefinedVariable)?
                    .borrow()
                    .get(ident),
            },
            Identifier::Index(id, index) => match self.get(id)? {
                Literal::List(v) => v.get(*index).ok_or(RuntimeError::OutOfRange).map(Clone::clone),
                _ => unreachable!("Index should have been type checked on list"),
            },
        }
    }

    pub fn assign(&mut self, ident: &Identifier, value: Literal) -> Result<Literal, RuntimeError> {
        match ident {
            Identifier::Variable(name) => match self.values.get_mut(name) {
                Some(v) => {
                    *v = value.clone();
                    Ok(value)
                }
                None => match &self.parent {
                    Some(p) => p.borrow_mut().assign(ident, value),
                    None => unreachable!("Type scope"),
                },
            },
            Identifier::Index(id, index) => {
                let prospective_list = self.get(id)?;
                match prospective_list {
                    Literal::List(mut list) => {
                        *list.get_mut(*index).ok_or(RuntimeError::OutOfRange)? = value.clone();
                        self.assign(id, Literal::List(list));
                        Ok(value)
                    }
                    _ => unreachable!("Index should have been type checked on list"),
                }
            }
        }
    }
}
