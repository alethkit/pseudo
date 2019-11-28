use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::ast::literal::Literal;
use crate::ast::operator::EvalError;

pub struct Environment {
    values: HashMap<String, Literal>,
    parent: Option<EnvWrapper>,
}

#[derive(PartialEq, Eq, Hash)]
enum Identifier {
	Variable(String),
	Index(Box<Identifier>, usize)
}


pub type EnvWrapper = Rc<RefCell<Environment>>;

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            parent: None,
        }
    }

    pub fn from_enclosing(env: EnvWrapper) -> Self {
        Environment {
            values: HashMap::new(),
            parent: Some(env)
        }
    }

    pub fn define(&mut self, name: String, value: Literal) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Literal {
        match self.values.get(name) {
            Some(v) => v.clone(),
            None => match &self.parent {
                Some(p) => p.borrow().get(name),
                None => unreachable!()
            }
        }
    }

    pub fn assign(&mut self, name: &str, value: Literal) -> Result<Literal, EvalError> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value.clone());
            Ok(value)
        }
        else {
            match &self.parent {
                Some(p) => p.borrow_mut().assign(name, value),
                None => Err(EvalError::UndefinedVariable)
            }
        }
    }



}
