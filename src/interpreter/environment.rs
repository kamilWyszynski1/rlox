use crate::interpreter::interpreter::RuntimeValue;
use anyhow::bail;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

#[derive(Clone)]
pub struct Environment {
    values: HashMap<String, RuntimeValue>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Debug for Environment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string("".to_string()))?;

        Ok(())
    }
}

impl Environment {
    pub fn new_empty() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn get(&self, key: &str) -> Option<RuntimeValue> {
        match self.values.get(key) {
            Some(v) => Some(v.clone()),
            None => {
                if let Some(ref enclosing) = self.enclosing {
                    let v = enclosing.borrow().get(key);
                    v.clone()
                } else {
                    None
                }
            }
        }
    }

    pub fn assign(&mut self, key: &str, value: RuntimeValue) -> anyhow::Result<()> {
        match self.values.get_mut(key) {
            None => {
                if let Some(ref mut enclosing) = self.enclosing {
                    enclosing.borrow_mut().assign(key, value)
                } else {
                    bail!("Undefined variable {}", key)
                }
            }
            Some(v) => {
                *v = value;
                Ok(())
            }
        }
    }

    pub fn define(&mut self, key: String, value: RuntimeValue) {
        self.values.insert(key, value);
    }

    pub fn depth(&self) -> usize {
        match &self.enclosing {
            Some(enclosing) => 1 + enclosing.borrow().depth(),
            None => 0,
        }
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    fn to_string(&self, ident: String) -> String {
        let mut s = format!("{:?}", self.values);
        if let Some(enclosing) = &self.enclosing {
            s += &enclosing.borrow().to_string(ident + "\t");
        }
        s
    }
}
