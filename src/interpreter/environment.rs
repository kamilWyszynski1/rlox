use crate::interpreter::runtime::RuntimeValue;
use anyhow::bail;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

#[derive(Clone)]
pub struct Environment {
    values: HashMap<String, Rc<RefCell<RuntimeValue>>>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
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

    pub fn get(&self, key: &str) -> Option<Rc<RefCell<RuntimeValue>>> {
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

    pub fn assign(&mut self, key: &str, value: Rc<RefCell<RuntimeValue>>) -> anyhow::Result<()> {
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

    pub fn define(&mut self, key: String, value: Rc<RefCell<RuntimeValue>>) {
        self.values.insert(key, value);
    }

    pub fn depth(&self) -> usize {
        match &self.enclosing {
            Some(enclosing) => 1 + enclosing.borrow().depth(),
            None => 1,
        }
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    fn to_string(&self, ident: String) -> String {
        let mut s = format!("\n{}{:?}", ident, self.values);
        if let Some(enclosing) = &self.enclosing {
            s += &enclosing.borrow().to_string(ident + "\t");
        }
        s
    }

    pub fn ancestor(&self, depth: usize) -> Option<Rc<RefCell<Environment>>> {
        if depth == 0 {
            return None; // should be cover by condition with 'depth == 1'
        }
        if depth == 1 && self.enclosing.is_some() {
            return Some(self.enclosing.as_ref()?.clone());
        }
        if depth != 0 && self.enclosing.is_none() {
            return None;
        }
        match &self.enclosing {
            None => None,
            Some(enclosing) => enclosing.borrow().ancestor(depth - 1),
        }
    }
}
