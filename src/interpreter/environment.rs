use crate::interpreter::interpreter::RuntimeValue;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Environment {
    values: HashMap<String, RuntimeValue>,
    nested: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            nested: None,
        }
    }

    pub fn new_scope(&mut self) {
        match &mut self.nested {
            None => self.nested = Some(Box::new(Self::new())),
            Some(ref mut nested) => nested.new_scope(),
        }
    }

    pub fn drop_scope(&mut self) {
        if let Some(nested) = self.nested.take() {
            if nested.nested.is_some() {
                self.nested = Some(nested);
                self.nested.as_mut().unwrap().drop_scope();
            }
        }
    }

    pub fn get(&self, key: &str) -> Option<&RuntimeValue> {
        match &self.nested {
            None => self.values.get(key),
            Some(nested) => match nested.get(key) {
                None => self.values.get(key),
                Some(value) => Some(value),
            },
        }
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut RuntimeValue> {
        match &mut self.nested {
            None => self.values.get_mut(key),
            Some(nested) => match nested.get_mut(key) {
                None => self.values.get_mut(key),
                Some(value) => Some(value),
            },
        }
    }

    pub fn define(&mut self, key: String, value: RuntimeValue) {
        match &mut self.nested {
            None => {
                self.values.insert(key, value);
            }
            Some(nested) => nested.define(key.clone(), value.clone()),
        }
    }
}
