use crate::interpreter::interpreter::RuntimeValue;
use std::collections::HashMap;

pub struct Environment {
    pub values: HashMap<String, RuntimeValue>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }
}
