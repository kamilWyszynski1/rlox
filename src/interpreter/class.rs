use crate::interpreter::interpreter::{Interpreter, LoxCallable};
use crate::interpreter::runtime::RuntimeValue;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct LoxInstance {
    klass: LoxClass,
}

impl LoxInstance {
    fn new(klass: LoxClass) -> LoxInstance {
        LoxInstance { klass }
    }
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.klass)
    }
}

#[derive(Debug, Clone)]
pub struct LoxClass {
    pub(crate) name: String,
}

impl LoxClass {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl LoxCallable for LoxClass {
    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _arguments: Vec<RuntimeValue>,
    ) -> anyhow::Result<RuntimeValue> {
        Ok(RuntimeValue::Instance(LoxInstance::new(self.clone())))
    }

    fn arity(&self) -> usize {
        0
    }
}
