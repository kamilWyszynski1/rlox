use crate::interpreter::interpreter::{Interpreter, LoxCallable};
use crate::interpreter::runtime::RuntimeValue;
use std::collections::HashMap;
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct LoxInstance {
    klass: LoxClass,
    fields: HashMap<String, RuntimeValue>,
}

impl LoxInstance {
    fn new(klass: LoxClass) -> LoxInstance {
        LoxInstance {
            klass,
            fields: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&RuntimeValue> {
        self.fields.get(name)
    }

    pub fn set(&mut self, name: String, value: RuntimeValue) {
        self.fields.insert(name, value);
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
        _arguments: Vec<Rc<RefCell<RuntimeValue>>>,
    ) -> anyhow::Result<Rc<RefCell<RuntimeValue>>> {
        Ok(Rc::new(RefCell::new(RuntimeValue::Instance(
            LoxInstance::new(self.clone()),
        ))))
    }

    fn arity(&self) -> usize {
        0
    }
}
