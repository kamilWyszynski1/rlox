use crate::interpreter::interpreter::{CallableObject, Interpreter, LoxCallable};
use crate::interpreter::runtime::RuntimeValue;
use anyhow::Context;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct LoxClass {
    pub(crate) name: String,
    methods: HashMap<String, CallableObject>, // stores RuntimeValue::Callable objects
    pub static_methods: HashMap<String, CallableObject>,
    pub static_fields: HashMap<String, Rc<RefCell<RuntimeValue>>>,
}

impl LoxClass {
    pub fn new(
        name: String,
        class_methods: HashMap<String, CallableObject>,
        static_methods: HashMap<String, CallableObject>,
        static_fields: HashMap<String, Rc<RefCell<RuntimeValue>>>,
    ) -> Self {
        Self {
            name,
            methods: class_methods,
            static_methods,
            static_fields,
        }
    }

    fn find_method(&self, name: &str) -> Option<&CallableObject> {
        self.methods.get(name)
    }

    pub fn get(&self, name: &str) -> Option<Rc<RefCell<RuntimeValue>>> {
        if let Some(field) = self.static_fields.get(name) {
            return Some(field.clone());
        }

        if let Some(method) = self.static_methods.get(name) {
            return Some(Rc::new(RefCell::new(RuntimeValue::Callable(Rc::new(
                method.clone(),
            )))));
        }
        None
    }

    /// This method can only set class' static field value.
    pub fn set(&self, name: &str, value: RuntimeValue) -> anyhow::Result<()> {
        let field_value = self
            .static_fields
            .get(name)
            .context("no static field with given name")?;

        *field_value.try_borrow_mut()? = value;
        Ok(())
    }
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Class {}", self.name)
    }
}

impl LoxCallable for LoxClass {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Rc<RefCell<RuntimeValue>>>,
    ) -> anyhow::Result<Rc<RefCell<RuntimeValue>>> {
        let instance = Rc::new(RefCell::new(RuntimeValue::Instance(LoxInstance::new(
            self.clone(),
        ))));

        if let Some(initializer) = self.find_method("init") {
            // run init method if  provided
            initializer
                .bind(instance.clone())
                .call(interpreter, arguments)?;
        }

        Ok(instance)
    }

    fn arity(&self) -> usize {
        self.find_method("init")
            .map_or(0, |initializer| initializer.arity())
    }
}

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

    pub fn get(&self, name: &str) -> Option<RuntimeValue> {
        if let Some(field) = self.fields.get(name) {
            return Some(field.clone());
        }

        if let Some(method) = self.klass.find_method(name) {
            let copy = Rc::new(RefCell::new(RuntimeValue::Instance(self.clone())));
            let binded = method.bind(copy);
            let result = RuntimeValue::Callable(Rc::new(binded));
            return Some(result);
        }
        None
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
