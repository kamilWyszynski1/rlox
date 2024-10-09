use crate::interpreter::interpreter::{CallableObject, Interpreter, LoxCallable};
use crate::interpreter::runtime::RuntimeValue;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct LoxClass {
    pub(crate) name: String,
    methods: HashMap<String, CallableObject>, // stores RuntimeValue::Callable objects
    pub static_methods: HashMap<String, CallableObject>,
}

impl LoxClass {
    pub fn new(
        name: String,
        class_methods: HashMap<String, CallableObject>,
        static_methods: HashMap<String, CallableObject>,
    ) -> Self {
        Self {
            name,
            methods: class_methods,
            static_methods,
        }
    }

    fn find_method(&self, name: &str) -> Option<&CallableObject> {
        self.methods.get(name)
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
