use crate::interpreter::interpreter::{Interpreter, LoxCallable};
use crate::interpreter::runtime::RuntimeValue;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ClockCaller {}

impl ClockCaller {
    pub fn new() -> Self {
        Self {}
    }
}

impl Display for ClockCaller {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "clock")
    }
}

impl LoxCallable for ClockCaller {
    fn call(
        &self,
        _: &mut Interpreter,
        _: Vec<Rc<RefCell<RuntimeValue>>>,
    ) -> anyhow::Result<Rc<RefCell<RuntimeValue>>> {
        let millis = std::time::SystemTime::now()
            .duration_since(std::time::SystemTime::UNIX_EPOCH)?
            .as_millis() as f64;
        Ok(Rc::new(RefCell::new(RuntimeValue::Number(millis / 1000.0))))
    }

    fn arity(&self) -> usize {
        0
    }
}
