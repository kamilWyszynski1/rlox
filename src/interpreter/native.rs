use crate::interpreter::interpreter::{Interpreter, LoxCallable};
use crate::interpreter::runtime::RuntimeValue;
use std::fmt::{Display, Formatter};

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
    fn call(&self, _: &mut Interpreter, _: Vec<RuntimeValue>) -> anyhow::Result<RuntimeValue> {
        let millis = std::time::SystemTime::now()
            .duration_since(std::time::SystemTime::UNIX_EPOCH)?
            .as_millis() as f64;
        Ok(RuntimeValue::Number(millis / 1000.0))
    }

    fn arity(&self) -> usize {
        0
    }
}
