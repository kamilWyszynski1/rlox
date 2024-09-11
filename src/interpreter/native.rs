use crate::interpreter::interpreter::{Interpreter, LoxCallable, RuntimeValue};

#[derive(Debug, Clone)]
pub struct ClockCaller {}

impl ClockCaller {
    pub fn new() -> Self {
        Self {}
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
