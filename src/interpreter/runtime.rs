use crate::ast::ast::LiteralValue;
use crate::interpreter::class::{LoxClass, LoxInstance};
use crate::interpreter::interpreter::LoxCallable;
use anyhow::{anyhow, bail};
use std::fmt::{Debug, Display};
use std::rc::Rc;

#[derive(Clone)]
pub enum RuntimeValue {
    Bool(bool),
    Null,
    Number(f64),
    String(String),
    Callable(Rc<dyn LoxCallable>),
    Class(LoxClass),
    Instance(LoxInstance),
}

impl RuntimeValue {
    /// Lox follows Ruby’s simple rule: false and nil are falsey, and everything else is truthy.
    pub fn is_truthy(&self) -> bool {
        match self {
            RuntimeValue::Bool(true)
            | RuntimeValue::Number(_)
            | RuntimeValue::String(_)
            | RuntimeValue::Callable(_)
            | RuntimeValue::Instance(_)
            | RuntimeValue::Class(_) => true,
            RuntimeValue::Bool(false) | RuntimeValue::Null => false,
        }
    }
}

impl PartialEq for RuntimeValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RuntimeValue::Bool(lhs), RuntimeValue::Bool(rhs)) => lhs == rhs,
            (RuntimeValue::Null, RuntimeValue::Null) => true,
            (RuntimeValue::Number(lhs), RuntimeValue::Number(rhs)) => lhs == rhs,
            (RuntimeValue::String(lhs), RuntimeValue::String(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl Debug for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            RuntimeValue::Bool(b) => b.to_string(),
            RuntimeValue::Null => "null".to_string(),
            RuntimeValue::Number(n) => n.to_string(),
            RuntimeValue::String(s) => s.clone(),
            RuntimeValue::Callable(c) => format!("{:?}", c),
            RuntimeValue::Class(c) => format!("Class {:?}", c),
            RuntimeValue::Instance(instance) => format!("{:?}", instance),
        };
        write!(f, "{}", str)
    }
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            RuntimeValue::Bool(b) => b.to_string(),
            RuntimeValue::Null => "null".to_string(),
            RuntimeValue::Number(n) => n.to_string(),
            RuntimeValue::String(s) => s.clone(),
            RuntimeValue::Callable(c) => format!("{}", c),
            RuntimeValue::Class(c) => format!("Class {}", c),
            RuntimeValue::Instance(instance) => format!("{}", instance),
        };
        write!(f, "{}", str)
    }
}

impl From<bool> for RuntimeValue {
    fn from(value: bool) -> Self {
        RuntimeValue::Bool(value)
    }
}

impl From<f64> for RuntimeValue {
    fn from(value: f64) -> Self {
        RuntimeValue::Number(value)
    }
}

impl TryInto<f64> for RuntimeValue {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            RuntimeValue::Bool(_) => bail!("Cannot convert runtime value (Bool) to number"),
            RuntimeValue::Null => bail!("Cannot convert runtime value (Null) to number"),
            RuntimeValue::Number(number) => Ok(number),
            RuntimeValue::String(string) => string
                .parse()
                .map_err(|_| anyhow!("Cannot convert runtime value (String, {string}) to number")),
            RuntimeValue::Callable(_) => bail!("Cannot convert runtime value (Callable) to number"),
            RuntimeValue::Instance(_) => bail!("Cannot convert runtime value (Instance) to number"),
            RuntimeValue::Class(_) => bail!("Cannot convert runtime value (Class) to number"),
        }
    }
}

impl From<String> for RuntimeValue {
    fn from(value: String) -> Self {
        RuntimeValue::String(value)
    }
}

impl From<LiteralValue> for RuntimeValue {
    fn from(value: LiteralValue) -> Self {
        Self::from(&value)
    }
}

impl From<&LiteralValue> for RuntimeValue {
    fn from(value: &LiteralValue) -> Self {
        match value {
            LiteralValue::True => RuntimeValue::Bool(true),
            LiteralValue::False => RuntimeValue::Bool(false),
            LiteralValue::Null => RuntimeValue::Null,
            LiteralValue::Number(n) => RuntimeValue::Number(*n),
            LiteralValue::String(s) => RuntimeValue::String(s.clone()),
        }
    }
}
