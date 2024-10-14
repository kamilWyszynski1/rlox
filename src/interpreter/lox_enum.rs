use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct LoxEnum {
    pub name: String,
    pub variants: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct LoxEnumVariant {
    pub enum_ref: Rc<LoxEnum>,
    pub variant: String,
}

impl Display for LoxEnumVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.enum_ref.name, self.variant)
    }
}
