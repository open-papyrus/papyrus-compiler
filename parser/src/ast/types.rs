use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub struct Type<'a> {
    pub name: Node<TypeName<'a>>,
    pub is_array: bool,
}

impl<'a> Type<'a> {
    pub fn new(name: Node<TypeName<'a>>, is_array: bool) -> Self {
        Self { name, is_array }
    }
}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if self.is_array {
            write!(f, "[]")?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TypeName<'a> {
    BaseType(BaseType),
    Identifier(Identifier<'a>),
}

impl<'a> Display for TypeName<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeName::BaseType(base_type) => write!(f, "{}", base_type),
            TypeName::Identifier(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, strum_macros::Display)]
pub enum BaseType {
    Bool,
    Int,
    Float,
    String,
    Var,
}
