use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::variable::ScriptVariable;
use std::fmt::{Display, Formatter};

pub type StructureField<'a> = ScriptVariable<'a>;

#[derive(Debug, PartialEq, Clone)]
pub struct Structure<'a> {
    pub name: Node<Identifier<'a>>,
    pub fields: Vec<Node<StructureField<'a>>>,
}

impl<'a> Structure<'a> {
    pub fn new(name: Node<Identifier<'a>>, fields: Vec<Node<StructureField<'a>>>) -> Self {
        Self { name, fields }
    }
}

impl<'a> Display for Structure<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Struct {}", self.name)?;

        for field in &self.fields {
            write!(f, "\n{}", field)?;
        }

        write!(f, "\nEndStruct")?;

        Ok(())
    }
}
