use crate::ast::flags::VariableFlag;
use crate::ast::identifier::Identifier;
use crate::ast::literal::Literal;
use crate::ast::node::Node;
use crate::ast::types::Type;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct ScriptVariable<'a> {
    pub type_node: Node<Type<'a>>,
    pub name: Node<Identifier<'a>>,
    pub value: Option<Node<Literal<'a>>>,
    pub flags: Option<Vec<Node<VariableFlag>>>,
}

impl<'a> ScriptVariable<'a> {
    pub fn new(
        type_node: Node<Type<'a>>,
        name: Node<Identifier<'a>>,
        value: Option<Node<Literal<'a>>>,
        flags: Option<Vec<Node<VariableFlag>>>,
    ) -> Self {
        Self {
            type_node,
            name,
            value,
            flags,
        }
    }
}

impl<'a> Display for ScriptVariable<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.type_node, self.name)?;
        match &self.value {
            Some(value) => write!(f, " = {}", value),
            None => Ok(()),
        }?;

        match &self.flags {
            Some(flags) => {
                for flag in flags {
                    write!(f, "{}", flag)?;
                }
            }
            None => {}
        };

        Ok(())
    }
}
