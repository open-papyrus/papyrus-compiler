use crate::ast::flags::{display_flags, ScriptFlag};
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub struct Script<'a> {
    pub name: Node<Identifier<'a>>,
    pub extends: Option<Node<Identifier<'a>>>,
    pub flags: Option<Vec<Node<ScriptFlag>>>,
}

impl<'a> Script<'a> {
    pub fn new(
        name: Node<Identifier<'a>>,
        extends: Option<Node<Identifier<'a>>>,
        flags: Option<Vec<Node<ScriptFlag>>>,
    ) -> Self {
        Self {
            name,
            extends,
            flags,
        }
    }
}

impl<'a> Display for Script<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ScriptName {}", self.name)?;

        match self.extends.as_ref() {
            Some(extends) => write!(f, "Extends {}", extends)?,
            None => {}
        }

        display_flags(&self.flags, f)?;

        Ok(())
    }
}
