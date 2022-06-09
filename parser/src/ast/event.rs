// TODO: events

use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub struct CustomEvent<'a> {
    pub name: Node<Identifier<'a>>,
}

impl<'a> CustomEvent<'a> {
    pub fn new(name: Node<Identifier<'a>>) -> Self {
        Self { name }
    }
}

impl<'a> Display for CustomEvent<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "CustomEvent {}", self.name)
    }
}
