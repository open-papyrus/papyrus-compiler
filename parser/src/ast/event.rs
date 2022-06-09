use crate::ast::flags::{display_flags, FunctionFlag};
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::statement::Statement;
use crate::ast::types::TypeName;
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

#[derive(Debug, PartialEq, Clone)]
pub struct EventParameter<'a> {
    pub type_name: Node<TypeName<'a>>,
    pub name: Node<Identifier<'a>>,
}

impl<'a> EventParameter<'a> {
    pub fn new(type_name: Node<TypeName<'a>>, name: Node<Identifier<'a>>) -> Self {
        Self { type_name, name }
    }
}

impl<'a> Display for EventParameter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.type_name, self.name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Event<'a> {
    // TODO: remote events
    pub name: Node<Identifier<'a>>,
    pub parameters: Option<Vec<Node<EventParameter<'a>>>>,
    pub flags: Option<Vec<Node<FunctionFlag>>>,
    pub statements: Vec<Node<Statement<'a>>>,
}

impl<'a> Event<'a> {
    pub fn new(
        name: Node<Identifier<'a>>,
        parameters: Option<Vec<Node<EventParameter<'a>>>>,
        flags: Option<Vec<Node<FunctionFlag>>>,
        statements: Vec<Node<Statement<'a>>>,
    ) -> Self {
        Self {
            name,
            parameters,
            flags,
            statements,
        }
    }
}

impl<'a> Display for Event<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Event {} (", self.name)?;

        match self.parameters.as_ref() {
            Some(parameters) => {
                for i in 0..parameters.len() {
                    let parameter = parameters.get(i).unwrap();
                    if i == parameters.len() - 1 {
                        write!(f, "{}", parameter)?;
                    } else {
                        write!(f, "{}, ", parameter)?;
                    }
                }
            }
            None => {}
        }

        write!(f, ")")?;

        display_flags(&self.flags, f)?;

        for statement in &self.statements {
            write!(f, "\n{}", statement)?;
        }

        write!(f, "\nEndEvent")?;

        Ok(())
    }
}
