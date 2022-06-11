use crate::ast::event::Event;
use crate::ast::function::Function;
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub struct State<'a> {
    pub is_auto: bool,
    pub name: Node<Identifier<'a>>,
    pub contents: Vec<Node<StateContent<'a>>>,
}

impl<'a> State<'a> {
    pub fn new(
        is_auto: bool,
        name: Node<Identifier<'a>>,
        contents: Vec<Node<StateContent<'a>>>,
    ) -> Self {
        Self {
            is_auto,
            name,
            contents,
        }
    }
}

impl<'a> Display for State<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_auto {
            write!(f, "Auto ")?;
        }

        write!(f, "State {}", self.name)?;

        for content in &self.contents {
            write!(f, "\n{}", content)?;
        }

        write!(f, "\nEndState")?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StateContent<'a> {
    Function(Function<'a>),
    Event(Event<'a>),
}

impl<'a> Display for StateContent<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StateContent::Function(function) => write!(f, "{}", function),
            StateContent::Event(event) => write!(f, "{}", event),
        }
    }
}
