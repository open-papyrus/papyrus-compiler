use crate::ast::event::{CustomEvent, Event};
use crate::ast::flags::{display_flags, ScriptFlag};
use crate::ast::function::Function;
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::property::{Property, PropertyGroup};
use crate::ast::state::State;
use crate::ast::structure::Structure;
use crate::ast::variable::ScriptVariable;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum ScriptContent<'a> {
    Variable(ScriptVariable<'a>),
    Structure(Structure<'a>),
    CustomEvent(CustomEvent<'a>),
    Property(Property<'a>),
    PropertyGroup(PropertyGroup<'a>),
    State(State<'a>),
    Function(Function<'a>),
    Event(Event<'a>),
}

impl<'a> Display for ScriptContent<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ScriptContent::Variable(content) => write!(f, "{}", content),
            ScriptContent::Structure(content) => write!(f, "{}", content),
            ScriptContent::CustomEvent(content) => write!(f, "{}", content),
            ScriptContent::Property(content) => write!(f, "{}", content),
            ScriptContent::PropertyGroup(content) => write!(f, "{}", content),
            ScriptContent::State(state) => write!(f, "{}", state),
            ScriptContent::Function(content) => write!(f, "{}", content),
            ScriptContent::Event(content) => write!(f, "{}", content),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Script<'a> {
    pub name: Node<Identifier<'a>>,
    pub extends: Option<Node<Identifier<'a>>>,
    pub flags: Option<Vec<Node<ScriptFlag>>>,
    pub contents: Option<Vec<Node<ScriptContent<'a>>>>,
}

impl<'a> Script<'a> {
    pub fn new(
        name: Node<Identifier<'a>>,
        extends: Option<Node<Identifier<'a>>>,
        flags: Option<Vec<Node<ScriptFlag>>>,
        contents: Option<Vec<Node<ScriptContent<'a>>>>,
    ) -> Self {
        Self {
            name,
            extends,
            flags,
            contents,
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

        match self.contents.as_ref() {
            Some(contents) => {
                for content in contents {
                    write!(f, "\n{}", content)?;
                }
            }
            None => {}
        }

        Ok(())
    }
}
