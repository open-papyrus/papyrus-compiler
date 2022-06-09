use crate::ast::flags::{display_flags, GroupFlag, PropertyFlag};
use crate::ast::function::Function;
use crate::ast::identifier::Identifier;
use crate::ast::literal::Literal;
use crate::ast::node::Node;
use crate::ast::types::Type;
use std::fmt::{Display, Formatter};

// TODO: FullProperty

#[derive(Debug, PartialEq, Clone)]
pub struct PropertyGroup<'a> {
    pub name: Node<Identifier<'a>>,
    pub flags: Option<Vec<Node<GroupFlag>>>,
    pub properties: Vec<Node<Property<'a>>>,
}

impl<'a> PropertyGroup<'a> {
    pub fn new(
        name: Node<Identifier<'a>>,
        flags: Option<Vec<Node<GroupFlag>>>,
        properties: Vec<Node<Property<'a>>>,
    ) -> Self {
        Self {
            name,
            flags,
            properties,
        }
    }
}

impl<'a> Display for PropertyGroup<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Group {}", self.name)?;

        display_flags(&self.flags, f)?;

        for property in &self.properties {
            write!(f, "\n{}", property)?;
        }

        write!(f, "\nEndGroup")?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Property<'a> {
    AutoProperty(AutoProperty<'a>),
    FullProperty(FullProperty<'a>),
}

impl<'a> Display for Property<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Property::AutoProperty(property) => write!(f, "{}", property),
            Property::FullProperty(property) => write!(f, "{}", property),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AutoProperty<'a> {
    pub type_node: Node<Type<'a>>,
    pub name: Node<Identifier<'a>>,
    pub value: Option<Node<Literal<'a>>>,
    pub flags: Option<Vec<Node<PropertyFlag>>>,
    pub is_read_only: bool,
}

impl<'a> AutoProperty<'a> {
    pub fn new(
        type_node: Node<Type<'a>>,
        name: Node<Identifier<'a>>,
        value: Option<Node<Literal<'a>>>,
        flags: Option<Vec<Node<PropertyFlag>>>,
        is_read_only: bool,
    ) -> Self {
        Self {
            type_node,
            name,
            value,
            flags,
            is_read_only,
        }
    }
}

impl<'a> Display for AutoProperty<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} Property {}", self.type_node, self.name)?;
        match &self.value {
            Some(value) => write!(f, " = {}", value)?,
            None => {}
        }

        if self.is_read_only {
            write!(f, " AutoReadOnly")?;
        } else {
            write!(f, " Auto")?;
        }

        display_flags(&self.flags, f)?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FullProperty<'a> {
    pub type_node: Node<Type<'a>>,
    pub name: Node<Identifier<'a>>,
    pub flags: Option<Vec<Node<PropertyFlag>>>,
    pub functions: Vec<Node<Function<'a>>>,
}

impl<'a> FullProperty<'a> {
    pub fn new(
        type_node: Node<Type<'a>>,
        name: Node<Identifier<'a>>,
        flags: Option<Vec<Node<PropertyFlag>>>,
        functions: Vec<Node<Function<'a>>>,
    ) -> Self {
        Self {
            type_node,
            name,
            flags,
            functions,
        }
    }
}

impl<'a> Display for FullProperty<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} Property {}", self.type_node, self.name)?;

        display_flags(&self.flags, f)?;

        for function in &self.functions {
            write!(f, "\n{}", function)?;
        }

        Ok(())
    }
}
