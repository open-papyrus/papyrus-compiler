use crate::ast::flags::FunctionFlag;
use crate::ast::identifier::Identifier;
use crate::ast::literal::Literal;
use crate::ast::node::Node;
use crate::ast::types::Type;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParameter<'a> {
    type_node: Node<Type<'a>>,
    name: Node<Identifier<'a>>,
    default_value: Option<Node<Literal<'a>>>,
}

impl<'a> FunctionParameter<'a> {
    pub fn new(
        type_node: Node<Type<'a>>,
        name: Node<Identifier<'a>>,
        default_value: Option<Node<Literal<'a>>>,
    ) -> Self {
        Self {
            type_node,
            name,
            default_value,
        }
    }

    pub fn type_node(&self) -> &Node<Type<'a>> {
        &self.type_node
    }

    pub fn name(&self) -> &Node<Identifier<'a>> {
        &self.name
    }

    pub fn default_value(&self) -> Option<&Node<Literal<'a>>> {
        self.default_value.as_ref()
    }
}

impl<'a> Display for FunctionParameter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.type_node, self.name)?;
        match self.default_value.as_ref() {
            Some(default_value) => write!(f, " = {}", default_value)?,
            None => {}
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function<'a> {
    return_type: Option<Node<Type<'a>>>,
    name: Node<Identifier<'a>>,
    parameters: Option<Vec<Node<FunctionParameter<'a>>>>,
    flags: Option<Vec<Node<FunctionFlag>>>,
}

impl<'a> Function<'a> {
    pub fn new(
        return_type: Option<Node<Type<'a>>>,
        name: Node<Identifier<'a>>,
        parameters: Option<Vec<Node<FunctionParameter<'a>>>>,
        flags: Option<Vec<Node<FunctionFlag>>>,
    ) -> Self {
        Self {
            return_type,
            name,
            parameters,
            flags,
        }
    }

    pub fn return_type(&self) -> Option<&Node<Type<'a>>> {
        self.return_type.as_ref()
    }

    pub fn name(&self) -> &Node<Identifier<'a>> {
        &self.name
    }

    pub fn parameters(&self) -> Option<&Vec<Node<FunctionParameter<'a>>>> {
        self.parameters.as_ref()
    }

    pub fn flags(&self) -> Option<&Vec<Node<FunctionFlag>>> {
        self.flags.as_ref()
    }
}

impl<'a> Display for Function<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.return_type.as_ref() {
            Some(return_type) => write!(f, "{} ", return_type)?,
            None => {}
        }

        write!(f, "Function {} (", self.name)?;

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
        write!(f, "\nEndFunction")?;

        Ok(())
    }
}
