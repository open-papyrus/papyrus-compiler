use crate::ast::flags::FunctionFlag;
use crate::ast::identifier::Identifier;
use crate::ast::literal::Literal;
use crate::ast::node::Node;
use crate::ast::statement::Statement;
use crate::ast::types::Type;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParameter<'a> {
    pub type_node: Node<Type<'a>>,
    pub name: Node<Identifier<'a>>,
    pub default_value: Option<Node<Literal<'a>>>,
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
    pub return_type: Option<Node<Type<'a>>>,
    pub name: Node<Identifier<'a>>,
    pub parameters: Option<Vec<Node<FunctionParameter<'a>>>>,
    pub flags: Option<Vec<Node<FunctionFlag>>>,
    pub statements: Option<Vec<Node<Statement<'a>>>>,
}

impl<'a> Function<'a> {
    pub fn new(
        return_type: Option<Node<Type<'a>>>,
        name: Node<Identifier<'a>>,
        parameters: Option<Vec<Node<FunctionParameter<'a>>>>,
        flags: Option<Vec<Node<FunctionFlag>>>,
        statements: Option<Vec<Node<Statement<'a>>>>,
    ) -> Self {
        Self {
            return_type,
            name,
            parameters,
            flags,
            statements,
        }
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

        match self.statements.as_ref() {
            Some(statements) => {
                for statement in statements {
                    write!(f, "\n{}", statement)?;
                }
            }
            None => {}
        }

        write!(f, "\nEndFunction")?;

        Ok(())
    }
}
