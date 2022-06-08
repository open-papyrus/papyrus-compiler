use crate::ast::expression::Expression;
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::types::Type;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    //TODO: If, While
    /// 'int x = 1'
    VariableDefinition {
        type_node: Node<Type<'a>>,
        name: Node<Identifier<'a>>,
        expression: Option<Node<Expression<'a>>>,
    },
    /// 'Return x'
    Return(Option<Node<Expression<'a>>>),
    /// 'x = y'
    Assignment {
        lhs: Node<Expression<'a>>,
        kind: Node<AssignmentKind>,
        rhs: Node<Expression<'a>>,
    },
}

impl<'a> Display for Statement<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::VariableDefinition {
                type_node,
                name,
                expression,
            } => match expression.as_ref() {
                Some(expression) => write!(f, "{} {} = {}", type_node, name, expression),
                None => write!(f, "{} {}", type_node, name),
            },
            Statement::Return(expression) => match expression.as_ref() {
                Some(expression) => write!(f, "Return {}", expression),
                None => write!(f, "Return"),
            },
            Statement::Assignment { lhs, kind, rhs } => write!(f, "{} {} {}", lhs, kind, rhs),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum AssignmentKind {
    /// '='
    Normal,
    /// '+='
    Addition,
    /// '-='
    Subtraction,
    /// '*='
    Multiplication,
    /// '/='
    Division,
    /// '%='
    Modulus,
}

impl Display for AssignmentKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AssignmentKind::Normal => write!(f, "="),
            AssignmentKind::Addition => write!(f, "+="),
            AssignmentKind::Subtraction => write!(f, "-="),
            AssignmentKind::Multiplication => write!(f, "*="),
            AssignmentKind::Division => write!(f, "/="),
            AssignmentKind::Modulus => write!(f, "%="),
        }
    }
}
