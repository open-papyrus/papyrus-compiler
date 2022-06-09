use crate::ast::identifier::Identifier;
use crate::ast::literal::Literal;
use crate::ast::node::Node;
use crate::ast::types::TypeName;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'a> {
    /// 'a && b'
    LogicalOperation {
        lhs: Node<Expression<'a>>,
        kind: Node<LogicalKind>,
        rhs: Node<Expression<'a>>,
    },
    /// 'a != b'
    Comparison {
        lhs: Node<Expression<'a>>,
        kind: Node<ComparisonKind>,
        rhs: Node<Expression<'a>>,
    },
    /// '!a'
    Unary {
        kind: Node<UnaryKind>,
        rhs: Node<Expression<'a>>,
    },
    /// 'a + b'
    Binary {
        lhs: Node<Expression<'a>>,
        kind: Node<BinaryKind>,
        rhs: Node<Expression<'a>>,
    },
    /// 'a[i]'
    ArrayAccess {
        array: Node<Expression<'a>>,
        index: Node<Expression<'a>>,
    },
    /// 'MyObject.MyProperty'
    Access {
        lhs: Node<Expression<'a>>,
        rhs: Node<Expression<'a>>,
    },
    /// 'a as string'
    Cast {
        lhs: Node<Expression<'a>>,
        rhs: Node<TypeName<'a>>,
    },
    /// 'a is string'
    TypeCheck {
        lhs: Node<Expression<'a>>,
        rhs: Node<TypeName<'a>>,
    },
    /// 'new int[10 * count]'
    NewArray {
        element_type: Node<TypeName<'a>>,
        size: Node<Expression<'a>>,
    },
    /// 'new Point'
    NewStructure(Node<TypeName<'a>>),
    /// 'DoSomething(a, b, 1, 3, "Hello World")'
    FunctionCall {
        name: Node<Identifier<'a>>,
        arguments: Option<Vec<Node<Expression<'a>>>>,
    },
    /// '1', '"Hello World"', '1.0', 'false', 'none'
    Constant(Node<Literal<'a>>),
    Identifier(Node<Identifier<'a>>),
}

impl<'a> Display for Expression<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::LogicalOperation { lhs, kind, rhs } => {
                write!(f, "{} {} {}", lhs, kind, rhs)
            }
            Expression::Comparison { lhs, kind, rhs } => write!(f, "{} {} {}", lhs, kind, rhs),
            Expression::Unary { kind, rhs } => write!(f, "{}{}", kind, rhs),
            Expression::Binary { lhs, kind, rhs } => write!(f, "{} {} {}", lhs, kind, rhs),
            Expression::ArrayAccess { array, index } => write!(f, "{}[{}]", array, index),
            Expression::Access { lhs, rhs } => write!(f, "{}.{}", lhs, rhs),
            Expression::Cast { lhs, rhs } => write!(f, "{} as {}", lhs, rhs),
            Expression::TypeCheck { lhs, rhs } => write!(f, "{} is {}", lhs, rhs),
            Expression::NewArray { element_type, size } => {
                write!(f, "new {}[{}]", element_type, size)
            }
            Expression::NewStructure(type_name) => write!(f, "new {}", type_name),
            Expression::FunctionCall { name, arguments } => {
                write!(f, "{} (", name)?;

                match arguments.as_ref() {
                    Some(arguments) => {
                        for i in 0..arguments.len() {
                            let argument = arguments.get(i).unwrap();
                            if i == arguments.len() - 1 {
                                write!(f, "{}", argument)?;
                            } else {
                                write!(f, "{}, ", argument)?;
                            }
                        }
                    }
                    None => {}
                }

                write!(f, ")")?;

                Ok(())
            }
            Expression::Constant(value) => write!(f, "{}", value),
            Expression::Identifier(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum LogicalKind {
    /// '&&'
    And,
    /// '||'
    Or,
}

impl Display for LogicalKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalKind::And => write!(f, "&&"),
            LogicalKind::Or => write!(f, "||"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ComparisonKind {
    /// '=='
    EqualTo,
    /// '!='
    NotEqualTo,
    /// '>'
    GreaterThan,
    /// '<'
    LessThan,
    /// '>='
    GreaterThanOrEqualTo,
    /// '<='
    LessThanOrEqualTo,
}

impl Display for ComparisonKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ComparisonKind::EqualTo => write!(f, "=="),
            ComparisonKind::NotEqualTo => write!(f, "!="),
            ComparisonKind::GreaterThan => write!(f, ">"),
            ComparisonKind::LessThan => write!(f, "<"),
            ComparisonKind::GreaterThanOrEqualTo => write!(f, ">="),
            ComparisonKind::LessThanOrEqualTo => write!(f, "<="),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum UnaryKind {
    /// '!'
    LogicalNot,
    /// '-'
    Negative,
}

impl Display for UnaryKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryKind::LogicalNot => write!(f, "!"),
            UnaryKind::Negative => write!(f, "-"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BinaryKind {
    /// '+'
    Addition,
    /// '-'
    Subtraction,
    /// '*'
    Multiplication,
    /// '/'
    Division,
    /// '%'
    Modulus,
}

impl Display for BinaryKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryKind::Addition => write!(f, "+"),
            BinaryKind::Subtraction => write!(f, "-"),
            BinaryKind::Multiplication => write!(f, "*"),
            BinaryKind::Division => write!(f, "/"),
            BinaryKind::Modulus => write!(f, "%"),
        }
    }
}
