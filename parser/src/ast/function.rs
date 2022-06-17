use crate::ast::flags::{display_flags, function_flag_parser, FunctionFlag};
use crate::ast::identifier::{identifier_parser, Identifier};
use crate::ast::literal::{literal_parser, Literal};
use crate::ast::node::Node;
use crate::ast::statement::{display_statements, statement_parser, Statement};
use crate::ast::types::{type_parser, type_with_identifier_parser, Type};
use crate::parse::TokenParser;
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;
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

        display_flags(&self.flags, f)?;
        display_statements(&self.statements, f)?;

        write!(f, "\nEndFunction")?;

        Ok(())
    }
}

/// ```ebnf
/// <parameter>  ::= <type> <identifier> ['=' <constant>]
/// ```
pub fn function_parameter_parser<'a>() -> impl TokenParser<'a, FunctionParameter<'a>> {
    type_with_identifier_parser()
        .then(
            just(Token::Operator(OperatorKind::Assignment))
                .ignore_then(literal_parser().map_with_span(Node::new))
                .or_not(),
        )
        .map(|output| {
            let ((type_node, identifier), default_value) = output;
            FunctionParameter::new(type_node, identifier, default_value)
        })
}

/// ```ebnf
/// <function> ::= <function header> [<function block> 'EndFunction']
///
/// <function header> ::= [<type>] 'Function' <identifier> '(' [<parameters>] ')' <flags>*
/// <function block> ::= <statement>*
///
/// <parameters> ::= <parameter> (',' <parameter>)*
/// ```
pub fn function_parser<'a>() -> impl TokenParser<'a, Function<'a>> {
    type_parser()
        .map_with_span(Node::new)
        .or_not()
        .then_ignore(just(Token::Keyword(KeywordKind::Function)))
        .then(identifier_parser().map_with_span(Node::new))
        .then_ignore(just(Token::Operator(OperatorKind::ParenthesisOpen)))
        .then(
            function_parameter_parser()
                .map_with_span(Node::new)
                .separated_by(just(Token::Operator(OperatorKind::Comma)))
                .map(|parameters| {
                    if parameters.is_empty() {
                        None
                    } else {
                        Some(parameters)
                    }
                }),
        )
        .then_ignore(just(Token::Operator(OperatorKind::ParenthesisClose)))
        .then(
            function_flag_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1)
                .or_not(),
        )
        .then(
            statement_parser()
                .map_with_span(Node::new)
                .repeated()
                .then_ignore(just(Token::Keyword(KeywordKind::EndFunction)))
                .or_not()
                .map(|statements| match statements {
                    Some(statements) => {
                        if statements.is_empty() {
                            None
                        } else {
                            Some(statements)
                        }
                    }
                    None => None,
                }),
        )
        .map(|output| {
            let ((((return_type, identifier), parameters), flags), statements) = output;
            Function::new(return_type, identifier, parameters, flags, statements)
        })
}

#[cfg(test)]
mod test {
    use crate::ast::expression::Expression;
    use crate::ast::flags::FunctionFlag;
    use crate::ast::function::{function_parser, Function, FunctionParameter};
    use crate::ast::literal::Literal;
    use crate::ast::node::Node;
    use crate::ast::statement::Statement;
    use crate::ast::types::{BaseType, Type, TypeName};
    use crate::parse::test_utils::run_tests;

    #[test]
    fn test_function_parser() {
        let data = vec![
            (
                "Function MyNativeFunction() Native",
                Function::new(
                    None,
                    Node::new("MyNativeFunction", (9..25).into()),
                    None,
                    Some(vec![Node::new(FunctionFlag::Native, (28..34).into())]),
                    None,
                ),
            ),
            (
                "int Function GetVersion(int version = 1)\nreturn version\nEndFunction",
                Function::new(
                    Some(Node::new(
                        Type::new(
                            Node::new(TypeName::BaseType(BaseType::Int), (0..3).into()),
                            false,
                        ),
                        (0..3).into(),
                    )),
                    Node::new("GetVersion", (13..23).into()),
                    Some(vec![Node::new(
                        FunctionParameter::new(
                            Node::new(
                                Type::new(
                                    Node::new(TypeName::BaseType(BaseType::Int), (24..27).into()),
                                    false,
                                ),
                                (24..27).into(),
                            ),
                            Node::new("version", (28..35).into()),
                            Some(Node::new(Literal::Integer(1), (38..39).into())),
                        ),
                        (24..39).into(),
                    )]),
                    None,
                    Some(vec![Node::new(
                        Statement::Return(Some(Node::new(
                            Expression::Identifier(Node::new("version", (48..55).into())),
                            (48..55).into(),
                        ))),
                        (41..55).into(),
                    )]),
                ),
            ),
        ];

        run_tests(data, function_parser);
    }
}
