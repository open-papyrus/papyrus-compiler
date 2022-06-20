use crate::ast::identifier::{identifier_parser, Identifier};
use crate::ast::node::Node;
use crate::parse::TokenParser;
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub struct Type<'a> {
    pub name: Node<TypeName<'a>>,
    pub is_array: bool,
}

impl<'a> Type<'a> {
    pub fn new(name: Node<TypeName<'a>>, is_array: bool) -> Self {
        Self { name, is_array }
    }
}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if self.is_array {
            write!(f, "[]")?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TypeName<'a> {
    Var,
    BaseType(BaseType),
    Identifier(Identifier<'a>),
    ParameterType(ParameterType),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, strum_macros::Display)]
pub enum BaseType {
    Bool,
    Int,
    Float,
    String,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, strum_macros::Display)]
pub enum ParameterType {
    ScriptEventName,
    CustomEventName,
    StructVarName,
}

impl<'a> Display for TypeName<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeName::Var => write!(f, "Var"),
            TypeName::BaseType(base_type) => write!(f, "{}", base_type),
            TypeName::Identifier(value) => write!(f, "{}", value),
            TypeName::ParameterType(parameter_type) => write!(f, "{}", parameter_type),
        }
    }
}

pub fn type_name_parser<'a>() -> impl TokenParser<'a, TypeName<'a>> {
    just(Token::Keyword(KeywordKind::Var))
        .map(|_| TypeName::Var)
        .or(select! {
            Token::Keyword(KeywordKind::Bool) => BaseType::Bool,
            Token::Keyword(KeywordKind::Int) => BaseType::Int,
            Token::Keyword(KeywordKind::Float) => BaseType::Float,
            Token::Keyword(KeywordKind::String) => BaseType::String,
        }
        .map(TypeName::BaseType))
        .or(identifier_parser().map(TypeName::Identifier))
        .or(select! {
            Token::Keyword(KeywordKind::ScriptEventName) => ParameterType::ScriptEventName,
            Token::Keyword(KeywordKind::CustomEventName) => ParameterType::CustomEventName,
            Token::Keyword(KeywordKind::StructVarName) => ParameterType::StructVarName,
        }
        .map(TypeName::ParameterType))
}

pub fn type_parser<'a>() -> impl TokenParser<'a, Type<'a>> {
    type_name_parser()
        .map_with_span(Node::new)
        .then(
            just(Token::Operator(OperatorKind::SquareBracketsOpen))
                .then_ignore(just(Token::Operator(OperatorKind::SquareBracketsClose)))
                .or_not()
                .map(|x| x.is_some()),
        )
        .map(|(type_name, is_array)| Type::new(type_name, is_array))
}

pub fn type_with_identifier_parser<'a>(
) -> impl TokenParser<'a, (Node<Type<'a>>, Node<Identifier<'a>>)> {
    type_parser()
        .map_with_span(Node::new)
        .then(identifier_parser().map_with_span(Node::new))
}

#[cfg(test)]
mod test {
    use crate::ast::node::Node;
    use crate::ast::types::{
        type_name_parser, type_parser, BaseType, ParameterType, Type, TypeName,
    };
    use crate::parse::test_utils::run_tests;

    #[test]
    fn test_type_name_parser() {
        let data = vec![
            ("var", TypeName::Var),
            ("bool", TypeName::BaseType(BaseType::Bool)),
            ("int", TypeName::BaseType(BaseType::Int)),
            ("float", TypeName::BaseType(BaseType::Float)),
            ("string", TypeName::BaseType(BaseType::String)),
            ("Quest", TypeName::Identifier("Quest")),
            (
                "ScriptEventName",
                TypeName::ParameterType(ParameterType::ScriptEventName),
            ),
            (
                "CustomEventName",
                TypeName::ParameterType(ParameterType::CustomEventName),
            ),
            (
                "StructVarName",
                TypeName::ParameterType(ParameterType::StructVarName),
            ),
        ];

        run_tests(data, type_name_parser);
    }

    #[test]
    fn test_type_parser() {
        let data = vec![
            (
                "bool",
                Type::new(Node::new(TypeName::BaseType(BaseType::Bool), 0..4), false),
            ),
            (
                "int[]",
                Type::new(Node::new(TypeName::BaseType(BaseType::Int), 0..3), true),
            ),
            (
                "Quest[]",
                Type::new(Node::new(TypeName::Identifier("Quest"), 0..5), true),
            ),
        ];

        run_tests(data, type_parser);
    }
}
