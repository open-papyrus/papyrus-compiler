use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::parser::{Parse, Parser, ParserResult};
use crate::{choose_optional, select_tokens};
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct Type<'source> {
    pub name: Node<TypeName<'source>>,
    pub is_array: bool,
}

impl<'source> Type<'source> {
    pub fn new(name: Node<TypeName<'source>>, is_array: bool) -> Self {
        Self { name, is_array }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TypeName<'source> {
    Var,
    BaseType(BaseType),
    Identifier(Identifier<'source>),
    ParameterType(ParameterType),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BaseType {
    Bool,
    Int,
    Float,
    String,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ParameterType {
    ScriptEventName,
    CustomEventName,
    StructVarName,
}

pub(crate) fn type_with_identifier_parser<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, (Node<Type<'source>>, Node<Identifier<'source>>)> {
    let type_node = parser.parse_node::<Type>()?;
    let identifier = parser.parse_node::<Identifier>()?;
    Ok((type_node, identifier))
}

impl<'source> Parse<'source> for Type<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        let type_name = parser.parse_node::<TypeName>()?;

        let is_array = match parser.peek() {
            Some(Token::Operator(OperatorKind::SquareBracketsOpen)) => {
                parser.consume()?;
                parser.expect_operator(OperatorKind::SquareBracketsClose)?;
                true
            }
            _ => false,
        };

        Ok(Type::new(type_name, is_array))
    }
}

fn type_name_var_parser<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, TypeName<'source>> {
    select_tokens!(parser, "Type Var",
        Token::Keyword(KeywordKind::Var) => TypeName::Var
    )
}

fn type_name_identifier_parser<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, TypeName<'source>> {
    select_tokens!(parser, "Type Identifier",
        Token::Identifier(value) => TypeName::Identifier(*value)
    )
}

fn base_type_parser<'source>(parser: &mut Parser<'source>) -> ParserResult<'source, BaseType> {
    select_tokens!(parser, "Base Type",
        Token::Keyword(KeywordKind::Bool) => BaseType::Bool,
        Token::Keyword(KeywordKind::Int) => BaseType::Int,
        Token::Keyword(KeywordKind::Float) => BaseType::Float,
        Token::Keyword(KeywordKind::String) => BaseType::String,
    )
}

fn parameter_type_parser<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, ParameterType> {
    select_tokens!(parser, "Parameter Type",
        Token::Keyword(KeywordKind::ScriptEventName) => ParameterType::ScriptEventName,
        Token::Keyword(KeywordKind::CustomEventName) => ParameterType::CustomEventName,
        Token::Keyword(KeywordKind::StructVarName) => ParameterType::StructVarName,
    )
}

impl<'source> Parse<'source> for TypeName<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        choose_optional!(
            parser,
            "Type Name",
            parser.optional(type_name_var_parser),
            parser.optional(type_name_identifier_parser),
            parser.optional(base_type_parser).map(TypeName::BaseType),
            parser
                .optional(parameter_type_parser)
                .map(TypeName::ParameterType),
        )
    }
}

#[cfg(test)]
mod test {
    use crate::ast::node::Node;
    use crate::ast::types::{BaseType, ParameterType, Type, TypeName};
    use crate::parser::test_utils::run_tests;

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

        run_tests(data);
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

        run_tests(data);
    }
}
