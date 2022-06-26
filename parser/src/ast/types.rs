use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::choose_result;
use crate::parser::{Parse, Parser, ParserError, ParserResult};
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

        let is_array = match parser.peek_token() {
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
    parser
        .expect_keyword(KeywordKind::Var)
        .map(|_| TypeName::Var)
}

fn type_name_identifier_parser<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, TypeName<'source>> {
    let (token, range) = parser.consume()?;
    match token {
        Token::Identifier(value) => Ok(TypeName::Identifier(*value)),
        _ => Err(ParserError::ExpectedToken {
            expected: Token::Identifier(""),
            found: (*token, range.clone()),
        }),
    }
}

fn base_type_parser<'source>(parser: &mut Parser<'source>) -> ParserResult<'source, BaseType> {
    choose_result!(
        parser.optional_result(|parser| parser
            .expect_keyword(KeywordKind::Bool)
            .map(|_| BaseType::Bool)),
        parser.optional_result(|parser| parser
            .expect_keyword(KeywordKind::Int)
            .map(|_| BaseType::Int)),
        parser.optional_result(|parser| parser
            .expect_keyword(KeywordKind::Float)
            .map(|_| BaseType::Float)),
        parser.optional_result(|parser| parser
            .expect_keyword(KeywordKind::String)
            .map(|_| BaseType::String)),
    )
}

fn parameter_type_parser<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, ParameterType> {
    choose_result!(
        parser.optional_result(|parser| parser
            .expect_keyword(KeywordKind::ScriptEventName)
            .map(|_| ParameterType::ScriptEventName)),
        parser.optional_result(|parser| parser
            .expect_keyword(KeywordKind::CustomEventName)
            .map(|_| ParameterType::CustomEventName)),
        parser.optional_result(|parser| parser
            .expect_keyword(KeywordKind::StructVarName)
            .map(|_| ParameterType::StructVarName)),
    )
}

impl<'source> Parse<'source> for TypeName<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        choose_result!(
            parser.optional_result(|parser| type_name_var_parser(parser)),
            parser.optional_result(|parser| type_name_identifier_parser(parser)),
            parser.optional_result(|parser| base_type_parser(parser).map(TypeName::BaseType)),
            parser.optional_result(
                |parser| parameter_type_parser(parser).map(TypeName::ParameterType)
            ),
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
