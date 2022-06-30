use crate::ast::flags::{is_native_function, FunctionFlag};
use crate::ast::identifier::Identifier;
use crate::ast::literal::Literal;
use crate::ast::node::Node;
use crate::ast::statement::Statement;
use crate::ast::types::{type_with_identifier_parser, Type};
use crate::parser::{Parse, Parser};
use crate::parser_error::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParameter<'source> {
    pub type_node: Node<Type<'source>>,
    pub name: Node<Identifier<'source>>,
    pub default_value: Option<Node<Literal<'source>>>,
}

impl<'source> FunctionParameter<'source> {
    pub fn new(
        type_node: Node<Type<'source>>,
        name: Node<Identifier<'source>>,
        default_value: Option<Node<Literal<'source>>>,
    ) -> Self {
        Self {
            type_node,
            name,
            default_value,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function<'source> {
    pub return_type: Option<Node<Type<'source>>>,
    pub name: Node<Identifier<'source>>,
    pub parameters: Option<Vec<Node<FunctionParameter<'source>>>>,
    pub flags: Option<Vec<Node<FunctionFlag>>>,
    pub statements: Option<Vec<Node<Statement<'source>>>>,
}

impl<'source> Function<'source> {
    pub fn new(
        return_type: Option<Node<Type<'source>>>,
        name: Node<Identifier<'source>>,
        parameters: Option<Vec<Node<FunctionParameter<'source>>>>,
        flags: Option<Vec<Node<FunctionFlag>>>,
        statements: Option<Vec<Node<Statement<'source>>>>,
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

/// ```ebnf
/// <parameter>  ::= <type> <identifier> ['=' <constant>]
/// ```
impl<'source> Parse<'source> for FunctionParameter<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        let (type_node, parameter_name) = type_with_identifier_parser(parser)?;

        let default_value = parser.optional(|parser| {
            parser.expect_operator(OperatorKind::Assignment)?;
            parser.parse_node::<Literal>()
        });

        Ok(FunctionParameter::new(
            type_node,
            parameter_name,
            default_value,
        ))
    }
}

/// ```ebnf
/// <function> ::= <function header> [<function body>]
///
/// <function header> ::= [<type>] 'Function' <identifier> '(' [<parameters>] ')' <flags>*
/// <function body> ::= <function block> 'EndFunction'
/// <function block> ::= <statement>*
///
/// <parameters> ::= <parameter> (',' <parameter>)*
/// ```
impl<'source> Parse<'source> for Function<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        let return_type = parser.parse_node_optional::<Type>();

        parser.expect_keyword(KeywordKind::Function)?;

        let function_name = parser.parse_node::<Identifier>()?;

        parser.expect_operator(OperatorKind::ParenthesisOpen)?;

        let parameters = parser.optional_separated(
            |parser| parser.parse_node::<FunctionParameter>(),
            OperatorKind::Comma,
            OperatorKind::ParenthesisClose,
        );

        parser.expect_operator(OperatorKind::ParenthesisClose)?;

        let flags = parser.parse_node_optional_repeated::<FunctionFlag>();

        let statements = parse_function_body(
            parser,
            !is_native_function(&flags),
            KeywordKind::EndFunction,
        )?;

        Ok(Function::new(
            return_type,
            function_name,
            parameters,
            flags,
            statements,
        ))
    }
}

pub(crate) fn parse_function_body<'script>(
    parser: &mut Parser<'script>,
    requires_function_body: bool,
    keyword: KeywordKind,
) -> ParserResult<'script, Option<Vec<Node<Statement<'script>>>>> {
    let statements = match requires_function_body {
        true => parser.optional_parse_node_until_keyword::<Statement>(keyword)?,
        false => None,
    };

    if requires_function_body {
        parser.expect_keyword(keyword)?;
    }

    Ok(statements)
}

#[cfg(test)]
mod test {
    use crate::ast::expression::Expression;
    use crate::ast::flags::FunctionFlag;
    use crate::ast::function::{Function, FunctionParameter};
    use crate::ast::literal::Literal;
    use crate::ast::node::Node;
    use crate::ast::statement::Statement;
    use crate::ast::types::{BaseType, Type, TypeName};
    use crate::parser::test_utils::run_tests;

    #[test]
    fn test_function_parser() {
        let data = vec![
            (
                "Function MyNativeFunction(string name, int age) Native",
                Function::new(
                    None,
                    Node::new("MyNativeFunction", 9..25),
                    Some(vec![
                        Node::new(
                            FunctionParameter::new(
                                Node::new(
                                    Type::new(
                                        Node::new(TypeName::BaseType(BaseType::String), 26..32),
                                        false,
                                    ),
                                    26..32,
                                ),
                                Node::new("name", 33..37),
                                None,
                            ),
                            26..37,
                        ),
                        Node::new(
                            FunctionParameter::new(
                                Node::new(
                                    Type::new(
                                        Node::new(TypeName::BaseType(BaseType::Int), 39..42),
                                        false,
                                    ),
                                    39..42,
                                ),
                                Node::new("age", 43..46),
                                None,
                            ),
                            39..46,
                        ),
                    ]),
                    Some(vec![Node::new(FunctionFlag::Native, 48..54)]),
                    None,
                ),
            ),
            (
                "Function Set(int newValue) EndFunction",
                Function::new(
                    None,
                    Node::new("Set", 9..12),
                    Some(vec![Node::new(
                        FunctionParameter::new(
                            Node::new(
                                Type::new(
                                    Node::new(TypeName::BaseType(BaseType::Int), 13..16),
                                    false,
                                ),
                                13..16,
                            ),
                            Node::new("newValue", 17..25),
                            None,
                        ),
                        13..25,
                    )]),
                    None,
                    None,
                ),
            ),
            (
                "int Function GetVersion(int version = 1) return version EndFunction",
                Function::new(
                    Some(Node::new(
                        Type::new(Node::new(TypeName::BaseType(BaseType::Int), 0..3), false),
                        0..3,
                    )),
                    Node::new("GetVersion", 13..23),
                    Some(vec![Node::new(
                        FunctionParameter::new(
                            Node::new(
                                Type::new(
                                    Node::new(TypeName::BaseType(BaseType::Int), 24..27),
                                    false,
                                ),
                                24..27,
                            ),
                            Node::new("version", 28..35),
                            Some(Node::new(Literal::Integer(1), 38..39)),
                        ),
                        24..39,
                    )]),
                    None,
                    Some(vec![Node::new(
                        Statement::Return(Some(Node::new(
                            Expression::Identifier("version"),
                            48..55,
                        ))),
                        41..55,
                    )]),
                ),
            ),
        ];

        run_tests(data);
    }
}
