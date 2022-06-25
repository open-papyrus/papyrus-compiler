use crate::ast::flags::{display_flags, FunctionFlag};
use crate::ast::identifier::Identifier;
use crate::ast::literal::Literal;
use crate::ast::node::Node;
use crate::ast::statement::{display_statements, Statement};
use crate::ast::types::{type_with_identifier_parser, Type};
use crate::parser::{Parse, Parser, ParserResult};
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use std::fmt::{Display, Formatter};

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

impl<'source> Display for FunctionParameter<'source> {
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

impl<'source> Display for Function<'source> {
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
        );

        parser.expect_operator(OperatorKind::ParenthesisClose)?;

        let flags = parser.parse_node_optional_repeated::<FunctionFlag>();

        // native functions don't have a function body, it's only a function declaration
        let statements = parser.parse_node_optional_repeated::<Statement>();
        if statements.is_some() {
            parser.expect_keyword(KeywordKind::EndFunction)?;
        }

        Ok(Function::new(
            return_type,
            function_name,
            parameters,
            flags,
            statements,
        ))
    }
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
                "Function MyNativeFunction() Native",
                Function::new(
                    None,
                    Node::new("MyNativeFunction", 9..25),
                    None,
                    Some(vec![Node::new(FunctionFlag::Native, 28..34)]),
                    None,
                ),
            ),
            (
                "Function Set(int newValue)",
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
                "int Function GetVersion(int version = 1)\nreturn version\nEndFunction",
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
                            Expression::Identifier(Node::new("version", 48..55)),
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
