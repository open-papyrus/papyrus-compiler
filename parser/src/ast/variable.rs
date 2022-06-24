use crate::ast::flags::{display_flags, VariableFlag};
use crate::ast::identifier::Identifier;
use crate::ast::literal::Literal;
use crate::ast::node::Node;
use crate::ast::types::{type_with_identifier_parser, Type};
use crate::parser::{Parse, Parser, ParserResult};
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct ScriptVariable<'source> {
    pub type_node: Node<Type<'source>>,
    pub name: Node<Identifier<'source>>,
    pub initial_value: Option<Node<Literal<'source>>>,
    pub flags: Option<Vec<Node<VariableFlag>>>,
}

impl<'source> ScriptVariable<'source> {
    pub fn new(
        type_node: Node<Type<'source>>,
        name: Node<Identifier<'source>>,
        initial_value: Option<Node<Literal<'source>>>,
        flags: Option<Vec<Node<VariableFlag>>>,
    ) -> Self {
        Self {
            type_node,
            name,
            initial_value,
            flags,
        }
    }
}

impl<'source> Display for ScriptVariable<'source> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.type_node, self.name)?;

        match self.initial_value.as_ref() {
            Some(value) => write!(f, " = {}", value)?,
            None => {}
        };

        display_flags(&self.flags, f)?;

        Ok(())
    }
}

/// ```ebnf
/// <variable definition> ::= <type> <identifier> ['=' <constant>] (<flags>)*
/// ```
impl<'source> Parse<'source> for ScriptVariable<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        let (type_node, variable_name) = type_with_identifier_parser(parser)?;

        let initial_value = parser.optional(|parser| {
            parser.expect(Token::Operator(OperatorKind::Assignment))?;
            parser.parse_node::<Literal>()
        });

        let flags = parser.parse_node_optional_repeated::<VariableFlag>();

        Ok(ScriptVariable::new(
            type_node,
            variable_name,
            initial_value,
            flags,
        ))
    }
}

#[cfg(test)]
mod test {
    use crate::ast::flags::VariableFlag;
    use crate::ast::literal::Literal;
    use crate::ast::node::Node;
    use crate::ast::types::{BaseType, Type, TypeName};
    use crate::ast::variable::ScriptVariable;
    use crate::parser::test_utils::run_tests;

    #[test]
    fn test_script_variable_parser() {
        let data = vec![
            (
                "int x = 0",
                ScriptVariable::new(
                    Node::new(
                        Type::new(Node::new(TypeName::BaseType(BaseType::Int), 0..3), false),
                        0..3,
                    ),
                    Node::new("x", 4..5),
                    Some(Node::new(Literal::Integer(0), 8..9)),
                    None,
                ),
            ),
            (
                "quest myQuest Conditional Const Hidden",
                ScriptVariable::new(
                    Node::new(
                        Type::new(Node::new(TypeName::Identifier("quest"), 0..5), false),
                        0..5,
                    ),
                    Node::new("myQuest", 6..13),
                    None,
                    Some(vec![
                        Node::new(VariableFlag::Conditional, 14..25),
                        Node::new(VariableFlag::Const, 26..31),
                        Node::new(VariableFlag::Hidden, 32..38),
                    ]),
                ),
            ),
        ];

        run_tests(data);
    }
}
