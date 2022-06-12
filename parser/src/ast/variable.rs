use crate::ast::flags::{display_flags, variable_flag_parser, VariableFlag};
use crate::ast::identifier::Identifier;
use crate::ast::literal::{literal_parser, Literal};
use crate::ast::node::Node;
use crate::ast::types::{type_with_identifier_parser, Type};
use crate::parse::TokenParser;
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct ScriptVariable<'a> {
    pub type_node: Node<Type<'a>>,
    pub name: Node<Identifier<'a>>,
    pub initial_value: Option<Node<Literal<'a>>>,
    pub flags: Option<Vec<Node<VariableFlag>>>,
}

impl<'a> ScriptVariable<'a> {
    pub fn new(
        type_node: Node<Type<'a>>,
        name: Node<Identifier<'a>>,
        initial_value: Option<Node<Literal<'a>>>,
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

impl<'a> Display for ScriptVariable<'a> {
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
pub fn script_variable_parser<'a>() -> impl TokenParser<'a, ScriptVariable<'a>> {
    type_with_identifier_parser()
        .then(
            just(Token::Operator(OperatorKind::Assignment))
                .ignore_then(literal_parser().map_with_span(Node::new))
                .or_not(),
        )
        .then(
            variable_flag_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1)
                .or_not(),
        )
        .map(|output| {
            let (((type_node, identifier), initial_value), flags) = output;
            ScriptVariable::new(type_node, identifier, initial_value, flags)
        })
}

#[cfg(test)]
mod test {
    use crate::ast::flags::VariableFlag;
    use crate::ast::literal::Literal;
    use crate::ast::node::Node;
    use crate::ast::types::{BaseType, Type, TypeName};
    use crate::ast::variable::{script_variable_parser, ScriptVariable};
    use crate::parse::test_utils::run_tests;

    #[test]
    fn test_script_variable_parser() {
        let data = vec![
            (
                "int x = 0",
                ScriptVariable::new(
                    Node::new(
                        Type::new(
                            Node::new(TypeName::BaseType(BaseType::Int), (0..3).into()),
                            false,
                        ),
                        (0..3).into(),
                    ),
                    Node::new("x", (4..5).into()),
                    Some(Node::new(Literal::Integer(0), (8..9).into())),
                    None,
                ),
            ),
            (
                "quest myQuest Conditional Const Hidden",
                ScriptVariable::new(
                    Node::new(
                        Type::new(
                            Node::new(TypeName::Identifier("quest"), (0..5).into()),
                            false,
                        ),
                        (0..5).into(),
                    ),
                    Node::new("myQuest", (6..13).into()),
                    None,
                    Some(vec![
                        Node::new(VariableFlag::Conditional, (14..25).into()),
                        Node::new(VariableFlag::Const, (26..31).into()),
                        Node::new(VariableFlag::Hidden, (32..38).into()),
                    ]),
                ),
            ),
        ];

        run_tests(data, script_variable_parser);
    }
}
