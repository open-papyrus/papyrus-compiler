use crate::ast::identifier::{identifier_parser, Identifier};
use crate::ast::node::{display_nodes, Node};
use crate::ast::variable::{script_variable_parser, ScriptVariable};
use crate::parse::TokenParser;
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::fmt::{Display, Formatter};

pub type StructureField<'a> = ScriptVariable<'a>;

#[derive(Debug, PartialEq, Clone)]
pub struct Structure<'a> {
    pub name: Node<Identifier<'a>>,
    pub fields: Vec<Node<StructureField<'a>>>,
}

impl<'a> Structure<'a> {
    pub fn new(name: Node<Identifier<'a>>, fields: Vec<Node<StructureField<'a>>>) -> Self {
        Self { name, fields }
    }
}

impl<'a> Display for Structure<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Struct {}", self.name)?;

        display_nodes(&self.fields, "\n", f)?;

        write!(f, "\nEndStruct")?;

        Ok(())
    }
}

/// ```ebnf
/// <variable definition> ::= <type> <identifier> ['=' <constant>] (<flags>)* (<terminator> <docstring>)?
/// ```
pub fn struct_field_parser<'a>() -> impl TokenParser<'a, StructureField<'a>> {
    script_variable_parser()
}

/// ```ebnf
/// <Struct> ::= 'Struct' <identifier> <variable definition>+ 'EndStruct'
/// ```
pub fn struct_parser<'a>() -> impl TokenParser<'a, Structure<'a>> {
    just(Token::Keyword(KeywordKind::Struct))
        .ignore_then(identifier_parser().map_with_span(Node::new))
        .then(
            struct_field_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1),
        )
        .then_ignore(just(Token::Keyword(KeywordKind::EndStruct)))
        .map(|output| {
            let (identifier, fields) = output;
            Structure::new(identifier, fields)
        })
}

#[cfg(test)]
mod test {
    use crate::ast::node::Node;
    use crate::ast::structure::{struct_parser, Structure, StructureField};
    use crate::ast::types::{BaseType, Type, TypeName};
    use crate::parse::test_utils::run_test;

    #[test]
    fn test_struct_parser() {
        let src = "Struct Point\nfloat X\nfloat Y\nEndStruct";
        let expected = Structure::new(
            Node::new("Point", (7..12).into()),
            vec![
                Node::new(
                    StructureField::new(
                        Node::new(
                            Type::new(
                                Node::new(TypeName::BaseType(BaseType::Float), (13..18).into()),
                                false,
                            ),
                            (13..18).into(),
                        ),
                        Node::new("X", (19..20).into()),
                        None,
                        None,
                    ),
                    (13..20).into(),
                ),
                Node::new(
                    StructureField::new(
                        Node::new(
                            Type::new(
                                Node::new(TypeName::BaseType(BaseType::Float), (21..26).into()),
                                false,
                            ),
                            (21..26).into(),
                        ),
                        Node::new("Y", (27..28).into()),
                        None,
                        None,
                    ),
                    (21..28).into(),
                ),
            ],
        );

        run_test(src, expected, struct_parser);
    }
}
