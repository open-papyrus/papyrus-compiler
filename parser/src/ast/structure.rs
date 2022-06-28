use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::variable::ScriptVariable;
use crate::parser::{Parse, Parser, ParserResult};
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;

pub type StructureField<'source> = ScriptVariable<'source>;

#[derive(Debug, PartialEq, Clone)]
pub struct Structure<'source> {
    pub name: Node<Identifier<'source>>,
    pub fields: Vec<Node<StructureField<'source>>>,
}

impl<'source> Structure<'source> {
    pub fn new(
        name: Node<Identifier<'source>>,
        fields: Vec<Node<StructureField<'source>>>,
    ) -> Self {
        Self { name, fields }
    }
}

/// ```ebnf
/// <Struct> ::= 'Struct' <identifier> <variable definition>+ 'EndStruct'
/// ```
impl<'source> Parse<'source> for Structure<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        parser.expect_keyword(KeywordKind::Struct)?;
        let struct_name = parser.parse_node::<Identifier>()?;

        let fields = parser.parse_node_until_keyword::<StructureField>(KeywordKind::EndStruct)?;
        parser.expect_keyword(KeywordKind::EndStruct)?;

        Ok(Structure::new(struct_name, fields))
    }
}

#[cfg(test)]
mod test {
    use crate::ast::node::Node;
    use crate::ast::structure::{Structure, StructureField};
    use crate::ast::types::{BaseType, Type, TypeName};
    use crate::parser::test_utils::run_test;

    #[test]
    fn test_struct_parser() {
        let src = "Struct Point\nfloat X\nfloat Y\nEndStruct";
        let expected = Structure::new(
            Node::new("Point", 7..12),
            vec![
                Node::new(
                    StructureField::new(
                        Node::new(
                            Type::new(
                                Node::new(TypeName::BaseType(BaseType::Float), 13..18),
                                false,
                            ),
                            13..18,
                        ),
                        Node::new("X", 19..20),
                        None,
                        None,
                    ),
                    13..20,
                ),
                Node::new(
                    StructureField::new(
                        Node::new(
                            Type::new(
                                Node::new(TypeName::BaseType(BaseType::Float), 21..26),
                                false,
                            ),
                            21..26,
                        ),
                        Node::new("Y", 27..28),
                        None,
                        None,
                    ),
                    21..28,
                ),
            ],
        );

        run_test(src, expected);
    }
}
