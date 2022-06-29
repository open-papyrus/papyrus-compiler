use crate::ast::flags::{GroupFlag, PropertyFlag};
use crate::ast::function::Function;
use crate::ast::identifier::Identifier;
use crate::ast::literal::Literal;
use crate::ast::node::Node;
use crate::ast::types::Type;
use crate::choose_result;
use crate::parser::{Parse, Parser};
use crate::parser_error::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::collections::HashSet;

#[derive(Debug, PartialEq, Clone)]
pub struct PropertyGroup<'source> {
    pub name: Node<Identifier<'source>>,
    pub flags: Option<Vec<Node<GroupFlag>>>,
    pub properties: Vec<Node<Property<'source>>>,
}

impl<'source> PropertyGroup<'source> {
    pub fn new(
        name: Node<Identifier<'source>>,
        flags: Option<Vec<Node<GroupFlag>>>,
        properties: Vec<Node<Property<'source>>>,
    ) -> Self {
        Self {
            name,
            flags,
            properties,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Property<'source> {
    AutoProperty(AutoProperty<'source>),
    FullProperty(FullProperty<'source>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct AutoProperty<'source> {
    pub type_node: Node<Type<'source>>,
    pub name: Node<Identifier<'source>>,
    pub initial_value: Option<Node<Literal<'source>>>,
    pub is_read_only: bool,
    pub flags: Option<Vec<Node<PropertyFlag>>>,
}

impl<'source> AutoProperty<'source> {
    pub fn new(
        type_node: Node<Type<'source>>,
        name: Node<Identifier<'source>>,
        initial_value: Option<Node<Literal<'source>>>,
        is_read_only: bool,
        flags: Option<Vec<Node<PropertyFlag>>>,
    ) -> Self {
        Self {
            type_node,
            name,
            initial_value,
            is_read_only,
            flags,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FullProperty<'source> {
    pub type_node: Node<Type<'source>>,
    pub name: Node<Identifier<'source>>,
    pub flags: Option<Vec<Node<PropertyFlag>>>,
    pub functions: Vec<Node<Function<'source>>>,
}

impl<'source> FullProperty<'source> {
    pub fn new(
        type_node: Node<Type<'source>>,
        name: Node<Identifier<'source>>,
        flags: Option<Vec<Node<PropertyFlag>>>,
        functions: Vec<Node<Function<'source>>>,
    ) -> Self {
        Self {
            type_node,
            name,
            flags,
            functions,
        }
    }
}

/// ```ebnf
/// <auto property> ::= <type> 'Property' <identifier> ['=' <constant>] 'Auto' <flags>*
/// <auto read-only property> ::= <type> 'Property' <identifier> '=' <constant> 'AutoReadOnly' <flags>*
/// ```
impl<'source> Parse<'source> for AutoProperty<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        let type_node = parser.parse_node::<Type>()?;

        parser.expect_keyword(KeywordKind::Property)?;

        let property_name = parser.parse_node::<Identifier>()?;

        let initial_value = parser.optional(|parser| {
            parser.expect_operator(OperatorKind::Assignment)?;
            parser.parse_node::<Literal>()
        });

        // a read-only auto property must have an initial value
        // a normal auto property can have an initial value
        let is_read_only = match initial_value {
            None => {
                parser.expect_keyword(KeywordKind::Auto)?;
                false
            }
            Some(_) => {
                let (token, range) = parser.consume()?;
                match token {
                    Token::Keyword(KeywordKind::Auto) => false,
                    Token::Keyword(KeywordKind::AutoReadOnly) => true,
                    _ => {
                        return Err(ParserError::AggregatedErrors(HashSet::from([
                            ParserError::ExpectedToken {
                                expected: Token::Keyword(KeywordKind::Auto),
                                found: (*token, range.clone()),
                            },
                            ParserError::ExpectedToken {
                                expected: Token::Keyword(KeywordKind::AutoReadOnly),
                                found: (*token, range.clone()),
                            },
                        ])))
                    }
                }
            }
        };

        let flags = parser.parse_node_optional_repeated::<PropertyFlag>();

        Ok(AutoProperty::new(
            type_node,
            property_name,
            initial_value,
            is_read_only,
            flags,
        ))
    }
}

/// ```ebnf
/// <property> ::= <type> 'Property' <identifier> <flags>* <function> [<function>] 'EndProperty'
/// ```
impl<'source> Parse<'source> for FullProperty<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        let type_node = parser.parse_node::<Type>()?;

        parser.expect_keyword(KeywordKind::Property)?;

        let property_name = parser.parse_node::<Identifier>()?;

        let flags = parser.parse_node_optional_repeated::<PropertyFlag>();

        let functions = parser.parse_node_until_keyword(KeywordKind::EndProperty)?;

        parser.expect_keyword(KeywordKind::EndProperty)?;

        Ok(FullProperty::new(
            type_node,
            property_name,
            flags,
            functions,
        ))
    }
}

impl<'source> Parse<'source> for Property<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        choose_result!(
            parser
                .optional_result(|parser| AutoProperty::parse(parser).map(Property::AutoProperty)),
            parser
                .optional_result(|parser| FullProperty::parse(parser).map(Property::FullProperty))
        )
    }
}

impl<'source> Parse<'source> for PropertyGroup<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        parser.expect_keyword(KeywordKind::Group)?;

        let group_name = parser.parse_node::<Identifier>()?;

        let flags = parser.parse_node_optional_repeated::<GroupFlag>();

        let properties = parser.parse_node_until_keyword(KeywordKind::EndGroup)?;

        parser.expect_keyword(KeywordKind::EndGroup)?;

        Ok(PropertyGroup::new(group_name, flags, properties))
    }
}

#[cfg(test)]
mod test {
    use crate::ast::flags::{GroupFlag, PropertyFlag};
    use crate::ast::function::{Function, FunctionParameter};
    use crate::ast::literal::Literal;
    use crate::ast::node::Node;
    use crate::ast::property::{AutoProperty, FullProperty, Property, PropertyGroup};
    use crate::ast::types::{BaseType, Type, TypeName};
    use crate::parser::test_utils::{run_test, run_tests};

    #[test]
    fn test_auto_property_parser() {
        let data = vec![
            (
                "float Property MyProperty = 1.0 Auto Conditional Const Hidden Mandatory",
                AutoProperty::new(
                    Node::new(
                        Type::new(Node::new(TypeName::BaseType(BaseType::Float), 0..5), false),
                        0..5,
                    ),
                    Node::new("MyProperty", 15..25),
                    Some(Node::new(Literal::Float(1.0), 28..31)),
                    false,
                    Some(vec![
                        Node::new(PropertyFlag::Conditional, 37..48),
                        Node::new(PropertyFlag::Const, 49..54),
                        Node::new(PropertyFlag::Hidden, 55..61),
                        Node::new(PropertyFlag::Mandatory, 62..71),
                    ]),
                ),
            ),
            (
                r#"String Property MyProperty = "Hello World!" AutoReadOnly"#,
                AutoProperty::new(
                    Node::new(
                        Type::new(Node::new(TypeName::BaseType(BaseType::String), 0..6), false),
                        0..6,
                    ),
                    Node::new("MyProperty", 16..26),
                    Some(Node::new(Literal::String("Hello World!"), 29..43)),
                    true,
                    None,
                ),
            ),
        ];

        run_tests(data);
    }

    #[test]
    fn test_full_property_parser() {
        let src = r#"
        int Property ValueProperty
            Function Set(int newValue) EndFunction
            int Function Get() EndFunction
        EndProperty
        "#;

        let expected = FullProperty::new(
            Node::new(
                Type::new(Node::new(TypeName::BaseType(BaseType::Int), 9..12), false),
                9..12,
            ),
            Node::new("ValueProperty", 22..35),
            None,
            vec![
                Node::new(
                    Function::new(
                        None,
                        Node::new("Set", 57..60),
                        Some(vec![Node::new(
                            FunctionParameter::new(
                                Node::new(
                                    Type::new(
                                        Node::new(TypeName::BaseType(BaseType::Int), 61..64),
                                        false,
                                    ),
                                    61..64,
                                ),
                                Node::new("newValue", 65..73),
                                None,
                            ),
                            61..73,
                        )]),
                        None,
                        None,
                    ),
                    48..86,
                ),
                Node::new(
                    Function::new(
                        Some(Node::new(
                            Type::new(Node::new(TypeName::BaseType(BaseType::Int), 99..102), false),
                            99..102,
                        )),
                        Node::new("Get", 112..115),
                        None,
                        None,
                        None,
                    ),
                    99..129,
                ),
            ],
        );

        run_test(src, expected);
    }

    #[test]
    fn test_property_group_parser() {
        let src = r#"
        Group MyGroup CollapsedOnRef CollapsedOnBase Collapsed
            int Property FirstProperty auto

            float Property SecondProperty
                float Function Get() EndFunction
            EndProperty
        EndGroup
        "#;

        let expected = PropertyGroup::new(
            Node::new("MyGroup", 15..22),
            Some(vec![
                Node::new(GroupFlag::CollapsedOnRef, 23..37),
                Node::new(GroupFlag::CollapsedOnBase, 38..53),
                Node::new(GroupFlag::Collapsed, 54..63),
            ]),
            vec![
                Node::new(
                    Property::AutoProperty(AutoProperty::new(
                        Node::new(
                            Type::new(Node::new(TypeName::BaseType(BaseType::Int), 76..79), false),
                            76..79,
                        ),
                        Node::new("FirstProperty", 89..102),
                        None,
                        false,
                        None,
                    )),
                    76..107,
                ),
                Node::new(
                    Property::FullProperty(FullProperty::new(
                        Node::new(
                            Type::new(
                                Node::new(TypeName::BaseType(BaseType::Float), 121..126),
                                false,
                            ),
                            121..126,
                        ),
                        Node::new("SecondProperty", 136..150),
                        None,
                        vec![Node::new(
                            Function::new(
                                Some(Node::new(
                                    Type::new(
                                        Node::new(TypeName::BaseType(BaseType::Float), 167..172),
                                        false,
                                    ),
                                    167..172,
                                )),
                                Node::new("Get", 182..185),
                                None,
                                None,
                                None,
                            ),
                            167..199,
                        )],
                    )),
                    121..223,
                ),
            ],
        );

        run_test(src, expected);
    }
}
