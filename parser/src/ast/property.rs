use crate::ast::flags::{display_flags, GroupFlag, PropertyFlag};
use crate::ast::function::Function;
use crate::ast::identifier::Identifier;
use crate::ast::literal::Literal;
use crate::ast::node::{display_nodes, Node};
use crate::ast::types::Type;
use crate::choose_optional;
use crate::parser::{Parse, Parser, ParserError, ParserResult};
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::fmt::{Display, Formatter};

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

impl<'source> Display for PropertyGroup<'source> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Group {}", self.name)?;

        display_flags(&self.flags, f)?;
        display_nodes(&self.properties, "\n", f)?;

        write!(f, "\nEndGroup")?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Property<'source> {
    AutoProperty(AutoProperty<'source>),
    FullProperty(FullProperty<'source>),
}

impl<'source> Display for Property<'source> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Property::AutoProperty(property) => write!(f, "{}", property),
            Property::FullProperty(property) => write!(f, "{}", property),
        }
    }
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

impl<'source> Display for AutoProperty<'source> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} Property {}", self.type_node, self.name)?;
        match self.initial_value.as_ref() {
            Some(value) => write!(f, " = {}", value)?,
            None => {}
        }

        if self.is_read_only {
            write!(f, " AutoReadOnly")?;
        } else {
            write!(f, " Auto")?;
        }

        display_flags(&self.flags, f)?;

        Ok(())
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

impl<'source> Display for FullProperty<'source> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} Property {}", self.type_node, self.name)?;

        display_flags(&self.flags, f)?;
        display_nodes(&self.functions, "\n", f)?;

        Ok(())
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
                let token = parser.consume()?;
                match token {
                    Token::Keyword(KeywordKind::Auto) => Ok(false),
                    Token::Keyword(KeywordKind::AutoReadOnly) => Ok(true),
                    _ => Err(ParserError::ExpectedOneOf {
                        found: *token,
                        expected: vec![
                            Token::Keyword(KeywordKind::Auto),
                            Token::Keyword(KeywordKind::AutoReadOnly),
                        ],
                    }),
                }?
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

        // a property must have at least one function: a 'Get' or 'Set' function
        let functions = parser.parse_node_repeated::<Function>()?;
        if functions.len() > 2 {
            return Err(ParserError::ExpectedAmount {
                name: "Function(s)",
                expected_amount: 2,
                actual_amount: functions.len(),
            });
        }

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
        choose_optional!(
            parser,
            "Property",
            parser
                .parse_optional::<AutoProperty>()
                .map(Property::AutoProperty),
            parser
                .parse_optional::<FullProperty>()
                .map(Property::FullProperty)
        )
    }
}

impl<'source> Parse<'source> for PropertyGroup<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        parser.expect_keyword(KeywordKind::Group)?;

        let group_name = parser.parse_node::<Identifier>()?;

        let flags = parser.parse_node_optional_repeated::<GroupFlag>();

        let properties = parser.parse_node_repeated::<Property>()?;

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
            Function Set(int newValue)
            int Function Get()
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
                    48..74,
                ),
                Node::new(
                    Function::new(
                        Some(Node::new(
                            Type::new(Node::new(TypeName::BaseType(BaseType::Int), 87..90), false),
                            87..90,
                        )),
                        Node::new("Get", 100..103),
                        None,
                        None,
                        None,
                    ),
                    87..105,
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
                float Function Get()
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
                            167..187,
                        )],
                    )),
                    121..211,
                ),
            ],
        );

        run_test(src, expected);
    }
}
