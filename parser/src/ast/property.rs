use crate::ast::flags::{
    display_flags, group_flag_parser, property_flag_parser, GroupFlag, PropertyFlag,
};
use crate::ast::function::{function_parser, Function};
use crate::ast::identifier::{identifier_parser, Identifier};
use crate::ast::literal::{literal_parser, Literal};
use crate::ast::node::{display_nodes, Node};
use crate::ast::types::{type_parser, Type};
use crate::parse::TokenParser;
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub struct PropertyGroup<'a> {
    pub name: Node<Identifier<'a>>,
    pub flags: Option<Vec<Node<GroupFlag>>>,
    pub properties: Vec<Node<Property<'a>>>,
}

impl<'a> PropertyGroup<'a> {
    pub fn new(
        name: Node<Identifier<'a>>,
        flags: Option<Vec<Node<GroupFlag>>>,
        properties: Vec<Node<Property<'a>>>,
    ) -> Self {
        Self {
            name,
            flags,
            properties,
        }
    }
}

impl<'a> Display for PropertyGroup<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Group {}", self.name)?;

        display_flags(&self.flags, f)?;
        display_nodes(&self.properties, "\n", f)?;

        write!(f, "\nEndGroup")?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Property<'a> {
    AutoProperty(AutoProperty<'a>),
    FullProperty(FullProperty<'a>),
}

impl<'a> Display for Property<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Property::AutoProperty(property) => write!(f, "{}", property),
            Property::FullProperty(property) => write!(f, "{}", property),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AutoProperty<'a> {
    pub type_node: Node<Type<'a>>,
    pub name: Node<Identifier<'a>>,
    pub initial_value: Option<Node<Literal<'a>>>,
    pub is_read_only: bool,
    pub flags: Option<Vec<Node<PropertyFlag>>>,
}

impl<'a> AutoProperty<'a> {
    pub fn new(
        type_node: Node<Type<'a>>,
        name: Node<Identifier<'a>>,
        initial_value: Option<Node<Literal<'a>>>,
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

impl<'a> Display for AutoProperty<'a> {
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
pub struct FullProperty<'a> {
    pub type_node: Node<Type<'a>>,
    pub name: Node<Identifier<'a>>,
    pub flags: Option<Vec<Node<PropertyFlag>>>,
    pub functions: Vec<Node<Function<'a>>>,
}

impl<'a> FullProperty<'a> {
    pub fn new(
        type_node: Node<Type<'a>>,
        name: Node<Identifier<'a>>,
        flags: Option<Vec<Node<PropertyFlag>>>,
        functions: Vec<Node<Function<'a>>>,
    ) -> Self {
        Self {
            type_node,
            name,
            flags,
            functions,
        }
    }
}

impl<'a> Display for FullProperty<'a> {
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
pub fn auto_property_parser<'a>() -> impl TokenParser<'a, AutoProperty<'a>> {
    type_parser()
        .map_with_span(Node::new)
        .then_ignore(just(Token::Keyword(KeywordKind::Property)))
        .then(identifier_parser().map_with_span(Node::new))
        .then(
            just(Token::Operator(OperatorKind::Assignment))
                .ignore_then(literal_parser().map_with_span(Node::new))
                .or_not(),
        )
        .then(
            just(Token::Keyword(KeywordKind::Auto))
                .map(|_| false)
                .or(just(Token::Keyword(KeywordKind::AutoReadOnly)).map(|_| true)),
        )
        .then(
            property_flag_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1)
                .or_not(),
        )
        .map(|output| {
            let ((((type_node, identifier), default_value), is_read_only), flags) = output;
            AutoProperty::new(type_node, identifier, default_value, is_read_only, flags)
        })
}

/// ```ebnf
/// <property> ::= <type> 'Property' <identifier> <flags>* <function> [<function>] 'EndProperty'
/// ```
pub fn full_property_parser<'a>() -> impl TokenParser<'a, FullProperty<'a>> {
    type_parser()
        .map_with_span(Node::new)
        .then_ignore(just(Token::Keyword(KeywordKind::Property)))
        .then(identifier_parser().map_with_span(Node::new))
        .then(
            property_flag_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1)
                .or_not(),
        )
        .then(
            function_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1)
                .at_most(2),
        )
        .then_ignore(just(Token::Keyword(KeywordKind::EndProperty)))
        .map(|output| {
            let (((type_node, identifier), flags), functions) = output;
            FullProperty::new(type_node, identifier, flags, functions)
        })
}

pub fn property_parser<'a>() -> impl TokenParser<'a, Property<'a>> {
    auto_property_parser()
        .map(Property::AutoProperty)
        .or(full_property_parser().map(Property::FullProperty))
}

pub fn property_group_parser<'a>() -> impl TokenParser<'a, PropertyGroup<'a>> {
    just(Token::Keyword(KeywordKind::Group))
        .ignore_then(identifier_parser().map_with_span(Node::new))
        .then(
            group_flag_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1)
                .or_not(),
        )
        .then(
            property_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1),
        )
        .then_ignore(just(Token::Keyword(KeywordKind::EndGroup)))
        .map(|output| {
            let ((identifier, flags), properties) = output;
            PropertyGroup::new(identifier, flags, properties)
        })
}

#[cfg(test)]
mod test {
    use crate::ast::flags::{GroupFlag, PropertyFlag};
    use crate::ast::function::{Function, FunctionParameter};
    use crate::ast::literal::Literal;
    use crate::ast::node::Node;
    use crate::ast::property::{
        auto_property_parser, full_property_parser, property_group_parser, AutoProperty,
        FullProperty, Property, PropertyGroup,
    };
    use crate::ast::types::{BaseType, Type, TypeName};
    use crate::parse::test_utils::{run_test, run_tests};

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

        run_tests(data, auto_property_parser);
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

        run_test(src, expected, full_property_parser);
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

        run_test(src, expected, property_group_parser);
    }
}
