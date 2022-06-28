use crate::ast::flags::FunctionFlag;
use crate::ast::identifier::Identifier;
use crate::ast::node::{range_union, Node};
use crate::ast::statement::Statement;
use crate::ast::types::{type_with_identifier_parser, Type, TypeName};
use crate::choose_result;
use crate::parser::{Parse, Parser, ParserResult};
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct CustomEvent<'source> {
    pub name: Node<Identifier<'source>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Event<'source> {
    pub header: Node<EventHeaderKind<'source>>,
    pub statements: Option<Vec<Node<Statement<'source>>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum EventHeaderKind<'source> {
    EventHeader(EventHeader<'source>),
    RemoteEvent(RemoteEventHeader<'source>),
    CustomEvent(CustomEventHeader<'source>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct EventParameter<'source> {
    pub type_node: Node<Type<'source>>,
    pub name: Node<Identifier<'source>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EventHeader<'source> {
    pub name: Node<Identifier<'source>>,
    pub parameters: Option<Vec<Node<EventParameter<'source>>>>,
    pub flags: Option<Vec<Node<FunctionFlag>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RemoteEventHeader<'source> {
    pub object_type: Node<Identifier<'source>>,
    pub name: Node<Identifier<'source>>,
    pub sender_parameter: Node<EventParameter<'source>>,
    pub parameters: Option<Vec<Node<EventParameter<'source>>>>,
    pub flags: Option<Vec<Node<FunctionFlag>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CustomEventHeader<'source> {
    pub object_type: Node<Identifier<'source>>,
    pub name: Node<Identifier<'source>>,
    pub sender_parameter: Node<EventParameter<'source>>,
    /// this has to be `var[]`
    pub args_parameter: Node<EventParameter<'source>>,
    pub flags: Option<Vec<Node<FunctionFlag>>>,
}

impl<'source> CustomEvent<'source> {
    pub fn new(name: Node<Identifier<'source>>) -> Self {
        Self { name }
    }
}

impl<'source> Event<'source> {
    pub fn new(
        header: Node<EventHeaderKind<'source>>,
        statements: Option<Vec<Node<Statement<'source>>>>,
    ) -> Self {
        Self { header, statements }
    }
}

impl<'source> EventParameter<'source> {
    pub fn new(type_node: Node<Type<'source>>, name: Node<Identifier<'source>>) -> Self {
        Self { type_node, name }
    }
}

impl<'source> EventHeader<'source> {
    pub fn new(
        name: Node<Identifier<'source>>,
        parameters: Option<Vec<Node<EventParameter<'source>>>>,
        flags: Option<Vec<Node<FunctionFlag>>>,
    ) -> Self {
        Self {
            name,
            parameters,
            flags,
        }
    }
}

impl<'source> CustomEventHeader<'source> {
    pub fn new(
        object_type: Node<Identifier<'source>>,
        name: Node<Identifier<'source>>,
        sender_parameter: Node<EventParameter<'source>>,
        args_parameter: Node<EventParameter<'source>>,
        flags: Option<Vec<Node<FunctionFlag>>>,
    ) -> Self {
        Self {
            object_type,
            name,
            sender_parameter,
            args_parameter,
            flags,
        }
    }
}

impl<'source> RemoteEventHeader<'source> {
    pub fn new(
        object_type: Node<Identifier<'source>>,
        name: Node<Identifier<'source>>,
        sender_parameter: Node<EventParameter<'source>>,
        parameters: Option<Vec<Node<EventParameter<'source>>>>,
        flags: Option<Vec<Node<FunctionFlag>>>,
    ) -> Self {
        Self {
            object_type,
            name,
            sender_parameter,
            parameters,
            flags,
        }
    }
}

/// ```ebnf
/// 'CustomEvent' <identifier>
/// ```
impl<'source> Parse<'source> for CustomEvent<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        parser.expect_keyword(KeywordKind::CustomEvent)?;

        let event_name = parser.parse_node::<Identifier>()?;

        Ok(CustomEvent::new(event_name))
    }
}

/// ```ebnf
/// <parameter> ::= <type> <identifier>
/// ```
impl<'source> Parse<'source> for EventParameter<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        let (type_node, parameter_name) = type_with_identifier_parser(parser)?;
        Ok(EventParameter::new(type_node, parameter_name))
    }
}

/// ```ebnf
/// <event header> ::= 'Event' <identifier> '(' [<parameters>] ')' <flags>*
/// ```
impl<'source> Parse<'source> for EventHeader<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        parser.expect_keyword(KeywordKind::Event)?;

        let event_name = parser.parse_node::<Identifier>()?;

        parser.expect_operator(OperatorKind::ParenthesisOpen)?;

        let parameters = parser.optional_separated(
            |parser| parser.parse_node::<EventParameter>(),
            OperatorKind::Comma,
        );

        parser.expect_operator(OperatorKind::ParenthesisClose)?;

        let flags = parser.parse_node_optional_repeated::<FunctionFlag>();

        Ok(EventHeader::new(event_name, parameters, flags))
    }
}

/// ```ebnf
/// <remote event header> ::= 'Event' <object type> '.' <identifier> '(' <sender parameter> [',' <parameters>] ')' <flags>*
/// ```
impl<'source> Parse<'source> for RemoteEventHeader<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        parser.expect_keyword(KeywordKind::Event)?;

        let object_name = parser.parse_node::<Identifier>()?;
        parser.expect_operator(OperatorKind::Access)?;
        let identifier = parser.parse_node::<Identifier>()?;

        parser.expect_operator(OperatorKind::ParenthesisOpen)?;

        let sender_parameter = parser.parse_node::<EventParameter>()?;
        parser.optional(|parser| parser.expect_operator(OperatorKind::Comma));

        let parameters = parser.optional_separated(
            |parser| parser.parse_node::<EventParameter>(),
            OperatorKind::Comma,
        );

        parser.expect_operator(OperatorKind::ParenthesisClose)?;

        let flags = parser.parse_node_optional_repeated::<FunctionFlag>();

        Ok(RemoteEventHeader::new(
            object_name,
            identifier,
            sender_parameter,
            parameters,
            flags,
        ))
    }
}

/// ```ebnf
/// <custom event header> ::= 'Event' <object type> '.' <identifier> '(' <sender parameter> ',' <args parameter> ')' ['Native'] <flags>*
/// <args parameter> ::= 'var' '[' ']'
/// ```
impl<'source> Parse<'source> for CustomEventHeader<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        parser.expect_keyword(KeywordKind::Event)?;

        let object_name = parser.parse_node::<Identifier>()?;
        parser.expect_operator(OperatorKind::Access)?;
        let identifier = parser.parse_node::<Identifier>()?;

        parser.expect_operator(OperatorKind::ParenthesisOpen)?;

        let sender_parameter = parser.parse_node::<EventParameter>()?;
        parser.optional(|parser| parser.expect_operator(OperatorKind::Comma));

        // the args parameter must be of type 'var[]'
        let args_parameter = {
            let start_range = parser.save_range(parser.position())?;
            parser.expect_keyword(KeywordKind::Var)?;
            parser.expect_operator(OperatorKind::SquareBracketsOpen)?;
            parser.expect_operator(OperatorKind::SquareBracketsClose)?;
            let end_range = parser.save_range(parser.position() - 1)?;

            let parameter_name = parser.parse_node::<Identifier>()?;

            let total_range = range_union(start_range.clone(), parameter_name.range());

            Node::new(
                EventParameter::new(
                    Node::new(
                        Type::new(Node::new(TypeName::Var, start_range.clone()), true),
                        range_union(start_range, end_range),
                    ),
                    parameter_name,
                ),
                total_range,
            )
        };

        parser.expect_operator(OperatorKind::ParenthesisClose)?;

        let flags = parser.parse_node_optional_repeated::<FunctionFlag>();

        Ok(CustomEventHeader::new(
            object_name,
            identifier,
            sender_parameter,
            args_parameter,
            flags,
        ))
    }
}

/// ```ebnf
/// <event header king> = (<event header> | <custom event header> | <remote event header> )
/// ```
impl<'source> Parse<'source> for EventHeaderKind<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        choose_result!(
            parser.optional_result(
                |parser| EventHeader::parse(parser).map(EventHeaderKind::EventHeader)
            ),
            parser.optional_result(
                |parser| CustomEventHeader::parse(parser).map(EventHeaderKind::CustomEvent)
            ),
            parser.optional_result(
                |parser| RemoteEventHeader::parse(parser).map(EventHeaderKind::RemoteEvent)
            ),
        )
    }
}

/// ```ebnf
/// <event> ::= <event header kind> [<event body>]
/// <event body> ::= <function block> 'EndEvent'
/// <function block> ::= <statement>*
/// ```
impl<'source> Parse<'source> for Event<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        let event_header_kind = parser.parse_node::<EventHeaderKind>()?;

        let statements =
            parser.optional_parse_node_until_keyword::<Statement>(KeywordKind::EndEvent)?;

        if statements.is_some()
            || parser.peek_token() == Some(&Token::Keyword(KeywordKind::EndEvent))
        {
            parser.expect_keyword(KeywordKind::EndEvent)?;
        }

        Ok(Event::new(event_header_kind, statements))
    }
}

#[cfg(test)]
mod test {
    use crate::ast::event::{
        CustomEvent, CustomEventHeader, Event, EventHeader, EventHeaderKind, EventParameter,
        RemoteEventHeader,
    };
    use crate::ast::node::Node;
    use crate::ast::statement::Statement;
    use crate::ast::types::{Type, TypeName};
    use crate::parser::test_utils::run_test;

    #[test]
    fn test_custom_event_parser() {
        let src = "CustomEvent MyEvent";
        let expected = CustomEvent::new(Node::new("MyEvent", 12..19));
        run_test(src, expected);
    }

    #[test]
    fn test_event_header_parser() {
        let src = "Event OnActivate(ObjectReference akActivator)";
        let expected = EventHeader::new(
            Node::new("OnActivate", 6..16),
            Some(vec![Node::new(
                EventParameter::new(
                    Node::new(
                        Type::new(
                            Node::new(TypeName::Identifier("ObjectReference"), 17..32),
                            false,
                        ),
                        17..32,
                    ),
                    Node::new("akActivator", 33..44),
                ),
                17..44,
            )]),
            None,
        );
        run_test(src, expected);
    }

    #[test]
    fn test_remote_event_header_parser() {
        let src = "Event ObjectReference.OnActivate(ObjectReference akSender, ObjectReference akActivator)";
        let expected = RemoteEventHeader::new(
            Node::new("ObjectReference", 6..21),
            Node::new("OnActivate", 22..32),
            Node::new(
                EventParameter::new(
                    Node::new(
                        Type::new(
                            Node::new(TypeName::Identifier("ObjectReference"), 33..48),
                            false,
                        ),
                        33..48,
                    ),
                    Node::new("akSender", 49..57),
                ),
                33..57,
            ),
            Some(vec![Node::new(
                EventParameter::new(
                    Node::new(
                        Type::new(
                            Node::new(TypeName::Identifier("ObjectReference"), 59..74),
                            false,
                        ),
                        59..74,
                    ),
                    Node::new("akActivator", 75..86),
                ),
                59..86,
            )]),
            None,
        );

        run_test(src, expected);
    }

    #[test]
    fn test_custom_event_header_parser() {
        let src = "Event MyQuestScript.MyCustomEvent(MyQuestScript akSender, Var[] akArgs)";
        let expected = CustomEventHeader::new(
            Node::new("MyQuestScript", 6..19),
            Node::new("MyCustomEvent", 20..33),
            Node::new(
                EventParameter::new(
                    Node::new(
                        Type::new(
                            Node::new(TypeName::Identifier("MyQuestScript"), 34..47),
                            false,
                        ),
                        34..47,
                    ),
                    Node::new("akSender", 48..56),
                ),
                34..56,
            ),
            Node::new(
                EventParameter::new(
                    Node::new(Type::new(Node::new(TypeName::Var, 58..61), true), 58..63),
                    Node::new("akArgs", 64..70),
                ),
                58..70,
            ),
            None,
        );

        run_test(src, expected);
    }

    #[test]
    fn test_event_parser() {
        let src = "Event OnActivate(ObjectReference akActivator)\nReturn\nEndEvent";
        let expected = Event::new(
            Node::new(
                EventHeaderKind::EventHeader(EventHeader::new(
                    Node::new("OnActivate", 6..16),
                    Some(vec![Node::new(
                        EventParameter::new(
                            Node::new(
                                Type::new(
                                    Node::new(TypeName::Identifier("ObjectReference"), 17..32),
                                    false,
                                ),
                                17..32,
                            ),
                            Node::new("akActivator", 33..44),
                        ),
                        17..44,
                    )]),
                    None,
                )),
                0..45,
            ),
            Some(vec![Node::new(Statement::Return(None), 46..52)]),
        );

        run_test(src, expected);
    }
}
