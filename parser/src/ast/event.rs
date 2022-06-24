use crate::ast::flags::{display_flags, FunctionFlag};
use crate::ast::identifier::Identifier;
use crate::ast::node::{display_optional_nodes, range_union, Node};
use crate::ast::statement::{display_statements, Statement};
use crate::ast::types::{type_with_identifier_parser, Type, TypeName};
use crate::choose_optional;
use crate::parser::{Parse, Parser, ParserResult};
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::operator_kind::OperatorKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub struct CustomEvent<'a> {
    pub name: Node<Identifier<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Event<'a> {
    pub header: Node<EventHeaderKind<'a>>,
    pub statements: Option<Vec<Node<Statement<'a>>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum EventHeaderKind<'a> {
    EventHeader(EventHeader<'a>),
    RemoteEvent(RemoteEventHeader<'a>),
    CustomEvent(CustomEventHeader<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct EventParameter<'a> {
    pub type_node: Node<Type<'a>>,
    pub name: Node<Identifier<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EventHeader<'a> {
    pub name: Node<Identifier<'a>>,
    pub parameters: Option<Vec<Node<EventParameter<'a>>>>,
    pub flags: Option<Vec<Node<FunctionFlag>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RemoteEventHeader<'a> {
    pub object_type: Node<Identifier<'a>>,
    pub name: Node<Identifier<'a>>,
    pub sender_parameter: Node<EventParameter<'a>>,
    pub parameters: Option<Vec<Node<EventParameter<'a>>>>,
    pub flags: Option<Vec<Node<FunctionFlag>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CustomEventHeader<'a> {
    pub object_type: Node<Identifier<'a>>,
    pub name: Node<Identifier<'a>>,
    pub sender_parameter: Node<EventParameter<'a>>,
    /// this has to be `var[]`
    pub args_parameter: Node<EventParameter<'a>>,
    pub flags: Option<Vec<Node<FunctionFlag>>>,
}

impl<'a> CustomEvent<'a> {
    pub fn new(name: Node<Identifier<'a>>) -> Self {
        Self { name }
    }
}

impl<'a> Event<'a> {
    pub fn new(
        header: Node<EventHeaderKind<'a>>,
        statements: Option<Vec<Node<Statement<'a>>>>,
    ) -> Self {
        Self { header, statements }
    }
}

impl<'a> EventParameter<'a> {
    pub fn new(type_node: Node<Type<'a>>, name: Node<Identifier<'a>>) -> Self {
        Self { type_node, name }
    }
}

impl<'a> EventHeader<'a> {
    pub fn new(
        name: Node<Identifier<'a>>,
        parameters: Option<Vec<Node<EventParameter<'a>>>>,
        flags: Option<Vec<Node<FunctionFlag>>>,
    ) -> Self {
        Self {
            name,
            parameters,
            flags,
        }
    }
}

impl<'a> CustomEventHeader<'a> {
    pub fn new(
        object_type: Node<Identifier<'a>>,
        name: Node<Identifier<'a>>,
        sender_parameter: Node<EventParameter<'a>>,
        args_parameter: Node<EventParameter<'a>>,
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

impl<'a> RemoteEventHeader<'a> {
    pub fn new(
        object_type: Node<Identifier<'a>>,
        name: Node<Identifier<'a>>,
        sender_parameter: Node<EventParameter<'a>>,
        parameters: Option<Vec<Node<EventParameter<'a>>>>,
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

impl<'a> Display for CustomEvent<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "CustomEvent {}", self.name)
    }
}

impl<'a> Display for Event<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.header)?;

        display_statements(&self.statements, f)?;

        write!(f, "\nEndEvent")?;

        Ok(())
    }
}

impl<'a> Display for EventHeaderKind<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EventHeaderKind::EventHeader(header) => write!(f, "{}", header),
            EventHeaderKind::RemoteEvent(header) => write!(f, "{}", header),
            EventHeaderKind::CustomEvent(header) => write!(f, "{}", header),
        }
    }
}

impl<'a> Display for EventParameter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.type_node, self.name)
    }
}

impl<'a> Display for EventHeader<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Event {} (", self.name)?;

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

        if self.flags.is_some() {
            write!(f, " ")?;
            display_flags(&self.flags, f)?;
        }

        Ok(())
    }
}

impl<'a> Display for RemoteEventHeader<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Event {}.{} ({}",
            self.object_type, self.name, self.sender_parameter
        )?;

        display_optional_nodes(&self.parameters, ", ", f)?;

        write!(f, ")")?;

        if self.flags.is_some() {
            write!(f, " ")?;
            display_flags(&self.flags, f)?;
        }

        Ok(())
    }
}

impl<'a> Display for CustomEventHeader<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Event {}.{} ({}, {})",
            self.object_type, self.name, self.sender_parameter, self.args_parameter
        )?;

        if self.flags.is_some() {
            write!(f, " ")?;
            display_flags(&self.flags, f)?;
        }

        Ok(())
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

        let parameters = parser.optional(|parser| {
            parser.separated(
                |parser| parser.parse_node::<EventParameter>(),
                Some(OperatorKind::Comma),
            )
        });

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

        let parameters = parser.optional(|parser| {
            parser.separated(
                |parser| parser.parse_node::<EventParameter>(),
                Some(OperatorKind::Comma),
            )
        });

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
        choose_optional!(
            parser,
            "Event Header",
            parser
                .parse_optional::<EventHeader>()
                .map(EventHeaderKind::EventHeader),
            parser
                .parse_optional::<CustomEventHeader>()
                .map(EventHeaderKind::CustomEvent),
            parser
                .parse_optional::<RemoteEventHeader>()
                .map(EventHeaderKind::RemoteEvent),
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

        let statements = parser.parse_node_optional_repeated::<Statement>();
        if statements.is_some() {
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
