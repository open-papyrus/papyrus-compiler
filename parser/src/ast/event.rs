use crate::ast::flags::{display_flags, function_flag_parser, FunctionFlag};
use crate::ast::identifier::{identifier_parser, Identifier};
use crate::ast::node::{display_optional_nodes, Node};
use crate::ast::statement::{display_statements, statement_parser, Statement};
use crate::ast::types::{type_name_parser, type_with_identifier_parser, Type};
use crate::parse::TokenParser;
use chumsky::prelude::*;
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
pub fn custom_event_parser<'a>() -> impl TokenParser<'a, CustomEvent<'a>> {
    just(Token::Keyword(KeywordKind::CustomEvent))
        .ignore_then(identifier_parser().map_with_span(Node::new))
        .map(CustomEvent::new)
}

/// ```ebnf
/// <parameter>  ::= <type> <identifier>
/// ```
pub fn event_parameter_parser<'a>() -> impl TokenParser<'a, EventParameter<'a>> {
    type_with_identifier_parser()
        .map(|(type_node, identifier)| EventParameter::new(type_node, identifier))
}

/// ```ebnf
/// <event header> ::= 'Event' <identifier> '(' [<parameters>] ')' ['Native'] <flags>*
/// ```
pub fn event_header_parser<'a>() -> impl TokenParser<'a, EventHeader<'a>> {
    just(Token::Keyword(KeywordKind::Event))
        .ignore_then(identifier_parser().map_with_span(Node::new))
        .then_ignore(just(Token::Operator(OperatorKind::ParenthesisOpen)))
        .then(
            event_parameter_parser()
                .map_with_span(Node::new)
                .separated_by(just(Token::Operator(OperatorKind::Comma)))
                .map(|parameters| {
                    if parameters.is_empty() {
                        None
                    } else {
                        Some(parameters)
                    }
                }),
        )
        .then_ignore(just(Token::Operator(OperatorKind::ParenthesisClose)))
        .then(
            function_flag_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1)
                .or_not(),
        )
        .map(|output| {
            let ((identifier, parameters), flags) = output;
            EventHeader::new(identifier, parameters, flags)
        })
}

/// ```ebnf
/// <remote event header> ::= 'Event' <object type> '.' <identifier> '(' <sender parameter> [',' <parameters>] ')' <flags>*
/// ```
pub fn remote_event_header_parser<'a>() -> impl TokenParser<'a, RemoteEventHeader<'a>> {
    just(Token::Keyword(KeywordKind::Event))
        .ignore_then(identifier_parser().map_with_span(Node::new))
        .then_ignore(just(Token::Operator(OperatorKind::Access)))
        .then(identifier_parser().map_with_span(Node::new))
        .then_ignore(just(Token::Operator(OperatorKind::ParenthesisOpen)))
        .then(event_parameter_parser().map_with_span(Node::new))
        .then(
            event_parameter_parser()
                .map_with_span(Node::new)
                .separated_by(just(Token::Operator(OperatorKind::Comma)))
                .allow_leading()
                .map(|parameters| {
                    if parameters.is_empty() {
                        None
                    } else {
                        Some(parameters)
                    }
                }),
        )
        .then_ignore(just(Token::Operator(OperatorKind::ParenthesisClose)))
        .then(
            function_flag_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1)
                .or_not(),
        )
        .map(|output| {
            let ((((object_type, name), sender_parameter), parameters), flags) = output;
            RemoteEventHeader::new(object_type, name, sender_parameter, parameters, flags)
        })
}

/// ```ebnf
/// <custom event header> ::= 'Event' <object type> '.' <identifier> '(' <sender parameter> ',' <args parameter> ')' ['Native'] <flags>*
/// ```
pub fn custom_event_header_parser<'a>() -> impl TokenParser<'a, CustomEventHeader<'a>> {
    just(Token::Keyword(KeywordKind::Event))
        .ignore_then(identifier_parser().map_with_span(Node::new))
        .then_ignore(just(Token::Operator(OperatorKind::Access)))
        .then(identifier_parser().map_with_span(Node::new))
        .then_ignore(just(Token::Operator(OperatorKind::ParenthesisOpen)))
        .then(event_parameter_parser().map_with_span(Node::new))
        .then_ignore(just(Token::Operator(OperatorKind::Comma)))
        .then(
            // the args parameter must of `var[] <identifier>`
            type_name_parser()
                .map_with_span(Node::new)
                .then_ignore(just(Token::Operator(OperatorKind::SquareBracketsOpen)))
                .then(
                    just(Token::Operator(OperatorKind::SquareBracketsClose))
                        .map_with_span(Node::new),
                )
                .then(identifier_parser().map_with_span(Node::new))
                .map(|output| {
                    let ((type_name, token), identifier) = output;
                    let span = type_name.span_union(&token);
                    let type_node = Type::new(type_name, true);

                    EventParameter::new(Node::new(type_node, span), identifier)
                })
                .map_with_span(Node::new),
        )
        .then_ignore(just(Token::Operator(OperatorKind::ParenthesisClose)))
        .then(
            function_flag_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1)
                .or_not(),
        )
        .map(|output| {
            let ((((object_type, name), sender_parameter), args_parameter), flags) = output;
            CustomEventHeader::new(object_type, name, sender_parameter, args_parameter, flags)
        })
}

/// ```ebnf
/// <event header king> = (<event header> | <custom event header> | <remote event header> )
/// ```
pub fn event_header_kind_parser<'a>() -> impl TokenParser<'a, EventHeaderKind<'a>> {
    event_header_parser()
        .map(EventHeaderKind::EventHeader)
        .or(custom_event_header_parser().map(EventHeaderKind::CustomEvent))
        .or(remote_event_header_parser().map(EventHeaderKind::RemoteEvent))
}

/// ```ebnf
/// <event> ::= <event header kind> [<function block> 'EndEvent']
/// ```
pub fn event_parser<'a>() -> impl TokenParser<'a, Event<'a>> {
    event_header_kind_parser()
        .map_with_span(Node::new)
        .then(
            statement_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1)
                .then_ignore(just(Token::Keyword(KeywordKind::EndEvent)))
                .or_not(),
        )
        .map(|output| {
            let (header, statements) = output;
            Event::new(header, statements)
        })
}

#[cfg(test)]
mod test {
    use crate::ast::event::{
        custom_event_header_parser, custom_event_parser, event_header_parser,
        remote_event_header_parser, CustomEvent, CustomEventHeader, EventHeader, EventParameter,
        RemoteEventHeader,
    };
    use crate::ast::node::Node;
    use crate::ast::types::{Type, TypeName};
    use crate::parse::test_utils::run_test;

    #[test]
    fn test_custom_event_parser() {
        let src = "CustomEvent MyEvent";
        let expected = CustomEvent::new(Node::new("MyEvent", (12..19).into()));
        run_test(src, expected, custom_event_parser);
    }

    #[test]
    fn test_event_header_parser() {
        let src = "Event OnActivate(ObjectReference akActivator)";
        let expected = EventHeader::new(
            Node::new("OnActivate", (6..16).into()),
            Some(vec![Node::new(
                EventParameter::new(
                    Node::new(
                        Type::new(
                            Node::new(TypeName::Identifier("ObjectReference"), (17..32).into()),
                            false,
                        ),
                        (17..32).into(),
                    ),
                    Node::new("akActivator", (33..44).into()),
                ),
                (17..44).into(),
            )]),
            None,
        );
        run_test(src, expected, event_header_parser);
    }

    #[test]
    fn test_remote_event_header_parser() {
        let src = "Event ObjectReference.OnActivate(ObjectReference akSender, ObjectReference akActivator)";
        let expected = RemoteEventHeader::new(
            Node::new("ObjectReference", (6..21).into()),
            Node::new("OnActivate", (22..32).into()),
            Node::new(
                EventParameter::new(
                    Node::new(
                        Type::new(
                            Node::new(TypeName::Identifier("ObjectReference"), (33..48).into()),
                            false,
                        ),
                        (33..48).into(),
                    ),
                    Node::new("akSender", (49..57).into()),
                ),
                (33..57).into(),
            ),
            Some(vec![Node::new(
                EventParameter::new(
                    Node::new(
                        Type::new(
                            Node::new(TypeName::Identifier("ObjectReference"), (59..74).into()),
                            false,
                        ),
                        (59..74).into(),
                    ),
                    Node::new("akActivator", (75..86).into()),
                ),
                (59..86).into(),
            )]),
            None,
        );

        run_test(src, expected, remote_event_header_parser);
    }

    #[test]
    fn test_custom_event_header_parser() {
        let src = "Event MyQuestScript.MyCustomEvent(MyQuestScript akSender, Var[] akArgs)";
        let expected = CustomEventHeader::new(
            Node::new("MyQuestScript", (6..19).into()),
            Node::new("MyCustomEvent", (20..33).into()),
            Node::new(
                EventParameter::new(
                    Node::new(
                        Type::new(
                            Node::new(TypeName::Identifier("MyQuestScript"), (34..47).into()),
                            false,
                        ),
                        (34..47).into(),
                    ),
                    Node::new("akSender", (48..56).into()),
                ),
                (34..56).into(),
            ),
            Node::new(
                EventParameter::new(
                    Node::new(
                        Type::new(Node::new(TypeName::Var, (58..61).into()), true),
                        (58..63).into(),
                    ),
                    Node::new("akArgs", (64..70).into()),
                ),
                (58..70).into(),
            ),
            None,
        );

        run_test(src, expected, custom_event_header_parser);
    }
}
