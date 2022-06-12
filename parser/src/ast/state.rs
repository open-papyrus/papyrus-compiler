use crate::ast::event::{event_parser, Event};
use crate::ast::function::{function_parser, Function};
use crate::ast::identifier::{identifier_parser, Identifier};
use crate::ast::node::{display_optional_nodes, Node};
use crate::parse::TokenParser;
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub struct State<'a> {
    pub is_auto: bool,
    pub name: Node<Identifier<'a>>,
    pub contents: Option<Vec<Node<StateContent<'a>>>>,
}

impl<'a> State<'a> {
    pub fn new(
        is_auto: bool,
        name: Node<Identifier<'a>>,
        contents: Option<Vec<Node<StateContent<'a>>>>,
    ) -> Self {
        Self {
            is_auto,
            name,
            contents,
        }
    }
}

impl<'a> Display for State<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_auto {
            write!(f, "Auto ")?;
        }

        write!(f, "State {}", self.name)?;

        display_optional_nodes(&self.contents, "\n", f)?;

        write!(f, "\nEndState")?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StateContent<'a> {
    Function(Function<'a>),
    Event(Event<'a>),
}

impl<'a> Display for StateContent<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StateContent::Function(function) => write!(f, "{}", function),
            StateContent::Event(event) => write!(f, "{}", event),
        }
    }
}

/// ```ebnf
/// <state content> = (<function> | <event>)
/// ```
pub fn state_content_parser<'a>() -> impl TokenParser<'a, StateContent<'a>> {
    event_parser()
        .map(StateContent::Event)
        .or(function_parser().map(StateContent::Function))
}

/// ```ebnf
/// <state> ::= ['Auto'] 'State' <identifier> <state content>* 'EndState'
/// ```
pub fn state_parser<'a>() -> impl TokenParser<'a, State<'a>> {
    just(Token::Keyword(KeywordKind::Auto))
        .or_not()
        .map(|x| x.is_some())
        .then_ignore(just(Token::Keyword(KeywordKind::State)))
        .then(identifier_parser().map_with_span(Node::new))
        .then(
            state_content_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1)
                .or_not(),
        )
        .then_ignore(just(Token::Keyword(KeywordKind::EndState)))
        .map(|output| {
            let ((is_auto, identifier), contents) = output;
            State::new(is_auto, identifier, contents)
        })
}

#[cfg(test)]
mod test {
    use crate::ast::node::Node;
    use crate::ast::state::{state_parser, State};
    use crate::parse::test_utils::run_test;

    #[test]
    fn test_state_parser() {
        let src = "Auto State MyState EndState";
        let expected = State::new(true, Node::new("MyState", (11..18).into()), None);

        run_test(src, expected, state_parser);
    }
}
