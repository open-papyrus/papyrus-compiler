use crate::ast::event::Event;
use crate::ast::function::Function;
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::choose_result;
use crate::parser::{Parse, Parser, ParserResult};
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;

#[derive(Debug, PartialEq, Clone)]
pub struct State<'source> {
    pub is_auto: bool,
    pub name: Node<Identifier<'source>>,
    pub contents: Option<Vec<Node<StateContent<'source>>>>,
}

impl<'source> State<'source> {
    pub fn new(
        is_auto: bool,
        name: Node<Identifier<'source>>,
        contents: Option<Vec<Node<StateContent<'source>>>>,
    ) -> Self {
        Self {
            is_auto,
            name,
            contents,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StateContent<'source> {
    Function(Function<'source>),
    Event(Event<'source>),
}

/// ```ebnf
/// <state content> = (<function> | <event>)
/// ```
impl<'source> Parse<'source> for StateContent<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        choose_result!(
            parser.optional_result(|parser| Event::parse(parser).map(StateContent::Event)),
            parser.optional_result(|parser| Function::parse(parser).map(StateContent::Function))
        )
    }
}

/// ```ebnf
/// <state> ::= ['Auto'] 'State' <identifier> <state content>* 'EndState'
/// ```
impl<'source> Parse<'source> for State<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        let is_auto = parser
            .optional(|parser| parser.expect_keyword(KeywordKind::Auto))
            .is_some();

        parser.expect_keyword(KeywordKind::State)?;

        let state_name = parser.parse_node::<Identifier>()?;

        let state_content = parser.parse_node_optional_repeated::<StateContent>();

        parser.expect_keyword(KeywordKind::EndState)?;

        Ok(State::new(is_auto, state_name, state_content))
    }
}

#[cfg(test)]
mod test {
    use crate::ast::node::Node;
    use crate::ast::state::State;
    use crate::parser::test_utils::run_test;

    #[test]
    fn test_state_parser() {
        let src = "Auto State MyState EndState";
        let expected = State::new(true, Node::new("MyState", 11..18), None);

        run_test(src, expected);
    }
}
