use crate::ast::event::{CustomEvent, Event};
use crate::ast::flags::ScriptFlag;
use crate::ast::function::Function;
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::property::{Property, PropertyGroup};
use crate::ast::state::State;
use crate::ast::structure::Structure;
use crate::ast::variable::ScriptVariable;
use crate::choose_result;
use crate::parser::{Parse, Parser, ParserResult};
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;

#[derive(Debug, PartialEq, Clone)]
pub enum ScriptContent<'source> {
    Import(Identifier<'source>),
    Variable(ScriptVariable<'source>),
    Structure(Structure<'source>),
    CustomEvent(CustomEvent<'source>),
    Property(Property<'source>),
    PropertyGroup(PropertyGroup<'source>),
    State(State<'source>),
    Function(Function<'source>),
    Event(Event<'source>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Script<'source> {
    pub name: Node<Identifier<'source>>,
    pub extends: Option<Node<Identifier<'source>>>,
    pub flags: Option<Vec<Node<ScriptFlag>>>,
    pub contents: Option<Vec<Node<ScriptContent<'source>>>>,
}

impl<'source> Script<'source> {
    pub fn new(
        name: Node<Identifier<'source>>,
        extends: Option<Node<Identifier<'source>>>,
        flags: Option<Vec<Node<ScriptFlag>>>,
        contents: Option<Vec<Node<ScriptContent<'source>>>>,
    ) -> Self {
        Self {
            name,
            extends,
            flags,
            contents,
        }
    }
}

fn import_parser<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, Identifier<'source>> {
    parser.expect_keyword(KeywordKind::Import)?;
    Identifier::parse(parser)
}

impl<'source> Parse<'source> for ScriptContent<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        choose_result!(
            parser.optional_result(|parser| import_parser(parser).map(ScriptContent::Import)),
            parser.optional_result(
                |parser| ScriptVariable::parse(parser).map(ScriptContent::Variable)
            ),
            parser.optional_result(|parser| Structure::parse(parser).map(ScriptContent::Structure)),
            parser.optional_result(
                |parser| CustomEvent::parse(parser).map(ScriptContent::CustomEvent)
            ),
            parser.optional_result(|parser| Property::parse(parser).map(ScriptContent::Property)),
            parser.optional_result(
                |parser| PropertyGroup::parse(parser).map(ScriptContent::PropertyGroup)
            ),
            parser.optional_result(|parser| State::parse(parser).map(ScriptContent::State)),
            parser.optional_result(|parser| Function::parse(parser).map(ScriptContent::Function)),
            parser.optional_result(|parser| Event::parse(parser).map(ScriptContent::Event))
        )
    }
}

impl<'source> Parse<'source> for Script<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        parser.expect_keyword(KeywordKind::ScriptName)?;
        let name = parser.parse_node::<Identifier>()?;

        let extends = parser.optional(|parser| {
            parser.expect_keyword(KeywordKind::Extends)?;
            parser.parse_node::<Identifier>()
        });

        let flags = parser.parse_node_optional_repeated::<ScriptFlag>();

        let contents = parser.parse_node_optional_repeated::<ScriptContent>();

        parser.expect_eoi()?;

        Ok(Script::new(name, extends, flags, contents))
    }
}

#[cfg(test)]
mod test {
    use crate::ast::flags::ScriptFlag;
    use crate::ast::node::Node;
    use crate::ast::script::Script;
    use crate::parser::test_utils::run_test;

    #[test]
    fn test_script_parser() {
        let src = "ScriptName MyScript extends OtherScript Native";
        let expected = Script::new(
            Node::new("MyScript", 11..19),
            Some(Node::new("OtherScript", 28..39)),
            Some(vec![Node::new(ScriptFlag::Native, 40..46)]),
            None,
        );

        run_test(src, expected);
    }
}
