use crate::ast::event::{CustomEvent, Event};
use crate::ast::flags::{display_flags, ScriptFlag};
use crate::ast::function::Function;
use crate::ast::identifier::Identifier;
use crate::ast::node::{display_optional_nodes, Node};
use crate::ast::property::{Property, PropertyGroup};
use crate::ast::state::State;
use crate::ast::structure::Structure;
use crate::ast::variable::ScriptVariable;
use crate::choose_optional;
use crate::parser::{Parse, Parser, ParserResult};
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use std::fmt::{Display, Formatter};

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

impl<'source> Display for ScriptContent<'source> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ScriptContent::Import(content) => write!(f, "Import {}", content),
            ScriptContent::Variable(content) => write!(f, "{}", content),
            ScriptContent::Structure(content) => write!(f, "{}", content),
            ScriptContent::CustomEvent(content) => write!(f, "{}", content),
            ScriptContent::Property(content) => write!(f, "{}", content),
            ScriptContent::PropertyGroup(content) => write!(f, "{}", content),
            ScriptContent::State(state) => write!(f, "{}", state),
            ScriptContent::Function(content) => write!(f, "{}", content),
            ScriptContent::Event(content) => write!(f, "{}", content),
        }
    }
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

impl<'source> Display for Script<'source> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ScriptName {}", self.name)?;

        match self.extends.as_ref() {
            Some(extends) => write!(f, "Extends {}", extends)?,
            None => {}
        }

        display_flags(&self.flags, f)?;
        display_optional_nodes(&self.contents, "\n", f)?;

        Ok(())
    }
}

pub(crate) fn import_parser<'source>(
    parser: &mut Parser<'source>,
) -> ParserResult<'source, Identifier<'source>> {
    parser.expect_keyword(KeywordKind::Import)?;
    Identifier::parse(parser)
}

impl<'source> Parse<'source> for ScriptContent<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        choose_optional!(
            parser,
            "Script Content",
            parser.optional(import_parser).map(ScriptContent::Import),
            parser
                .parse_optional::<ScriptVariable>()
                .map(ScriptContent::Variable),
            parser
                .parse_optional::<Structure>()
                .map(ScriptContent::Structure),
            parser
                .parse_optional::<CustomEvent>()
                .map(ScriptContent::CustomEvent),
            parser
                .parse_optional::<Property>()
                .map(ScriptContent::Property),
            parser
                .parse_optional::<PropertyGroup>()
                .map(ScriptContent::PropertyGroup),
            parser.parse_optional::<State>().map(ScriptContent::State),
            parser
                .parse_optional::<Function>()
                .map(ScriptContent::Function),
            parser.parse_optional::<Event>().map(ScriptContent::Event)
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

        Ok(Script::new(name, extends, flags, contents))
    }
}

#[cfg(test)]
mod test {
    use crate::ast::flags::ScriptFlag;
    use crate::ast::node::Node;
    use crate::parser::test_utils::run_test;
    use crate::Script;

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
