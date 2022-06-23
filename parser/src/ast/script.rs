use crate::ast::event::{custom_event_parser, event_parser, CustomEvent, Event};
use crate::ast::flags::{custom_script_flag_parser, display_flags, script_flag_parser, ScriptFlag};
use crate::ast::function::{function_parser, Function};
use crate::ast::identifier::{custom_identifier_parser, identifier_parser, Identifier};
use crate::ast::node::{display_optional_nodes, range_union, Node};
use crate::ast::property::{property_group_parser, property_parser, Property, PropertyGroup};
use crate::ast::state::{state_parser, State};
use crate::ast::structure::{struct_parser, Structure};
use crate::ast::variable::{script_variable_parser, ScriptVariable};
use crate::parse::TokenParser;
use crate::parser::{CustomParser, ParserError, ParserResult};
use crate::select_tokens;
use chumsky::prelude::*;
use papyrus_compiler_diagnostics::SourceRange;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum ScriptContent<'a> {
    Import(Identifier<'a>),
    Variable(ScriptVariable<'a>),
    Structure(Structure<'a>),
    CustomEvent(CustomEvent<'a>),
    Property(Property<'a>),
    PropertyGroup(PropertyGroup<'a>),
    State(State<'a>),
    Function(Function<'a>),
    Event(Event<'a>),
}

impl<'a> Display for ScriptContent<'a> {
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
pub struct Script<'a> {
    pub name: Node<Identifier<'a>>,
    pub extends: Option<Node<Identifier<'a>>>,
    pub flags: Option<Vec<Node<ScriptFlag>>>,
    pub contents: Option<Vec<Node<ScriptContent<'a>>>>,
}

impl<'a> Script<'a> {
    pub fn new(
        name: Node<Identifier<'a>>,
        extends: Option<Node<Identifier<'a>>>,
        flags: Option<Vec<Node<ScriptFlag>>>,
        contents: Option<Vec<Node<ScriptContent<'a>>>>,
    ) -> Self {
        Self {
            name,
            extends,
            flags,
            contents,
        }
    }
}

impl<'a> Display for Script<'a> {
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

pub fn import_parser<'a>() -> impl TokenParser<'a, Identifier<'a>> {
    just(Token::Keyword(KeywordKind::Import)).ignore_then(identifier_parser())
}

pub fn script_content_parser<'a>() -> impl TokenParser<'a, ScriptContent<'a>> {
    let import = import_parser().map(ScriptContent::Import);
    let variable = script_variable_parser().map(ScriptContent::Variable);
    let structure = struct_parser().map(ScriptContent::Structure);
    let custom_event = custom_event_parser().map(ScriptContent::CustomEvent);
    let property = property_parser().map(ScriptContent::Property);
    let property_group = property_group_parser().map(ScriptContent::PropertyGroup);
    let state = state_parser().map(ScriptContent::State);
    let function = function_parser().map(ScriptContent::Function);
    let event = event_parser().map(ScriptContent::Event);

    choice((
        import,
        variable,
        structure,
        custom_event,
        property,
        property_group,
        state,
        function,
        event,
    ))
}

pub fn script_parser<'a>() -> impl TokenParser<'a, Script<'a>> {
    just(Token::Keyword(KeywordKind::ScriptName))
        .ignore_then(
            identifier_parser()
                .map_with_span(Node::new)
                .then(
                    just(Token::Keyword(KeywordKind::Extends))
                        .ignore_then(identifier_parser().map_with_span(Node::new))
                        .or_not(),
                )
                .then(
                    script_flag_parser()
                        .map_with_span(Node::new)
                        .repeated()
                        .at_least(1)
                        .or_not(),
                ),
        )
        .then(
            script_content_parser()
                .map_with_span(Node::new)
                .repeated()
                .at_least(1)
                .or_not(),
        )
        .then_ignore(end())
        .map(|output| {
            let (((name_identifier, extends_identifier), script_flags), contents) = output;
            Script::new(name_identifier, extends_identifier, script_flags, contents)
        })
}

pub(crate) fn custom_script_parser<'source>(
    parser: &mut CustomParser<'source>,
) -> ParserResult<'source, Script<'source>> {
    parser.expect(Token::Keyword(KeywordKind::ScriptName))?;
    let name = parser.map_with_span(custom_identifier_parser)?;

    let extends = match parser.consume_expected(Token::Keyword(KeywordKind::Extends)) {
        Some(_) => Some(parser.map_with_span(custom_identifier_parser)?),
        None => None,
    };

    let flags = parser
        .repeated(|parser| parser.map_with_span(custom_script_flag_parser), 1)
        .ok();

    Ok(Script::new(name, extends, flags, None))
}

#[cfg(test)]
mod test {
    use crate::ast::flags::ScriptFlag;
    use crate::ast::node::Node;
    use crate::ast::script::{custom_script_parser, script_parser};
    use crate::parse::test_utils::run_test;
    use crate::parser::CustomParser;
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

        run_test(src, expected, script_parser);
    }

    #[test]
    fn test_custom_script_parser() {
        let src = "ScriptName MyScript extends OtherScript Native";
        let expected = Script::new(
            Node::new("MyScript", 11..19),
            Some(Node::new("OtherScript", 28..39)),
            Some(vec![Node::new(ScriptFlag::Native, 40..46)]),
            None,
        );

        let tokens = papyrus_compiler_lexer::run_lexer(src);
        let mut parser = CustomParser::new(&tokens);

        let script = custom_script_parser(&mut parser).unwrap();
        parser.expect_eoi().unwrap();

        assert_eq!(script, expected);
    }
}
