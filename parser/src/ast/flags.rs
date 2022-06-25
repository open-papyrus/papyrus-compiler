use crate::parser::{Parse, Parser, ParserResult};

macro_rules! case_ignore_identifier {
    ( $parser:ident, $name:literal, $( $flag_bytes:ident => $out:expr ),+ $(,)? ) => {{
        let token = $parser.consume()?;

        match token {
            papyrus_compiler_lexer::syntax::token::Token::Identifier(value) => {
                let bytes = value.as_bytes();

                $( if bytes.eq_ignore_ascii_case( $flag_bytes ) {
                    return Ok($out);
                } )*

                ::core::result::Result::Err($crate::parser::ParserError::ExpectedNodeWithName {
                name: $name,
                found: *token,
            })
            },
            _ => ::core::result::Result::Err($crate::parser::ParserError::ExpectedNodeWithName {
                name: $name,
                found: *token,
            }),
        }
    }}
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ScriptFlag {
    Conditional,
    Const,
    DebugOnly,
    BetaOnly,
    Hidden,
    Native,
    Default,
}

const CONDITIONAL: &[u8] = "Conditional".as_bytes();
const CONST: &[u8] = "Const".as_bytes();
const DEBUG_ONLY: &[u8] = "DebugOnly".as_bytes();
const BETA_ONLY: &[u8] = "BetaOnly".as_bytes();
const HIDDEN: &[u8] = "Hidden".as_bytes();
const NATIVE: &[u8] = "Native".as_bytes();
const DEFAULT: &[u8] = "Default".as_bytes();

impl<'source> Parse<'source> for ScriptFlag {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        case_ignore_identifier!(parser, "Script Flag",
            CONDITIONAL => ScriptFlag::Conditional,
            CONST => ScriptFlag::Const,
            DEBUG_ONLY => ScriptFlag::DebugOnly,
            BETA_ONLY => ScriptFlag::BetaOnly,
            HIDDEN => ScriptFlag::Hidden,
            NATIVE => ScriptFlag::Native,
            DEFAULT => ScriptFlag::Default
        )
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum PropertyFlag {
    Conditional,
    Const,
    Hidden,
    Mandatory,
}

const MANDATORY: &[u8] = "Mandatory".as_bytes();

impl<'source> Parse<'source> for PropertyFlag {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        case_ignore_identifier!(parser, "Property Flag",
            CONDITIONAL => PropertyFlag::Conditional,
            CONST => PropertyFlag::Const,
            HIDDEN => PropertyFlag::Hidden,
            MANDATORY => PropertyFlag::Mandatory
        )
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum VariableFlag {
    Conditional,
    Const,
    Hidden,
}

impl<'source> Parse<'source> for VariableFlag {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        case_ignore_identifier!(parser, "Variable Flag",
            CONDITIONAL => VariableFlag::Conditional,
            CONST => VariableFlag::Const,
            HIDDEN => VariableFlag::Hidden,
        )
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum GroupFlag {
    CollapsedOnRef,
    CollapsedOnBase,
    Collapsed,
}

const COLLAPSED_ON_REF: &[u8] = "CollapsedOnRef".as_bytes();
const COLLAPSED_ON_BASE: &[u8] = "CollapsedOnBase".as_bytes();
const COLLAPSED: &[u8] = "Collapsed".as_bytes();

impl<'source> Parse<'source> for GroupFlag {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        case_ignore_identifier!(parser, "Group Flag",
            COLLAPSED_ON_REF => GroupFlag::CollapsedOnRef,
            COLLAPSED_ON_BASE => GroupFlag::CollapsedOnBase,
            COLLAPSED => GroupFlag::Collapsed,
        )
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum FunctionFlag {
    Global,
    Native,
    DebugOnly,
    BetaOnly,
}

const GLOBAL: &[u8] = "Global".as_bytes();

impl<'source> Parse<'source> for FunctionFlag {
    fn parse(parser: &mut Parser<'source>) -> ParserResult<'source, Self> {
        case_ignore_identifier!(parser, "Function Flag",
            GLOBAL => FunctionFlag::Global,
            NATIVE => FunctionFlag::Native,
            DEBUG_ONLY => FunctionFlag::DebugOnly,
            BETA_ONLY => FunctionFlag::BetaOnly,
        )
    }
}
