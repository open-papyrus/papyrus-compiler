use crate::parser::{Parse, Parser};
use crate::parser_error::*;

macro_rules! case_ignore_identifier {
    ( $parser:ident, $( $flag_bytes:ident, $flag_name:literal => $out:expr ),+ $(,)? ) => {{
        let (token, range) = $parser.consume()?;
        match token {
            papyrus_compiler_lexer::syntax::token::Token::Identifier(value) => {
                let bytes = value.as_bytes();

                $(
                    if bytes.eq_ignore_ascii_case($flag_bytes) {
                        return Ok($out);
                    }
                )*

                ::core::result::Result::Err($crate::parser_error::ParserError::AggregatedErrors(::std::collections::HashSet::from([
                    $(
                        $crate::parser_error::ParserError::ExpectedToken {
                            expected: papyrus_compiler_lexer::syntax::token::Token::Identifier($flag_name),
                            found: (*token, range.clone())
                        }
                    ),*
                ])))
            },
            _ => ::core::result::Result::Err($crate::parser_error::ParserError::ExpectedToken {
                expected: papyrus_compiler_lexer::syntax::token::Token::Identifier(""),
                found: (*token, range.clone())
            })
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
        case_ignore_identifier!(parser,
            CONDITIONAL, "Conditional" => ScriptFlag::Conditional,
            CONST, "Const" => ScriptFlag::Const,
            DEBUG_ONLY, "DebugOnly" => ScriptFlag::DebugOnly,
            BETA_ONLY, "BetaOnly" => ScriptFlag::BetaOnly,
            HIDDEN, "Hidden" => ScriptFlag::Hidden,
            NATIVE, "Native" => ScriptFlag::Native,
            DEFAULT, "Default" => ScriptFlag::Default
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
        case_ignore_identifier!(parser,
            CONDITIONAL, "Conditional" => PropertyFlag::Conditional,
            CONST, "Const" => PropertyFlag::Const,
            HIDDEN, "Hidden" => PropertyFlag::Hidden,
            MANDATORY, "Mandatory" => PropertyFlag::Mandatory
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
        case_ignore_identifier!(parser,
            CONDITIONAL, "Conditional" => VariableFlag::Conditional,
            CONST, "Const" => VariableFlag::Const,
            HIDDEN, "Hidden" => VariableFlag::Hidden,
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
        case_ignore_identifier!(parser,
            COLLAPSED_ON_REF, "CollapsedOnRef" => GroupFlag::CollapsedOnRef,
            COLLAPSED_ON_BASE, "CollapsedOnBase" => GroupFlag::CollapsedOnBase,
            COLLAPSED, "Collapsed" => GroupFlag::Collapsed,
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
        case_ignore_identifier!(parser,
            GLOBAL, "Global" => FunctionFlag::Global,
            NATIVE, "Native" => FunctionFlag::Native,
            DEBUG_ONLY, "DebugOnly" => FunctionFlag::DebugOnly,
            BETA_ONLY, "BetaOnly" => FunctionFlag::BetaOnly,
        )
    }
}
