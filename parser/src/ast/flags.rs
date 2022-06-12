use crate::ast::node::{display_optional_nodes, Node};
use crate::parse::TokenParser;
use chumsky::prelude::*;
use papyrus_compiler_lexer::syntax::keyword_kind::KeywordKind;
use papyrus_compiler_lexer::syntax::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Eq, Copy, Clone, strum_macros::Display)]
pub enum ScriptFlag {
    Conditional,
    Const,
    DebugOnly,
    BetaOnly,
    Hidden,
    Native,
    Default,
}

pub fn script_flag_parser<'a>() -> impl TokenParser<'a, ScriptFlag> {
    select! {
        Token::Keyword(KeywordKind::Conditional) => ScriptFlag::Conditional,
        Token::Keyword(KeywordKind::Const) => ScriptFlag::Const,
        Token::Keyword(KeywordKind::DebugOnly) => ScriptFlag::DebugOnly,
        Token::Keyword(KeywordKind::BetaOnly) => ScriptFlag::BetaOnly,
        Token::Keyword(KeywordKind::Hidden) => ScriptFlag::Hidden,
        Token::Keyword(KeywordKind::Native) => ScriptFlag::Native,
        Token::Keyword(KeywordKind::Default) => ScriptFlag::Default,
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, strum_macros::Display)]
pub enum PropertyFlag {
    Conditional,
    Const,
    Hidden,
    Mandatory,
}

pub fn property_flag_parser<'a>() -> impl TokenParser<'a, PropertyFlag> {
    select! {
        Token::Keyword(KeywordKind::Conditional) => PropertyFlag::Conditional,
        Token::Keyword(KeywordKind::Const) => PropertyFlag::Const,
        Token::Keyword(KeywordKind::Hidden) => PropertyFlag::Hidden,
        Token::Keyword(KeywordKind::Mandatory) => PropertyFlag::Mandatory,
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, strum_macros::Display)]
pub enum VariableFlag {
    Conditional,
    Const,
    Hidden,
}

pub fn variable_flag_parser<'a>() -> impl TokenParser<'a, VariableFlag> {
    select! {
        Token::Keyword(KeywordKind::Conditional) => VariableFlag::Conditional,
        Token::Keyword(KeywordKind::Const) => VariableFlag::Const,
        Token::Keyword(KeywordKind::Hidden) => VariableFlag::Hidden,
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, strum_macros::Display)]
pub enum GroupFlag {
    CollapsedOnRef,
    CollapsedOnBase,
    Collapsed,
}

pub fn group_flag_parser<'a>() -> impl TokenParser<'a, GroupFlag> {
    select! {
        Token::Keyword(KeywordKind::CollapsedOnRef) => GroupFlag::CollapsedOnRef,
        Token::Keyword(KeywordKind::CollapsedOnBase) => GroupFlag::CollapsedOnBase,
        Token::Keyword(KeywordKind::Collapsed) => GroupFlag::Collapsed,
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, strum_macros::Display)]
pub enum FunctionFlag {
    Global,
    Native,
    DebugOnly,
    BetaOnly,
}

pub fn function_flag_parser<'a>() -> impl TokenParser<'a, FunctionFlag> {
    select! {
        Token::Keyword(KeywordKind::Global) => FunctionFlag::Global,
        Token::Keyword(KeywordKind::Native) => FunctionFlag::Native,
        Token::Keyword(KeywordKind::DebugOnly) => FunctionFlag::DebugOnly,
        Token::Keyword(KeywordKind::BetaOnly) => FunctionFlag::BetaOnly,
    }
}

pub fn display_flags<Flag: Display>(
    flags: &Option<Vec<Node<Flag>>>,
    f: &mut Formatter<'_>,
) -> std::fmt::Result {
    display_optional_nodes(flags, " ", f)?;
    Ok(())
}
