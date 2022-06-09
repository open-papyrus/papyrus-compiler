use crate::ast::node::Node;
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

#[derive(Debug, PartialEq, Eq, Copy, Clone, strum_macros::Display)]
pub enum PropertyFlag {
    Conditional,
    Const,
    Hidden,
    Mandatory,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, strum_macros::Display)]
pub enum VariableFlag {
    Conditional,
    Const,
    Hidden,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, strum_macros::Display)]
pub enum GroupFlag {
    CollapsedOnRef,
    CollapsedOnBase,
    Collapsed,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, strum_macros::Display)]
pub enum FunctionFlag {
    Global,
    Native,
    DebugOnly,
    BetaOnly,
}

pub fn display_flags<Flag: Display>(
    flags: &Option<Vec<Node<Flag>>>,
    f: &mut Formatter<'_>,
) -> std::fmt::Result {
    match flags.as_ref() {
        Some(flags) => {
            for flag in flags {
                write!(f, "{}", flag)?;
            }
        }
        None => {}
    }

    Ok(())
}
