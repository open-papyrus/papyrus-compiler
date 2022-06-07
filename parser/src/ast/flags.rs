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
