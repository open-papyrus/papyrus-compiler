pub type Span = core::ops::Range<usize>;
pub type Identifier<'a> = &'a str;
pub type Spanned<T> = (T, Span);

#[derive(Debug)]
pub struct Script<'a> {
    pub name: Spanned<Identifier<'a>>,
    pub extends: Option<Spanned<Identifier<'a>>>,
    pub flags: Option<Vec<Spanned<ScriptFlag>>>,
    
    pub expressions: Vec<Spanned<Expr<'a>>>
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    /// 'Import <identifier>'
    Import(Identifier<'a>),

    /// '<identifier>'
    Identifier(Identifier<'a>),

    /// '1', '1.0', '"Hello World"', 'true', 'none'
    Literal(LiteralKind<'a>),

    /// 'Return <expr>'
    Return(Box<Expr<'a>>),
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum PropertyFlag {
    Conditional,
    Const,
    Hidden,
    Mandatory,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum VariableFlag {
    Conditional,
    Const,
    Hidden,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum GroupFlag {
    CollapsedOnRef,
    CollapsedOnBase,
    Collapsed,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum FunctionFlag {
    DebugOnly,
    BetaOnly,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum LiteralKind<'a> {
    BooleanLiteral(bool),
    IntegerLiteral(i32),
    FloatLiteral(f32),
    StringLiteral(&'a str),
    NoneLiteral,
}
