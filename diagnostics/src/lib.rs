use yansi::Paint;

#[cfg(feature = "ariadne-support")]
pub mod ariadne_support;

pub type SourceId = u32;
pub type SourceRange = std::ops::Range<usize>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum_macros::Display)]
pub enum SeverityLevel {
    Error,
    Warning,
    Suggestion,
}

pub trait Diagnostic {
    fn prefix(&self) -> &'static str;

    fn documentation_section(&self) -> &'static str;

    fn id(&self) -> u32;

    fn message(&self) -> String;

    fn level(&self) -> SeverityLevel;

    fn documentation_heading(&self) -> &'static str;

    fn source_id(&self) -> SourceId;

    fn range(&self) -> SourceRange;
}

pub fn error_paint<T>(item: T) -> Paint<T> {
    Paint::red(item)
}

pub fn good_paint<T>(item: T) -> Paint<T> {
    Paint::cyan(item)
}

pub fn disable_paint() {
    Paint::disable()
}

pub fn enable_paint() {
    Paint::enable()
}

pub fn convert_diagnostics<'a, T: Diagnostic + 'a>(items: Vec<T>) -> Vec<Box<dyn Diagnostic + 'a>> {
    items
        .into_iter()
        .map(|item| Box::new(item) as Box<dyn Diagnostic>)
        .collect::<Vec<Box<dyn Diagnostic>>>()
}
