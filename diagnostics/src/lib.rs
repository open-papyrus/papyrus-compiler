pub type SourceId = u32;
pub type SourceRange = std::ops::Range<usize>;

pub trait Diagnostic {
    fn id(&self) -> u32;

    fn prefix(&self) -> &'static str;

    fn message(&self) -> String;

    fn source_id(&self) -> SourceId;

    fn range(&self) -> SourceRange;
}
