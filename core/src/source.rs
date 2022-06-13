pub type SourceId = u32;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SourceSpan {
    pub range: core::ops::Range<usize>,
    pub id: SourceId,
}

impl ariadne::Span for SourceSpan {
    type SourceId = SourceId;

    fn source(&self) -> &Self::SourceId {
        &self.id
    }

    fn start(&self) -> usize {
        self.range.start
    }

    fn end(&self) -> usize {
        self.range.end
    }
}
