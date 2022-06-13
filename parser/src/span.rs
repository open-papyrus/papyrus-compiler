use std::fmt::{Display, Formatter};
use std::ops::Range;
pub type SourceId = u32;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub id: SourceId,
    pub range: Range<usize>,
}

impl Span {
    pub fn union(&self, other: &Self) -> Self {
        Self {
            id: self.id,
            range: self.range.start.min(other.range.start)..self.range.end.max(other.range.end),
        }
    }
}

#[cfg(test)]
impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            id: u32::MAX,
            range,
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.range.start, self.range.end)
    }
}

impl chumsky::Span for Span {
    type Context = SourceId;
    type Offset = usize;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self { id: context, range }
    }

    fn context(&self) -> Self::Context {
        self.id
    }

    fn start(&self) -> Self::Offset {
        self.range.start
    }

    fn end(&self) -> Self::Offset {
        self.range.end()
    }
}
