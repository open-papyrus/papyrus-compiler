pub type Span = core::ops::Range<usize>;

pub fn union(a: &Span, b: &Span) -> Span {
    a.start.min(b.start)..a.end.max(b.end)
}
