use papyrus_compiler_diagnostics::SourceRange;
use std::ops::{Deref, DerefMut};

#[derive(Debug, Clone)]
pub struct Node<T> {
    inner: Box<T>,
    range: SourceRange,
}

pub(crate) fn range_union(range_a: SourceRange, range_b: SourceRange) -> SourceRange {
    range_a.start.min(range_b.start)..range_a.end.max(range_b.end)
}

impl<T> Node<T> {
    pub fn new(inner: T, range: SourceRange) -> Self {
        Node {
            inner: Box::new(inner),
            range,
        }
    }

    pub fn into_inner(self) -> T {
        *self.inner
    }

    pub fn range(&self) -> SourceRange {
        self.range.clone()
    }

    pub fn range_union<TOther>(&self, other: &Node<TOther>) -> SourceRange {
        range_union(self.range(), other.range())
    }

    pub fn map<Other, F: Fn(T) -> Other>(self, map_fn: F) -> Node<Other> {
        let range = self.range();
        Node::new(map_fn(*self.inner), range)
    }
}

impl<T> Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.deref()
    }
}

impl<T> DerefMut for Node<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner.deref_mut()
    }
}

impl<T: PartialEq> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        self.range == other.range && self.inner.deref() == other.inner.deref()
    }
}

#[cfg(test)]
mod test {
    use crate::ast::node::Node;

    #[test]
    fn test_range_union() {
        let a = Node::new("a", 0..1);
        let b = Node::new("b", 1..2);
        let res = a.range_union(&b);
        assert_eq!(res, 0..2);
    }
}
