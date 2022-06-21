use papyrus_compiler_diagnostics::SourceRange;
use smallbox::{space, SmallBox};
use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};

#[derive(Debug, Clone)]
pub struct Node<T> {
    inner: SmallBox<T, space::S4>,
    range: SourceRange,
}

pub(crate) fn range_union(range_a: SourceRange, range_b: SourceRange) -> SourceRange {
    range_a.start.min(range_b.start)..range_a.end.max(range_b.end)
}

impl<T> Node<T> {
    pub fn new(inner: T, range: SourceRange) -> Self {
        Node {
            inner: SmallBox::new(inner),
            range,
        }
    }

    pub fn inner(&self) -> &T {
        self.inner.deref()
    }

    pub fn inner_mut(&mut self) -> &mut T {
        self.inner.deref_mut()
    }

    pub fn into_inner(self) -> T {
        self.inner.into_inner()
    }

    pub fn range(&self) -> SourceRange {
        self.range.clone()
    }

    pub fn range_union<TOther>(&self, other: &Node<TOther>) -> SourceRange {
        range_union(self.range(), other.range())
    }

    pub fn map<Other, F: Fn(T) -> Other>(self, map_fn: F) -> Node<Other> {
        let range = self.range();
        Node::new(map_fn(self.inner.into_inner()), range)
    }
}

impl<T> Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}

impl<T> DerefMut for Node<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner_mut()
    }
}

impl<T: Display> Display for Node<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner.deref())
    }
}

impl<T: PartialEq> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        self.range == other.range && self.inner.deref() == other.inner.deref()
    }
}

pub fn display_optional_nodes<T: Display>(
    nodes: &Option<Vec<Node<T>>>,
    prefix: &'static str,
    f: &mut Formatter<'_>,
) -> std::fmt::Result {
    match nodes.as_ref() {
        Some(nodes) => {
            display_nodes(nodes, prefix, f)?;
        }
        None => {}
    };

    Ok(())
}

pub fn display_nodes<T: Display>(
    nodes: &Vec<Node<T>>,
    prefix: &'static str,
    f: &mut Formatter<'_>,
) -> std::fmt::Result {
    for node in nodes {
        write!(f, "{}{}", prefix, node)?;
    }

    Ok(())
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
