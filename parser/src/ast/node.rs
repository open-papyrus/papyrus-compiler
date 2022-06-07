use crate::span::Span;
use smallbox::{space, SmallBox};
use std::fmt::{Display, Formatter};
use std::ops::Deref;

#[derive(Debug, Clone)]
pub struct Node<T> {
    inner: SmallBox<T, space::S4>,
    span: Span,
}

impl<T> Node<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Node {
            inner: SmallBox::new(inner),
            span,
        }
    }

    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }
}

impl<T: Display> Display for Node<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner.deref())
    }
}

impl<T: PartialEq> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        self.span == other.span && self.inner.deref() == other.inner.deref()
    }
}
