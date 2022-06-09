use crate::span::{union, Span};
use smallbox::{space, SmallBox};
use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};

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
        self.inner.deref()
    }

    pub fn inner_mut(&mut self) -> &mut T {
        self.inner.deref_mut()
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }

    pub fn span_union<TOther>(&self, other: &Node<TOther>) -> Span {
        union(&self.span, &other.span)
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
        self.span == other.span && self.inner.deref() == other.inner.deref()
    }
}
