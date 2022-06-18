use std::error::Error;
use std::fmt::Debug;

pub trait Diagnostic: Error {
    type Span: Clone + Debug;

    fn id(&self) -> u32;

    fn message(&self) -> String;

    fn span(&self) -> Self::Span;

    fn display(&self) -> String {
        format!("[{:03}] {}", self.id(), self.message())
    }
}
