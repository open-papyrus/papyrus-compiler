use crate::ast::flags::ScriptFlag;
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;

#[derive(Debug, PartialEq)]
pub struct Script<'a> {
    pub name: Node<Identifier<'a>>,
    pub extends: Option<Node<Identifier<'a>>>,
    pub flags: Option<Vec<Node<ScriptFlag>>>,
}

impl<'a> Script<'a> {
    pub fn new(
        name: Node<Identifier<'a>>,
        extends: Option<Node<Identifier<'a>>>,
        flags: Option<Vec<Node<ScriptFlag>>>,
    ) -> Self {
        Self {
            name,
            extends,
            flags,
        }
    }
}
