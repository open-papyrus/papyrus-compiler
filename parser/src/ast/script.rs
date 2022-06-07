use crate::ast::flags::ScriptFlag;
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;

#[derive(Debug, PartialEq)]
pub struct Script<'a> {
    name: Node<Identifier<'a>>,
    extends: Option<Node<Identifier<'a>>>,
    flags: Option<Vec<Node<ScriptFlag>>>,
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

    pub fn name(&self) -> &Node<Identifier<'a>> {
        &self.name
    }

    pub fn extends(&self) -> Option<&Node<Identifier<'a>>> {
        self.extends.as_ref()
    }

    pub fn flags(&self) -> Option<&Vec<Node<ScriptFlag>>> {
        self.flags.as_ref()
    }
}
