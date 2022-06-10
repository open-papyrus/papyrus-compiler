use crate::SourceId;
use anyhow::Context;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::fs;
use std::path::Path;

#[derive(Default)]
pub struct SourceCache {
    sources: HashMap<SourceId, ariadne::Source>,
}

impl SourceCache {
    pub fn add_file<P: AsRef<Path>>(&mut self, path: P) -> Result<String, anyhow::Error> {
        let id = path
            .as_ref()
            .to_str()
            .with_context(|| format!("Path {} is not valid UTF8!", path.as_ref().display()))?
            .to_string();

        let script = fs::read_to_string(path)?;
        let source = ariadne::Source::from(&script);

        self.sources.insert(id, source);

        Ok(script)
    }
}

impl ariadne::Cache<SourceId> for SourceCache {
    fn fetch(&mut self, id: &SourceId) -> Result<&ariadne::Source, Box<dyn Debug + '_>> {
        let source = self.sources.get(id);
        match source {
            Some(source) => Ok(source),
            None => Err(Box::new(format!("Unknown id: {}", id))),
        }
    }

    fn display<'a>(&self, id: &'a SourceId) -> Option<Box<dyn Display + 'a>> {
        Some(Box::new(id))
    }
}
