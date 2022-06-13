use crate::SourceId;
use anyhow::Context;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::fs;
use std::path::Path;

#[derive(Default)]
pub struct SourceCache {
    sources: HashMap<u32, ariadne::Source>,
    paths: HashMap<u32, String>,
    next_id: u32,
}

impl SourceCache {
    pub fn add_file<P: AsRef<Path>>(&mut self, path: P) -> Result<(u32, String), anyhow::Error> {
        let path_string = path
            .as_ref()
            .to_str()
            .with_context(|| format!("Path {} is not valid UTF8!", path.as_ref().display()))?
            .to_string();

        let script = fs::read_to_string(path)?;
        let source = ariadne::Source::from(&script);

        let id = self.next_id;
        self.next_id += 1;

        self.sources.insert(id, source);
        self.paths.insert(id, path_string);

        Ok((id, script))
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
        let path = self.paths.get(id);
        match path {
            Some(path) => Some(Box::new(path.clone())),
            None => None,
        }
    }
}
