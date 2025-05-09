use ariadne::Cache;
use camino::Utf8Path;
use kola_utils::{PathInterner, PathKey};
use std::{collections::HashMap, fmt, fs, io, sync::Arc};

pub type Source = ariadne::Source<Arc<str>>;

#[derive(Debug, Clone, Default)]
pub struct SourceCache {
    interner: PathInterner,
    sources: HashMap<PathKey, Source>,
}

impl Cache<Utf8Path> for SourceCache {
    type Storage = Arc<str>;

    fn fetch(&mut self, path: &Utf8Path) -> Result<&Source, impl fmt::Debug> {
        if let Some(key) = self.interner.lookup(path) {
            Ok::<_, io::Error>(&self.sources[&key])
        } else {
            let key = self.interner.intern(path);
            let text: Arc<str> = fs::read_to_string(path)?.into();
            Ok(self.sources.entry(key).or_insert(Source::from(text)))
        }
    }

    fn display<'a>(&self, id: &'a Utf8Path) -> Option<impl fmt::Display + 'a> {
        Some(id)
    }
}

impl Cache<PathKey> for &SourceCache {
    type Storage = Arc<str>;

    fn fetch(&mut self, path: &PathKey) -> Result<&Source, impl fmt::Debug> {
        self.sources.get(path).ok_or("Path key not found")
    }

    fn display<'a>(&self, id: &'a PathKey) -> Option<impl fmt::Display + 'a> {
        Some(self.interner[*id].to_owned())
    }
}

impl SourceCache {
    pub fn new() -> Self {
        Self::default()
    }
}
