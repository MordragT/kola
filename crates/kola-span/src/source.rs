use ariadne::Cache;
use camino::Utf8Path;
use kola_utils::{
    interner::{PathInterner, PathKey},
    io::{FileSystem, RealFileSystem},
};
use std::{collections::HashMap, fmt, io, ops::Index, sync::Arc};

pub type Source = ariadne::Source<Arc<str>>;

#[derive(Debug, Clone, Default)]
pub struct SourceManager<Io = RealFileSystem> {
    io: Io,
    interner: PathInterner,
    sources: HashMap<PathKey, Source>,
}

impl<Io> SourceManager<Io>
where
    Io: FileSystem,
{
    pub fn new(io: Io) -> Self {
        Self {
            io,
            interner: PathInterner::new(),
            sources: HashMap::new(),
        }
    }

    pub fn fetch(&mut self, path: &Utf8Path) -> io::Result<&Source> {
        if let Some(key) = self.interner.lookup(path) {
            Ok(&self.sources[&key])
        } else {
            let text: Arc<str> = self.io.read_file(path)?.into();
            let key = self.interner.intern(path);
            Ok(self.sources.entry(key).or_insert(Source::from(text)))
        }
    }

    pub fn lookup(&self, path: &Utf8Path) -> Option<PathKey> {
        self.interner.lookup(path)
    }

    pub fn contains(&self, path: &Utf8Path) -> bool {
        self.interner.contains(path)
    }

    pub fn get(&self, path: PathKey) -> Option<&Source> {
        self.sources.get(&path)
    }
}

impl<Io: FileSystem> Cache<Utf8Path> for SourceManager<Io> {
    type Storage = Arc<str>;

    fn fetch(&mut self, path: &Utf8Path) -> Result<&Source, impl fmt::Debug> {
        SourceManager::fetch(self, path)
    }

    fn display<'a>(&self, id: &'a Utf8Path) -> Option<impl fmt::Display + 'a> {
        Some(id)
    }
}

impl<Io: FileSystem> Cache<PathKey> for &SourceManager<Io> {
    type Storage = Arc<str>;

    fn fetch(&mut self, path: &PathKey) -> Result<&Source, impl fmt::Debug> {
        self.sources.get(path).ok_or("Path key not found")
    }

    fn display<'a>(&self, id: &'a PathKey) -> Option<impl fmt::Display + 'a> {
        Some(self.interner[*id].to_owned())
    }
}

impl<Io: FileSystem> Index<PathKey> for SourceManager<Io> {
    type Output = Source;

    fn index(&self, index: PathKey) -> &Self::Output {
        self.sources.get(&index).unwrap()
    }
}
