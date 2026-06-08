use std::{borrow::Cow, collections::HashMap, fmt, io, ops::Index, sync::Arc};

use ariadne::Cache;
use camino::Utf8Path;

use kola_utils::{
    interner::{PathInterner, PathKey},
    io::{FileSystem, RealFileSystem},
};

pub type SourceId = PathKey;
pub type Source = ariadne::Source<&'static str>;

pub struct SourceManager {
    interner: PathInterner,
    sources: HashMap<SourceId, Source>,
    fs: Arc<dyn FileSystem>,
}

impl fmt::Debug for SourceManager {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceManager")
            .field("interner", &self.interner)
            .field("sources", &self.sources)
            .finish()
    }
}

impl Clone for SourceManager {
    fn clone(&self) -> Self {
        Self {
            interner: self.interner.clone(),
            sources: self.sources.clone(),
            fs: Arc::clone(&self.fs),
        }
    }
}

impl Default for SourceManager {
    fn default() -> Self {
        Self::new(Arc::new(RealFileSystem))
    }
}

impl SourceManager {
    pub fn new(fs: Arc<dyn FileSystem>) -> Self {
        Self {
            interner: PathInterner::default(),
            sources: HashMap::new(),
            fs,
        }
    }

    pub fn fetch<'p>(
        &mut self,
        path: impl Into<Cow<'p, Utf8Path>>,
    ) -> io::Result<(SourceId, &'static str)> {
        let path = path.into();

        let text: Box<str> = self.fs.read_file(&path)?.into();
        let leaked: &'static str = Box::leak(text);
        let key = self.interner.intern(path);
        self.sources.entry(key).or_insert(Source::from(leaked));

        Ok((key, leaked))
    }

    pub fn lookup(&self, path: &Utf8Path) -> Option<SourceId> {
        assert!(path.is_absolute());
        self.interner.lookup(path)
    }

    pub fn contains(&self, path: &Utf8Path) -> bool {
        assert!(path.is_absolute());
        self.interner.contains(path)
    }

    pub fn contains_key(&self, path: SourceId) -> bool {
        self.sources.contains_key(&path)
    }

    pub fn get(&self, id: SourceId) -> &Source {
        &self.sources[&id]
    }

    pub fn get_path(&self, id: SourceId) -> &Utf8Path {
        self.interner[id].as_path()
    }

    pub fn fs(&self) -> &dyn FileSystem {
        &*self.fs
    }
}

impl Cache<SourceId> for &SourceManager {
    type Storage = &'static str;

    fn fetch(&mut self, path: &SourceId) -> Result<&Source, impl fmt::Debug> {
        self.sources.get(path).ok_or("Path key not found")
    }

    fn display<'a>(&self, id: &'a SourceId) -> Option<impl fmt::Display + 'a> {
        Some(self.interner[*id].to_owned())
    }
}

impl Index<SourceId> for SourceManager {
    type Output = Source;

    fn index(&self, index: SourceId) -> &Self::Output {
        self.sources.get(&index).unwrap()
    }
}
