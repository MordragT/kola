use ariadne::Cache;
use camino::{Utf8Path, Utf8PathBuf};
use std::{borrow::Cow, collections::HashMap, fmt, io, ops::Index, sync::Arc};

use kola_utils::{
    interner::{PathInterner, PathKey, StrInterner, StrKey},
    io::FileSystem,
};

pub type SourceId = PathKey;
pub type Source = ariadne::Source<Arc<str>>;

#[derive(Debug, Clone, Default)]
pub struct SourceManager {
    interner: PathInterner,
    sources: HashMap<SourceId, Source>,
    import_dirs: HashMap<SourceId, Utf8PathBuf>,
}

impl SourceManager {
    pub const EXTENSION: &'static str = "kl";

    pub fn new() -> Self {
        Self::default()
    }

    pub fn refetch<'p>(
        &mut self,
        path: impl Into<Cow<'p, Utf8Path>>,
        io: &impl FileSystem,
    ) -> io::Result<(SourceId, &Source)> {
        let path = path.into();

        let name = path.file_stem().unwrap(); // TODO assert that files have the right Extension
        let import_path = path.parent().unwrap().join(name); // path is absolute so this mustn't panic

        let text: Arc<str> = io.read_file(&path)?.into();
        let key = self.interner.intern(path);

        if io.is_dir(&import_path) {
            self.import_dirs.insert(key, import_path);
        }

        Ok((key, self.sources.entry(key).or_insert(Source::from(text))))
    }

    pub fn fetch<'p>(
        &mut self,
        path: impl Into<Cow<'p, Utf8Path>>,
        io: &impl FileSystem,
    ) -> io::Result<(SourceId, &Source)> {
        let path = path.into();
        assert!(path.is_absolute());

        if let Some(key) = self.interner.lookup(&path) {
            Ok((key, &self.sources[&key]))
        } else {
            self.refetch(path, io)
        }
    }

    pub fn fetch_import(
        &mut self,
        from: SourceId,
        name: StrKey,
        interner: &StrInterner,
        io: &impl FileSystem,
    ) -> io::Result<(SourceId, &Source)> {
        let path = self.resolve_import(from, name, interner)?;
        self.fetch(path, io)
    }

    pub fn resolve_import(
        &self,
        from: SourceId,
        name: StrKey,
        interner: &StrInterner,
    ) -> io::Result<Utf8PathBuf> {
        let path = self
            .import_dirs
            .get(&from)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Import path not found"))?
            .join(&interner[name])
            .with_extension(Self::EXTENSION);

        Ok(path)
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

    pub fn get(&self, path: SourceId) -> Option<&Source> {
        self.sources.get(&path)
    }

    pub fn get_import_dir(&self, path: SourceId) -> Option<&Utf8Path> {
        self.import_dirs.get(&path).map(|p| p.as_path())
    }
}

impl Cache<SourceId> for &SourceManager {
    type Storage = Arc<str>;

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
