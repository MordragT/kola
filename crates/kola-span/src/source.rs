use ariadne::Cache;
use camino::{Utf8Path, Utf8PathBuf};
use kola_utils::{
    interner::{PathInterner, PathKey, STR_INTERNER, StrKey},
    io::{FileSystem, RealFileSystem},
};
use log::debug;
use owo_colors::OwoColorize;
use std::{borrow::Cow, collections::HashMap, fmt, io, ops::Index, sync::Arc};

pub type Source = ariadne::Source<Arc<str>>;

#[derive(Debug, Clone, Default)]
pub struct SourceManager<Io = RealFileSystem> {
    io: Io,
    interner: PathInterner,
    sources: HashMap<PathKey, Source>,
    import_dirs: HashMap<PathKey, Utf8PathBuf>,
}

impl<Io> SourceManager<Io>
where
    Io: FileSystem,
{
    pub const EXTENSION: &'static str = "kl";

    pub fn new(io: Io) -> Self {
        Self {
            io,
            interner: PathInterner::new(),
            sources: HashMap::new(),
            import_dirs: HashMap::new(),
        }
    }

    pub fn fetch<'p>(
        &mut self,
        path: impl Into<Cow<'p, Utf8Path>>,
    ) -> io::Result<(PathKey, &Source)> {
        let path = path.into();
        assert!(path.is_absolute());

        if let Some(key) = self.interner.lookup(&path) {
            Ok((key, &self.sources[&key]))
        } else {
            let name = path.file_stem().unwrap(); // TODO assert that files have the right Extension
            let import_path = path.parent().unwrap().join(name); // path is absolute so this mustn't panic

            let text: Arc<str> = self.io.read_file(&path)?.into();
            let key = self.interner.intern(path);

            if self.io.is_dir(&import_path) {
                self.import_dirs.insert(key, import_path);
            }

            Ok((key, self.sources.entry(key).or_insert(Source::from(text))))
        }
    }

    pub fn fetch_import(&mut self, from: PathKey, name: StrKey) -> io::Result<(PathKey, &Source)> {
        let path = self.resolve_import(from, name)?;
        self.fetch(path)
    }

    pub fn resolve_import(&mut self, from: PathKey, name: StrKey) -> io::Result<Utf8PathBuf> {
        let path = self
            .import_dirs
            .get(&from)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Import path not found"))?
            .join(&STR_INTERNER.read().unwrap()[name]);

        Ok(path)
    }

    pub fn lookup(&self, path: &Utf8Path) -> Option<PathKey> {
        assert!(path.is_absolute());

        self.interner.lookup(path)
    }

    pub fn contains(&self, path: &Utf8Path) -> bool {
        assert!(path.is_absolute());

        self.interner.contains(path)
    }

    pub fn get(&self, path: PathKey) -> Option<&Source> {
        self.sources.get(&path)
    }

    pub fn get_import_dir(&self, path: PathKey) -> Option<&Utf8Path> {
        self.import_dirs.get(&path).map(|p| p.as_path())
    }
}

impl<Io: FileSystem> Cache<Utf8Path> for SourceManager<Io> {
    type Storage = Arc<str>;

    fn fetch(&mut self, path: &Utf8Path) -> Result<&Source, impl fmt::Debug> {
        SourceManager::fetch(self, path).map(|(_key, source)| source)
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
