use camino::Utf8Path;
use derive_more::Display;
use std::{io, sync::Arc};

// TODO explicit errors?

// TODO maybe with Repr pattern only one Arc
#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display("{path}")]
pub struct FilePath {
    pub(crate) path: Arc<Utf8Path>,
    name: Arc<str>,
}

impl FilePath {
    pub fn new_unchecked(path: impl AsRef<Utf8Path>, name: impl AsRef<str>) -> Self {
        Self {
            path: path.as_ref().into(),
            name: name.as_ref().into(),
        }
    }

    pub fn open(file_path: impl AsRef<Utf8Path>) -> io::Result<(Self, Option<ImportPath>)> {
        // TODO this throws an error if file does not exist,
        // but maybe check that explictely with a better error message ?
        let path = file_path.as_ref().canonicalize_utf8()?;

        if path.is_dir() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Path is a directory",
            ));
        }

        let name = path
            .file_stem()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Path has no file name"))?;

        let import_path = if let Some(parent) = path.parent() {
            ImportPath::open(parent.join(name)).ok()
        } else {
            None
        };

        Ok((
            Self {
                name: name.into(),
                path: path.into(),
            },
            import_path,
        ))
    }

    pub fn name(&self) -> Arc<str> {
        self.name.clone()
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImportPath(Arc<Utf8Path>);

impl ImportPath {
    pub const EXTENSION: &'static str = "kl";

    pub fn new_unchecked(path: impl AsRef<Utf8Path>) -> Self {
        Self(path.as_ref().into())
    }

    pub fn open(path: impl AsRef<Utf8Path>) -> io::Result<Self> {
        // TODO this throws an error if dir does not exist,
        // but maybe check that explictely with a better error message ?
        let path = path.as_ref().canonicalize_utf8()?;

        if path.is_file() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Path is a file",
            ));
        }

        Ok(Self(path.into()))
    }

    pub fn discover(&self, name: impl AsRef<str>) -> io::Result<(FilePath, Option<Self>)> {
        let name = name.as_ref();
        let base = self.0.join(name);
        let path = base.with_extension(Self::EXTENSION);

        let file_path = if path.is_file() {
            FilePath {
                path: path.into(),
                name: name.into(),
            }
        } else {
            return Err(io::Error::new(io::ErrorKind::NotFound, "File not found"));
        };

        let import_dir = if base.is_dir() {
            Some(Self(base.into()))
        } else {
            None
        };

        Ok((file_path, import_dir))
    }
}
