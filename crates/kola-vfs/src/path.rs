use camino::Utf8Path;
use derive_more::Display;
use std::{io, sync::Arc};

// TODO maybe with Repr pattern only one Arc
#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display("{path}")]
pub struct FilePath {
    pub(crate) path: Arc<Utf8Path>,
    name: Arc<str>,
}

impl FilePath {
    pub fn name(&self) -> Arc<str> {
        self.name.clone()
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModulePath(Arc<Utf8Path>);

impl ModulePath {
    pub const EXTENSION: &'static str = "kl";

    pub fn open(path: impl AsRef<Utf8Path>) -> io::Result<Self> {
        let path = path.as_ref().canonicalize_utf8()?;

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
