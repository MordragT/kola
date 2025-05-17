use camino::{Utf8Path, Utf8PathBuf};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fs, io,
};

/// Takes in a source path and a target path and determines a relative path
/// from `source` to `target`.
/// If given a relative target path, no calculation occurs.
///
/// # Panics
///
/// The provided source path should be absolute, otherwise will panic.
pub fn make_relative(source: &Utf8Path, target: &Utf8Path) -> Utf8PathBuf {
    assert!(source.is_absolute());

    match target.is_absolute() {
        true => pathdiff::diff_utf8_paths(target, source)
            .expect("Should not fail on two absolute paths"),

        false => target.into(),
    }
}

pub struct DirWalker {
    queue: VecDeque<Utf8PathBuf>,
    walked: HashSet<Utf8PathBuf>,
}

impl DirWalker {
    pub fn new(dir: Utf8PathBuf) -> Self {
        let mut queue = VecDeque::new();
        queue.push_back(dir);
        Self {
            queue,
            walked: HashSet::new(),
        }
    }

    /// Advance the directory walker to the next file. The returned path will
    /// be relative to the starting directory's path, even with symlinks
    /// (it is not canonicalised).
    pub fn next(&mut self, io: &impl FileSystem) -> io::Result<Option<Utf8PathBuf>> {
        while let Some(path) = self.queue.pop_front() {
            let full_path = io.canonicalize(&path)?;

            if io.is_file(&full_path) {
                return Ok(Some(path));
            }

            if !io.is_dir(&full_path) {
                continue;
            }

            // Check if already seen this path
            if !self.walked.insert(full_path) {
                continue;
            }

            for entry in io.read_dir(&path)? {
                self.queue.push_back(entry?);
            }
        }
        Ok(None)
    }
}

// pub trait HasFileSystem {
//     fn io(&self) -> &dyn FileSystem;
// }

// impl HasFileSystem for dyn FileSystem {
//     fn io(&self) -> &dyn FileSystem {
//         self
//     }
// }

// impl HasFileSystem for RealFileSystem {
//     fn io(&self) -> &dyn FileSystem {
//         self
//     }
// }

// impl HasFileSystem for MockFileSystem {
//     fn io(&self) -> &dyn FileSystem {
//         self
//     }
// }

pub type ReadDir = Vec<io::Result<Utf8PathBuf>>;

pub trait FileSystem {
    fn read_dir(&self, path: &Utf8Path) -> io::Result<ReadDir>;
    fn read_file(&self, path: &Utf8Path) -> io::Result<String>;

    fn is_dir(&self, path: &Utf8Path) -> bool;
    fn is_file(&self, path: &Utf8Path) -> bool;

    fn canonicalize(&self, path: &Utf8Path) -> io::Result<Utf8PathBuf>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RealFileSystem;

impl FileSystem for RealFileSystem {
    fn read_dir(&self, path: &Utf8Path) -> io::Result<ReadDir> {
        fs::read_dir(path).map(|entries| {
            entries
                .filter_map(|entry| match entry {
                    Ok(entry) => {
                        let path = Utf8PathBuf::from_path_buf(entry.path()).ok()?;
                        Some(Ok(path))
                    }
                    Err(err) => Some(Err(err)),
                })
                .collect()
        })
    }

    fn read_file(&self, path: &Utf8Path) -> io::Result<String> {
        fs::read_to_string(path)
    }

    fn is_dir(&self, path: &Utf8Path) -> bool {
        path.is_dir()
    }

    fn is_file(&self, path: &Utf8Path) -> bool {
        path.is_file()
    }

    fn canonicalize(&self, path: &Utf8Path) -> io::Result<Utf8PathBuf> {
        path.canonicalize_utf8()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MockFileSystem {
    files: HashMap<Utf8PathBuf, Option<String>>,
}

impl MockFileSystem {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
    }

    pub fn add_file(&mut self, path: Utf8PathBuf, content: String) {
        self.files.insert(path, Some(content));
    }

    pub fn add_dir(&mut self, path: Utf8PathBuf) {
        self.files.insert(path, None);
    }
}

impl FileSystem for MockFileSystem {
    fn read_dir(&self, path: &Utf8Path) -> io::Result<ReadDir> {
        let mut entries = Vec::new();
        for key in self.files.keys() {
            if key.parent() == Some(path) {
                entries.push(Ok(key.clone()));
            }
        }
        Ok(entries)
    }

    fn read_file(&self, path: &Utf8Path) -> io::Result<String> {
        match self.files.get(path) {
            Some(Some(content)) => Ok(content.clone()),
            Some(None) => Err(io::Error::new(
                io::ErrorKind::NotFound,
                "Path is a directory",
            )),
            None => Err(io::Error::new(io::ErrorKind::NotFound, "File not found")),
        }
    }

    fn is_dir(&self, path: &Utf8Path) -> bool {
        self.files.get(path).map_or(false, |v| v.is_none())
    }

    fn is_file(&self, path: &Utf8Path) -> bool {
        self.files.get(path).map_or(false, |v| v.is_some())
    }

    fn canonicalize(&self, path: &Utf8Path) -> io::Result<Utf8PathBuf> {
        Ok(path.to_owned())
    }
}
