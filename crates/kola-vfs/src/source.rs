use kola_syntax::span::Span;
use std::{fmt, fs, io, sync::Arc};

use crate::path::{FilePath, ImportPath};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Source {
    file_path: FilePath,
    import_path: Option<ImportPath>,
    content: Arc<str>,
}

impl Source {
    pub fn new(
        file_path: FilePath,
        import_path: Option<ImportPath>,
        content: impl AsRef<str>,
    ) -> Self {
        Self {
            file_path,
            import_path,
            content: Arc::from(content.as_ref()),
        }
    }

    pub fn from_path(file_path: FilePath, import_path: Option<ImportPath>) -> io::Result<Self> {
        let content = fs::read_to_string(file_path.path.as_std_path())?;

        Ok(Self::new(file_path, import_path, content))
    }

    #[inline]
    pub fn name(&self) -> Arc<str> {
        self.file_path.name()
    }

    #[inline]
    pub fn file_path(&self) -> FilePath {
        self.file_path.clone()
    }

    #[inline]
    pub fn import_path(&self) -> Option<ImportPath> {
        self.import_path.clone()
    }

    #[inline]
    pub fn try_import_path(&self) -> io::Result<ImportPath> {
        if let Some(path) = self.import_path() {
            Ok(path)
        } else {
            Err(io::Error::new(
                io::ErrorKind::NotFound,
                "import path not found",
            ))
        }
    }

    #[inline]
    pub fn content(&self) -> Arc<str> {
        self.content.clone()
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.content
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.content.len()
    }

    #[inline]
    pub fn end_of_input(&self) -> Span {
        Span {
            start: self.len(),
            end: self.len(),
            context: (),
        }
    }
}

impl miette::SourceCode for Source {
    fn read_span<'a>(
        &'a self,
        span: &miette::SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::MietteError> {
        let inner_contents =
            self.content
                .read_span(span, context_lines_before, context_lines_after)?;

        let contents = miette::MietteSpanContents::new_named(
            self.name().to_string(),
            inner_contents.data(),
            *inner_contents.span(),
            inner_contents.line(),
            inner_contents.column(),
            inner_contents.line_count(),
        );

        Ok(Box::new(contents))
    }
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.content)
    }
}
