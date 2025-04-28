use std::{fmt, fs, io, path::Path, sync::Arc};

use miette::SourceCode;

use crate::Span;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Source {
    path: Arc<Path>,
    name: Arc<str>,
    source: Arc<str>,
}

impl Source {
    pub fn new(path: impl AsRef<Path>, name: impl AsRef<str>, source: impl AsRef<str>) -> Self {
        Self {
            path: Arc::from(path.as_ref()),
            name: Arc::from(name.as_ref()),
            source: Arc::from(source.as_ref()),
        }
    }

    pub fn from_path(path: impl AsRef<Path>) -> io::Result<Self> {
        let path = path.as_ref().canonicalize()?;
        let name = path.file_name().unwrap().to_string_lossy();
        let source = fs::read_to_string(&path)?;

        Ok(Self::new(&path, name, source))
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn as_str(&self) -> &str {
        &self.source
    }

    pub fn len(&self) -> usize {
        self.source.len()
    }

    pub fn end_of_input(&self) -> Span {
        Span {
            start: self.len(),
            end: self.len(),
            context: (),
        }
    }
}

impl SourceCode for Source {
    fn read_span<'a>(
        &'a self,
        span: &miette::SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::MietteError> {
        let inner_contents =
            self.source
                .read_span(span, context_lines_before, context_lines_after)?;

        let contents = miette::MietteSpanContents::new_named(
            self.name.to_string(),
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
        f.write_str(&self.source)
    }
}
