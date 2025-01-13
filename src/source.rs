use miette::NamedSource;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Source(NamedSource<Arc<str>>);

impl Source {
    pub fn new(name: impl AsRef<str>, source: impl AsRef<str>) -> Self {
        Self(NamedSource::new(name, Arc::from(source.as_ref())))
    }

    pub fn name(&self) -> &str {
        self.0.name()
    }

    pub fn as_str(&self) -> &str {
        self.0.inner()
    }

    pub fn named_source(&self) -> NamedSource<Arc<str>> {
        self.0.clone()
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }
}
