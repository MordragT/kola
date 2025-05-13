use std::sync::atomic::{AtomicU32, Ordering};

static GENERATOR: AtomicU32 = AtomicU32::new(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleKey(u32);

impl ModuleKey {
    pub fn new() -> Self {
        let id = GENERATOR.fetch_add(1, Ordering::Relaxed);
        Self(id)
    }
}
