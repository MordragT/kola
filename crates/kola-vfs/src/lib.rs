pub mod error;
pub mod file;
pub mod path;
pub mod source;

pub mod prelude {
    pub use crate::error::{SourceDiagnostic, SourceReport, SourceResult};
    pub use crate::file::{FileInfo, FileInfoTable, FileParser};
    pub use crate::path::{FilePath, ModulePath};
    pub use crate::source::Source;
}
