pub mod diag;
pub mod file;
pub mod path;
pub mod source;

pub mod prelude {
    pub use crate::diag::{
        IntoSourceDiagnostic, SourceDiagnostic, SourceReport, SourceReports, SourceResult,
    };
    pub use crate::file::{FileInfo, FileInfoTable, FileParser};
    pub use crate::path::{FilePath, ImportPath};
    pub use crate::source::Source;
}
