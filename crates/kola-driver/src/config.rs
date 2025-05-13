//! Module for compiler configuration options.

/// Options for controlling the driver's behavior
#[derive(Debug, Clone)]
pub struct DriverOptions {
    /// Whether to continue compilation after errors
    pub continue_on_error: bool,

    /// Maximum number of errors to report
    pub error_limit: Option<usize>,

    /// Whether to generate debug information
    pub debug_info: bool,

    /// Optimization level (0-3)
    pub opt_level: u8,

    /// Whether to emit warnings
    pub warnings: bool,

    /// Whether to treat warnings as errors
    pub warnings_as_errors: bool,
}

impl Default for DriverOptions {
    fn default() -> Self {
        Self {
            continue_on_error: false,
            error_limit: Some(20),
            debug_info: true,
            opt_level: 0,
            warnings: true,
            warnings_as_errors: false,
        }
    }
}
