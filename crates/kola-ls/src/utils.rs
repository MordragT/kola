use camino::Utf8PathBuf;
use kola_span::{Report, Source};
use tower_lsp_server::ls_types as ls;

pub fn uri_to_path(uri: &ls::Uri) -> Option<Utf8PathBuf> {
    uri.to_file_path()
        .and_then(|path| Utf8PathBuf::from_path_buf(path.into_owned()).ok())
}

pub fn report_to_diags(report: Report, source: &Source) -> Vec<ls::Diagnostic> {
    report
        .diagnostics
        .into_iter()
        .filter_map(
            |kola_span::Diagnostic {
                 message,
                 severity,
                 loc,
                 ..
             }| {
                // Convert loc (span) to LSP Range
                let (start_line, start_line_num, start_byte_col) =
                    source.get_byte_line(loc.span.start)?;
                let (end_line, end_line_num, end_byte_col) = source.get_byte_line(loc.span.end)?;

                let start_line_text = source.get_line_text(start_line)?;
                let end_line_text = source.get_line_text(end_line)?;

                let start_utf16_col =
                    start_line_text[..start_byte_col].encode_utf16().count() as u32;
                let end_utf16_col = end_line_text[..end_byte_col].encode_utf16().count() as u32;

                let range = ls::Range {
                    start: ls::Position {
                        line: start_line_num as u32,
                        character: start_utf16_col,
                    },
                    end: ls::Position {
                        line: end_line_num as u32,
                        character: end_utf16_col,
                    },
                };

                let severity = match severity {
                    kola_span::Severity::Error => ls::DiagnosticSeverity::ERROR,
                    kola_span::Severity::Warning => ls::DiagnosticSeverity::WARNING,
                    kola_span::Severity::Info => ls::DiagnosticSeverity::INFORMATION,
                };

                Some(ls::Diagnostic {
                    range,
                    severity: Some(severity),
                    source: Some("kola".to_string()),
                    message,
                    ..Default::default()
                })
            },
        )
        .collect()
}
