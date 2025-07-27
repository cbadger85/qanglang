use crate::SourceMap;
use crate::ast::SourceSpan;
use crate::tokenizer::Token;
use std::fmt;

/// Represents different types of errors that can occur during language processing
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    Syntax,
    Runtime,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::Syntax => write!(f, "Syntax Error"),
            ErrorKind::Runtime => write!(f, "Runtime Error"),
        }
    }
}

/// Represents an error with detailed location information
#[derive(Debug, Clone, PartialEq)]
pub struct QangError {
    pub kind: ErrorKind,
    pub span: SourceSpan,
    pub message: String,
}

impl QangError {
    pub fn new(kind: ErrorKind, message: String, span: SourceSpan) -> Self {
        Self {
            kind,
            span,
            message,
        }
    }

    pub fn parse_error(message: &str, span: SourceSpan) -> Self {
        Self::new(ErrorKind::Syntax, message.to_string(), span)
    }

    pub fn runtime_error(message: &str, span: SourceSpan) -> Self {
        Self::new(ErrorKind::Runtime, message.to_string(), span)
    }
}

impl fmt::Display for QangError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.kind, self.message)
    }
}

impl std::error::Error for QangError {}

/// Collection of errors
#[derive(Debug, Clone, PartialEq, Default)]
pub struct QangErrors(pub Vec<QangError>);

impl QangErrors {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    fn push(&mut self, error: QangError) {
        self.0.push(error);
    }

    pub fn single(error: QangError) -> Self {
        Self(vec![error])
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn all(&self) -> &[QangError] {
        &self.0
    }

    pub fn take(&mut self) -> QangErrors {
        QangErrors(std::mem::take(&mut self.0))
    }
}

impl fmt::Display for QangErrors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, error) in self.0.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            write!(f, "{}", error)?;
        }
        Ok(())
    }
}

impl std::error::Error for QangErrors {}

pub type QangResult<T> = Result<T, QangErrors>;

/// Handles error reporting and pretty printing for the QangLang compiler.
pub struct ErrorReporter<'a> {
    source_map: &'a SourceMap,
    errors: QangErrors,
}

impl<'a> ErrorReporter<'a> {
    pub fn new(source_map: &'a SourceMap) -> Self {
        Self {
            source_map,
            errors: QangErrors::new(),
        }
    }

    /// Add an error to the error list
    pub fn report_error(&mut self, error: QangError) {
        self.errors.push(error);
    }

    /// Report a parse error
    pub fn report_parse_error(&mut self, message: &str, span: SourceSpan) {
        let error = QangError::parse_error(message, span);
        self.report_error(error);
    }

    /// Report a runtime error
    pub fn report_runtime_error(&mut self, message: &str, span: SourceSpan) {
        let error = QangError::runtime_error(message, span);
        self.report_error(error);
    }

    /// Report an error with a specific kind
    pub fn report(&mut self, kind: ErrorKind, message: String, span: SourceSpan) {
        let error = QangError::new(kind, message, span);
        self.report_error(error);
    }

    /// Report an error at a specific token
    pub fn report_at_token(&mut self, kind: ErrorKind, message: String, token: Option<&Token>) {
        let span = token.map(SourceSpan::from_token).unwrap_or_default();
        self.report(kind, message, span);
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get the number of errors
    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    /// Print all errors with pretty formatting to stderr
    pub fn print_errors(&self) {
        for error in &self.errors.0 {
            eprintln!("{}", self.pretty_print_error(error));
        }
    }

    /// Print a specific error with pretty formatting
    pub fn print_error(&self, error: &QangError) {
        eprintln!("{}", self.pretty_print_error(error));
    }

    /// Get a pretty printed string for all errors
    pub fn format_errors(&self) -> String {
        self.errors
            .all()
            .iter()
            .map(|error| self.pretty_print_error(error))
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Pretty print a single error with source context
    pub fn pretty_print_error(&self, error: &QangError) -> String {
        let mut output = String::new();

        let line_num = self.source_map.get_line_number(error.span.start);
        let col_num = self.source_map.get_column_number(error.span.start);

        // Error header with kind
        output.push_str(&format!(
            "{} at line {}, column {}: {}\n",
            error.kind, line_num, col_num, error.message
        ));

        // Get the problematic line
        let line_chars = self.source_map.get_line(line_num);
        let line_str: String = line_chars.iter().collect();

        // Line number padding for alignment
        let line_num_str = line_num.to_string();
        let padding = " ".repeat(line_num_str.len());

        // Show the line with line number
        output.push_str(&format!(" {} | {}\n", line_num, line_str));

        // Create the error pointer
        let error_pointer = self.create_error_pointer(error, &line_str, col_num);
        output.push_str(&format!(" {} | {}\n", padding, error_pointer));

        // Add additional context for runtime errors (e.g., call stack)
        if error.kind == ErrorKind::Runtime {
            // TODO This should also hold a stack trace.
            output.push_str(&format!(
                " {} | Note: Error occurred during execution\n",
                padding
            ));
        }

        output
    }

    /// Create a visual pointer to the error location
    fn create_error_pointer(&self, error: &QangError, line_str: &str, col_num: u32) -> String {
        let mut pointer = String::new();

        // Add spaces up to the error column
        for i in 1..col_num {
            if i <= line_str.len() as u32 {
                let ch = line_str.chars().nth((i - 1) as usize).unwrap_or(' ');
                if ch == '\t' {
                    pointer.push('\t');
                } else {
                    pointer.push(' ');
                }
            } else {
                pointer.push(' ');
            }
        }

        // Calculate the span length within the line
        let start_col = self.source_map.get_column_number(error.span.start) as usize;
        let end_col = self
            .source_map
            .get_column_number(error.span.end.saturating_sub(1)) as usize;

        // Add the error indicators
        if start_col == end_col || error.span.start == error.span.end {
            pointer.push('^');
        } else {
            // Multi-character span
            let span_length = (end_col - start_col + 1).max(1);
            for i in 0..span_length {
                if i == 0 {
                    pointer.push('^');
                } else {
                    pointer.push('~');
                }
            }
        }

        pointer
    }

    /// Create a summary of all errors
    pub fn error_summary(&self) -> String {
        if self.errors.is_empty() {
            "No errors found.".to_string()
        } else if self.errors.len() == 1 {
            "Found 1 error.".to_string()
        } else {
            format!("Found {} errors.", self.errors.len())
        }
    }

    /// Convert accumulated errors into QangErrors for returning as a result
    pub fn take_errors(&mut self) -> QangErrors {
        self.errors.take()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SourceMap;

    #[test]
    fn test_error_reporter_basic_functionality() {
        let source = "var x = 5 +\nvar y = 10;";
        let source_map = SourceMap::new(source.to_string());
        let mut reporter = ErrorReporter::new(&source_map);

        // Report an error
        reporter.report(
            ErrorKind::Syntax,
            "Expected expression after '+'".to_string(),
            SourceSpan::new(11, 11),
        );

        assert!(reporter.has_errors());
        assert_eq!(reporter.error_count(), 1);

        let output = reporter.format_errors();
        assert!(output.contains("Syntax Error at line 1, column 12"));
        assert!(output.contains("var x = 5 +"));
        assert!(output.contains("^"));
    }

    #[test]
    fn test_multiple_errors() {
        let source = "var x = 5 +\nvar y = 10 *\nvar z;";
        let source_map = SourceMap::new(source.to_string());
        let mut reporter = ErrorReporter::new(&source_map);

        // Report multiple errors
        reporter.report(
            ErrorKind::Syntax,
            "Missing operand after '+'".to_string(),
            SourceSpan::new(11, 11),
        );

        reporter.report(
            ErrorKind::Syntax,
            "Missing operand after '*'".to_string(),
            SourceSpan::new(23, 23),
        );

        assert_eq!(reporter.error_count(), 2);

        let summary = reporter.error_summary();
        assert_eq!(summary, "Found 2 errors.");
    }

    #[test]
    fn test_multichar_span_error() {
        let source = "var invalidIdentifier123! = 5;";
        let source_map = SourceMap::new(source.to_string());
        let mut reporter = ErrorReporter::new(&source_map);

        // Error spanning the invalid identifier
        reporter.report(
            ErrorKind::Syntax,
            "Invalid identifier name".to_string(),
            SourceSpan::new(4, 24), // "invalidIdentifier123!"
        );

        let output = reporter.format_errors();
        assert!(output.contains("^~~~~~~~~~~~~~~~~~~~"));
    }

    #[test]
    fn test_tokenizer_error_integration() {
        let source = "var x = \"unterminated string";
        let source_map = SourceMap::new(source.to_string());
        let mut reporter = ErrorReporter::new(&source_map);

        // Simulate a tokenizer error
        reporter.report(
            ErrorKind::Syntax,
            "Unterminated string".to_string(),
            SourceSpan::new(8, 28),
        );

        let output = reporter.format_errors();
        assert!(output.contains("Unterminated string"));
        assert!(output.contains("\"unterminated string"));
    }

    #[test]
    fn test_error_summary_no_errors() {
        let source = "var x = 5;";
        let source_map = SourceMap::new(source.to_string());
        let reporter = ErrorReporter::new(&source_map);

        let summary = reporter.error_summary();
        assert!(summary.contains("No errors found."));
    }

    #[test]
    fn test_error_summary_single_errors() {
        let source = "var x = 5;";
        let source_map = SourceMap::new(source.to_string());
        let mut reporter = ErrorReporter::new(&source_map);

        reporter.report_parse_error("Syntax error 1", SourceSpan::new(0, 3));

        let summary = reporter.error_summary();
        assert!(summary.contains("Found 1 error."));
    }

    #[test]
    fn test_error_summary_multiple_errors() {
        let source = "var x = 5;";
        let source_map = SourceMap::new(source.to_string());
        let mut reporter = ErrorReporter::new(&source_map);

        reporter.report_parse_error("Syntax error 1", SourceSpan::new(0, 3));
        reporter.report_parse_error("Syntax error 2", SourceSpan::new(4, 5));

        let summary = reporter.error_summary();
        assert!(summary.contains("Found 2 errors."));
    }

    #[test]
    fn test_pretty_printing_with_kinds() {
        let source = "var x = 5 + y;";
        let source_map = SourceMap::new(source.to_string());
        let mut reporter = ErrorReporter::new(&source_map);

        reporter.report_runtime_error("Undefined variable 'y'", SourceSpan::new(12, 13));

        let output = reporter.format_errors();
        assert!(output.contains("Runtime Error at line 1, column 13"));
        assert!(output.contains("Undefined variable 'y'"));
        assert!(output.contains("var x = 5 + y;"));
        assert!(output.contains("^"));
    }
}
