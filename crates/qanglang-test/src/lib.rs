mod checker;
mod test_file;
mod test_runner;

pub use checker::{CheckResult, check_files_from_sources, check_single_file, format_check_results};
pub use test_file::{SourceFile, SourceFileResolver};
pub use test_runner::{TestResult, TestSuiteResult, format_results, run_tests_from_files};
