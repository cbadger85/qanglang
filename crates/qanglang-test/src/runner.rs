use std::{collections::HashMap, fs, path::PathBuf};

use qanglang_core::{
    CompilerPipeline, FunctionValueKind, HeapObject, ObjectHandle, ObjectHeap, SourceMap, Value, Vm,
};

/// Represents the result of running a single test function
#[derive(Debug, Clone)]
pub struct TestResult {
    pub name: String,
    pub passed: bool,
    pub failure_reason: Option<String>,
}

impl TestResult {
    pub fn success(name: String) -> Self {
        Self {
            name,
            passed: true,
            failure_reason: None,
        }
    }

    pub fn failure(name: String, failure_reason: String) -> Self {
        Self {
            name,
            passed: false,
            failure_reason: Some(failure_reason),
        }
    }
}

/// Represents the result of running a test suite (single .ql file)
#[derive(Debug, Clone)]
pub struct TestSuiteResult {
    pub file_path: String,
    pub relative_path: String,
    pub description: Option<String>,
    pub tests: Vec<TestResult>,
    pub suite_error: Option<String>,
}

impl TestSuiteResult {
    pub fn success(file_path: String, relative_path: String, description: Option<String>, tests: Vec<TestResult>) -> Self {
        Self {
            file_path,
            relative_path,
            description,
            tests,
            suite_error: None,
        }
    }

    pub fn failure(file_path: String, relative_path: String, error: String) -> Self {
        Self {
            file_path,
            relative_path,
            description: None,
            tests: Vec::new(),
            suite_error: Some(error),
        }
    }

    pub fn is_fatal(&self) -> bool {
        self.suite_error.is_some()
    }

    pub fn passed_count(&self) -> usize {
        self.tests.iter().filter(|t| t.passed).count()
    }

    pub fn failed_count(&self) -> usize {
        self.tests.iter().filter(|t| !t.passed).count()
    }
}

/// Runs tests from a collection of test files and returns results
pub fn run_tests(files: Vec<PathBuf>) -> Vec<TestSuiteResult> {
    let current_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::new());
    
    files.into_iter()
        .map(|file| {
            // Convert to absolute path to ensure we can read the file
            let absolute_file = if file.is_absolute() {
                file.clone()
            } else {
                current_dir.join(&file)
            };
            
            let file_path = absolute_file.to_string_lossy().to_string();
            let relative_path = file.strip_prefix(&current_dir)
                .unwrap_or(&file)
                .to_string_lossy()
                .to_string();
            
            run_test(&file_path, &relative_path)
        })
        .collect()
}

/// Runs a single test file and returns the results
pub fn run_test(file_path: &str, relative_path: &str) -> TestSuiteResult {
    // Read the test file
    let source = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(err) => {
            let error = format!("Error reading file '{}': {}", file_path, err);
            return TestSuiteResult::failure(
                file_path.to_string(),
                relative_path.to_string(),
                error,
            );
        }
    };

    let source_map = SourceMap::new(source);
    let mut heap = ObjectHeap::new();

    // Compile the test file
    let program = match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => program,
        Err(errors) => {
            let error_messages: Vec<String> = errors
                .into_errors()
                .into_iter()
                .map(|e| e.message)
                .collect();
            
            let error = format!("Compilation failed: {}", error_messages.join("; "));
            return TestSuiteResult::failure(
                file_path.to_string(),
                relative_path.to_string(),
                error,
            );
        }
    };

    // Create VM and run the initial script
    let mut vm = Vm::new(heap);
    if let Err(error) = vm.interpret(program) {
        let error = format!("Runtime error during initialization: {}", error.message);
        return TestSuiteResult::failure(
            file_path.to_string(),
            relative_path.to_string(),
            error,
        );
    }

    // Extract test information
    let (description, test_functions) = extract_test_info(vm.globals(), vm.heap());

    // Run each test function
    let mut test_results = Vec::new();
    for (test_name, function_handle) in test_functions {
        match vm.call_function_with_args(function_handle, Vec::new()) {
            Ok(_) => {
                test_results.push(TestResult::success(test_name));
            }
            Err(error) => {
                test_results.push(TestResult::failure(test_name, error.message));
            }
        }
    }

    TestSuiteResult::success(
        file_path.to_string(),
        relative_path.to_string(),
        description,
        test_results,
    )
}

/// Extracts test description and test functions from the VM globals
fn extract_test_info(globals: &HashMap<usize, Value>, heap: &ObjectHeap) -> (Option<String>, Vec<(String, ObjectHandle)>) {
    let mut description = None;
    let mut test_functions = Vec::new();

    for (handle_id, value) in globals.iter() {
        let handle = ObjectHandle::new(*handle_id);

        // Get the identifier name for this global
        let identifier = heap
            .get(handle)
            .and_then(|obj| match obj {
                HeapObject::String(name) => Some(name.as_ref()),
                _ => None,
            });

        if let Some(identifier) = identifier {
            match value {
                // Check if this is a test function (starts with "test_")
                Value::Function(FunctionValueKind::QangFunction(func_handle)) => {
                    if identifier.starts_with("test_") {
                        test_functions.push((identifier.to_string(), *func_handle));
                    }
                }
                // Check if this is the test description
                Value::String(string_handle) => {
                    if identifier == "test_description" {
                        if let Some(HeapObject::String(desc)) = heap.get(*string_handle) {
                            description = Some(desc.to_string());
                        }
                    }
                }
                _ => {}
            }
        }
    }

    // Sort test functions by name for consistent output
    test_functions.sort_by(|a, b| a.0.cmp(&b.0));

    (description, test_functions)
}

/// Formats test results for console output
pub fn format_results(results: &[TestSuiteResult]) -> String {
    let mut output = String::new();

    // Print each test suite
    for result in results {
        if let Some(description) = &result.description {
            output.push_str(&format!("{} - {}\n", result.relative_path, description));
        } else {
            output.push_str(&format!("{}\n", result.relative_path));
        }

        if let Some(error) = &result.suite_error {
            output.push_str(&format!("  ✗ Suite failed: {}\n", error));
        } else {
            for test in &result.tests {
                if test.passed {
                    output.push_str(&format!("  ✓ {}\n", test.name));
                } else {
                    output.push_str(&format!("  ✗ {}\n", test.name));
                    if let Some(reason) = &test.failure_reason {
                        output.push_str(&format!("    {}\n", reason));
                    }
                }
            }
        }
    }

    // Summary statistics
    let fatal_count = results.iter().filter(|r| r.is_fatal()).count();
    let total_passed: usize = results.iter().map(|r| r.passed_count()).sum();
    let total_failed: usize = results.iter().map(|r| r.failed_count()).sum();

    output.push('\n');
    if fatal_count > 0 {
        output.push_str(&format!("Fatal errors: {}\n", fatal_count));
    }
    if total_failed > 0 {
        output.push_str(&format!("Failed tests: {}\n", total_failed));
    }
    output.push_str(&format!("Passed tests: {}\n", total_passed));

    let total_tests = total_passed + total_failed;
    if total_tests > 0 {
        output.push_str(&format!("Total: {} tests\n", total_tests));
    }

    output
}