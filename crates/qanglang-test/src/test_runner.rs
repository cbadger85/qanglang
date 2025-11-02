use qanglang_core::{
    ClosureHandle, GlobalCompilerPipeline, HeapAllocator, StringHandle, Value, ValueKind, Vm,
};
use rustc_hash::FxHashMap;

use crate::test_file::SourceFile;

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

#[derive(Debug, Clone)]
pub struct TestSuiteResult {
    pub display_path: String,
    pub description: Option<String>,
    pub tests: Vec<TestResult>,
    pub suite_error: Option<String>,
}

impl TestSuiteResult {
    pub fn success(
        display_path: String,
        description: Option<String>,
        tests: Vec<TestResult>,
    ) -> Self {
        Self {
            display_path,
            description,
            tests,
            suite_error: None,
        }
    }

    pub fn failure(display_path: String, error: String) -> Self {
        Self {
            display_path,
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

pub fn run_tests_from_files(
    source_files: Vec<SourceFile>,
    vm_builder: Option<fn(&mut Vm)>,
) -> Vec<TestSuiteResult> {
    source_files
        .into_iter()
        .map(|source_file| run_test_file(source_file, vm_builder))
        .collect()
}

pub fn run_test_file(source_file: SourceFile, vm_builder: Option<fn(&mut Vm)>) -> TestSuiteResult {
    let mut allocator = HeapAllocator::new();

    let mut pipeline = GlobalCompilerPipeline::new();
    let program = match pipeline.parse_from_root(source_file.file_path.clone(), &mut allocator) {
        Ok(_) => {
            let main_module_path = match pipeline.modules.get_main_path() {
                Some(path) => path.to_path_buf(),
                None => {
                    let error_messages = ["No main module found".to_string()];
                    let error = format!("Compilation failed: {}", error_messages.join("; "));
                    return TestSuiteResult::failure(source_file.display_path, error);
                }
            };
            match pipeline.compile_module(&main_module_path, &mut allocator) {
                Ok(program) => program,
                Err(errors) => {
                    let error_messages: Vec<String> = errors
                        .into_errors()
                        .into_iter()
                        .map(|e| e.message)
                        .collect();

                    let error = format!("Compilation failed: {}", error_messages.join("; "));
                    return TestSuiteResult::failure(source_file.display_path, error);
                }
            }
        }
        Err(errors) => {
            let error_messages: Vec<String> = errors
                .into_errors()
                .into_iter()
                .map(|e| e.message)
                .collect();

            let error = format!("Compilation failed: {}", error_messages.join("; "));
            return TestSuiteResult::failure(source_file.display_path, error);
        }
    };

    // Create VM and run the initial script
    let mut vm = Vm::new(allocator);

    if let Some(builder) = vm_builder {
        builder(&mut vm);
    }

    if let Err(error) = vm.interpret(program) {
        let error = format!("Runtime error during initialization: {}", error.message);
        return TestSuiteResult::failure(source_file.display_path, error);
    }

    // Extract test information
    let (description, test_functions) = extract_test_info(vm.globals(), &vm.alloc);

    // Run each test function
    let mut test_results = Vec::new();
    let args: [qanglang_core::Value; 0] = [];
    for (test_name, function_handle) in test_functions {
        match vm.call_function(function_handle, &args) {
            Ok(_) => {
                test_results.push(TestResult::success(test_name));
            }
            Err(error) => {
                test_results.push(TestResult::failure(test_name, error.message));
            }
        }
    }

    TestSuiteResult::success(source_file.display_path, description, test_results)
}

fn extract_test_info(
    globals: &FxHashMap<StringHandle, Value>,
    allocator: &HeapAllocator,
) -> (Option<String>, Vec<(String, ClosureHandle)>) {
    let mut description = None;
    let mut test_functions = Vec::new();

    for (handle, value) in globals.iter() {
        let identifier = allocator.strings.get(*handle);

        match value.kind() {
            ValueKind::Closure(func_handle) => {
                if identifier.starts_with("test_") {
                    test_functions.push((identifier.to_string(), func_handle));
                }
            }
            ValueKind::String(string_handle) => {
                if identifier == "test_description" {
                    description = Some(allocator.strings.get(string_handle).to_string());
                }
            }
            _ => {}
        }
    }

    test_functions.sort_by(|a, b| a.0.cmp(&b.0));

    (description, test_functions)
}

pub fn format_results(results: &[TestSuiteResult]) -> String {
    let mut output = String::new();

    for result in results {
        if let Some(description) = &result.description {
            output.push_str(&format!("{} - {}\n", result.display_path, description));
        } else {
            output.push_str(&format!("{}\n", result.display_path));
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
