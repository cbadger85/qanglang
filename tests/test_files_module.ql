mod files = import("qang::files");

var test_description = "Tests the files module";

fn test_file_module() {
  var path = files.Path(".");

  assert(path is files.Path);
}