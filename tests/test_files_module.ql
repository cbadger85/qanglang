mod path = import("qang::path");
var Path = path.Path;

mod files = import("qang::files");

var test_description = "Tests the file system module.";

fn test_write_file() {
  var contents = "the contents of the file.";

  var write_result = files.write_file(Path("./write_test.txt").normalize(), contents);

  assert(files.write_file(Path("./write_test.txt").normalize(), contents).is_ok());
  assert(files.remove_file(Path("./write_test.txt").normalize(), contents).is_ok());
}