mod path = import("qang::path");
var Path = path.Path;

mod files = import("qang::files");

var test_description = "Tests the file system module.";

fn test_write_file() {
  var contents = "the contents of the file.";
  var test_path = Path("./write_test.txt").normalize();

  assert(files.write_file(test_path, contents).is_ok());
  assert(files.remove_file(test_path).is_ok());
}

fn test_read_file() {
  var contents = "Hello, World!";
  var test_path = Path("./read_test.txt").normalize();

  assert(files.write_file(test_path, contents).is_ok());

  var read_result = files.read_file(test_path);
  assert(read_result.is_ok());
  assert_eq(read_result.unwrap(), contents);

  assert(files.remove_file(test_path).is_ok());
}

fn test_read_file_invalid_path() {
  var result = files.read_file("not a Path object");
  assert(result.is_err());
}

fn test_append_file() {
  var initial_contents = "Line 1\n";
  var appended_contents = "Line 2\n";
  var test_path = Path("./append_test.txt").normalize();

  assert(files.write_file(test_path, initial_contents).is_ok());
  assert(files.append_file(test_path, appended_contents).is_ok());

  var read_result = files.read_file(test_path);
  assert(read_result.is_ok());
  assert_eq(read_result.unwrap(), initial_contents + appended_contents);

  assert(files.remove_file(test_path).is_ok());
}

fn test_append_file_not_exists() {
  var test_path = Path("./nonexistent_append.txt").normalize();
  var result = files.append_file(test_path, "content");
  assert(result.is_err());
}

fn test_read_chars() {
  var contents = "Hello, World! 世界\nLine 2";
  var test_path = Path("./read_chars_test.txt").normalize();

  assert(files.write_file(test_path, contents).is_ok());

  var result = files.read_chars(test_path);
  assert(result.is_ok());

  var iterator = result.unwrap();
  var read_back = "";

  while (iterator.has_next()) {
    read_back += iterator.next();
  }

  assert_eq(read_back, contents);
  assert(files.remove_file(test_path).is_ok());
}

fn test_create_and_remove_dir() {
  var dir_path = Path("./test_dir").normalize();

  assert(files.create_dir(dir_path).is_ok());
  assert(dir_path.is_dir());
  assert(files.remove_dir(dir_path).is_ok());
}

fn test_create_dirs() {
  var nested_path = Path("./test_parent/test_child").normalize();

  assert(files.create_dirs(nested_path).is_ok());
  assert(nested_path.is_dir());

  // Clean up - remove child first, then parent
  assert(files.remove_dir(nested_path).is_ok());
  assert(files.remove_dir(Path("./test_parent").normalize()).is_ok());
}

fn test_remove_all() {
  var parent_path = Path("./test_remove_all").normalize();
  var child_path = parent_path.join(Path("child"));
  var file_path = child_path.join(Path("file.txt"));

  assert(files.create_dirs(child_path).is_ok());
  assert(files.write_file(file_path, "test content").is_ok());

  // remove_all should remove directory and all contents
  assert(files.remove_all(parent_path).is_ok());
}

fn test_list() {
  var dir_path = Path("./test_list_dir").normalize();
  var file1_path = dir_path.join(Path("file1.txt"));
  var file2_path = dir_path.join(Path("file2.txt"));

  assert(files.create_dir(dir_path).is_ok());
  assert(files.write_file(file1_path, "content1").is_ok());
  assert(files.write_file(file2_path, "content2").is_ok());

  var list_result = files.list(dir_path);
  assert(list_result.is_ok());

  var entries = list_result.unwrap();
  assert_eq(entries.length(), 2);

  assert(files.remove_all(dir_path).is_ok());
}

fn test_get_file_size() {
  var contents = "Hello, World!";
  var test_path = Path("./size_test.txt").normalize();

  assert(files.write_file(test_path, contents).is_ok());

  var size_result = files.get_file_size(test_path);
  assert(size_result.is_ok());
  assert(size_result.unwrap() > 0);

  assert(files.remove_file(test_path).is_ok());
}

fn test_get_file_modified_time() {
  var contents = "test";
  var test_path = Path("./modified_test.txt").normalize();

  assert(files.write_file(test_path, contents).is_ok());

  var time_result = files.get_file_modified_time(test_path);
  assert(time_result.is_ok());
  assert(time_result.unwrap() > 0);

  assert(files.remove_file(test_path).is_ok());
}

fn test_copy_file() {
  var contents = "file to copy";
  var source_path = Path("./copy_source.txt").normalize();
  var dest_path = Path("./copy_dest.txt").normalize();

  assert(files.write_file(source_path, contents).is_ok());
  assert(files.copy_file(source_path, dest_path).is_ok());

  var read_result = files.read_file(dest_path);
  assert(read_result.is_ok());
  assert_eq(read_result.unwrap(), contents);

  assert(files.remove_file(source_path).is_ok());
  assert(files.remove_file(dest_path).is_ok());
}

fn test_move() {
  var contents = "file to move";
  var source_path = Path("./move_source.txt").normalize();
  var dest_path = Path("./move_dest.txt").normalize();

  assert(files.write_file(source_path, contents).is_ok());
  assert(files.move(source_path, dest_path).is_ok());

  assert(!source_path.exists());
  assert(dest_path.exists());

  var read_result = files.read_file(dest_path);
  assert(read_result.is_ok());
  assert_eq(read_result.unwrap(), contents);

  assert(files.remove_file(dest_path).is_ok());
}

fn test_write_file_invalid_path() {
  var result = files.write_file("not a Path", "content");
  assert(result.is_err());
}

fn test_write_file_invalid_content() {
  var test_path = Path("./test.txt").normalize();
  var result = files.write_file(test_path, 123);
  assert(result.is_err());
}