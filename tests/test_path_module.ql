mod path = import("qang::path");
var Path = path.Path;
var OS_PATH_SEPARATOR = path.OS_PATH_SEPARATOR;

var test_description = "Tests the path module";

fn test_path_init() {
  var path = Path(".");

  assert(path is Path);
}

fn test_path_join() {
  var path1 = Path("foo");
  var path2 = Path("bar");
  var joined = path1.join(path2);

  assert(joined is Path);
  assert_eq(joined.to_string(), "foo" + OS_PATH_SEPARATOR + "bar");
}

fn test_path_join_other_leading_seperator() {
  var path1 = Path("foo");
  var path2 = Path(OS_PATH_SEPARATOR + "bar");
  var joined = path1.join(path2);

  assert(joined is Path);
  assert_eq(joined.to_string(), "foo" + OS_PATH_SEPARATOR + "bar");
}

fn test_path_join_this_trailing_seperator() {
  var path1 = Path("foo" + OS_PATH_SEPARATOR);
  var path2 = Path("bar");
  var joined = path1.join(path2);

  assert(joined is Path);
  assert_eq(joined.to_string(), "foo" + OS_PATH_SEPARATOR + "bar");
}

fn test_path_join_other_absolute() {
  var path1 = Path("foo");
  // On Windows: C:\absolute\path, On Unix: /absolute/path
  var absolute_path = when {
    OS_PATH_SEPARATOR == "\\" => "C:" + OS_PATH_SEPARATOR + "absolute" + OS_PATH_SEPARATOR + "path",
    else => OS_PATH_SEPARATOR + "absolute" + OS_PATH_SEPARATOR + "path"
  };
  var path2 = Path(absolute_path);
  var joined = path1.join(path2);

  // When other is absolute, it should return other
  assert(joined is Path);
  assert_eq(joined.to_string(), absolute_path);
}

fn test_path_join_other_blank() {
  var path1 = Path("foo");
  var path2 = Path("");
  var joined = path1.join(path2);

  // When other is empty, it should return this
  assert(joined is Path);
  assert_eq(joined.to_string(), "foo");
}

fn test_path_join_this_blank() {
  var path1 = Path("");
  var path2 = Path("bar");
  var joined = path1.join(path2);

  // When this is empty, it should return other
  assert(joined is Path);
  assert_eq(joined.to_string(), "bar");
}

fn test_os_path_separator_not_blank() {
  assert(OS_PATH_SEPARATOR.length());
}

fn test_path_normalize_simple() {
  var path = Path("foo" + OS_PATH_SEPARATOR + "." + OS_PATH_SEPARATOR + "bar");
  var normalized = path.normalize();

  assert(normalized is Path);
  assert_eq(normalized.to_string(), "foo" + OS_PATH_SEPARATOR + "bar");
}

fn test_path_normalize_parent_directory() {
  var path = Path("foo" + OS_PATH_SEPARATOR + "bar" + OS_PATH_SEPARATOR + ".." + OS_PATH_SEPARATOR + "baz");
  var normalized = path.normalize();

  assert(normalized is Path);
  assert_eq(normalized.to_string(), "foo" + OS_PATH_SEPARATOR + "baz");
}

fn test_path_normalize_empty_result() {
  var path = Path("");
  var normalized = path.normalize();

  assert(normalized is Path);
  assert_eq(normalized.to_string(), ".");
}

fn test_path_dirname() {
  var path = Path("foo" + OS_PATH_SEPARATOR + "bar" + OS_PATH_SEPARATOR + "file.txt");
  var dir = path.dirname();

  assert(dir is Path);
  assert_eq(dir.to_string(), "foo" + OS_PATH_SEPARATOR + "bar");
}

fn test_path_dirname_single_part() {
  var path = Path("file.txt");
  var dir = path.dirname();

  assert(dir is Path);
  assert_eq(dir.to_string(), ".");
}

fn test_path_basename() {
  var path = Path("foo" + OS_PATH_SEPARATOR + "bar" + OS_PATH_SEPARATOR + "file.txt");
  var base = path.basename();

  assert_eq(base, "file.txt");
}

fn test_path_basename_no_file() {
  var path = Path("foo" + OS_PATH_SEPARATOR);
  var base = path.basename();

  // A path ending with separator still has "foo" as the basename after trailing separators are removed
  assert_eq(base, "foo");
}

fn test_path_extension() {
  var path = Path("file.txt");
  var ext = path.extension();

  assert_eq(ext, "txt");
}

fn test_path_extension_multiple_dots() {
  var path = Path("file.tar.gz");
  var ext = path.extension();

  assert_eq(ext, "gz");
}

fn test_path_extension_no_extension() {
  var path = Path("file");
  var ext = path.extension();

  assert_eq(ext, "file");
}

fn test_path_is_relative() {
  var path = Path("foo" + OS_PATH_SEPARATOR + "bar");

  assert(path.is_relative());
}

fn test_path_to_string() {
  var path_str = "foo" + OS_PATH_SEPARATOR + "bar";
  var path = Path(path_str);

  assert_eq(path.to_string(), path_str);
}