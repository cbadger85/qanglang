mod path = import("qang::path");
var Path = path.Path;
var OS_PATH_SEPARATOR = path.OS_PATH_SEPARATOR;

var test_description = "Tests the path module";

fn test_path_init() {
  var path = Path(".");

  assert(path is Path);
}

fn test_path_join() {
  
}

fn test_path_join_other_leading_seperator() {

}

fn test_path_join_this_trailing_seperator() {

}
  
fn test_path_join_other_absolute() {
  
}

fn test_path_join_other_blank() { 

}

fn test_path_join_this_blank() {

}

fn test_os_path_separator_not_blank() {
  assert(OS_PATH_SEPARATOR.length());
}