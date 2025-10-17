var test_description = "Testing Result.";

fn test_ok() {
  var result = Ok(true);

  assert(result is Result);
  assert(result.is_ok());
  assert_eq(result.ok(), true);
  assert_eq(result.err(), nil);
  assert_eq(result.unwrap(), true);
}

fn test_err() {
  var result = Err("Bad error.");

  assert(result is Result);
  assert(result.is_err());
  assert_eq(result.err(), "Bad error.");
  assert_eq(result.ok(), nil);
  assert_throws(() -> result.unwrap());
}

fn test_unwrap_or() {
  var result = Err("Bad error.");
  var value = result.unwrap_or("Not error.");

  assert_eq(value, "Not error.");
}

fn test_unwrap_or_when_ok() {
  var result = Ok(true);
  var value = result.unwrap_or(0);
  
  assert_eq(value, true);
}

fn test_unwrap_or_else() {
  var result = Err("Bad error.");
  var value = result.unwrap_or_else(() -> "Not error.");

  assert_eq(value, "Not error.");
}

fn test_unwrap_or_else_when_ok() {
  var result = Ok(true);
  var value = result.unwrap_or_else(() -> assert(false, "This shold not be called."));

  assert_eq(value, true);
}

fn test_or_else() {
  var result = Err("Bad error.");

  var new_result = result.or_else(() -> Ok("No error."));

  assert_eq(new_result.unwrap(), "No error.");
}

fn test_or_else_when_ok() {
  var result = Ok(true);

  var new_result = result.or_else(() -> Ok(0));

  assert_eq(new_result.unwrap(), true);
}

fn test_map() {
  var result = Ok(0);

  var value = result.map((num) -> num + 1).unwrap();

  assert_eq(value, 1);
}

fn test_map_not_called_when_err() {
  var result = Err("Bad error.");
  
  var has_ran = false;
  var is_err = result
    .map((num) -> {
      has_ran = true;
      return num;
    })
    .is_err();
  assert(!has_ran);
  assert(is_err);
}

fn test_flat_map() {
  var result = Ok(0);
  
  var value = result.flat_map((num) -> Ok(num + 1)).unwrap();
  assert_eq(value, 1);
}

fn test_flat_map_not_called_when_err() {
  var result = Err("Bad error.");
  
  var has_ran = false;
  var is_err = result
    .flat_map((num) -> {
      has_ran = true;
      return Ok(num);
    })
    .is_err();
  assert(!has_ran);
  assert(is_err);
}

fn test_map_err() {
  var result = Err("Bad error.");
  
  var value = result.map_err((err) -> "Really " + err).err();

  assert_eq(value, "Really Bad error.");
}


fn test_map_err_not_called_when_ok() {
  var result = Ok(true);
  
  var has_ran = false;
  var value = result
    .map_err((err) -> {
      has_ran = true;
      return err;
    })
    .unwrap();

  assert(!has_ran);
  assert_eq(value, true);
}

fn test_if_ok_when_ok() {
  var result = Ok(true);

  var has_ran = false;

  result.if_ok((value) -> {
    has_ran = value;
  });

  assert(has_ran);
}

fn test_if_ok_when_err() {
  var result = Err("Bad error.");

  var has_ran = false;

  result.if_ok((value) -> {
    has_ran = value;
  });

  assert(!has_ran);
}

fn test_if_err_when_ok() {
  var result = Ok(true);

  var has_ran = false;

  result.if_err((value) -> {
    has_ran = value;
  });

  assert(!has_ran);
}

fn test_if_err_when_err() {
  var result = Err("Bad error.");

  var has_ran = false;

  result.if_err((value) -> {
    has_ran = value;
  });

  assert(has_ran);
}