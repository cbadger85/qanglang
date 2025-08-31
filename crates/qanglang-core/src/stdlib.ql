// Higher-order functions
fn transform(value, transformer) {
  return transformer(value);
}

fn identity(x) {
  return x;
}

fn compose(f, g) {
  return (x) -> f(g(x));
}

// Array utilities
fn map(array, fun) {
  var result = [];
  var i = 0;
  while (i < array.length()) {
    result.push(fun(array.get(i)));
    i = i + 1;
  }
  return result;
}

fn filter(array, predicate) {
  var result = [];
  var i = 0;
  while (i < array.length()) {
    var item = array.get(i);
    if (predicate(item)) {
      result.push(item);
    }
    i = i + 1;
  }
  return result;
}

fn reduce(array, fun, initial) {
  var acc = initial;
  var i = 0;
  while (i < array.length()) {
    acc = fun(acc, array.get(i));
    i = i + 1;
  }
  return acc;
}
