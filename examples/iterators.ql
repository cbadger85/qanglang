var arr = [1, 2, 3, 4];

arr.iter().for_each((value) -> {
  println(value);
});

assert(arr.iter() is ArrayIterator);
