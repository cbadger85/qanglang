class Iterator {
  has_next() { return false; }
  next() { return nil; }
}

class ArrayIterator : Iterator {
  length;
  arr;

  init(arr) {
    this.arr = arr;
    this.len = length_of(arr);
  }
  
  has_next() {
    return this.length != 0;
  }

  next() {
    this.length = this.length - 1;
    return this.has_next() ? arr |> index_of(length) : nil;
  }
}

fn for_each(iter, cb) {
  while (iter.has_next()) {
    cb(iter.next());
  }
}