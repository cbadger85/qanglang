class Iterator {
  has_next() { return false; }
  next() { return nil; }
}

class ArrayIterator : Iterator {
  length;
  arr;

  init(arr) {
    this.arr = arr;
    this.length = arr.length();
  }
  
  has_next() {
    return this.length != 0;
  }

  next() {
    this.length = this.length - 1;
    return this.has_next() ? this.arr |> this.arr[this.length] : nil;
  }
}

fn for_each(iter, cb) {
  while (iter.has_next()) {
    cb(iter.next());
  }
}