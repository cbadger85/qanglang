fn transform(value, transformer) {
  return transformer(value);
}

fn identity(x) {
  return x;
}

class Iterator {
  has_next() {}

  next() {}
}

class ArrayIterator : Iterator {
  index = 0;

  init(arr) {
    this.arr = arr;
  }

  has_next() {
    return this.index < this.arr.length();
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    var value = this.arr[this.index];
    this.index += 1;

    return value;
  }
}

fn iter_array(arr) {
  var iter = ArrayIterator(arr);
  println(iter);

  return iter;
}

class MapIterator : Iterator {
  init(iterator, transform) {
    this.iterator = iterator;
    this.transform = transform;
  }

  has_next() {
    return this.iterator.has_next();
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    var value = this.iterator.next();
    return this.transform(value);
  }
}

fn iter_map(iter, transform) {
  return MapIterator(iter, transform);
}

class FilterIterator : Iterator {
  next_value = nil;
  has_cached_value = false;

  init(iterator, predicate) {
    this.iterator = iterator;
    this.predicate = predicate;
  }

  has_next() {
    if (this.has_cached_value) {
      return true;
    }

    while (this.iterator.has_next()) {
      var value = this.iterator.next();

      if (this.predicate(value)) {
        this.next_value = value;
        this.has_cached_value = true;
        return true;
      }
    }

    return false;
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    
    var value = this.next_value;
    this.has_cached_value = false;
    this.next_value = nil;
    return value;
  }
}

fn iter_filter(iter, predicate) {
  return FilterIterator(iter, predicate);
}

fn iter_for_each(iter, cb) {
  while (iter.has_next()) {
    cb(iter.next());
  }
}

fn iter_collect(iter) {
  var arr = [];

  while (iter.has_next()) {
    arr.push(iter.next());
  }

  return arr;
}
