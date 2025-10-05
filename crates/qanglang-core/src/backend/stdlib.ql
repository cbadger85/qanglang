fn transform(value, transformer) {
  return transformer(value);
}

fn identity(x) {
  return x;
}

class Iterator {
  has_next() {}

  next() {}

  map(transform) {
    return MapIterator(this, transform);
  }

  map_indexed(transform) {
    return MapIndexedIterator(this, transform);
  }

  filter(predicate) {
    return FilterIterator(this, predicate);
  }

  filter_indexed(predicate) {
    return FilterIndexedIterator(this, predicate);
  }

  collect() {
    var arr = [];

    while (this.has_next()) {
      arr.push(this.next());
    }

    return arr;
  }

  for_each(cb) {
    while (this.has_next()) {
      cb(this.next());
    }
  }

  for_each_indexed(cb) {
    var index = 0;
    while (this.has_next()) {
      cb(index, this.next());
      index += 1;
    }
  }

  fold(initial, reducer) {
    var accumulator = initial;

    while (this.has_next()) {
      accumulator = reducer(accumulator, this.next());
    }

    return accumulator;
  }

  reduce(reducer, initial) {
    if (initial != nil) {
      return this.fold(initial, reducer);
    }

    if (!this.has_next()) {
      return nil;
    }

    var accumulator = this.next();

    while (this.has_next()) {
      accumulator = reducer(accumulator, this.next());
    }

    return accumulator;
  }

  take(n) {
    return TakeIterator(this, n);
  }

  skip(n) {
    var count = 0;
    while (count < n and this.has_next()) {
      this.next();
      count += 1;
    }
    return this;
  }

  find(predicate) {
    while (this.has_next()) {
      var value = this.next();
      if (predicate(value)) {
        return value;
      }
    }
    return nil;
  }

  any(predicate) {
    while (this.has_next()) {
      if (predicate(this.next())) {
        return true;
      }
    }
    return false;
  }

  all(predicate) {
    while (this.has_next()) {
      if (!predicate(this.next())) {
        return false;
      }
    }
    return true;
  }

  count() {
    var n = 0;
    while (this.has_next()) {
      this.next();
      n += 1;
    }
    return n;
  }

  join(other_iterator) {
    return ChainIterator(this, other_iterator);
  }
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

class MapIndexedIterator : Iterator {
  index = 0;

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
    var result = this.transform(this.index, value);
    this.index += 1;
    return result;
  }
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

class FilterIndexedIterator : Iterator {
  next_value = nil;
  has_cached_value = false;
  index = 0;

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

      if (this.predicate(this.index, value)) {
        this.next_value = value;
        this.has_cached_value = true;
        this.index += 1;
        return true;
      }

      this.index += 1;
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

class TakeIterator : Iterator {
  taken = 0;

  init(iterator, limit) {
    this.iterator = iterator;
    this.limit = limit;
  }

  has_next() {
    return this.taken < this.limit and this.iterator.has_next();
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    this.taken += 1;
    return this.iterator.next();
  }
}

class ChainIterator : Iterator {
  first_exhausted = false;

  init(first, second) {
    this.first = first;
    this.second = second;
  }

  has_next() {
    if (!this.first_exhausted) {
      if (this.first.has_next()) {
        return true;
      }
      this.first_exhausted = true;
    }
    return this.second.has_next();
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    if (!this.first_exhausted) {
      return this.first.next();
    }

    return this.second.next();
  }
}

class Range : Iterator {
  init(start, end) {
    this.current = start;
    this.end = end;
  }

  has_next() {
    return this.current < this.end;
  }

  next() {
    if (this.has_next()) {
      var current = this.current;
      this.current += 1;
      return current;
    }

    return nil;
  }
}

fn array_of(length, init) {
  var arr = array_of_length(length);

  for (var i = 0; i < length; i += 1) {
    arr[i] = init?.call(i) or i;
  }

  return arr;
}