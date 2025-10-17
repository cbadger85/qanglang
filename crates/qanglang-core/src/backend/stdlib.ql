

fn identity(x) {
  return x;
}

fn assert_not_nil(value, message) {
  assert(value != nil, message or "Expected value was nil.");
}

class Iterator {
  has_next() {
    return false;
  }

  next() {
    return nil;
  }

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

  flat_map(transform) {
    return FlatMapIterator(this, transform);
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

  reduce(reducer) {
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

  chain(other) {
    return ChainIterator(this, other);
  }

  zip(other) {
    return ZipIterator(this, other);
  }

  enumerate(start) {
    return EnumerateIterator(this, start);
  }

  reverse() {
    var arr = this.collect();
    arr.reverse();
    return arr.iter();
  }
}

class ArrayIterator : Iterator {
  _index = 0;

  init(arr) {
    this._arr = arr;
  }

  has_next() {
    return this._index < this._arr.length();
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    var value = this._arr[this._index];
    this._index += 1;

    return value;
  }
}

class MapIterator : Iterator {
  init(iterator, transform) {
    this._iterator = iterator;
    this._transform = transform;
  }

  has_next() {
    return this._iterator.has_next();
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    var value = this._iterator.next();
    return this._transform(value);
  }
}

class MapIndexedIterator : Iterator {
  _index = 0;

  init(iterator, transform) {
    this._iterator = iterator;
    this._transform = transform;
  }

  has_next() {
    return this._iterator.has_next();
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    var value = this._iterator.next();
    var result = this._transform(this._index, value);
    this._index += 1;
    return result;
  }
}

class FilterIterator : Iterator {
  _next_value = nil;
  _has_cached_value = false;

  init(iterator, predicate) {
    this._iterator = iterator;
    this._predicate = predicate;
  }

  has_next() {
    if (this._has_cached_value) {
      return true;
    }

    while (this._iterator.has_next()) {
      var value = this._iterator.next();

      if (this._predicate(value)) {
        this._next_value = value;
        this._has_cached_value = true;
        return true;
      }
    }

    return false;
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    var value = this._next_value;
    this._has_cached_value = false;
    this._next_value = nil;
    return value;
  }
}

class FilterIndexedIterator : Iterator {
  _next_value = nil;
  _has_cached_value = false;
  _index = 0;

  init(iterator, predicate) {
    this._iterator = iterator;
    this._predicate = predicate;
  }

  has_next() {
    if (this._has_cached_value) {
      return true;
    }

    while (this._iterator.has_next()) {
      var value = this._iterator.next();

      if (this._predicate(this._index, value)) {
        this._next_value = value;
        this._has_cached_value = true;
        this._index += 1;
        return true;
      }

      this._index += 1;
    }

    return false;
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    var value = this._next_value;
    this._has_cached_value = false;
    this._next_value = nil;
    return value;
  }
}

class FlatMapIterator : Iterator {
  _current_inner_iterator = nil;

  init(iterator, transform) {
    this._iterator = iterator;
    this._transform = transform;
  }

  has_next() {
    while (true) {
      if (this._current_inner_iterator != nil and this._current_inner_iterator.has_next()) {
        return true;
      }

      if (!this._iterator.has_next()) {
        return false;
      }

      var next_value = this._iterator.next();
      this._current_inner_iterator = this._transform(next_value);
    }
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    return this._current_inner_iterator.next();
  }
}

class TakeIterator : Iterator {
  _taken = 0;

  init(iterator, limit) {
    this._iterator = iterator;
    this._limit = limit;
  }

  has_next() {
    return this._taken < this._limit and this._iterator.has_next();
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    this._taken += 1;
    return this._iterator.next();
  }
}

class ChainIterator : Iterator {
  _first_exhausted = false;

  init(first, second) {
    this._first = first;
    this._second = second;
  }

  has_next() {
    if (!this._first_exhausted) {
      if (this._first.has_next()) {
        return true;
      }
      this._first_exhausted = true;
    }
    return this._second.has_next();
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    if (!this._first_exhausted) {
      return this._first.next();
    }

    return this._second.next();
  }
}

class ZipIterator : Iterator {

  init(left, right) {
    this._left = left;
    this._right = right;
  }

  has_next() {
    return this._left.has_next() and this._right.has_next();
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    return Pair(this._left.next(), this._right.next());
  }
}

class Sequence : Iterator {
  init(start, step) {
    this._current = start or 0;
    this._step = step or 1;
  }

  has_next() {
    return true;
  }

  next() {
    var current = this._current;
    this._current += this._step;
    return current;
  }
}

class Range : Sequence {
  init(start, end) {
    super.init(start, 1);
    this._end = end;
  }

  has_next() {
    return this._current < this._end;
  }

  next() {
    if (this.has_next()) {
      var current = this._current;
      this._current += this._step;
      return current;
    }

    return nil;
  }
}

class RangeInclusive : Range {
  init(start, end) {
    super.init(start, end + 1);
  }
}

class EnumerateIterator : Iterator {

  init(iterator, start) {
    this._iterator = iterator;
    this._current = start or 0;
  }

  has_next() {
    return this._iterator.has_next();
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    var current = this._current;
    this._current += 1;
    return Pair(current, this._iterator.next());
  }
}

fn array_of(length, init) {
  var arr = array_of_length(length);

  for (var i = 0; i < length; i += 1) {
    arr[i] = init?.call(i) or i;
  }

  return arr;
}

class Pair {
  init(left, right) {
    this.left = left;
    this.right = right;
  }

  to_array() {
    return [this.left, this.right];
  }
}

class Result {
  unwrap() {
    // Override in subclasses
    assert(false, "unwrap() must be implemented in subclass");
  }

  expect(message) {
    // Override in subclasses
    assert(false, "expect() must be implemented in subclass");
  }

  unwrap_or(value) {
    // Override in subclasses
    assert(false, "unwrap_or() must be implemented in subclass");
  }

  unwrap_or_else(cb) {
    // Override in subclasses
    assert(false, "unwrap_or_else() must be implemented in subclass");
  }

  or_else(cb) {
    // Override in subclasses
    assert(false, "or_else() must be implemented in subclass");
  }

  map(transform) {
    // Override in subclasses
    assert(false, "map() must be implemented in subclass");
  }

  flat_map(transform) {
    // Override in subclasses
    assert(false, "flat_map() must be implemented in subclass");
  }

  map_err(transform) {
    // Override in subclasses
    assert(false, "map_err() must be implemented in subclass");
  }

  is_ok() {
    // Override in subclasses
    assert(false, "is_ok() must be implemented in subclass");
  }

  is_err() {
    // Override in subclasses
    assert(false, "is_err() must be implemented in subclass");
  }

  ok() {
    // Override in subclasses
    assert(false, "ok() must be implemented in subclass");
  }

  err() {
    // Override in subclasses
    assert(false, "err() must be implemented in subclass");
  }
}

class Ok : Result {
  init(value) {
    super.init();
    this._value = value;
  }

  unwrap() {
    return this._value;
  }

  expect(message) {
    return this._value;
  }

  unwrap_or(value) {
    return this._value;
  }

  unwrap_or_else(cb) {
    return this._value;
  }

  or_else(cb) {
    return this;
  }

  map(transform) {
    return Ok(transform(this._value));
  }

  flat_map(transform) {
    return transform(this._value);
  }

  map_err(transform) {
    return this;
  }

  is_ok() {
    return true;
  }

  is_err() {
    return false;
  }

  if_ok(cb) {
    cb(this._value);
    return this;
  }
  
  if_err(cb) {
    return this;
  }

  ok() {
    return this._value;
  }

  err() {
    return nil;
  }
}

class Err : Result {
  init(error) {
    super.init();
    this._error = error;
  }

  unwrap() {
    assert(false, this.to_string());
  }

  expect(message) {
    assert(false, message);
  }

  unwrap_or(value) {
    return value;
  }

  unwrap_or_else(cb) {
    return cb();
  }

  or_else(cb) {
    return cb();
  }

  map(transform) {
    return this;
  }

  flat_map(transform) {
    return this;
  }

  map_err(transform) {
    return Err(transform(this._error));
  }

  is_ok() {
    return false;
  }

  is_err() {
    return true;
  }

  if_ok(cb) {
    return this;
  }

  if_err(cb) {
    cb(this._error);
    return this;
  }

  ok() {
    return nil;
  }

  err() {
    return this._error;
  }

  to_string() {
    return to_string(this._error) or "Unexpected Err Result.";
  }
}

class HashMap {
  init() {
    this._buckets = array_of(64, () -> []);
    this._entry_count = 0;
    this._capacity = 64;
  }

  set(key, value) {
    var index = this._get_bucket_index(key);
    var bucket = this._buckets[index];

    for (var i = 0; i < bucket.length(); i += 1) {
      var entry = bucket[i];

      if (entry.key == key) {
        entry.value = value;
        return;
      }
    }

    bucket.push(Entry(key, value));
    this._entry_count += 1;

    if (this._entry_count / this._capacity > 0.75) {
      this._resize();
    }
  }

  get(key) {
    var index = this._get_bucket_index(key);
    var bucket = this._buckets[index];

    for (var i = 0; i < bucket.length(); i += 1) {
      var entry = bucket[i];

      if (entry.key == key) {
        return entry.value;
      }
    }

    return nil;
  }

  get_or_default(key, default) {
    return this.has(key) ? this.get(key) : default;
  }

  get_or_insert(key, cb) {
    if (this.has(key)) {
      return this.get(key);
    }

    var value = cb();
    this.set(key, value);

    return value;
  }

  has(key) {
    var bucket = this._buckets[this._get_bucket_index(key)];

    for (var i = 0; i < bucket.length(); i += 1) {
      var entry = bucket[i];

      if (entry.key == key) {
        return true;
      }
    }

    return false;
  }

  clear() {
    this._buckets = array_of(64, () -> []);
    this._entry_count = 0;
    this._capacity = 64;
  }

  size() {
    return this._entry_count;
  }

  delete(key) {
    var bucket = this._buckets[this._get_bucket_index(key)];

    var index_to_remove = nil;
    for (var i = 0; i < bucket.length(); i += 1) {
      var entry = bucket[i];

      if (entry.key == key) {
        index_to_remove = i;
        break;
      }
    }

    if (index_to_remove != nil) {
        bucket.remove_at(index_to_remove);
        this._entry_count -=1;
        return true;
    }

    return false;
  }
  
  entries() {
    return ArrayIterator(this._buckets).flat_map((bucket) -> ArrayIterator(bucket));
  }

  keys() {
    return this.entries().map((entry) -> entry.key);
  }

  values() {
    return this.entries().map((entry) -> entry.value);
  }

  _get_bucket_index(key) {
    return hash(key) % this._capacity;
  }

  _resize() {
    this._capacity *= 2;

    var buckets = array_of(this._capacity, () -> []);

    for (var i = 0; i < this._buckets.length(); i += 1) {
      var bucket = this._buckets[i];

      for (var j = 0; j < bucket.length(); j += 1) {
        var entry = bucket[j];

        var index = this._get_bucket_index(entry.key);

        buckets[index].push(entry);
      }
    }

    this._buckets = buckets;
  }
}

class Entry {
  init(key, value) {
    this.key = key;
    this.value = value;
  }
}

class HashSet {
  init(arr) {
    assert(arr == nil or arr is ARRAY, "Provided value must be an array.");
    this._map = HashMap();

    if (arr) {
      for (var i = 0; i < arr.length(); i += 1) {
        this.add(arr[i]);
      }
    }
  }

  add(value) {
    var before_size = this._map.size();
    this._map.set(value, nil);

    return this._map.size() > before_size;
  }

  has(value) {
    return this._map.has(value);
  }

  delete(value) {
    return this._map.delete(value);
  }

  size() {
    return this._map.size();
  }

  clear() {
    return this._map.clear();
  }

  values() {
    return this._map.keys();
  }

  union(other) {
    var set = HashSet();

    this.values()
      .chain(other.values())
      .for_each((value) -> set.add(value));

    return set;
  }
  
  intersection(other) {
    var set = HashSet();

    var smaller = this.size() <= other.size() ? this : other;
    var larger = this.size() > other.size() ? this : other;

    smaller.values().for_each((value) -> {
      if (larger.has(value)) {
        set.add(value);
      }
    });

    return set;
  }
  
  difference(other) {
    var set = HashSet();

    this.values().for_each((value) -> {
      if (!other.has(value)) {
        set.add(value);
      }
    });

    return set;
  }
}
