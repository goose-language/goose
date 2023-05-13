#include <string.h>
#include <stdlib.h>
#include "list.h"
#include "eq.h"
#include "num.h"
#include "conversion.h"
#include "type.h"
#include "error.h"


#include "io.h"

int length(VALUE list) {
  if (get_type(list) == TYPE_ARRAY) {
    return decode_pointer(list)->as_array.length;
  } else {
    return 0;
  }
}

VALUE intern_property_(VALUE dict, uint64_t idx) {
  if (get_type(dict) == TYPE_INTERN) {
    Intern heap = decode_pointer(dict)->as_intern;
    return heap.data[idx];
  } else {
    throwError("expected intern, got %s", decode_string(Type_of(list(2, unit(), dict))));
    return unit();
  }
}

VALUE is(VALUE dict, char* type) {
  if (get_type(dict) == TYPE_INTERN) {
    Intern heap = decode_pointer(dict)->as_intern;
    return boolean(strcmp(heap.name, type) == 0);
  } else {
    throwError("expected intern, got %s", decode_string(Type_of(list(2, unit(), dict))));
    return unit();
  }
}

VALUE in(VALUE dict, char* key) {
  if (get_type(dict) == TYPE_DICT) {
    Dict heap = decode_pointer(dict)->as_dict;
    int i = hash((const char*) key, heap.length);
    struct Entry entry = heap.entries[i];
    if (strcmp(entry.key, key) == 0) {
      return boolean(1);
    }
    while (entry.next != NULL) {
      if (strcmp(entry.key, key) == 0) {
        return boolean(1);
      }
      entry = *entry.next;
    }
    return boolean(0);
  } else if (get_type(dict) == TYPE_ARRAY) {
    Array heap = decode_pointer(dict)->as_array;
    for (int i = 0; i < heap.length; i++) {
      if (eq(heap.data[i], string(key))) {
        return boolean(1);
      }
    }
    return boolean(0);
  } else {
    throwError("expected structure or list, got %s", decode_string(Type_of(list(2, unit(), dict))));
    return unit();
  }
}

VALUE index_(VALUE v, int i) {
  if (get_type(v) == TYPE_ARRAY) {
    Array list_ = decode_pointer(v)->as_array;
    if (i < list_.length && i >= 0) {
      return list_.data[i];
    } else {
      throwError("index out of bounds (%d) in %s", i, toString(v));
    }
  } else {
    throwError("expected list, got %s", decode_string(Type_of(list(2, unit(), v))));
  }
  return unit();
}

VALUE Array_push(VALUE args) {
  VALUE lst = index_(args, 1);
  VALUE value = index_(args, 2);
  VALUE result = lst;
  if (get_type(lst) == TYPE_ARRAY) {
    Array list_ = decode_pointer(lst)->as_array;
    list_.data = realloc(list_.data, sizeof(VALUE) * (list_.length + 1));

    list_.data[list_.length] = value;
    list_.length = list_.length + 1;

    HeapValue* ptr = (HeapValue*)malloc(sizeof(HeapValue));
    ptr->type = TYPE_ARRAY;
    ptr->as_array = list_;

    return unit();
  } else {
    throwError("expected list, got %s", decode_string(Type_of(list(2, unit(), lst))));
  }
  return result;
}

VALUE Array_length(VALUE args) {
  VALUE array = index_(args, 1);
  if (get_type(array) == TYPE_ARRAY) {
    return integer(decode_pointer(array)->as_array.length);
  } else {
    throwError("expected list, got %s", decode_string(Type_of(list(2, unit(), array))));
    return unit();
  }
}

VALUE property_(VALUE dict, char* key) {
  if (get_type(dict) == TYPE_DICT) {
    Dict heap = decode_pointer(dict)->as_dict;
    int i = hash((const char*) key, heap.length);
    struct Entry entry = heap.entries[i];
    if (strcmp(entry.key, key) == 0) {
      return entry.value;
    }
    while (entry.next != NULL) {
      if (strcmp(entry.key, key) == 0) {
        return entry.value;
      }
      entry = *entry.next;
    }
    throwError("structure has no property named %s in %s", key, toString(dict));
    return unit();
  } else {
    return unit();
  }
}

VALUE Array_has(VALUE args) {
  VALUE dict = index_(args, 1);
  VALUE key = index_(args, 2);

  const char* key_str = decode_string(key);

  if (get_type(dict) == TYPE_DICT) {
    Dict heap = decode_pointer(dict)->as_dict;
    int i = hash((const char*) key_str, heap.length);
    struct Entry entry = heap.entries[i];
    if (entry.key == NULL) {
      return boolean(0);
    }
    if (strcmp(entry.key, key_str) == 0) {
      return boolean(1);
    }
    while (entry.next != NULL) {
      if (strcmp(entry.key, key_str) == 0) {
        return boolean(1);
      }
      entry = *entry.next;
    }
    return boolean(0);
  } else {
    return boolean(0);
  }
}

VALUE Array_create(VALUE args) {
  VALUE size = index_(args, 1);
  VALUE value = index_(args, 2);

  int size_ = decode_integer(size);
  VALUE* data = malloc(sizeof(VALUE) * size_);
  for (int i = 0; i < size_; i++) {
    data[i] = value;
  }
  HeapValue* heap = malloc(sizeof(HeapValue));
  heap->type = TYPE_ARRAY;
  heap->as_array.length = size_;
  heap->as_array.data = data;
  return create_pointer(heap);
}