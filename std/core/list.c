#include <string.h>
#include <stdlib.h>
#include "list.h"
#include "eq.h"
#include "num.h"
#include "conversion.h"
#include "type.h"
#include "error.h"
#include "garbage/tgc.h"
#include "garbage.h"
#include "io.h"

nanbox_t index_(nanbox_t v, int i) {
  if (get_type(v) == TYPE_ARRAY) {
    Array list_ = decode_pointer(v)->as_array;
    if (i < list_.length && i >= 0) {
      return list_.data[i];
    } else {
      throwError("index out of bounds (%d) in %s", i, toString(v));
    }
  } else if (get_type(v) == TYPE_MUTABLE) {
    Array list_ = decode_pointer(get_mutable(v))->as_array;
    if (i < list_.length && i >= 0) {
      return create_mutable(list_.data[i]);
    } else {
      throwError("index out of bounds (%d) in %s", i, toString(v));
    }
  } else {
    throwError("expected list, got %s", decode_string(Type_of(list(1, v))));
  }
}

nanbox_t Array_push(nanbox_t args) {
  nanbox_t lst = index_(args, 0);
  nanbox_t value = index_(args, 1);
  nanbox_t result = lst;
  if (get_type(lst) == TYPE_MUTABLE) {
    nanbox_t* mutable = unbox_mutable(lst);
    Array list_ = decode_pointer(*mutable)->as_array;
    list_.data = realloc(list_.data, sizeof(nanbox_t) * (list_.length + 1));

    list_.data[list_.length] = value;
    list_.length = list_.length + 1;

    HeapValue* ptr = (HeapValue*)malloc(sizeof(HeapValue));
    ptr->type = TYPE_ARRAY;
    ptr->as_array = list_;

    *mutable = create_pointer(ptr);
    return unit();
  } else {
    throwError("expected mutable list, got %s", decode_string(Type_of(list(1, lst))));
  }
  return result;
}

nanbox_t Array_length(nanbox_t args) {
  nanbox_t array = index_(args, 0);
  if (get_type(array) == TYPE_ARRAY) {
    return integer(decode_pointer(array)->as_array.length);
  } else if (get_type(array) == TYPE_MUTABLE) {
    return integer(decode_pointer(get_mutable(array))->as_array.length);
  } else {
    throwError("expected list, got %s", decode_string(Type_of(list(1, array))));
  }
}

nanbox_t property_(nanbox_t dict, char* key) {
  if (get_type(dict) == TYPE_DICT) {
    Dict heap = decode_pointer(dict)->as_dict;
    for (int i = 0; i < heap.length; i++) {
      if (strcmp(heap.keys[i], key) == 0) {
        return heap.values[i];
      }
    }
    throwError("structure has no property named %s in %s", key, toString(dict));
  } else {
    return unit();
  }
}

nanbox_t Array_has(nanbox_t args) {
  nanbox_t dict = index_(args, 0);
  nanbox_t key = index_(args, 1);

  const char* key_str = decode_string(key);

  if (get_type(dict) == TYPE_DICT) {
    Dict heap = decode_pointer(dict)->as_dict;
    for (int i = 0; i < heap.length; i++) {
      if (strcmp(heap.keys[i], key_str) == 0) {
        return boolean(1);
      }
    }
    return boolean(0);
  } else {
    return boolean(0);
  }
}

nanbox_t IO_clone(nanbox_t args)
{
  nanbox_t item = index_(args, 0);
  nanbox_t result;

  switch (get_type(item)) {
    case TYPE_BOOL:
    case TYPE_CHAR:
    case TYPE_FLOAT:
    case TYPE_INTEGER:
      result = item;
      break;
    case TYPE_ARRAY: {
      HeapValue* heap = malloc(sizeof(HeapValue));
      heap->type = TYPE_ARRAY;
      heap->as_array.length = decode_pointer(item)->as_array.length;
      heap->as_array.data = malloc(sizeof(nanbox_t) * heap->as_array.length);
      for (int i = 0; i < heap->as_array.length; i++) {
        heap->as_array.data[i] = IO_clone(list(1, decode_pointer(item)->as_array.data[i]));
      }
      return create_pointer(heap);
    }
    case TYPE_DICT: {
      HeapValue* heap = malloc(sizeof(HeapValue));
      heap->type = TYPE_DICT;
      HeapValue* item_ = decode_pointer(item);
      heap->as_dict.length = item_->as_dict.length;
      heap->as_dict.values = malloc(sizeof(nanbox_t) * heap->as_dict.length);
      heap->as_dict.keys = malloc(sizeof(char*) * heap->as_dict.length);
      for (int i = 0; i < heap->as_dict.length; i++) {
        heap->as_dict.values[i] = IO_clone(list(1, item_->as_dict.values[i]));
        heap->as_dict.keys[i] = malloc(sizeof(char) * strlen(item_->as_dict.keys[i]));
        strcpy(heap->as_dict.keys[i], item_->as_dict.keys[i]);
      }
      return create_pointer(heap);
    }
    case TYPE_NULL:
      result = unit();
      break;
    case TYPE_LAMBDA: {
      HeapValue* heap = malloc(sizeof(HeapValue));
      heap->type = TYPE_LAMBDA;
      heap->as_lambda = decode_pointer(item)->as_lambda;
      return create_pointer(heap);
    }
    default: 
      throwError("IO::clone: cannot clone unknown type, got %s", decode_string(Type_of(list(1, (nanbox_t[1]) { item }))));
  }
  return result;
}

nanbox_t Array_create(nanbox_t args) {
  nanbox_t size = index_(args, 0);
  nanbox_t value = index_(args, 1);

  int size_ = decode_integer(size);
  nanbox_t* data = malloc(sizeof(nanbox_t) * size_);
  for (int i = 0; i < size_; i++) {
    data[i] = IO_clone(list(1, value));
  }
  HeapValue* heap = malloc(sizeof(HeapValue));
  heap->type = TYPE_ARRAY;
  heap->as_array.length = size_;
  heap->as_array.data = data;
  return create_pointer(heap);
}