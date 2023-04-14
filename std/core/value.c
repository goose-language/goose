#include <stdio.h>
#include <stdlib.h>
#include "value.h"
#include <string.h>
#include "garbage/tgc.h"
#include "conversion.h"
#include "garbage.h"
#include "io.h"
#include <stdarg.h>
#include "value/nanbox.h"

nanbox_t integer(int32_t value) {
  return nanbox_from_int(value);
}

nanbox_t create_pointer(HeapValue* value) {
  return nanbox_from_pointer(value);
}

nanbox_t floating(double value) {
  return nanbox_from_double(value);
}

nanbox_t character(char value) {
  HeapValue* ptr = (HeapValue*)malloc(sizeof(HeapValue));
  ptr->type = TYPE_CHAR;
  ptr->as_char = value;
  return create_pointer(ptr);
}

nanbox_t list(int count, ...) {
  HeapValue* ptr = (HeapValue*)malloc(sizeof(HeapValue));
  ptr->type = TYPE_ARRAY;

  size_t value_buffer_size = sizeof(nanbox_t) * count;
  ptr->as_array.data = (nanbox_t*) malloc(value_buffer_size);

  va_list args;
  va_start(args, count);
  

  for (int i = 0; i < count; i++) {
    ptr->as_array.data[i] = va_arg(args, nanbox_t);
  }

  va_end(args);

  ptr->as_array.length = count;

  return create_pointer(ptr);
}

nanbox_t create_mutable(nanbox_t value) {
  HeapValue* ptr = (HeapValue*)malloc(sizeof(HeapValue));
  nanbox_t* ptr_ = (nanbox_t*)malloc(sizeof(nanbox_t));
  *ptr_ = value;
  ptr->type = TYPE_MUTABLE;
  ptr->as_pointer = ptr_;
  return create_pointer(ptr);
}

inline nanbox_t get_mutable(nanbox_t value) {
  return *(((HeapValue*)nanbox_to_pointer(value))->as_pointer);
}

nanbox_t structure(int length, ...) {
  HeapValue* dict = (HeapValue*) malloc(sizeof(HeapValue));
  dict->type = TYPE_DICT;
  dict->as_dict.length = length;

  dict->as_dict.keys = malloc(sizeof(char*) * length);
  dict->as_dict.values = malloc(sizeof(nanbox_t) * length);

  va_list args;
  va_start(args, length);

  for (int i = 0; i < length; i++) {
    dict->as_dict.keys[i] = va_arg(args, char*);
    dict->as_dict.values[i] = va_arg(args, nanbox_t);
  }

  va_end(args);

  return create_pointer(dict);
}

nanbox_t* unbox_mutable(nanbox_t value) {
  return ((HeapValue*)nanbox_to_pointer(value))->as_pointer;
}

nanbox_t unit() {
  return nanbox_null();
}

nanbox_t boolean(int value) {
  return value ? nanbox_true() : nanbox_false();
}

nanbox_t makeLambda(nanbox_t (*f)(nanbox_t)) {
  HeapValue* lambda = (HeapValue*) malloc(sizeof(HeapValue));
  lambda->type = TYPE_LAMBDA;
  lambda->as_lambda.f = f;
  return create_pointer(lambda);
}

nanbox_t emptyList() {
  HeapValue* list_ = (HeapValue*) malloc(sizeof(HeapValue));
  list_->type = TYPE_ARRAY;
  list_->as_array.length = 0;
  list_->as_array.data = NULL;
  return create_pointer(list_);
}

nanbox_t string(char* value) {
  HeapValue* string = (HeapValue*) malloc(sizeof(HeapValue));
  string->type = TYPE_ARRAY;
  string->as_array.length = strlen(value);
  nanbox_t* data = malloc(sizeof(nanbox_t) * strlen(value));
  for (int i = 0; i < strlen(value); i++) {
    data[i] = character(value[i]);
  }

  string->as_array.data = data;

  return create_pointer(string);
}

ValueType get_type(nanbox_t value) {
  if (nanbox_is_pointer(value)) {
    return ((HeapValue*) nanbox_to_pointer(value))->type;
  }

  if (nanbox_is_int(value)) return TYPE_INTEGER;
  if (nanbox_is_double(value)) return TYPE_FLOAT;
  if (nanbox_is_boolean(value)) return TYPE_BOOL;
  if (nanbox_is_null(value)) return TYPE_NULL;
  
  return TYPE_NULL;
}