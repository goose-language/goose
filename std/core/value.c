#include <stdio.h>
#include <stdlib.h>
#include "value.h"
#include <string.h>

#include "conversion.h"

#include "io.h"
#include <stdarg.h>

VALUE integer(int32_t value) {
  return SIGNATURE_INTEGER | (uint32_t) value;
}

VALUE create_pointer(HeapValue* value) {
  return SIGNATURE_POINTER | (uint64_t) value;
}

VALUE floating(double value) {
  return *(VALUE*)(&value);
}

VALUE character(char value) {
  return SIGNATURE_CHAR | (uint32_t) value;
}

VALUE list(int count, ...) {
  HeapValue* ptr = (HeapValue*)malloc(sizeof(HeapValue));
  ptr->type = TYPE_ARRAY;

  size_t value_buffer_size = sizeof(VALUE) * count;
  ptr->as_array.data = (VALUE*) malloc(value_buffer_size);

  va_list args;
  va_start(args, count);
  

  for (int i = 0; i < count; i++) {
    ptr->as_array.data[i] = va_arg(args, VALUE);
  }

  va_end(args);

  ptr->as_array.length = count;

  return create_pointer(ptr);
}

VALUE structure(int length, ...) {
  HeapValue* dict = (HeapValue*) malloc(sizeof(HeapValue));
  dict->type = TYPE_DICT;
  dict->as_dict.length = length;

  dict->as_dict.keys = malloc(sizeof(char*) * length);
  dict->as_dict.values = malloc(sizeof(VALUE) * length);

  va_list args;
  va_start(args, length);

  for (int i = 0; i < length; i++) {
    dict->as_dict.keys[i] = va_arg(args, char*);
    dict->as_dict.values[i] = va_arg(args, VALUE);
  }

  va_end(args);

  return create_pointer(dict);
}

VALUE unit() {
  return kNull;
}

VALUE boolean(int value) {
  return value ? kTrue : kFalse;
}

VALUE makeLambda(VALUE (*f)(VALUE)) {
  HeapValue* lambda = (HeapValue*) malloc(sizeof(HeapValue));
  lambda->type = TYPE_LAMBDA;
  lambda->as_lambda.f = f;
  return create_pointer(lambda);
}

VALUE emptyList() {
  return list(0);
}

VALUE string(char* value) {
  HeapValue* string = (HeapValue*) malloc(sizeof(HeapValue));
  string->type = TYPE_ARRAY;
  string->as_array.length = strlen(value);
  VALUE* data = malloc(sizeof(VALUE) * strlen(value));
  for (int i = 0; i < strlen(value); i++) {
    data[i] = character(value[i]);
  }

  string->as_array.data = data;

  return create_pointer(string);
}

ValueType get_type(VALUE value) {
  uint64_t signature = value & MASK_SIGNATURE;
  if ((~value & MASK_EXPONENT) != 0) return TYPE_FLOAT;

  // Check for encoded pointer
  if (signature == SIGNATURE_POINTER) {
    HeapValue* ptr = decode_pointer(value);
    return ptr->type;
  }

  // Short encoded types
  switch (signature) {
    case SIGNATURE_NAN:     return TYPE_FLOAT;
    case SIGNATURE_FALSE:
    case SIGNATURE_TRUE:    return TYPE_BOOL;
    case SIGNATURE_NULL:    return TYPE_NULL;
    case SIGNATURE_INTEGER: return TYPE_INTEGER;
    case SIGNATURE_CHAR:    return TYPE_CHAR;
  }

  return TYPE_NULL;
}