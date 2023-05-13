#include <stdio.h>
#include <stdlib.h>
#include "value.h"
#include <string.h>

#define LOAD_FACTOR 1.25

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

uint32_t hash(const char* str, int CAPACITY) {
  unsigned long i = 0;
  for (int j = 0; str[j]; j++)
      i += str[j];

  return i % CAPACITY;
}

void insert(struct Entry* node, const char* key, VALUE value) {
  struct Entry *current = node;
  int i = 0;
  while (current->next != NULL) {
    current = current->next;
    i++;
  }

  current->key = key;
  current->value = value;
  current->next = (struct Entry*) malloc(sizeof(struct Entry));
  current->next->next = NULL;
}

VALUE structure(int length, ...) {
  HeapValue* dict = (HeapValue*) malloc(sizeof(HeapValue));
  dict->type = TYPE_DICT;

  int size = 4;
  dict->as_dict.length = size;
  dict->as_dict.count = length;

  va_list args;
  va_start(args, length);

  dict->as_dict.entries = (struct Entry*) malloc(sizeof(struct Entry) * size);
  struct Entry* entries = dict->as_dict.entries;

  for (int i = 0; i < length; i++) {
    const char* key = va_arg(args, const char*);
    VALUE value = va_arg(args, VALUE);

    uint32_t index = hash(key, size);

    insert(&entries[index], key, value);
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

VALUE intern(char* name, int length, ...) {
  HeapValue* intern = (HeapValue*) malloc(sizeof(HeapValue));
  intern->type = TYPE_INTERN;
  intern->as_intern.name = name;
  intern->as_intern.length = length;

  va_list args;
  va_start(args, length);

  intern->as_intern.data = malloc(sizeof(VALUE) * length);
  for (int i = 0; i < length; i++) {
    intern->as_intern.data[i] = va_arg(args, VALUE);
  }

  va_end(args);

  return create_pointer(intern);
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