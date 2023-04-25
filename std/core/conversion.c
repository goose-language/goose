#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "conversion.h"
#include "value.h"
#include "list.h"
#include "io.h"

HeapValue* decode_pointer(VALUE value) {
  return (HeapValue*)(value & MASK_PAYLOAD_PTR);
}

int32_t decode_integer(VALUE v) {
  return v & MASK_PAYLOAD_INT;
}

double decode_floating(VALUE v) {
  return *(double*)(&v);
}

char decode_character(VALUE v) {
  return v & MASK_PAYLOAD_INT;
}

int decode_boolean(VALUE v) {
  return v == kTrue;
}

char* decode_string(VALUE v) {
  HeapValue* heap = decode_pointer(v);
  char* string = (char*) malloc(sizeof(char) * (heap->as_array.length + 1));

  for (int i = 0; i < heap->as_array.length; i++) {
    string[i] = decode_character(heap->as_array.data[i]);
  }

  string[heap->as_array.length] = '\0';

  return string;
}
result decode_lambda(VALUE v) {
  HeapValue* heap = decode_pointer(v);
  return heap->as_lambda.f;
}