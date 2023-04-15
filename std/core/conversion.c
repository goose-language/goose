#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "conversion.h"
#include "garbage/tgc.h"
#include "garbage.h"
#include "value.h"
#include "list.h"
#include "io.h"
#include "value/nanbox.h"

HeapValue* decode_pointer(nanbox_t value) {
  return nanbox_to_pointer(value);
}

int32_t decode_integer(nanbox_t v) {
  return nanbox_to_int(v);
}

double decode_floating(nanbox_t v) {
  return nanbox_to_double(v);
}

char decode_character(nanbox_t v) {
  return decode_pointer(v)->as_char;
}

int decode_boolean(nanbox_t v) {
  return nanbox_to_boolean(v);
}

char* decode_string(nanbox_t v) {
  HeapValue* heap = decode_pointer(v);
  char* string = (char*) malloc(sizeof(char) * (heap->as_array.length + 1));

  for (int i = 0; i < heap->as_array.length; i++) {
    string[i] = decode_character(heap->as_array.data[i]);
  }

  string[heap->as_array.length] = '\0';

  return string;
}
result decode_lambda(nanbox_t v) {
  HeapValue* heap = decode_pointer(v);
  return heap->as_lambda.f;
}