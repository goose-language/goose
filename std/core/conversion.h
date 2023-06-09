#ifndef CONVERSION_H
#define CONVERSION_H
#include "value.h"


typedef struct _Converted {
  int length;
  void** values;
} Converted;

typedef VALUE (*result)(VALUE args);

int32_t decode_integer(VALUE value);
double decode_floating(VALUE value);
char decode_character(VALUE value);
int decode_boolean(VALUE value);
Converted toList(VALUE* value);
char* decode_string(VALUE value);
HeapValue* decode_pointer(VALUE value);
result decode_lambda(VALUE value);

void update_index(VALUE array, int index, VALUE value);
void update_property(VALUE dict, char* key, VALUE value);

#endif