#ifndef CONVERSION_H
#define CONVERSION_H
#include "value.h"
#include "value/nanbox.h"

typedef struct _Converted {
  int length;
  void** values;
} Converted;

typedef nanbox_t (*result)(nanbox_t args);

int32_t decode_integer(nanbox_t value);
double decode_floating(nanbox_t value);
char decode_character(nanbox_t value);
int decode_boolean(nanbox_t value);
Converted toList(nanbox_t* value);
char* decode_string(nanbox_t value);
HeapValue* decode_pointer(nanbox_t value);
result decode_lambda(nanbox_t value);

#endif