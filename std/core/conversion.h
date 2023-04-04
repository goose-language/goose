#ifndef CONVERSION_H
#define CONVERSION_H
#include "value.h"

typedef struct _Converted {
  int length;
  void** values;
} Converted;

int toInt(Value* value);
float toFloat(Value* value);
char toChar(Value* value);
int toBool(Value* value);
Converted toList(Value* value);
char* toString(Value* value);

#endif