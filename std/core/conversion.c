#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "conversion.h"
#include "value.h"
#include "list.h"
#include "io.h"
int toInt(Value* value) {
    return value->i;
}

float toFloat(Value* value) {
    return value->f;
}

char toChar(Value* value) {
    return value->c;
}

int toBool(Value* value) {
    return value->b;
}

char* toString(Value* value) {
  int len = Array_length(list(value, NULL))->i;
  char* result = malloc(sizeof(char) * len);
  int i = 0;
  while (value != NULL && value->l.value != NULL) {
    result[i] = value->l.value->c;
    value = value->l.next;
    i++;
  }
  return result;
}