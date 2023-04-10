#include <stdlib.h>
#include <string.h>
#include "num.h"
#include "error.h"
#include "type.h"
#include "conversion.h"

Value *add(Value *a, Value *b) {
  if (a == NULL) {
    return b;
  } else if (b == NULL) {
    return a;
  } else if (a == NULL && b == NULL) {
    throwError("Tried to add NULL values");
  } else if (a->type == INT && b->type == INT) {
    return integer(a->i + b->i);
  } else if (a->type == FLOAT && b->type == FLOAT) {
    return floating(a->f + b->f);
  } else if (a->type == INT && b->type == FLOAT) {
    return floating(a->i + b->f);
  } else if (a->type == FLOAT && b->type == INT) {
    return floating(a->f + b->i);
  } else if (a->type == LIST && b->type == LIST) {
    Value *a0 = (Value*) malloc(sizeof(Value));
    a0->type = LIST;
    a0->l.value = a->l.value;
    a0->l.next = add(a->l.next, b);

    return a0;
  } else if (a->type == UNIT && b->type == UNIT) {
    return unit();
  } else {
    printf("Cannot add values of different types (between %s and %s)\n", toString(Type_of(list(a, NULL))), toString(Type_of(list(b, NULL))));
    throwError("Tried to add values of different types");
  }  
}

Value* sub(Value* a, Value* b) {
  if (a->type == INT && b->type == INT) {
    return integer(a->i - b->i);
  } else if (a->type == FLOAT && b->type == FLOAT) {
    return floating(a->f - b->f);
  } else if (a->type == INT && b->type == FLOAT) {
    return floating(a->i - b->f);
  } else if (a->type == FLOAT && b->type == INT) {
    return floating(a->f - b->i);
  } else if (a->type == UNIT && b->type == UNIT) {
    return unit();
  } else if (a == NULL || b == NULL) {
    throwError("Cannot subtract NULL");
  } else {
    return a;
  }
}

Value* mul(Value* a, Value* b) {
  if (a->type == INT && b->type == INT) {
    return integer(a->i * b->i);
  } else if (a->type == FLOAT && b->type == FLOAT) {
    return floating(a->f * b->f);
  } else if (a->type == INT && b->type == FLOAT) {
    return floating(a->i * b->f);
  } else if (a->type == FLOAT && b->type == INT) {
    return floating(a->f * b->i);
  } else if (a->type == UNIT && b->type == UNIT) {
    return unit();
  } else if (a == NULL || b == NULL) {
    throwError("Cannot multiply NULL");
  } else {
    return a;
  }
}

Value* div_(Value* a, Value* b) {
  if (a->type == INT && b->type == INT) {
    return integer(a->i / b->i);
  } else if (a->type == FLOAT && b->type == FLOAT) {
    return floating(a->f / b->f);
  } else if (a->type == INT && b->type == FLOAT) {
    return floating(a->i / b->f);
  } else if (a->type == FLOAT && b->type == INT) {
    return floating(a->f / b->i);
  } else if (a->type == UNIT && b->type == UNIT) {
    return unit();
  } else if (a == NULL || b == NULL) {
    throwError("Cannot divide NULL");
  } else {
    return a;
  }
}