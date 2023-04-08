#include <stdlib.h>
#include <string.h>
#include "num.h"
#include "error.h"
#include "type.h"

Value *add(Value *a, Value *b) {
  if (a->type == INT && b->type == INT) {
    return integer(a->i + b->i);
  } else if (a->type == FLOAT && b->type == FLOAT) {
    return floating(a->f + b->f);
  } else if (a->type == INT && b->type == FLOAT) {
    return floating(a->i + b->f);
  } else if (a->type == FLOAT && b->type == INT) {
    return floating(a->f + b->i);
  } else if (a->type == LIST && b->type == LIST) {
    Value *a0 = (Value*) malloc(sizeof(Value));
    if (a->l.value == NULL) {
      memcpy(a0, b, sizeof(Value));
      return a0;
    } else {
      memcpy(a0, a, sizeof(Value));
      Value* tmp = a0;
      while (tmp->l.next != NULL)
      {
        tmp = tmp->l.next;
      }
      tmp->l.next = b;
    }

    return a0;
  } else if (a->type == UNIT && b->type == UNIT) {
    return unit();
  } else {
    throwError("Cannot add values of different types");
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