#include <stdlib.h>
#include <string.h>
#include "num.h"

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
    Value *a0 = malloc(sizeof(Value));
    memcpy(a0, a, sizeof(Value));
    Value* tmp = a0;
    while (tmp->l.next != NULL)
    {
      tmp = tmp->l.next;
    }
    tmp->l.next = b;

    return a0;
  } else if (a->type == UNIT && b->type == UNIT) {
    return unit();
  } else {
    return NULL;
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
  } else {
    return NULL;
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
  } else {
    return NULL;
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
  } else {
    return NULL;
  }
}