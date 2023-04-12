#include <stdio.h>
#include <stdlib.h>
#include "value.h"
#include <string.h>
#include "garbage/tgc.h"
#include "garbage.h"
#include "io.h"
#include <stdarg.h>

Value* integer(int value) {
  Value* v = (Value*) tgc_alloc(gc(), sizeof(Value));
  v->type = INT;
  v->i = value;
  return v;
}

Value* floating(float value) {
  Value* v = (Value*) tgc_alloc(gc(), sizeof(Value));
  v->type = FLOAT;
  v->f = value;
  return v;
}

Value* character(char value) {
  Value* v = (Value*) tgc_alloc(gc(), sizeof(Value));
  v->type = CHAR;
  v->c = value;
  return v;
}

Value* list(int length, ...) {
  va_list args;
  va_start(args, length);
  Value** v = (Value**) tgc_alloc(gc(), sizeof(Value*) * length);
  int i = 0;
  
  while (i < length) {
    v[i] = va_arg(args, Value*);
    i += 1;
  }

  va_end(args);

  Value* list = (Value*) tgc_alloc(gc(), sizeof(Value));
  list->type = LIST;
  list->l.value = v;
  list->l.length = i;
  return list;
}

Value* structure(int length, ...) {
  va_list args;
  va_start(args, length);
  
  struct Element** v = (Value*) tgc_alloc(gc(), sizeof(struct Element*) * length);

  int i = 0;
  while (i < length) {
    v[i] = va_arg(args, struct Element*);
    i += 1;
  }

  va_end(args);

  Value* dict = (Value*) tgc_alloc(gc(), sizeof(Value));
  dict->type = STRUCT;
  dict->s.length = i;
  dict->s.elements = v;
  return dict;
}

struct Element* element(char* name, Value* value) {
  struct Element* e = (struct Element*) tgc_alloc(gc(), sizeof(struct Element));
  e->name = name;
  e->value = value;
  return e;
}

Value* unit() {
  Value* v = (Value*) tgc_alloc(gc(), sizeof(Value));
  v->type = UNIT;
  return v;
}

Value* boolean(int value) {
  Value* v = (Value*) tgc_alloc(gc(), sizeof(Value));
  v->type = BOOL;
  v->b = value;
  return v;
}

Value* makeLambda(Value* (*f)(Value*)) {
  Value* v = (Value*) tgc_alloc(gc(), sizeof(Value));
  v->type = LAMBDA;
  v->$$fun = f;
  return v;
}

Value* emptyList() {
  Value* v = (Value*) tgc_alloc(gc(), sizeof(Value));
  v->type = LIST;
  v->l.length = 0;
  return v;
}

Value* string(char* value) {
  Value** container = (Value**) tgc_alloc(gc(), sizeof(Value*) * strlen(value));
  for (int i = 0; i < strlen(value); i++) {
    container[i] = character(value[i]);
  }
  Value* v = (Value*) tgc_alloc(gc(), sizeof(Value));
  v->type = LIST;
  v->l.length = strlen(value);
  v->l.value = container;
  return v;
}