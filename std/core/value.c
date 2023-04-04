#include <stdio.h>
#include <stdlib.h>
#include "value.h"
#include <string.h>

Value* integer(int value) {
  Value* v = malloc(sizeof(Value));
  v->type = INT;
  v->i = value;
  return v;
}

Value* floating(float value) {
  Value* v = malloc(sizeof(Value));
  v->type = FLOAT;
  v->f = value;
  return v;
}

Value* character(char value) {
  Value* v = malloc(sizeof(Value));
  v->type = CHAR;
  v->c = value;
  return v;
}

Value* list(Value* value, Value* next) {
  Value* v = malloc(sizeof(Value));
  v->type = LIST;
  v->l.value = value;
  v->l.next = next;
  return v;
}

Value* structure(char* name, Value* value, Value* next) {
  Value* v = malloc(sizeof(Value));
  v->type = STRUCT;
  v->s.name = name;
  v->s.value = value;
  v->s.next = next;
  return v;
}

Value* unit() {
  Value* v = malloc(sizeof(Value));
  v->type = UNIT;
  return v;
}

Value* boolean(int value) {
  Value* v = malloc(sizeof(Value));
  v->type = BOOL;
  v->b = value;
  return v;
}

Value* makeLambda(Value* (*f)(Value*)) {
  Value* v = malloc(sizeof(Value));
  v->type = LAMBDA;
  v->$$fun = f;
  return v;
}

Value* emptyList() {
  Value* v = malloc(sizeof(Value));
  v->type = LIST;
  return v;
}

Value* string(char* value) {
  Value* v = malloc(sizeof(Value));
  Value* current = v;
  for (int i = 0; i < strlen(value); i++) {
    current->type = LIST;
    current->l.value = character(value[i]);
    if (i != strlen(value) - 1) {
      current->l.next = malloc(sizeof(Value));
      current = current->l.next;
    } else {
      current->l.next = NULL;
    }
  }
  return v;
}