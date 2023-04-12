#include <string.h>
#include <stdlib.h>
#include "list.h"
#include "eq.h"
#include "num.h"
#include "conversion.h"
#include "type.h"
#include "error.h"
#include "garbage/tgc.h"
#include "garbage.h"

Value* index_(Value *v, int i) {
  if (v->type == LIST) {
    if (v->l.length == 0) {
      throwError("index out of bounds");
    }
    if (v->l.length < i) {
      throwError("index out of bounds");
    }
    return v->l.value[i];
  } else {
    printf("Expected list, got %s", Type_of(list(1, v, NULL)));
    throwError("expected list");
  }
}

Value* push(Value* lst, Value* value) {
  Value* result = lst;
  if (result == NULL) {
    return list(1, value);
  } else if (result->type == LIST) {
    result->l.value = (Value**) tgc_realloc(gc(), result->l.value, sizeof(Value*) * (result->l.length + 1));
    result->l.value[result->l.length] = value;
    result->l.length++;
  } else {
    printf("Expected list, got %s", Type_of(list(1, result, NULL)));
    throwError("expected list");
  }
  return result;
}

Value* Array_length(Value* args) {
  Value* array = index_(args, 0);
  if (array == NULL) {
    return integer(0);
  } else if (array->type == LIST) {
    return integer(array->l.length);
  } else {
    printf("Expected list, got %s", Type_of(list(1, array, NULL)));
    throwError("expected list");
  }
}

Value* property_(Value* dict, char* key) {
  if (dict == NULL) throwError("cannot access structure property of NULL");
  if (dict->type == STRUCT) {
    for (int i = 0; i < dict->s.length; i++) {
      if (strcmp(dict->s.elements[i]->name, key) == 0) {
        return dict->s.elements[i]->value;
      }
    }
    printf("Structure has no property named %s", key);
    throwError("structure has no property");
  } else {
    return NULL;
  }
}

Value* Array_has(Value* args) {
  Value* dict = index_(args, 0);
  if (dict == NULL) throwError("Array::has: expected 2 arguments, got 0");

  Value* key = index_(args, 1);
  if (key == NULL) throwError("Array::has: expected 2 arguments, got 1");

  if (dict->type == STRUCT) {
    for (int i = 0; i < dict->s.length; i++) {
      if (strcmp(dict->s.elements[i]->name, toString(key)) == 0) {
        return boolean(1);
      }
    }
    return boolean(0);
  } else {
    return boolean(0);
  }
}

Value* IO_clone(Value *args)
{
  Value *v = index_(args, 0);
  
  Value *c = (Value*) tgc_alloc(gc(), sizeof(Value));
  if (v == NULL)
  {
    return NULL;
  }
  c->type = v->type;
  switch (v->type)
  {
  case INT:
    c->i = v->i;
    break;
  case FLOAT:
    c->f = v->f;
    break;
  case CHAR:
    c->c = v->c;
    break;
  case LIST:
    c->l.value = (Value**) tgc_alloc(gc(), sizeof(Value*) * v->l.length);
    c->l.length = v->l.length;
    for (int i = 0; i < v->l.length; i++) {
      c->l.value[i] = IO_clone(list(1, v->l.value[i], NULL));
    }
    break;
  case STRUCT:
    c->s.elements = (struct Element**) tgc_alloc(gc(), sizeof(struct Element*) * v->s.length);
    c->s.length = v->s.length;
    for (int i = 0; i < v->s.length; i++) {
      c->s.elements[i] = element(v->s.elements[i]->name, IO_clone(list(1, v->s.elements[i]->value, NULL)));
    }
    break;
  case UNIT:
    break;
  case BOOL:
    c->b = v->b;
    break;
  case LAMBDA:
    c->$$fun = v->$$fun;
    break;
  }
  return c;
}

Value* Array_create(Value* args) {
  Value* size = index_(args, 0);
  Value* value = index_(args, 1);
  if (eq(size, integer(0))->b) {
    return NULL;
  } else {
    Value** array = (Value**) tgc_alloc(gc(), sizeof(Value*) * toInt(size));
    for (int i = 0; i < toInt(size); i++) {
      array[i] = IO_clone(list(1, value, NULL));
    }
    Value* result = tgc_alloc(gc(), sizeof(Value));
    result->type = LIST;
    result->l.value = array;
    result->l.length = toInt(size);
    return result;
  }
}