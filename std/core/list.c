#include <string.h>
#include <stdlib.h>
#include "list.h"
#include "eq.h"
#include "num.h"
#include "conversion.h"

Value* index_(Value *v, int i) {
  if (v->type == LIST) {
    int count = 0;
    while (v != NULL) {
      if (count == i) {
        return v->l.value;
      } else {
        count++;
        v = v->l.next;
      }
    }
    return NULL;
  } else {
    return NULL;
  }
}

Value* push(Value* lst, Value* value) {
  Value* result = lst;
  if (result == NULL) {
    result = list(value, NULL);
  } else {
    while (result->l.next != NULL) {
      result = result->l.next;
    }
    result->l.next = list(value, NULL);
  }
  return result;
}

Value* Array_next(Value*args) {
  Value* array = index_(args, 0);
  if (array->type == LIST) {
    return array->l.next;
  } else {
    return NULL;
  }
}

Value* Array_length(Value* args) {
  Value* array = index_(args, 0);
  if (array == NULL) {
    return integer(0);
  } else if (array->type == LIST) {
    if (eq(array, emptyList())->b) {
      return integer(0);
    } else {
      return add(integer(1), Array_length(list(array->l.next, NULL)));
    }
  } else {
    return NULL;
  }
}

Value* property_(Value* dict, char* key) {
  if (dict->type == STRUCT) {
    if (strcmp(dict->s.name, key) == 0) {
      return dict->s.value;
    } else {
      return property_(dict->s.next, key);
    }
  } else {
    return NULL;
  }
}

Value* Array_has(Value* args) {
  Value* dict = index_(args, 0);
  Value* key = index_(args, 1);
  if (dict->type == STRUCT) {
    if (strcmp(dict->s.name, toString(key)) == 0) {
      return boolean(1);
    } else if (dict->s.next != NULL) {
      return Array_has(list(dict->s.next, list(key, NULL)));
    } else {
      return boolean(0);
    }
  } else {
    return boolean(0);
  }
}

Value* IO_clone(Value *args)
{
  Value *v = index_(args, 0);
  Value *c = (Value*) malloc(sizeof(Value));
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
    c->l.value = IO_clone(list(v->l.value, NULL));
    if (v!= NULL && v->l.next != NULL) c->l.next = IO_clone(list(v->l.next, NULL));
    break;
  case STRUCT:
    c->s.name = v->s.name;
    c->s.value = IO_clone(list(v->s.value, NULL));
    if (v != NULL && v->s.next != NULL) c->s.next = IO_clone(list(v->s.next, NULL));
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
    return list(IO_clone(list(value, NULL)), Array_create(list(sub(size, integer(1)), list(value, NULL))));
  }
}