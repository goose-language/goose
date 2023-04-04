#include "value.h"
#include "type.h"
#include "list.h"
#include "num.h"
#include <stdlib.h>

Value* Type_of(Value* args) {
  Value* value = index_(args, 0);
  if (value == NULL) return string("unknown");
  switch (value->type) {
    case LAMBDA: return string("lambda");
    case INT: return string("int");
    case FLOAT: return string("float");
    case CHAR: return string("char");
    case LIST: 
      if (value->l.value != NULL) {
        return add(string("list<"), add(Type_of(list(value->l.value, NULL)), string(">")));
      }
      return string("list");
    case UNIT: return string("unit");
    case BOOL: return string("bool");
    case STRUCT: return string("struct");
    default: return string("unknown");
  }
}