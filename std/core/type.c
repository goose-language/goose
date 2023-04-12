#include "value.h"
#include "type.h"
#include "list.h"
#include "num.h"
#include <stdlib.h>
#include <stdio.h>
#include "io.h"
#include <string.h>
#include "conversion.h"

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
        return add(string("list<"), add(Type_of(list(1, value->l.value)), string(">")));
      }
      return string("list");
    case UNIT: return string("nil");
    case BOOL: return string("bool");
    case STRUCT: {
      Value *acc = string("structure<");
      Value *current = value;
      for (int i = 0; i < value->s.length; i++) {
        acc = add(acc, string(value->s.elements[i]->name));
        acc = add(acc, string(": "));
        acc = add(acc, Type_of(list(1, value->s.elements[i]->value)));
        if (i < value->s.length - 1) {
          acc = add(acc, string(", "));
        }
      }
      acc = add(acc, string(">"));
      return acc;
    }
    default: return string("unknown");
  }
}

char* toString_(Value* value) {
  if (value == NULL) return "unknown";
  switch (value->type) {
    case LAMBDA: return "lambda";
    case INT: {
      int length = snprintf( NULL, 0, "%d", value->i);
      char* str = malloc( length + 1 );
      snprintf( str, length + 1, "%d",value->i);
      return str;
    }
    case FLOAT: {
      char *buffer = malloc(16);
      sprintf(buffer, "%f", value->f);
      return buffer;
    };
    case CHAR: return "char";
    case LIST: {
      if (value->l.value[0]->type == CHAR) {
        char* buffer = malloc(sizeof(char) * value->l.length);
        for (int i = 0; i < value->l.length; i++) {
          buffer[i] = value->l.value[i]->c;
        }
        return buffer;
      } else {
        char* buffer = malloc(sizeof(char) * 8);
        buffer[0] = '[';
        Value* current = value;
        for (int i = 0; i < value->l.length; i++) {
          strcat(buffer, toString_(current->l.value[i]));
          if (i < value->l.length - 1) {
            strcat(buffer, ", ");
          }
        }
        strcat(buffer, "]");
        return buffer;
      }
    }
    case UNIT: return "nil";
    case BOOL: return "bool";
    case STRUCT: {
      if (Array_has(list(2, value, string("$$enum")))->b && property_(value, "$$enum")->b) {
        char* buffer = malloc(sizeof(char) * 16);
        strcat(buffer, toString(property_(value, "type")));
        strcat(buffer, "(");
        Value* varArgs = getVariantArguments(value);
        for (int i = 0; i < varArgs->l.length; i++) {
          strcat(buffer, toString_(varArgs->l.value[i]));
          if (i < varArgs->l.length - 1) {
            strcat(buffer, ", ");
          }
        }
        strcat(buffer, ")");
        return buffer;
      } else {
        if (value->s.length == 0) return "{}";
        char* buffer = malloc(sizeof(char) * 8);
        strcat(buffer, "{ ");
        for (int i = 0; i < value->s.length; i++) {
          strcat(buffer, value->s.elements[i]->name);
          strcat(buffer, ": ");
          strcat(buffer, toString_(value->s.elements[i]->value));
          if (i < value->s.length - 1) {
            strcat(buffer, ", ");
          }
        }
        strcat(buffer, " }");
        return buffer;
      }
    };
    default: return "unknown";
  }
}

Value* String_from(Value* args) {
  Value* value = index_(args, 0);
  if (value == NULL) return string("unknown");
  return string(toString_(value));
}