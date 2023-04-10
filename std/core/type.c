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
        return add(string("list<"), add(Type_of(list(value->l.value, NULL)), string(">")));
      }
      return string("list");
    case UNIT: return string("nil");
    case BOOL: return string("bool");
    case STRUCT: {
      Value *acc = string("structure<");
      Value *current = value;
      while (current != NULL) {
        acc = add(acc, add(string(current->s.name), string(": ")));
        acc = add(acc, current->s.next == NULL ? Type_of(list(current->s.value, NULL)) : add(Type_of(list(current->s.value, NULL)), string(", ")));
        current = current->s.next;
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
      if (value->l.value->type == CHAR) {
        char* buffer = malloc(sizeof(char) * Array_length(list(value, NULL))->i);
        Value* current = value;
        int i = 0;
        while (current != NULL) {
          buffer[i] = current->l.value->c;
          current = current->l.next;
          i++;
        }
        return buffer;
      } else {
        char* buffer = malloc(sizeof(char) * 8);
        buffer[0] = '[';
        Value* current = value;
        int i = 1;
        while (current != NULL) {
          strcat(buffer, toString_(current->l.value));
          if (current->l.next != NULL) {
            strcat(buffer, ", ");
            i += 2;
          }
          i += strlen(toString_(current->l.value));
          realloc(buffer, sizeof(char) * (strlen(buffer) + i));
          current = current->l.next;
        }
        strcat(buffer, "]");
        return buffer;
      }
    }
    case UNIT: return "nil";
    case BOOL: return "bool";
    case STRUCT: {
      if (Array_has(list(value, list(string("$$enum"), NULL)))->b && property_(value, "$$enum")->b) {
        char* buffer = malloc(sizeof(char) * 16);
        strcat(buffer, toString(property_(value, "type")));
        strcat(buffer, "(");
        Value* varArgs = getVariantArguments(value);
        while (varArgs != NULL) {
          if (varArgs->l.value == NULL) break;
          strcat(buffer, toString_(varArgs->l.value));
          if (varArgs->l.next != NULL && varArgs->l.next->l.value != NULL) {
            strcat(buffer, ", ");
          }
          varArgs = varArgs->l.next;
        }
        strcat(buffer, ")");
        return buffer;
      } else {
        Value *current = value;
        if (current == NULL) return "{}";
        char* buffer = malloc(sizeof(char) * 16);
        buffer[0] = '{';
        buffer[1] = ' ';
        int i = 1;
        while (current != NULL) {
          strcat(buffer, current->s.name);
          strcat(buffer, ": ");
          i += strlen(current->s.name) + 2;
          char* value = toString_(current->s.value);
          strcat(buffer, value);
          if (current->s.next != NULL) {
            strcat(buffer, ", ");
            i += 2;
          }
          i += strlen(value);
          buffer = realloc(buffer, sizeof(char) * (strlen(buffer) + i + 1));
          current = current->s.next;
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