#include "value.h"
#include "type.h"
#include "list.h"
#include "num.h"
#include <stdlib.h>
#include <stdio.h>
#include "io.h"
#include <string.h>
#include "conversion.h"

VALUE Type_of(VALUE args) {
  VALUE value = index_(args, 1);
  switch (get_type(value)) {
    case TYPE_LAMBDA: return string("lambda");
    case TYPE_INTEGER: return string("int");
    case TYPE_FLOAT: return string("float");
    case TYPE_CHAR: return string("char");
    case TYPE_ARRAY: return string("list");
    case TYPE_BOOL: return string("boolean");
    case TYPE_DICT: return string("struct");
    case TYPE_NULL: return string("nil");
    default: return string("unknown");
  }
}

char* toString_(VALUE value, int depth) {
  switch (get_type(value)) {
    case TYPE_LAMBDA: {
      char* result = malloc(sizeof(char) * 100);
      sprintf(result, "lambda<#%p>", decode_pointer(value));
      return result;
    }
    case TYPE_INTEGER: {
      char* result = malloc(sizeof(char) * 100);
      sprintf(result, "%d", decode_integer(value));
      return result;
    };
    case TYPE_FLOAT: {
      char* result = malloc(sizeof(char) * 100);
      sprintf(result, "%f", decode_floating(value));
      return result;
    };
    case TYPE_CHAR: {
      char c = decode_character(value);
      char* result = malloc(sizeof(char) * 2);
      result[0] = c;
      result[1] = '\0';
      return result;
    };
    case TYPE_ARRAY: {
      Array array = decode_pointer(value)->as_array;
      char* result = malloc(sizeof(char) * 2);
      if (array.length == 0) {
        result[0] = '[';
        result[1] = ']';
        return result;
      }
      if (get_type(array.data[0]) == TYPE_CHAR) {
        if (depth > 0) {
          char* result = malloc(sizeof(char) * 2);
          result[0] = '"';
          for (int i = 0; i < array.length; i++) {
            char c = decode_character(array.data[i]);
            result[i + 1] = c;
          }
          strcat(result, "\"\0");
          return result;
        } else {
          char* result = malloc(sizeof(char) * (array.length + 1));
          int i = 0;
          for (;i < array.length; i++) {
            result[i] = decode_character(array.data[i]);
          }
          result[i] = '\0';
          return result;
        }
      }
      strcat(result, "[");
      for (int i = 0; i < array.length; i++) {
        strcat(result, toString_(array.data[i], depth + 1));
        if (i != array.length - 1) {
          strcat(result, ", ");
        }
      }
      strcat(result, "]");
      return result;
    }
    case TYPE_BOOL: {
      char* result = malloc(sizeof(char) * 6);
      if (decode_boolean(value)) {
        strcpy(result, "true");
      } else {
        strcpy(result, "false");
      }
      return result;
    }
    case TYPE_DICT: {
      Dict dict = decode_pointer(value)->as_dict;
      char* result = malloc(sizeof(char) * 2);
      if (dict.length == 0) {
        result[0] = '{';
        result[1] = '}';
        return result;
      }
      strcat(result, "{ ");
      struct Entry* entries = getElements(dict);
      for (int i = 0; i < dict.count; i++) {
        strcat(result, entries[i].key);
        strcat(result, ": ");
        strcat(result, toString_(entries[i].value, depth + 1));
        if (i != dict.count - 1) {
          strcat(result, ", ");
        }
      }
      strcat(result, " }");
      return result;
    }

    case TYPE_INTERN: {
      char* result = malloc(sizeof(char) * 100);
      Intern intern = decode_pointer(value)->as_intern;
      strcat(result, intern.name);
      strcat(result, "(");
      for (int i = 0; i < intern.length; i++) {
        strcat(result, toString_(intern.data[i], depth + 1));
        if (i != intern.length - 1) {
          strcat(result, ", ");
        }
      }
      strcat(result, ")");
      return result;
    }

    case TYPE_NULL: {
      char* result = malloc(sizeof(char) * 4);
      strcpy(result, "nil");
      return result;
    }
    default: return "unknown";
  }
}

char* toString(VALUE value) {
  return toString_(value, 0);
}

VALUE String_from(VALUE args) {
  VALUE value = index_(args, 1);
  return string(toString(value));
}