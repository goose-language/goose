#include <stdio.h>
#include "io.h"
#include "list.h"
#include <stdlib.h>
#include <sys/stat.h> 
#include <stdbool.h>
#include <dirent.h>
#include "conversion.h"
#include "error.h"
#include <string.h>
#include "garbage.h"
#include "garbage/tgc.h"
#include "type.h"
#include "value.h"

nanbox_t IO_fileExists (nanbox_t filename) {
  nanbox_t v = index_(filename, 0);
  
  char *filename_ = decode_string(v);
  struct stat buffer;   
  return boolean(stat(filename_, &buffer) == 0);
}

nanbox_t IO_readDirectory(nanbox_t args) {
  nanbox_t v = index_(args, 0);

  char *filename_ = decode_string(v);
  DIR *d;
  struct dirent *dir;
  d = opendir(filename_);
  nanbox_t result = emptyList();
  if (d) {
    while ((dir = readdir(d)) != NULL) {
      result = Array_push(list(2, result, string(dir->d_name)));
    }
    closedir(d);
  }
  return result;
}

HeapValue* getVariantArguments(nanbox_t dict) {
  Dict heap = decode_pointer(dict)->as_dict;
  nanbox_t* data = malloc(sizeof(nanbox_t) * heap.length);
  int j = 0; 
  for (int i = 0; i < heap.length; i++) {
    if (strcmp(heap.keys[i], "$$enum") == 0) continue;
    if (strcmp(heap.keys[i], "type") == 0) continue;

    data[j] = heap.values[i];
    j++;
  }

  HeapValue* result = malloc(sizeof(HeapValue));
  result->type = TYPE_ARRAY;
  result->as_array.length = heap.length - 2;
  result->as_array.data = data;
  return result;
}

int hasProperty(nanbox_t dict, char* key) {
  Dict heap = decode_pointer(dict)->as_dict;
  for (int i = 0; i < heap.length; i++) {
    if (strcmp(heap.keys[i], key) == 0) return 1;
  }
  return 0;
}

nanbox_t IO_print(nanbox_t args) {
  nanbox_t v = index_(args, 0);

  switch (get_type(v)) {
    case TYPE_INTEGER: 
      printf("%d", v); break;
    case TYPE_FLOAT:
      printf("%f", decode_floating(v)); break;
    case TYPE_CHAR:
      printf("%c", decode_character(v)); break;
    case TYPE_BOOL:
      printf("%s", decode_boolean(v) ? "true" : "false"); break;
    case TYPE_NULL:
      printf("nil"); break;
    case TYPE_ARRAY: {
      Array list_ = decode_pointer(v)->as_array;
      
      if (list_.length == 0) {
        printf("[]");
      } else if (get_type(list_.data[0]) == TYPE_CHAR) {
        printf("\"");
        for (int i = 0; i < list_.length; i++) {
          printf("%c", decode_character(list_.data[i]));
        }
        printf("\"");
      } else {
        printf("[");
        for (int i = 0; i < list_.length; i++) {
          IO_print(list(1, list_.data[i]));
          if (i != list_.length - 1) printf(", ");
        }
        printf("]");
      }
      break;
    }

    case TYPE_DICT: {
      Dict dict = decode_pointer(v)->as_dict;
      if (hasProperty(v, "$$enum")) {
        printf("%s", decode_string(property_(v, "type")));
        printf("(");
        Array list_ = getVariantArguments(v)->as_array;
        for (int i = 0; i < list_.length; i++) {
          IO_print(list(1, list_.data[i]));
          if (i != list_.length - 1) printf(", ");
        }
        printf(")");
      } else {
        printf("{");
        for (int i = 0; i < dict.length; i++) {
          printf("%s: ", dict.keys[i]);
          IO_print(list(1, dict.values[i]));
          if (i != dict.length - 1) printf(", ");
        }
        printf("}");
      }
      break;
    }

    case TYPE_MUTABLE: {
      nanbox_t value = get_mutable(v);
      printf("mutable ");
      IO_print(list(1, value));
    }

    case TYPE_LAMBDA: {
      printf("lambda<#%p>", decode_pointer(v));
      break;
    }
  }
}

void update(nanbox_t* v, nanbox_t value) {
  if (v == NULL) throwError("expected variable, got NULL");
  switch (get_type(*v)) {
    case TYPE_INTEGER: {
      *v = integer(decode_integer(value));
      break;
    }
    case TYPE_FLOAT: {
      *v = floating(decode_floating(value));
      break;
    }
    case TYPE_CHAR: {
      *v = character(decode_character(value));
      break;
    }
    case TYPE_BOOL: {
      *v = value;
      break;
    }
    case TYPE_NULL: {
      *v = value;
      break;
    }
    case TYPE_ARRAY: {
      Array list_ = decode_pointer(*v)->as_array;
      Array list_2 = decode_pointer(value)->as_array;
      list_.data = realloc(list_.data, sizeof(nanbox_t) * list_2.length);
      for (int i = 0; i < list_2.length; i++) {
        printf("%d\n", i);
        IO_print(list(1, list_2.data[i]));
        list_.data[i] = list_2.data[i];
      }
      break;
    }
    case TYPE_DICT: {
      Dict dict = decode_pointer(*v)->as_dict;
      Dict dict_2 = decode_pointer(value)->as_dict;
      dict.values = realloc(dict.values, sizeof(nanbox_t) * dict_2.length);
      for (int i = 0; i < dict.length; i++) {
        dict.values[i] = dict_2.values[i];
      }
      break;
    }
    case TYPE_MUTABLE: {
      nanbox_t* value_ = decode_pointer(*v)->as_pointer;
      *value_ = value;
      break;
    }
  }
}



void IO_exit(nanbox_t args) {
  nanbox_t v = index_(args, 0);
  exit(decode_integer(v));
}
nanbox_t IO_readFile(nanbox_t args) {
  nanbox_t path = index_(args, 0);

  FILE* file = fopen(decode_string(path), "r");
  fseek(file, 0L, SEEK_END);
  size_t len = ftell(file);
  rewind(file);
  char* buffer = malloc(sizeof(char) * len);
  int i = 0;
  char c;
  while ((c = fgetc(file)) != EOF) {
    buffer[i] = c;
    i++;
  }
  buffer[len] = '\0';
  fclose(file);
  return string(buffer);
}

nanbox_t IO_writeFile(nanbox_t args) {
  nanbox_t path = index_(args, 0);
  nanbox_t content = index_(args, 1);

  FILE* file = fopen(decode_string(path), "w");
  fputs(decode_string(content), file);
  fclose(file);
  return unit();
}

nanbox_t IO_input(nanbox_t args) {
  nanbox_t prompt = index_(args, 0);

  printf("%s", decode_string(prompt));
  char* buffer;
  scanf("%s", buffer);
  return string(buffer);
}