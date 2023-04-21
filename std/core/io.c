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

VALUE IO_fileExists (VALUE filename) {
  VALUE v = index_(filename, 1);
  
  char *filename_ = decode_string(v);
  struct stat buffer;   
  return boolean(stat(filename_, &buffer) == 0);
}

VALUE IO_readDirectory(VALUE args) {
  VALUE v = index_(args, 1);

  char *filename_ = decode_string(v);
  DIR *d;
  struct dirent *dir;
  d = opendir(filename_);
  VALUE result = emptyList();
  if (d) {
    while ((dir = readdir(d)) != NULL) {
      result = Array_push(list(2, result, string(dir->d_name)));
    }
    closedir(d);
  }
  return result;
}

HeapValue* getVariantArguments(VALUE dict) {
  Dict heap = decode_pointer(dict)->as_dict;
  VALUE* data = malloc(sizeof(VALUE) * heap.length);
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

int hasProperty(VALUE dict, char* key) {
  Dict heap = decode_pointer(dict)->as_dict;
  for (int i = 0; i < heap.length; i++) {
    if (strcmp(heap.keys[i], key) == 0) return 1;
  }
  return 0;
}

VALUE IO_print(VALUE args) {
  VALUE v = index_(args, 1);

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

    case TYPE_LAMBDA: {
      printf("lambda<#%p>", decode_pointer(v));
      break;
    }
  }
  return unit();
}



void IO_exit(VALUE args) {
  VALUE v = index_(args, 1);
  exit(decode_integer(v));
}
VALUE IO_readFile(VALUE args) {
  VALUE path = index_(args, 0);

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

VALUE IO_writeFile(VALUE args) {
  VALUE path = index_(args, 0);
  VALUE content = index_(args, 1);

  FILE* file = fopen(decode_string(path), "w");
  fputs(decode_string(content), file);
  fclose(file);
  return unit();
}

VALUE IO_input(VALUE args) {
  VALUE prompt = index_(args, 1);

  printf("%s", decode_string(prompt));
  char* buffer = malloc(sizeof(char) * 100);
  scanf("%s", buffer);
  return string(buffer);
}