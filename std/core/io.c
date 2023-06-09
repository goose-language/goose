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


#include "type.h"
#include "value.h"

struct Entry* getElements(Dict dict) {
  struct Entry *result = malloc(sizeof(struct Entry) * dict.count);
  int j = 0;
  for (int i = 0; i < dict.length; i++) {
    if (dict.entries[i].key == NULL) continue;
    result[j] = dict.entries[i];
    j++;
    while (dict.entries[i].next != NULL) {
      result[j] = *dict.entries[i].next;
      j++;
      dict.entries[i].next = dict.entries[i].next->next;
    }
  }
  return result;
}

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
          IO_print(list(2, unit(), list_.data[i]));
          if (i != list_.length - 1) printf(", ");
        }
        printf("]");
      }
      break;
    }

    case TYPE_INTERN: {
      Intern dict = decode_pointer(v)->as_intern;
      printf("%s", dict.name);
      printf("(");
      for (int i = 0; i < dict.length; i++) {
        IO_print(list(2, unit(), dict.data[i]));
        if (i != dict.length - 1) printf(", ");
      }
      printf(")");
      break;
    }

    case TYPE_DICT: {
      Dict dict = decode_pointer(v)->as_dict;
      printf("{");
      if (dict.length > 0) {
        printf(" ");
        struct Entry* e = getElements(dict);
        for (int i = 0; i < dict.count; i++) {
          printf("%s", e[i].key);
          printf(": ");
          IO_print(list(2, unit(), e[i].value));
          if (i != dict.count - 1) printf(", ");
        }
        printf(" ");
      }
      printf("}");
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
  VALUE path = index_(args, 1);

  FILE* file = fopen(decode_string(path), "r");

  int MAX_SIZE = 128;
  char* buffer = malloc(sizeof(char) * MAX_SIZE);
  int i = 0;
  char* c = malloc(sizeof(char) * MAX_SIZE);
  while (fgets(buffer, MAX_SIZE, file) != NULL) {
    c = strcat(c, buffer);
  }
  fclose(file);
  return string(c);
}

VALUE IO_writeFile(VALUE args) {
  VALUE path = index_(args, 1);
  VALUE content = index_(args, 2);

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