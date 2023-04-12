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

Value* IO_fileExists (Value *filename) {
  Value *v = index_(filename, 0);
  if (v == NULL) throwError("IO::fileExists: expected 1 argument, got 0");
  
  char *filename_ = toString(v);
  struct stat buffer;   
  return boolean(stat(filename_, &buffer) == 0);
}

Value* IO_readDirectory(Value* args) {
  Value *v = index_(args, 0);
  if (v == NULL) throwError("IO::readDirectory: expected 1 argument, got 0");

  char *filename_ = toString(v);
  DIR *d;
  struct dirent *dir;
  d = opendir(filename_);
  Value *result = emptyList();
  if (d) {
    while ((dir = readdir(d)) != NULL) {
      result = push(result, string(dir->d_name));
    }
    closedir(d);
  }
  return result;
}

Value* getVariantArguments(Value* dict) {
  Value *array = (Value*) tgc_alloc(gc(), sizeof(Value));
  array->type = LIST;
  Value **result = tgc_alloc(gc(), sizeof(Value*) * (dict->s.length - 2));
  int j = 0;
  for (int i = 0; i < dict->s.length; i++) {
    if (strcmp(dict->s.elements[i]->name, "type") == 0) continue;
    if (strcmp(dict->s.elements[i]->name, "$$enum") == 0) continue;
    result[j] = dict->s.elements[i]->value;
    j++;
  }
  array->l.value = result;
  array->l.length = dict->s.length - 2;
  return array;
}

Value* IO_print(Value *args)
{
  Value *v = args->l.value[0];
  if (v == NULL) throwError("IO::print: expected 1 argument, got 0");

  switch (v->type)
  {
  case LAMBDA:
    printf("lambda<#%p>", v->$$fun);
    break;
  case INT:
    printf("%lld", v->i);
    break;
  case FLOAT:
    printf("%f", v->f);
    break;
  case CHAR:
    printf("%c", v->c);
    break;
  case LIST: {
    if (v->l.value == NULL) {
      printf("[]");
    } else if (v->l.value[0]->type == CHAR) {
      printf("\"");
      for (int i = 0; i < v->l.length; i++) {
        printf("%c", v->l.value[i]->c);
      }
      printf("\"");
    }
    else
    {
      printf("[");
      if (v->l.value != NULL) {
        for (int i = 0; i < v->l.length; i++) {
          IO_print(list(1, v->l.value[i], NULL));
          if (i != v->l.length - 1) {
            printf(", ");
          }
        }
      }
      printf("]");
    }
    break;
  }
  case UNIT:
    printf("()");
    break;
  case BOOL:
    printf("%s", v->b ? "true" : "false");
    break;
  case STRUCT:
    if (Array_has(list(2, v, string("$$enum")))->b && property_(v, "$$enum")->b) {
      printf("%s(", toString(property_(v, "type")));
      Value* varArgs = getVariantArguments(v);
      for (int i = 0; i < varArgs->l.length; i++) {
        IO_print(list(1, varArgs->l.value[i], NULL));
        if (i != varArgs->l.length - 1) {
          printf(", ");
        }
      }
      printf(")");
    } else {
      printf("{");
      if (v->s.length > 0) {
        printf(" ");
        for (int i = 0; i < v->s.length; i++) {
          printf("%s: ", v->s.elements[i]->name);
          IO_print(list(1, v->s.elements[i]->value, NULL));
          if (i != v->s.length - 1) {
            printf(", ");
          }
        }
        printf(" ");
      }
      printf("}");
    }
  }
  return integer(0);
}

void update(Value* v, Value* value) {
  if (v == NULL) throwError("expected variable, got NULL");
  if (value == NULL) throwError("expected value, got NULL");

  switch (v->type) {
    case INT:
      v->i = value->i;
      break;
    case FLOAT:
      v->f = value->f;
      break;
    case CHAR:
      v->c = value->c;
      break;
    case LIST: {
      v->l.length = value->l.length;
      v->l.value = tgc_realloc(gc(), v->l.value, sizeof(Value*) * v->l.length);
      for (int i = 0; i < v->l.length; i++) {
        v->l.value[i] = value->l.value[i];
      }
      break;
    }
    case STRUCT: {
      v->s.length = value->s.length;
      v->s.elements = tgc_realloc(gc(), v->s.elements, sizeof(Value*) * v->s.length);
      for (int i = 0; i < v->s.length; i++) {
        v->s.elements[i] = value->s.elements[i];
      }
      break;
    }
    case UNIT:
      break;
    case BOOL:
      v->b = value->b;
      break;
  }
}

void IO_exit(Value* args) {
  Value* v = index_(args, 0);
  if (v == NULL) {
    exit(0);
  } else {
    exit(v->i);
  }
}
Value* IO_readFile(Value* args) {
  Value* path = index_(args, 0);
  if (path == NULL) throwError("IO::readFile: expected 1 argument, got 0");

  Value* result = emptyList();
  FILE* file = fopen(toString(path), "r");
  char c;
  while ((c = fgetc(file)) != EOF) {
    push(result, character(c));
  }
  fclose(file);
  return result;
}

Value* IO_writeFile(Value* args) {
  Value* path = index_(args, 0);
  if (path == NULL) throwError("IO::writeFile: expected 2 arguments, got 0");

  Value* content = index_(args, 1);
  if (path == NULL) throwError("IO::writeFile: expected 2 arguments, got 1");

  FILE* file = fopen(toString(path), "w");
  fputs(toString(content), file);
  fclose(file);
  return unit();
}

Value* IO_input(Value* args) {
  Value* prompt = index_(args, 0);
  if (prompt == NULL) throwError("IO::input: expected 1 argument, got 0");

  printf("%s", toString(prompt));
  char* buffer;
  scanf("%s", buffer);
  return string(buffer);
}