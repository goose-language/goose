#include <stdio.h>
#include "io.h"
#include "list.h"
#include <stdlib.h>
#include <sys/stat.h> 
#include <stdbool.h>
#include <dirent.h>
#include "conversion.h"

Value* IO_fileExists (Value *filename) {
  Value *v = index_(filename, 0);
  char *filename_ = toString(v);
  struct stat buffer;   
  return boolean(stat(filename_, &buffer) == 0);
}

Value* IO_readDirectory(Value* args) {
  Value *v = index_(args, 0);
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

Value* IO_print(Value *args)
{
  Value *v = index_(args, 0);
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
    } else if (v->l.value->type == CHAR)
    {
      printf("\"");
      for (Value *i = v; i != NULL; i = i->l.next)
      {
        IO_print(list(i->l.value, NULL));
      }
      printf("\"");
    }
    else
    {
      printf("[");
      if (v->l.value != NULL) {
        for (Value *i = v; i != NULL; i = i->l.next)
        {
          if (i->l.value == NULL) break;
          IO_print(list(i->l.value, NULL));
          if (i->l.next != NULL)
          {
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
    printf("{");
    if (v->s.value != NULL) {
      printf(" ");
      for (Value *i = v; i != NULL; i = i->s.next)
      {
        if (i->s.value == NULL) break;
        printf("%s: ", i->s.name);
        IO_print(list(i->s.value, NULL));
        if (i->s.next != NULL)
        {
          printf(", ");
        }
      }
      printf(" ");
    }
    printf("}");
  }
  return integer(0);
}

void update(Value* v, Value* value) {
  if (v == NULL) return;
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
    case LIST:
      v->l.value = value->l.value;
      v->l.next = value->l.next;
      break;
    case STRUCT:
      v->s.name = value->s.name;
      v->s.value = value->s.value;
      v->s.next = value->s.next;
      break;
    case UNIT:
      break;
    case BOOL:
      v->b = value->b;
      break;
  }
}

void freeValue(Value* v) {
  if (v == NULL) return;
  switch (v->type) {
    case INT:
      break;
    case FLOAT:
      break;
    case CHAR:
      break;
    case LIST:
      freeValue(v->l.value);
      freeValue(v->l.next);
      break;
    case STRUCT:
      free(v->s.name);
      freeValue(v->s.value);
      freeValue(v->s.next);
      break;
    case UNIT:
      break;
    case BOOL:
      break;
  }
  free(v);
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
  Value* result = emptyList();
  FILE* file = fopen(toString(path), "r");
  char c;
  while ((c = fgetc(file)) != EOF) {
    if (result->l.value == NULL) {
      result->l.value = character(c);
      continue;
    }
    push(result, character(c));
  }
  fclose(file);
  return result;
}

Value* IO_writeFile(Value* args) {
  Value* path = index_(args, 0);
  Value* content = index_(args, 1);
  FILE* file = fopen(toString(path), "w");
  fputs(toString(content), file);
  fclose(file);
  return unit();
}

Value* IO_input(Value* args) {
  Value* prompt = index_(args, 0);
  printf("%s", toString(prompt));
  char* buffer;
  scanf("%s", buffer);
  return string(buffer);
}