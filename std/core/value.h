#ifndef VALUE_H
#define VALUE_H

#define INT 0
#define FLOAT 1
#define CHAR 2
#define LIST 3
#define UNIT 4
#define BOOL 5
#define STRUCT 6
#define LAMBDA 7

typedef struct _Value {
  int type;
  union {
    long long int i;
    float f;
    char c;
    struct List {
      struct _Value* value;
      struct _Value* next;
    } l;
    struct Struct {
      char* name;
      struct _Value* value;
      struct _Value* next;
    } s;
    int b;
    struct _Value* (*$$fun)(struct _Value*);
  };
} Value;

Value* integer(int value);
Value* floating(float value);
Value* character(char value);
Value* list(Value* value, Value* next);
Value* unit(void);
Value* boolean(int value);
Value* structure(char* name, Value* value, Value* next);
Value* makeLambda(Value* (*f)(Value*));
Value* string(char* value);
Value* emptyList();

#endif