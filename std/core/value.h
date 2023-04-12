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

struct Element {
  char* name;
  struct _Value* value;
};

struct Struct {
  int length;
  struct Element** elements;
};

struct List {
  int length;
  struct _Value** value;
};

typedef struct _Value {
  int type;
  union {
    long long int i;
    float f;
    char c;
    struct List l;
    struct Struct s;
    int b;
    struct _Value* (*$$fun)(struct _Value* args);
  };
} Value;

Value* integer(int value);
Value* floating(float value);
Value* character(char value);
Value* list(int length, ...);
Value* unit(void);
Value* boolean(int value);
struct Element* element(char* name, Value* value);
Value* structure(int length, ...);
Value* makeLambda(Value* (*f)(Value* args));
Value* string(char* value);
Value* emptyList();

#endif