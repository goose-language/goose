#ifndef VALUE_H
#define VALUE_H
#include "value/nanbox.h"
#include <stdlib.h>

// The type of the stored value
typedef enum {
  TYPE_INTEGER,
  TYPE_FLOAT,
  TYPE_BOOL,
  TYPE_NULL,
  TYPE_CHAR,
  TYPE_ARRAY,
  TYPE_DICT,
  TYPE_LAMBDA,
  TYPE_MUTABLE,
} ValueType;

// Container for arrays
typedef struct {
  nanbox_t* data;
  uint32_t length;
  uint32_t capacity;
} Array;

// Container for dictionaries
typedef struct {
  char** keys;
  nanbox_t* values;
  uint32_t capacity;
  uint32_t length;
} Dict;

// Container for lambdas
typedef struct {
  nanbox_t (*f)(nanbox_t args);
} Lambda;

// Container type for values
typedef struct {
  ValueType type;

  union {
    Array as_array;
    Dict as_dict;
    Lambda as_lambda;
    char as_char;
    nanbox_t* as_pointer;
  };
} HeapValue;

nanbox_t integer(int32_t value);
nanbox_t floating(double value);
nanbox_t character(char value);
nanbox_t list(int length, ...);
nanbox_t unit(void);
nanbox_t boolean(int value);
nanbox_t structure(int length, ...);
nanbox_t makeLambda(nanbox_t (*f)(nanbox_t args));
nanbox_t string(char* value);
nanbox_t emptyList();
ValueType get_type(nanbox_t value);
nanbox_t create_pointer(HeapValue* ptr);
nanbox_t create_mutable(nanbox_t value);
nanbox_t get_mutable(nanbox_t value);

nanbox_t* unbox_mutable(nanbox_t value);

#endif