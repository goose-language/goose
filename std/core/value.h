#ifndef VALUE_H
#define VALUE_H
#include <stdlib.h>
typedef uint64_t VALUE;

// Masks for important segments of a float value
#define MASK_SIGN        0x8000000000000000
#define MASK_EXPONENT    0x7ff0000000000000
#define MASK_QUIET       0x0008000000000000
#define MASK_TYPE        0x0007000000000000
#define MASK_SIGNATURE   0xffff000000000000
#define MASK_PAYLOAD_PTR 0x0000ffffffffffff
#define MASK_PAYLOAD_INT 0x00000000ffffffff

// Type IDs for short encoded types
#define MASK_TYPE_NAN     0x0000000000000000
#define MASK_TYPE_FALSE   0x0001000000000000
#define MASK_TYPE_TRUE    0x0002000000000000
#define MASK_TYPE_NULL    0x0003000000000000
#define MASK_TYPE_INTEGER 0x0004000000000000
#define MASK_TYPE_CHAR    0x0005000000000000

// Constant short encoded values
#define kNaN   (MASK_EXPONENT | MASK_QUIET)
#define kFalse (kNaN | MASK_TYPE_FALSE)
#define kTrue  (kNaN | MASK_TYPE_TRUE)
#define kNull  (kNaN | MASK_TYPE_NULL)

// Signatures of encoded types
#define SIGNATURE_NAN     kNaN
#define SIGNATURE_FALSE   kFalse
#define SIGNATURE_TRUE    kTrue
#define SIGNATURE_NULL    kNull
#define SIGNATURE_INTEGER (kNaN | MASK_TYPE_INTEGER)
#define SIGNATURE_CHAR    (kNaN | MASK_TYPE_CHAR)
#define SIGNATURE_POINTER (kNaN | MASK_SIGN)

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
  TYPE_INTERN
} ValueType;

// Container for arrays
typedef struct {
  VALUE* data;
  uint32_t length;
  uint32_t capacity;
} Array;

struct Entry {
  char* key;
  VALUE value;
  struct Entry* next;
};

// Container for dictionaries
typedef struct {
  struct Entry* entries;
  uint32_t capacity;
  uint32_t count;
  uint32_t length;
} Dict;

// Container for lambdas
typedef struct {
  VALUE (*f)(VALUE args);
} Lambda;

typedef struct {
  uint32_t length;
  uint32_t capacity;
  char* name;
  VALUE* data;
} Intern;

// Container type for values
typedef struct {
  ValueType type;

  union {
    Array as_array;
    Dict as_dict;
    Lambda as_lambda;
    Intern as_intern;
  };
} HeapValue;


VALUE integer(int32_t value);
VALUE floating(double value);
VALUE character(char value);
VALUE list(int length, ...);
VALUE unit(void);
VALUE boolean(int value);
VALUE structure(int length, ...);
VALUE makeLambda(VALUE (*f)(VALUE args));
VALUE string(char* value);
VALUE emptyList();
ValueType get_type(VALUE value);
VALUE create_pointer(HeapValue* ptr);

uint32_t hash(const char* string, int);

#endif