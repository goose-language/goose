#ifndef LIST_H
#define LIST_H
#include "value.h"

// List functions
int length(VALUE list);
VALUE index_(VALUE list, int index);
VALUE Array_next(VALUE list);
VALUE Array_length(VALUE list);
VALUE Array_create(VALUE args);
VALUE Array_push(VALUE args);

// Dictionary functions
VALUE property_(VALUE dict, char* key);
VALUE intern_property_(VALUE dict, uint64_t idx);
VALUE Array_has(VALUE args);
VALUE in(VALUE dict, char* key);

// Memory management functions
VALUE IO_clone(VALUE value);


#endif