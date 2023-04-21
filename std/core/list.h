#ifndef LIST_H
#define LIST_H
#include "value.h"

// List functions
VALUE index_(VALUE list, int index);
VALUE Array_next(VALUE list);
VALUE Array_length(VALUE list);
VALUE Array_create(VALUE args);
VALUE Array_push(VALUE args);

// Dictionary functions
VALUE property_(VALUE dict, char* key);
VALUE Array_has(VALUE args);

// Memory management functions
VALUE IO_clone(VALUE value);


#endif