#ifndef LIST_H
#define LIST_H
#include "value.h"

// List functions
Value* index_(Value* list, int index);
Value* Array_next(Value* list);
Value* Array_length(Value* list);
Value* Array_create(Value* args);
Value* push(Value* list, Value* value);

// Dictionary functions
Value* property_(Value* dict, char* key);
Value* Array_has(Value* args);

// Memory management functions
Value* IO_clone(Value* value);


#endif