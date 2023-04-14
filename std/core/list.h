#ifndef LIST_H
#define LIST_H
#include "value.h"

// List functions
nanbox_t index_(nanbox_t list, int index);
nanbox_t Array_next(nanbox_t list);
nanbox_t Array_length(nanbox_t list);
nanbox_t Array_create(nanbox_t args);
nanbox_t Array_push(nanbox_t args);

// Dictionary functions
nanbox_t property_(nanbox_t dict, char* key);
nanbox_t Array_has(nanbox_t args);

// Memory management functions
nanbox_t IO_clone(nanbox_t value);


#endif