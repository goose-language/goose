#ifndef TYPE_H
#define TYPE_H

#include "value.h"

VALUE Type_of(VALUE value);
VALUE String_from(VALUE value);
char* toString(VALUE value);

#endif