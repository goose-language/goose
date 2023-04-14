#ifndef TYPE_H
#define TYPE_H

#include "value.h"

nanbox_t Type_of(nanbox_t value);
nanbox_t String_from(nanbox_t value);
char* toString(nanbox_t value);

#endif