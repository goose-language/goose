#ifndef EQ_H
#define EQ_H
#include "value.h"

nanbox_t eq(nanbox_t a, nanbox_t b);
nanbox_t neq(nanbox_t a, nanbox_t b);
nanbox_t lt(nanbox_t a, nanbox_t b);
nanbox_t gt(nanbox_t a, nanbox_t b);
nanbox_t lte(nanbox_t a, nanbox_t b);
nanbox_t gte(nanbox_t a, nanbox_t b);

// Logical operators
nanbox_t and_(nanbox_t a, nanbox_t b);
nanbox_t or_(nanbox_t a, nanbox_t b);

#endif