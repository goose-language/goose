#ifndef EQ_H
#define EQ_H
#include "value.h"

VALUE eq(VALUE a, VALUE b);
VALUE neq(VALUE a, VALUE b);
VALUE lt(VALUE a, VALUE b);
VALUE gt(VALUE a, VALUE b);
VALUE lte(VALUE a, VALUE b);
VALUE gte(VALUE a, VALUE b);

// Logical operators
VALUE and_(VALUE a, VALUE b);
VALUE or_(VALUE a, VALUE b);

#endif