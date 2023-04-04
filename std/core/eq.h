#ifndef EQ_H
#define EQ_H
#include "value.h"

Value* eq(Value* a, Value* b);
Value* neq(Value* a, Value* b);
Value* lt(Value* a, Value* b);
Value* gt(Value* a, Value* b);
Value* lte(Value* a, Value* b);
Value* gte(Value* a, Value* b);

// Logical operators
Value* and_(Value* a, Value* b);
Value* or_(Value* a, Value* b);

#endif