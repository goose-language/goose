#ifndef REGEX_H
#define REGEX_H

#include "value.h"

Value* Regex_match(Value* args);
Value* Regex_test(Value* args);

#endif