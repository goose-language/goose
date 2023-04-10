#ifndef IO_H
#define IO_H

#include "value.h"

Value* IO_print(Value* args);
void update(Value* variable, Value* value);
void freeValue(Value* value);
void IO_exit(Value* args);
Value* IO_readDirectory(Value* args);
Value* IO_fileExists(Value* args);
Value* IO_readFile(Value* path);
Value* IO_input(Value* prompt);
Value* IO_writeFile(Value* args);

Value* getVariantArguments(Value* args);

#endif