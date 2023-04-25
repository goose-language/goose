#ifndef IO_H
#define IO_H


#include "value.h"

VALUE IO_print(VALUE args);
void update(VALUE *variable, VALUE value);
void freeValue(VALUE value);
void IO_exit(VALUE args);
VALUE IO_readDirectory(VALUE args);
VALUE IO_fileExists(VALUE args);
VALUE IO_readFile(VALUE path);
VALUE IO_input(VALUE prompt);
VALUE IO_writeFile(VALUE args);

HeapValue* getVariantArguments(VALUE args);
int hasProperty(VALUE value, char* key);

#endif