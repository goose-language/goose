#ifndef IO_H
#define IO_H

#include "value/nanbox.h"
#include "value.h"

nanbox_t IO_print(nanbox_t args);
void update(nanbox_t *variable, nanbox_t value);
void freeValue(nanbox_t value);
void IO_exit(nanbox_t args);
nanbox_t IO_readDirectory(nanbox_t args);
nanbox_t IO_fileExists(nanbox_t args);
nanbox_t IO_readFile(nanbox_t path);
nanbox_t IO_input(nanbox_t prompt);
nanbox_t IO_writeFile(nanbox_t args);

HeapValue* getVariantArguments(nanbox_t args);

#endif