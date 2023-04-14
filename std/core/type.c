#include "value.h"
#include "type.h"
#include "list.h"
#include "num.h"
#include <stdlib.h>
#include <stdio.h>
#include "io.h"
#include <string.h>
#include "conversion.h"

nanbox_t Type_of(nanbox_t args) {
  nanbox_t value = index_(args, 0);
  switch (get_type(value)) {
    case TYPE_LAMBDA: return string("lambda");
    case TYPE_INTEGER: return string("int");
    case TYPE_FLOAT: return string("float");
    case TYPE_CHAR: return string("char");
    case TYPE_ARRAY: return string("list");
    case TYPE_BOOL: return string("boolean");
    case TYPE_DICT: return string("struct");
    case TYPE_NULL: return string("nil");
    default: return string("unknown");
  }
}
