#include <stdlib.h>
#include <string.h>
#include "num.h"
#include "error.h"
#include "type.h"
#include "conversion.h"
#include "eq.h"
#include "garbage.h"
#include "garbage/tgc.h"

nanbox_t add(nanbox_t a, nanbox_t b) {
  ValueType type_a = get_type(a);
  ValueType type_b = get_type(b);

  if (type_a == TYPE_INTEGER && type_b == TYPE_INTEGER) {
    return integer(a.as_int64 + b.as_int64);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_FLOAT) {
    return floating(a.as_double + b.as_double);
  } else if (type_a == TYPE_INTEGER && type_b == TYPE_FLOAT) {
    return floating(a.as_int64 + b.as_double);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_INTEGER) {
    return floating(a.as_double + b.as_int64);
  } else if (type_a == TYPE_ARRAY && type_b == TYPE_ARRAY) {
    Array a_array = decode_pointer(a)->as_array;
    Array b_array = decode_pointer(b)->as_array;

    nanbox_t* new_data = tgc_alloc(gc(), sizeof(nanbox_t) * (a_array.length + b_array.length));
    for (int i = 0; i < a_array.length; i++) {
      new_data[i] = a_array.data[i];
    }
    for (int i = 0; i < b_array.length; i++) {
      new_data[i + a_array.length] = b_array.data[i];
    }

    Array new_array = {new_data, a_array.length + b_array.length};
    HeapValue* new_heap_value = tgc_alloc(gc(), sizeof(HeapValue));
    new_heap_value->type = TYPE_ARRAY;
    new_heap_value->as_array = new_array;
    return create_pointer(new_heap_value);
  } else {
    printf("Cannot add %s and %s together", Type_of(list(1, (nanbox_t[1]) { a })), Type_of(list(1, (nanbox_t[1]) { b })));
    exit(0);
  }
}

nanbox_t subtract(nanbox_t a, nanbox_t b) {
  ValueType type_a = get_type(a);
  ValueType type_b = get_type(b);

  if (type_a == TYPE_INTEGER && type_b == TYPE_INTEGER) {
    return integer(a.as_int64 - b.as_int64);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_FLOAT) {
    return floating(a.as_double - b.as_double);
  } else if (type_a == TYPE_INTEGER && type_b == TYPE_FLOAT) {
    return floating(a.as_int64 - b.as_double);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_INTEGER) {
    return floating(a.as_double - b.as_int64);
  } else {
    printf("Cannot subtract %s and %s together", Type_of(list(1, (nanbox_t[1]) { a })), Type_of(list(1, (nanbox_t[1]) { b })));
    exit(0);
  }
}

nanbox_t multiply(nanbox_t a, nanbox_t b) {
  ValueType type_a = get_type(a);
  ValueType type_b = get_type(b);

  if (type_a == TYPE_INTEGER && type_b == TYPE_INTEGER) {
    return integer(a.as_int64 * b.as_int64);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_FLOAT) {
    return floating(a.as_double * b.as_double);
  } else if (type_a == TYPE_INTEGER && type_b == TYPE_FLOAT) {
    return floating(a.as_int64 * b.as_double);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_INTEGER) {
    return floating(a.as_double * b.as_int64);
  } else {
    printf("Cannot multiply %s and %s together", Type_of(list(1, (nanbox_t[1]) { a })), Type_of(list(1, (nanbox_t[1]) { b })));
    exit(0);
  }
}

nanbox_t divide(nanbox_t a, nanbox_t b) {
  ValueType type_a = get_type(a);
  ValueType type_b = get_type(b);

  if (type_a == TYPE_INTEGER && type_b == TYPE_INTEGER) {
    return integer(a.as_int64 / b.as_int64);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_FLOAT) {
    return floating(a.as_double / b.as_double);
  } else if (type_a == TYPE_INTEGER && type_b == TYPE_FLOAT) {
    return floating(a.as_int64 / b.as_double);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_INTEGER) {
    return floating(a.as_double / b.as_int64);
  } else {
    printf("Cannot divide %s and %s together", Type_of(list(1, (nanbox_t[1]) { a })), Type_of(list(1, (nanbox_t[1]) { b })));
    exit(0);
  }
}
