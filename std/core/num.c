#include <stdlib.h>
#include <string.h>
#include "num.h"
#include "error.h"
#include "type.h"
#include "conversion.h"
#include "eq.h"
#include "garbage.h"
#include "garbage/tgc.h"

VALUE add(VALUE a, VALUE b) {
  ValueType type_a = get_type(a);
  ValueType type_b = get_type(b);

  if (type_a == TYPE_INTEGER && type_b == TYPE_INTEGER) {
    return integer(a + b);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_FLOAT) {
    return floating(a + b);
  } else if (type_a == TYPE_INTEGER && type_b == TYPE_FLOAT) {
    return floating(a + b);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_INTEGER) {
    return floating(a + b);
  } else if (type_a == TYPE_ARRAY && type_b == TYPE_ARRAY) {
    Array a_array = decode_pointer(a)->as_array;
    Array b_array = decode_pointer(b)->as_array;

    VALUE* new_data = malloc(sizeof(VALUE) * (a_array.length + b_array.length));
    for (int i = 0; i < a_array.length; i++) {
      new_data[i] = a_array.data[i];
    }
    for (int i = 0; i < b_array.length; i++) {
      new_data[i + a_array.length] = b_array.data[i];
    }

    Array new_array = {new_data, a_array.length + b_array.length};
    HeapValue* new_heap_value = malloc(sizeof(HeapValue));
    new_heap_value->type = TYPE_ARRAY;
    new_heap_value->as_array = new_array;
    return create_pointer(new_heap_value);
  } else {
    throwError("Cannot add %s and %s together", decode_string(Type_of(list(1, a))), decode_string(Type_of(list(1, b))));
  }
  return unit();
}

VALUE subtract(VALUE a, VALUE b) {
  ValueType type_a = get_type(a);
  ValueType type_b = get_type(b);

  if (type_a == TYPE_INTEGER && type_b == TYPE_INTEGER) {
    return integer(a - b);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_FLOAT) {
    return floating(a - b);
  } else if (type_a == TYPE_INTEGER && type_b == TYPE_FLOAT) {
    return floating(a - b);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_INTEGER) {
    return floating(a - b);
  } else {
    throwError("Cannot subtract %s and %s together", decode_string(Type_of(list(1, a))), decode_string(Type_of(list(1, b))));
  }
  return unit();
}

VALUE multiply(VALUE a, VALUE b) {
  ValueType type_a = get_type(a);
  ValueType type_b = get_type(b);

  if (type_a == TYPE_INTEGER && type_b == TYPE_INTEGER) {
    return integer(a * b);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_FLOAT) {
    return floating(a * b);
  } else if (type_a == TYPE_INTEGER && type_b == TYPE_FLOAT) {
    return floating(a * b);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_INTEGER) {
    return floating(a * b);
  } else {
    throwError("Cannot multiply %s and %s together", decode_string(Type_of(list(1, a))), decode_string(Type_of(list(1, b))));
    exit(0);
  }
  return unit();
}

VALUE divide(VALUE a, VALUE b) {
  ValueType type_a = get_type(a);
  ValueType type_b = get_type(b);

  if (type_a == TYPE_INTEGER && type_b == TYPE_INTEGER) {
    return integer(a / b);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_FLOAT) {
    return floating(a / b);
  } else if (type_a == TYPE_INTEGER && type_b == TYPE_FLOAT) {
    return floating(a / b);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_INTEGER) {
    return floating(a / b);
  } else {
    throwError("Cannot divide %s and %s together", decode_string(Type_of(list(1, a))), decode_string(Type_of(list(1, b))));
    exit(0);
  }
  return unit();
}
