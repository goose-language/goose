#include <stdlib.h>
#include "eq.h"
#include "conversion.h"

VALUE eq(VALUE a, VALUE b) {
  ValueType type_a = get_type(a);
  ValueType type_b = get_type(b);

  if (type_a == TYPE_NULL && type_b == TYPE_NULL) {
    return boolean(1);
  } else if (type_a == TYPE_INTEGER && type_b == TYPE_INTEGER) {
    return boolean(a == b);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_FLOAT) {
    float a_float = decode_floating(a);
    float b_float = decode_floating(b);

    return boolean(a_float == b_float);
  } else if (type_a == TYPE_CHAR && type_b == TYPE_CHAR) {
    char a_char = decode_character(a);
    char b_char = decode_character(b);

    return boolean(a_char == b_char);
  } else if (type_a == TYPE_BOOL && type_b == TYPE_BOOL) {
    int a_bool = decode_boolean(a);
    int b_bool = decode_boolean(b);

    return boolean(a_bool == b_bool);
  } else if (type_a == TYPE_ARRAY && type_b == TYPE_ARRAY) {
    Array a_array = decode_pointer(a)->as_array;
    Array b_array = decode_pointer(b)->as_array;

    if (a_array.length != b_array.length) {
      return boolean(0);
    }

    for (int i = 0; i < a_array.length; i++) {
      int boo = !decode_boolean(eq(a_array.data[i], b_array.data[i]));
      if (boo) {
        return boolean(0);
      }
    }

    return boolean(1);
  } else if (type_a == TYPE_DICT && type_b == TYPE_DICT) {
    Dict a_dict = decode_pointer(a)->as_dict;
    Dict b_dict = decode_pointer(b)->as_dict;

    if (a_dict.length != b_dict.length) {
      return boolean(0);
    }

    for (int i = 0; i < a_dict.length; i++) {
      int boo = !eq(a_dict.values[i], b_dict.values[i]);
      if (boo) {
        return boolean(0);
      }
    }

    return boolean(1);
  } else {
    return boolean(0);
  }
}

VALUE neq(VALUE a, VALUE b) {
  int boo = decode_boolean(eq(a, b));
  return boolean(!boo);
}

VALUE lt(VALUE a, VALUE b) {
  ValueType type_a = get_type(a);
  ValueType type_b = get_type(b);

  if (type_a == TYPE_INTEGER && type_b == TYPE_INTEGER) {
    int a_int = decode_integer(a);
    int b_int = decode_integer(b);

    return boolean(a_int < b_int);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_FLOAT) {
    float a_float = decode_floating(a);
    float b_float = decode_floating(b);

    return boolean(a_float < b_float);
  } else if (type_a == TYPE_CHAR && type_b == TYPE_CHAR) {
    char a_char = decode_character(a);
    char b_char = decode_character(b);

    return boolean(a_char < b_char);
  } else {
    return boolean(0);
  }
}

VALUE gt(VALUE a, VALUE b) {
  ValueType type_a = get_type(a);
  ValueType type_b = get_type(b);

  if (type_a == TYPE_INTEGER && type_b == TYPE_INTEGER) {
    int a_int = decode_integer(a);
    int b_int = decode_integer(b);

    return boolean(a_int > b_int);
  } else if (type_a == TYPE_FLOAT && type_b == TYPE_FLOAT) {
    float a_float = decode_floating(a);
    float b_float = decode_floating(b);

    return boolean(a_float > b_float);
  } else if (type_a == TYPE_CHAR && type_b == TYPE_CHAR) {
    char a_char = decode_character(a);
    char b_char = decode_character(b);

    return boolean(a_char > b_char);
  } else {
    return boolean(0);
  }
}

VALUE lte(VALUE a, VALUE b) {
  int boo = decode_boolean(gt(a, b));
  return boolean(!boo);
}

VALUE gte(VALUE a, VALUE b) {
  int boo = decode_boolean(lt(a, b));
  return boolean(!boo);
}


