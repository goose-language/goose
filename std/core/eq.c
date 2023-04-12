#include <stdlib.h>
#include "eq.h"

Value* eq(Value* a, Value* b) {
  if (a == NULL && b == NULL) {
    return boolean(1);
  } else if (a == NULL || b == NULL) {
    return boolean(0);
  } else if (a->type == INT && b->type == INT) {
    return boolean(a->i == b->i);
  } else if (a->type == FLOAT && b->type == FLOAT) {
    return boolean(a->f == b->f);
  } else if (a->type == INT && b->type == FLOAT) {
    return boolean(a->i == b->f);
  } else if (a->type == FLOAT && b->type == INT) {
    return boolean(a->f == b->i);
  } else if (a->type == CHAR && b->type == CHAR) {
    return boolean(a->c == b->c);
  } else if (a->type == UNIT && b->type == UNIT) {
    return boolean(1);
  } else if (a->type == BOOL && b->type == BOOL) {
    return boolean(a->b == b->b);
  } else if (a->type == LIST && b->type == LIST) {
    if (a->l.length != b->l.length) {
      return boolean(0);
    }
    for (int i = 0; i < a->l.length; i++) {
      if (!eq(a->l.value[i], b->l.value[i])->b) {
        return boolean(0);
      }
    }
    return boolean(1);
  } else {
    return boolean(0);
  }
}

Value* neq(Value* a, Value* b) {
  return boolean(!eq(a, b)->i);
}

Value* lt(Value* a, Value* b) {
  if (a->type == INT && b->type == INT) {
    return boolean(a->i < b->i);
  } else if (a->type == FLOAT && b->type == FLOAT) {
    return boolean(a->f < b->f);
  } else if (a->type == INT && b->type == FLOAT) {
    return boolean(a->i < b->f);
  } else if (a->type == FLOAT && b->type == INT) {
    return boolean(a->f < b->i);
  } else if (a->type == CHAR && b->type == CHAR) {
    return boolean(a->c < b->c);
  } else {
    return boolean(0);
  }
}

Value* gt(Value* a, Value* b) {
  if (a->type == INT && b->type == INT) {
    return boolean(a->i > b->i);
  } else if (a->type == FLOAT && b->type == FLOAT) {
    return boolean(a->f > b->f);
  } else if (a->type == INT && b->type == FLOAT) {
    return boolean(a->i > b->f);
  } else if (a->type == FLOAT && b->type == INT) {
    return boolean(a->f > b->i);
  } else if (a->type == CHAR && b->type == CHAR) {
    return boolean(a->c > b->c);
  } else {
    return boolean(0);
  }
}

Value* lte(Value* a, Value* b) {
  return boolean(!gt(a, b)->b);
}

Value* gte(Value* a, Value* b) {
  return boolean(!lt(a, b)->b);
}

Value* and_(Value* a, Value* b) {
  return boolean(a->b && b->b);
}

Value* or_(Value* a, Value* b) {
  return boolean(a->b || b->b);
}


