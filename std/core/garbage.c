
#include "garbage/tgc.h"
#include "garbage.h"

static tgc_t collector;

tgc_t* gc() {
  return &collector;
}