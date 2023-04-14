#include <stdio.h>
#include "color.h"
#include "execinfo.h"

void throwError(char* message) {
  printf("%s[ERROR]%s ", BRED, COLOR_RESET);
  printf(message);
  printf("\n");
  
  void* callstack[10];
  int i, frames = backtrace(callstack, 10);

  char** strs = backtrace_symbols(callstack, frames);
  for (i = 0; i < frames; ++i) {
    printf("%s\n", strs[i]);
  }
  free(strs);

  exit(0);
}