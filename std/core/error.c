#include <stdio.h>
#include "color.h"
#include "execinfo.h"
#include <stdarg.h>

void throwError(char* message, ...) {
  printf("%s[ERROR]%s ", BRED, COLOR_RESET);
  
  va_list args;
  va_start(args, message);
  vprintf(message, args);
  va_end(args);

  printf("\n");
  
  void* callstack[10];
  int i, frames = backtrace(callstack, 10);
  printf(HBLK);
  char** strs = backtrace_symbols(callstack, frames);
  for (i = 0; i < frames; ++i) {
    printf("  %s\n", strs[i]);
  }
  printf(COLOR_RESET);
  free(strs);

  exit(0);
}