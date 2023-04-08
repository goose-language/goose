#include <stdio.h>
#include "color.h"

void throwError(char* message) {
  printf("%s[ERROR]%s ", BRED, COLOR_RESET);
  printf(message);
  printf("\n");
  
  exit(0);
}