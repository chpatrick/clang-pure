#include "stdio.h"

void foo() {
  lol:
  printf("%d", (2 + 2) + (2 * 2));
  goto lol;
}