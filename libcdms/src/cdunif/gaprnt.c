#include <stdlib.h>
#include "grads.h"
/*---
  dump this routine defined in gauser.c but needed for linking by the GrADS routines
---*/

void gaprnt (int i, char *ch) {
  printf ("%s",ch);
}

/* Query env symbol */

char *gxgsym(char *ch) {
  return (getenv(ch));
}
