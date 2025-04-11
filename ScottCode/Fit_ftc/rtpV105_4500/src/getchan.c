
#include <stdio.h>
#include "rtp.h"

int getchan() {

  int ci;

  for (ci=0; ci < MAXOPEN; ci++)
    if (oflag[ci] == 0) {
      oflag[ci] = 1;
      break;
    }
  if (ci == MAXOPEN) {
    fprintf(stderr, "getchan(): no free channels\n");
    return (-1);
  }
  return(ci);
}

