
#include <stdio.h>
#include "pvdefs.h"

void dump_flist(struct FLIST (*flist)[], int n, char *s) {
  int i;

  fprintf(stdout, "------------ FLIST dump ---------------\n");
  fprintf(stdout, "%s\n", s);
  fprintf(stdout, "%8s  %8s  %8s\n", "name", "type", "order");

  for (i=0; i<n; i++) {
    fprintf(stdout, "%8s %8d %8d\n", 
	    (*flist)[i].fname,
	    (*flist)[i].htype,
	    (*flist)[i].order);
  }
  fprintf(stdout, "---------------------------------------\n");

}

