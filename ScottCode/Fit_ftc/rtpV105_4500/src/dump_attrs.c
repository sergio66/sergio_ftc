
#include <stdio.h>
#include "pvdefs.h"


void dump_attrs(struct ALIST (*alist)[], int n, char *s) {
  int i;

  fprintf(stdout, "----- attribute dump -----------\n");
  fprintf(stdout, "%s\n", s);
  fprintf(stdout, "%8s   %8s   %8s\n", "owner", "name", "value");

  for (i=0; i<n; i++) {
    fprintf(stdout, "%8s   %8s   %8s\n", 
	    (*alist)[i].fname,
	    (*alist)[i].aname,
	    (*alist)[i].atext);
  }
  fprintf(stdout, "--------------------------------\n");

}

