
/* dump the vdata buffer via field list pointers 
 */

#include <stdio.h>
#include "hdf.h"
#include "pvdefs.h"

void dump_vbuf(
	      struct FLIST *flist, 
	      char *buf,
	      int p1[], 
	      int p2[],
	      int n, 
	      char *s
	      ) {
  int i, j;
  double x;

  fprintf(stdout, "------------------------------------------------------\n");
  fprintf(stdout, "%s\n", s);
  fprintf(stdout, "%8s  %8s %8s %8s%8s %8s\n", 
	  "name", "type", "order", "value", "p1", "p2");

  for (i=0; i<n; i++) {
    if (flist[i].htype == 24)
      fprintf(stdout, "%8s %8d %8d %8d %8d %8d\n", 
	      flist[i].fname,
	      flist[i].htype,
	      flist[i].order,
	      j = ((int32 *)buf)[p1[i]],
	      p1[i],
	      p2[i]
	      );
    else
      fprintf(stdout, "%8s %8d %8d %8g %8d %8d\n", 
	      flist[i].fname,
	      flist[i].htype,
	      flist[i].order,
	      x = ((float32 *)buf)[p1[i]],
	      p1[i],
	      p2[i]
	      );
  }
  fprintf(stdout, "------------------------------------------------------\n");
}

