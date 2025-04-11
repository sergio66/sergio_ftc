
/* dump_chan -- dump RTP channel information
 * 
 */

#include <stdio.h>
#include "hdf.h"
#include "rtp.h"

void dump_chan(int ci) {

  int i, j, k;
  float64 x;
  char c, *p, *q, s1[128], s2[128];

  int nvf;
  int32 vdata_id;
  char *buf;
  int *p1, *p2, *p3;
  struct FLIST (*flist)[];

  p1 = chan[ci].p1;
  p2 = chan[ci].p2;
  p3 = chan[ci].p3;
  nvf = chan[ci].nvf;
  vdata_id = chan[ci].vdata_id;

  buf = chan[ci].vbuf;  /* vdata buffer */
  flist = chan[ci].flist;

  fprintf(stdout, 
	  "---------------------------------------------------------\n");
  fprintf(stdout, "\t\t    channel %d dump\n", ci);
  fprintf(stdout, "%8s  %7s %7s %8s%6s %6s %6s\n", 
	  "name", "type", "order", "value", "p1", "p2", "p3");

  for (i=0; i < nvf; i++) {

    /* copy contents of buf to a word-aligned string */
    p = s1;
    q = &buf[p1[i]];
    k = hsize((*flist)[i].htype);
    for (j=0; j<k; j++) *p++ = *q++;

    if ((*flist)[i].htype == DFNT_INT32) {
      x = *((int32 *) s1);
      sprintf(s2, "%8g", x);
    }
    else if ((*flist)[i].htype == DFNT_FLOAT32) {
      x = *((float32 *) s1);
      sprintf(s2, "%8g", x);
    }
    else if ((*flist)[i].htype == DFNT_FLOAT64) {
      x = *((float64 *) s1);
      sprintf(s2, "%8g", x);
    }
    else if ( strcmp((*flist)[i].fname, "calflag") == 0 ) {
      x = *((uchar8 *) s1);
      sprintf(s2, "%8g", x);
    }
    else if ( (*flist)[i].htype == DFNT_CHAR8 ) {
      strncpy(s2, &buf[p1[i]], 8);
      s2[8] = 0;
    }
    else {
      fprintf(stderr, "dump_chan(): unknown HDF type %d\n", 
	      (*flist)[i].htype);
      exit(-1);
    }

    fprintf(stdout, "%8s %7d %7d %8s %6d %6d %6d\n", 
	    (*flist)[i].fname,
	    (*flist)[i].htype,
	    (*flist)[i].order,
	    s2, p1[i], p2[i], p3[i]);
  }
  fprintf(stdout, 
	  "---------------------------------------------------------\n");
}

