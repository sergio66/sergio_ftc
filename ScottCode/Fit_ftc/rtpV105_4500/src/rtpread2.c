
/* rtpread2 -- read HDF vdata into struct buffer
 *
 * returns -1 on errors, number of records read on success
 *
 */

#include "hdf.h"
#include "rtp.h"

int rtpread2(int ci,		/* RTP channel 		IN */
	     char *sbuf		/* structure buffer 	IN */
	     ) {

  int i, j, k;
  int s1, nvf;
  int32 vdata_id;
  char *q1, *q2;
  int *p1, *p2, *p3;

  p1 = chan[ci].p1;
  p2 = chan[ci].p2;
  p3 = chan[ci].p3;
  nvf = chan[ci].nvf;
  vdata_id = chan[ci].vdata_id;

  q1 = chan[ci].vbuf;	/* vdata buffer */
  q2 = sbuf;  	    	/* structure buffer */

  s1 = pvread2(vdata_id, 1, q1);
  if (s1 == -1) return (s1);

  /* copy vdata fields from  vdata buffer to structure */
  for (i=0; i < nvf; i++)
    for (j=0; j < p3[i]; j++)
      q2[p2[i]+j] = q1[p1[i]+j];

  /* dump_chan(ci); */

  return(s1);
}

