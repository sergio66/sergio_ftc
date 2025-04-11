
/* rtpclose1 -- detach and close
 *
 * note: we close the HDF file ID if we were reading or writing
 * profiles, which makes the dubious assumption that this will
 * always be the last step; this is safe enough if rtpread1 was
 * used to do the setup and the header read, but this procedure
 * shouldn't be used for a general purpose close.  
 */

#include <stdlib.h>
#include <stdio.h>
#include "rtp.h"

int rtpclose1(int ci) {

  int i, j, k, cj;
  int32 s1;

  /*
  fprintf(stderr, "rtpclose(): closing channel %d\n", ci);
  */

  /* detach the channel vdata */
  s1 = VSdetach (chan[ci].vdata_id); 
  if (s1 == -1) {
    fprintf(stderr, "rtpclose(): VSsdetach failed\n");
    return (-1);
  }

  /* close if HDF file we are reading or writing profiles */
  if (chan[ci].mode == 3 || chan[ci].mode == 4) {
    /* fprintf(stderr, "closing HDF file ID\n"); */
    pvclose(chan[ci].file_id);    
  }

  /* free the buffer memory */
  free(chan[ci].vbuf);

  /* flag the channel as closed */
  oflag[ci] = 0;

  return 0;
}

