
/* rtpsize -- structure sizes and basic sanity checks
 *
 * USAGE
 *
 *   rtpsize
 *
 * OPTIONS
 *
 *   none
 *
 * BUGS
 *
 *   needs the first and last structure field names hard-coded
 *   to find the true structure size
 */

#include <stdio.h>
#include <stdlib.h>
#include "hdf.h"
#define RTPDEF
#include "rtp.h"

struct rtp_prof testprof;
struct rtp_head testhead;

main () {

  int i, j, k;
  int hfsize, pfsize, hspan, pspan;

  /* sanity check that flist and structure sizes match, note that
     we need the first and last fields here to calculate the actual
     size of the structure */

  /* header flist size */
  for (i = 0, j=0; i < NHFIELD; i++)
    j += hfield[i].order * hsize(hfield[i].htype);
  hfsize = j;

  /* header structure spanning size */
  hspan = (char *)&(testhead.udef2) - (char *)&(testhead.ptype) + 4;

  if (hfsize != hspan) {
    fprintf(stdout,
	    "ERROR: header flist size does not match structure span!\n");
    fprintf(stdout,
	    "ERROR: header flist size = %d, header structure span = %d\n",
	    hfsize, hspan);
  }

  /* profile flist size */
  for (i = 0, j=0; i < NPFIELD; i++)
    j += pfield[i].order * hsize(pfield[i].htype);
  pfsize = j;

  /* profile structure spanning size */
  pspan = (char *)&(testprof.udef2) - (char *)&(testprof.plat) + 4;

  if (pfsize != pspan) {
    fprintf(stdout,
	    "ERROR: profile flist size does not match structure span!\n");
    fprintf(stdout,
	    "ERROR: profile flist size = %d, profile structure span = %d\n",
	    pfsize, pspan);
  }

  fprintf(stdout, "the RTP header structure size is %d bytes\n", 
	  i = sizeof(struct rtp_head));

  fprintf(stdout, "the RTP header structure span is %d bytes\n", 
	  hspan);

  fprintf(stdout, "the RTP profile structure size is %d bytes\n", 
	  i = sizeof(struct rtp_prof));

  fprintf(stdout, "the RTP profile structure span is %d bytes\n", 
	  pspan);

  fprintf(stdout, "size of header flist is %d bytes, %d fields\n",
          i = sizeof(hfield),
          i = sizeof(hfield)/(sizeof(char *)+sizeof(int)*2));

  fprintf(stdout, "size of profile flist is %d bytes, %d fields\n",
          i = sizeof(pfield),
          i = sizeof(pfield)/(sizeof(char*)+sizeof(int)*2));

  fprintf(stdout, "static space for attributes is %d bytes\n", 
	  i = sizeof(struct rtpfatt) * MAXNATTR);
}

