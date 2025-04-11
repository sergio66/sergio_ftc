
/* pvwrite2 -- write an HDF vdata
 * 
 * returns -1 on error, 0 on success
 *
 * H. Motteler
 * 12 Jan 01
 */

#include <stdio.h>
#include "hdf.h"
#include "pvdefs.h"

int pvwrite2(int32 vdata_id, 	     /* HDF vdata ID		     IN  */
	    int nrec,		     /* number of records in buffer  IN  */
	    char *buf 		     /* output data buffer	     IN  */
	    ) {

  int i, j, k;

  /* write out the vdata buffer */
  k = VSwrite (vdata_id, (uint8 *) buf, nrec, FULL_INTERLACE);
  if (k != nrec) {
    fprintf(stderr, "pvwrite2(): VSwrite bad count = %d\n", k);
    return (-1);
  }

  return (0);
}

