
/* pvread2 -- read an HDF vdata, after the pvread1 setup
 * 
 * returns -1 on errors, number of records read on success
 *
 * H. Motteler
 * 12 Jan 01
 */

#include <stdio.h>
#include <stdlib.h>
#include "hdf.h"
#include "pvdefs.h"

int pvread2(int32 vdata_id,	/* HDF vdata ID		      IN   */
	    int n_records,	/* number of records   	      IN   */
	    char *buf		/* output data buffer	      OUT  */
	    ) {

  int i, j, k;

  /* read the entire vdata into buf */
  k = VSread(vdata_id, (uint8 *) buf, n_records, FULL_INTERLACE);
  if (k != n_records) {
    /* fprintf(stderr, "pvread2(): VSread got %d record(s)\n", k); */
    return (-1);
  }

  return (k);
}

