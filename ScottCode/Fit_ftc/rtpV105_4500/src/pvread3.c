
/* pvread3 -- wrapper for VSdetach
 * 
 * returns -1 on failure, 0 on success
 *
 * H. Motteler
 * 12 Jan 01
 */

#include <stdio.h>
#include "hdf.h"

int pvread3(int32 vdata_id) {

  int32 s1;

  /* detach the vdata */
  s1 = VSdetach (vdata_id); 
  if (s1 == -1) {
    fprintf(stderr, "pvread3(): VSsdetach failed\n");
    return (-1);
  }

  return(0);
}

