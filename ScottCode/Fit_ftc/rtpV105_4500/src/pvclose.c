
/* pvclose -- stop the V interface and close the vdata file
 * 
 * returns -1 on error, 0 on success
 *
 * H. Motteler
 * 20 Dec 00
 */

#include "hdf.h"

int32 pvclose(int32 file_id      /* HDF vdata file ID	 IN  */
	      )
{
  int32 status = 0;
  
  /* end vgroup interface access
   */
  status = Vend(file_id);
  if (status == -1) {
    fprintf(stderr, "pvclose(): Vend failed\n");
    return (status);
  }

  /* close the HDF file
   */
  status = Hclose(file_id);
  if (status == -1) {
    fprintf(stderr, "pvclose(): Hclose failed\n");
    return (status);
  }

  return (0);
}

