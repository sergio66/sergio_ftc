
/* pvopen -- open an HDF vdata file, start the VS interface
 * 
 * returns an HDF file ID, or -1 on errors
 *
 * H. Motteler
 * 20 Dec 00
 */

#include <stdio.h>
#include "hdf.h"

int32 pvopen(char *hfile,  /* HDF Vdata file name   	IN  */
	     char *mode	   /* "c"=create, "r"=read  	IN  */
	     )
{
  int32 file_id, status;

  if (*mode == 'c') {
    
    file_id = Hopen (hfile, DFACC_CREATE, 0);
    if (file_id == -1) {
      fprintf(stderr, "pvopen(): can't create %s\n", hfile);
      return (-1);
    }
  }
  else if (*mode == 'r') {
      
    file_id = Hopen (hfile, DFACC_READ, 0);
    if (file_id == -1) {
      fprintf(stderr, "pvopen(): can't open %s\n", hfile);
      return (-1);
    }
  }
  else {
    fprintf(stderr, "pvopen(): bad open mode %s\n", mode);
    return (-1);
  }

  status = Vstart (file_id);
  if (status == -1) {
    fprintf(stderr, "pvopen(): vstart failed\n");
    return (-1);
  }

  return (file_id);

}
