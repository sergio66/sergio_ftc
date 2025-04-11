
/* pv ("profile vdata") function prototypes
 */

int32 pvopen(char *hfile, char *mode);

int32 pvclose(int32 file_id);

int pvread1(char *vname,              /* HDF vdata name               IN   */
           int32 file_id,             /* HDF vdata file ID            IN   */
           struct FLIST (**flistp)[], /* field names, types, sizes    OUT  */
           int *nfieldp,              /* number of fields in record   OUT  */
           struct ALIST (**alistp)[], /* attribute names and values   OUT  */
           int *nattrp,               /* number of attributes         OUT  */
           int *nrecp,                /* number of records in file    OUT  */
           int32 *vdata_idp           /* HDF vdata ID                 OUT */
	    );

int pvread2(int32 vdata_id,           /* HDF vdata ID               IN   */
            int n_records,            /* number of records          IN   */
            char *buf                 /* output data buffer         OUT  */
            );

int pvread3(int32 vdata_id);

int pvwrite1(char *vname,             /* HDF vdata name              IN  */
             int32 file_id,           /* HDF vdata file ID           IN  */
             struct FLIST (*flist)[], /* field names, types, sizes   IN  */
             int nfield,              /* number of fields in record  IN  */
             struct ALIST (*alist)[], /* attribute names and values  IN  */
             int nattr,               /* number of attributes        IN  */
             int32 *vdata_idp         /* HDF vdata ID                OUT */
             );

int pvwrite2(int32 vdata_id,          /* HDF vdata ID                 IN  */
	     int nrec,                /* number of records in buffer  IN  */
	     char *buf                /* output data buffer           IN  */
	     );

int pvwrite3(int32 vdata_id);

