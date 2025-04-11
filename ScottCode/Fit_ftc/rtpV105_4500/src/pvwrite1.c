
/* pvwrite1 -- vdata write setup
 * 
 *  - attach a new vdata to the HDF file_id
 *  - assigns it the name vname
 *  - uses the FLIST to set the vdata fields
 *  - writes the attributes from the ALIST
 *  - returns the new vdata ID in *vdata_idp
 *  - return -1 on error, 0 on success
 *
 * H. Motteler
 * 12 Jan 01
 */

#include <stdio.h>
#include "hdf.h"
#include "pvdefs.h"

int pvwrite1(char *vname, 	      /* HDF vdata name 	     IN  */
	     int32 file_id, 	      /* HDF vdata file ID	     IN  */
	     struct FLIST (*flist)[], /* field names, types, sizes   IN  */
	     int nfield,	      /* number of fields in record  IN  */
	     struct ALIST (*alist)[], /* attribute names and values  IN  */
	     int nattr,		      /* number of attributes	     IN  */
	     int32 *vdata_idp	      /* HDF vdata ID	 	     OUT */
	     ) {

  char fnames[MAXFLIST];
  int i, j, k;

  int32 s1, field_index, vdata_id;

  /* attach to a new vdata */
  vdata_id = VSattach (file_id, -1, "w");
  if (vdata_id == -1) {
    fprintf(stderr, "pvwrite1(): VSattach to create vdata failed\n");
    return (-1);
  }

  /* assign the vdata a name */
  s1 = VSsetname (vdata_id, vname);
  if (s1 == -1) {
    fprintf(stderr, "pvwrite1(): VSsetname failed\n");
    return (-1);
  }

  /* assign the vdata a class */
  s1 = VSsetclass (vdata_id, "RTP data");
  if (s1 == -1) {
    fprintf(stderr, "pvwrite1(): VSsetclass failed\n");
    return (-1);
  }

  /* define the vdata fields */
  *fnames = '\0';
  for (i = 0; i < nfield; i++) {
    if (VSfdefine (vdata_id, 
		   (*flist)[i].fname, 
		   (*flist)[i].htype, 
		   (*flist)[i].order) == -1) {
      fprintf(stderr, "pvwrite1(): VSfdefine failed\n");
      fprintf(stderr, "pvwrite1(): name=%s type=%d order=%d\n",
	      (*flist)[i].fname, 
	      (*flist)[i].htype,
	      (*flist)[i].order);
      return (-1);
    }
    if (i != 0) strcat(fnames, ",");
    strcat(fnames, (*flist)[i].fname);
  }

  /* assign the defined fields */
  s1 = VSsetfields (vdata_id, fnames);
  if (s1 == -1) {
    fprintf(stderr, "pvwrite1(): VSsetfields failed\n");
    return (-1);
  }

  /* write vdata attributes from alist */
  for (i=0; i < nattr; i++ ) {
    if (strcmp((*alist)[i].fname, vname) == 0) {
      field_index = -1;
    }
    else {
      s1 = VSfindex (vdata_id, (*alist)[i].fname, &field_index);
      if (s1 == -1) {
	fprintf(stderr, 
		"pvwrite1(): WARNING -- no match for attribute field '%s'\n",
		(*alist)[i].fname);
	/*
	fprintf(stderr, 
		"pvwrite1(): fname=%s, aname=%s, atext=%s'\n",
		(*alist)[i].fname, (*alist)[i].aname, (*alist)[i].atext);
	*/
	/* dump_flist(flist, nfield, "field list"); */
	/* dump_attrs(alist, nattr, "attribute list"); */
	/* return (-1); */
	continue; 
      }
    }
    s1 = VSsetattr (vdata_id, field_index,
		    (*alist)[i].aname, 
		    DFNT_CHAR,
		    strlen((*alist)[i].atext) + 1,
		    (*alist)[i].atext);
    if (s1 == -1) {
      /* fprintf(stderr, "pvwrite1(): VSsetattr failed\n"); */
      fprintf(stderr, 
	      "pvwrite1(): WARNING -- could not set attribute '%s'\n",
	      (*alist)[i].aname);
      fprintf(stderr, 
	      "pvwrite1(): fname=%s, aname=%s, atext=%s\n",
	      (*alist)[i].fname, (*alist)[i].aname, (*alist)[i].atext);
      fprintf(stderr, 
	      "pvwrite1(): maybe '%s' is not unique for '%s' ???\n",
	      (*alist)[i].aname, (*alist)[i].fname);
      /* return (-1); */
      continue;
    }
  }

  *vdata_idp = vdata_id;
  return (0);
}

