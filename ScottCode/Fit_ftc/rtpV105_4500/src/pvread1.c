
/* pvread1 -- read vdata attributes, setup to read vdata data
 * 
 *  - attach to the vdata "vname"
 *  - initialize & get vdata info
 *  - save vdata field info in an flist
 *  - save vdata attributes in an alist
 *  - returns -1 on errors, 0 on success
 * 
 * H. Motteler
 * 2 Jan 01
 */

#include <stdio.h>
#include <stdlib.h>
#include "hdf.h"
#include "pvdefs.h"

int pvread1(char *vname,	      /* HDF vdata name 	      IN   */
	   int32 file_id, 	      /* HDF vdata file ID 	      IN   */
	   struct FLIST (**flistp)[], /* field names, types, sizes    OUT  */
	   int *nfieldp,	      /* number of fields in record   OUT  */
           struct ALIST (**alistp)[], /* attribute names and values   OUT  */
           int *nattrp,               /* number of attributes         OUT  */
	   int *nrecp,		      /* number of records in file    OUT  */
	   int32 *vdata_idp	      /* HDF vdata ID	 	      OUT  */
	   ) {

  int i, j, k, m, n;
  char *p1, *p2;
  char *buf;			/* output data buffer */
  struct FLIST (*flist)[];	/* field info struct array */
  struct ALIST (*alist)[];	/* field info struct array */

  int32 s1;			/* VS status */
  int32 ref_num;		/* vdata reference number */
  int32 n_records;     	 	/* number of records in the vdata */
  int32 n_fields;	 	/* number of fields in the vdata */
  int32 interlace_mode;	 	/* interlace mode of the vdata */
  int32 vdata_id;
  int32 vdata_size;    	 	/* size of all specified fields */
  char fname_list[MAXFLIST]; 	/* list of all fieldnames */
  char vname2[MAXVNAME];	/* temp vdata name buffer */

  char vattr_name[MAXANAME];	/* vdata attribute name */
  char vattr_buf[MAXATEXT];	/* vdata attribute buffer */
  char *fname;			/* vdata field name pointer */
  int32 field_index;		/* vdata field index */
  int32 n_vdattrs;		/* total number of vdata attributes */
  int32 nfldattrs;		/* number of attributes, current field */
  int32 attr_index;		/* attribute index, current field */
  int32 attr_type;		/* attribute data type */
  int32 attr_size;		/* length of attribute */
  int32 attr_n_values;		/* number of attribute values */

  /* --------------------------- *
   * initialize & get vdata info *
   * --------------------------- */

  /* look up the vdata ref number by vdata name */
  ref_num = VSfind(file_id, vname);
  if (ref_num == -1) {
    fprintf(stderr, "pvread1(): VSfind of header vdata failed\n");
    return (-1);
  }

  /* attach to the vdata, get the ID */
  vdata_id = VSattach (file_id, ref_num, "r");
  if (vdata_id == -1) {
    fprintf(stderr, "pvread1(): VSattach to read vdata failed\n");
    return (-1);
  }

  /* get info about the entire vdata */
  s1 = VSinquire(vdata_id, &n_records, &interlace_mode, 
		 fname_list, &vdata_size, vname2);
  if (s1 == -1) {
    fprintf(stderr, "pvread1(): VSinquire failed\n");
    fprintf(stderr, "vdata_id = %d, n_records = %d, interlace_mode = %d\n",
	    vdata_id, n_records, interlace_mode);
    fprintf(stderr, "vname = %s, vdata_size = %d\n", vname2, vdata_size);
    fprintf(stderr, "fname_list = %s\n", fname_list);
    return (-1);
  }

  n_fields = VSgetfields(vdata_id, fname_list);

  /*
  fprintf(stderr, "%s vdata: %d bytes, %d fields, %d records, %s interlace\n",
          vname, vdata_size, n_fields, n_records, 
	  interlace_mode == FULL_INTERLACE ? "full" : "no");
  fprintf(stderr, "%s fields: %s\n", vname, fname_list);
  */

  /* --------------------------------- *
   * get vdata field info, build flist *
   * --------------------------------- */

  /* loop on vdata fields, build field info structure */
  flist = (struct FLIST (*)[]) malloc(n_fields * sizeof(struct FLIST));
  *flistp = flist;

  for (i = 0; i < n_fields; i++) {
    p1 = VFfieldname(vdata_id, i);
    p2 = (char *) malloc(strlen(p1) + 1);
    strcpy(p2, p1);
    (*flist)[i].fname = p2;
    (*flist)[i].htype = VFfieldtype(vdata_id, i);
    (*flist)[i].order = VFfieldorder(vdata_id, i);
    j = VFfieldisize(vdata_id, i);

    /*
    fprintf(stderr, "name %s, type %d, bytes %d, order %d\n",
	   (*flist)[i].fname, (*flist)[i].htype, j, (*flist)[i].order);
    */
  }

  /* assign the fields */
  s1 = VSsetfields (vdata_id, fname_list);
  if (s1 == -1) {
    fprintf(stderr, "pvread1(): VSsetfields failed\n");
    return (-1);
  }

  /* -------------------------------- *
   * return vdata attributes in alist *
   * -------------------------------- */

  n_vdattrs = VSnattrs (vdata_id);
  k = 0;  /* alist attribute index */
  alist = (struct ALIST (*)[]) malloc(n_vdattrs * sizeof(struct ALIST));
  *alistp = alist;

  /*
  fprintf(stderr, "n_vdattrs = %d, n_fields = %d\n", n_vdattrs, n_fields);
  */

  /* loop on fields, look for attr's */
  for (j=-1; j < n_fields; j++) {	

    if (j == -1) {
      /* field index is -1 for "general" vdata attr's */
      fname = vname;
      field_index = -1;
    }
    else {
      /* get the field index for field fname */
      fname = (*flist)[j].fname;
      s1 = VSfindex (vdata_id, fname, &field_index);
      if (s1 == -1) {
	fprintf(stderr, "pvread1(): VSfindex failed\n");
	return (-1);
      }
    }

    /* get number of attributes associated with this field */
    nfldattrs = VSfnattrs (vdata_id, field_index);

    /*
    fprintf(stderr, "pvread1(): field %s, nfldattrs %d\n", fname, nfldattrs);
    */

    for (attr_index = 0; attr_index < nfldattrs; attr_index++) {

      /* get assorted attribute info (everything but attr value) */
      s1 = VSattrinfo (vdata_id, field_index, attr_index,
		       vattr_name,  &attr_type, 
		       &attr_n_values, &attr_size);
      if (s1 == -1) {
	fprintf(stderr, "pvread1(): VSattrinfo failed\n");
	return (-1);
      }
      if (attr_size > MAXATEXT) {
	fprintf(stderr, 
		"pvread1(): attribute %s length %d exceeds MAXATEXT\n",
		vattr_name, attr_size);
	return (-1);
      }

      /* get attribute value */
      s1 = VSgetattr (vdata_id, field_index, attr_index, vattr_buf);
      if (s1 == -1) {
	fprintf(stderr, "pvread1(): VSgetattr failed\n");
	return (-1);
      }
 
      /* terminate the attribute value string */
      m = attr_size >= MAXATEXT ? MAXATEXT-1 : attr_size;
      vattr_buf[m] = '\0';

      /*
      fprintf(stderr, "pvread1(): attr_n_values=%d attr_size=%d\n",
	      m = attr_n_values, n = attr_size);
      fprintf(stderr, "pvread1(): %s | %s | %s\n", 
	      fname, vattr_name, vattr_buf);
      */

      /* copy the attributes out */
      (*alist)[k].fname = (char *) malloc(strlen(fname)+1);
      strcpy((*alist)[k].fname, fname);
      (*alist)[k].aname = (char *) malloc(strlen(vattr_name)+1);
      strcpy((*alist)[k].aname, vattr_name);
      (*alist)[k].atext = (char *) malloc(strlen(vattr_buf)+1);
      strcpy((*alist)[k].atext, vattr_buf);
      k++;
    }
  }

  /* ---------------------*
   * finish up and return *
   * -------------------- */

  /* copy out record and field counts */
  *nfieldp   = n_fields;
  *nattrp    = n_vdattrs;
  *nrecp     = n_records;
  *vdata_idp = vdata_id;

  return (0);
}

