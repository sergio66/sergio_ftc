
/* gsplit -- expand gamnt in an FLIST to a gas_<n> sublist
 *
 * gsplit takes an FLIST pointing to a rtp profile structure 
 * and replaces the "gamnt" field which points to a 2d array
 * with ngas "gas_<i>" fields corresponding to the ngas elements 
 * of the gas ID list glist[], set to point of columns of gamnt.
 * 
 */

#include "rtp.h"

void gsplit(
	    int32 ngas,			/* number of gasses	IN  */
	    int32 glist[],		/* list of gasses	IN  */
	    int nflist1,		/* input FLIST size	IN  */
	    struct FLIST (*flist1)[],	/* input FLIST		IN  */
	    int *nflist2,		/* output FLIST size    OUT */
	    struct FLIST (*flist2)[]	/* output FLIST		OUT */
	    ) {

  int i, j, k;
  char *p;

  /* loop on fields of the input FLIST
   */
  for (i=0,j=0; i < nflist1; i++) {

    if (strcmp((*flist1)[i].fname, "gamnt") == 0) {

      /* expand gamnt to ngas gas_<i> fields 
       */
      for (k=0; k < ngas; k++) {
	p = (char *) malloc(8);
	sprintf(p, "gas_%d", glist[k]);
	(*flist2)[j].fname = p;
	(*flist2)[j].htype = DFNT_FLOAT32;
	(*flist2)[j].order = MAXLEV;
	j++;
      }
      if (k < MAXGAS) {
	p = (char *) malloc(8);
	(*flist2)[j].fname = "gas_xx";
	(*flist2)[j].htype = DFNT_FLOAT32;
	(*flist2)[j].order = MAXLEV * (MAXGAS - k);
	j++;
      }
    }
    else {

      /* just copy the flist record 
       */
      (*flist2)[j].fname = (*flist1)[i].fname;
      (*flist2)[j].htype = (*flist1)[i].htype;
      (*flist2)[j].order = (*flist1)[i].order;
      j++;

    }
  }
  *nflist2 = j;
}
