
/* fmatch -- match vdata and struture buffer fields
 *
 * An FLIST is an array of records describing fields in a buffer.
 * The records contain field name, HDF data type, and the number
 * of elements in a field.
 * 
 * fmatch takes a source flist f1 and a destination flist f2 and
 * looks for matching field names.  It returns three vectors p1, p2,
 * and p3.  p1 is field offsets in the f1 buffer, p2 is offsets for
 * the matching f1 fields in the f2 buffer, and p3 is the number of
 * bytes to copy.  fmatch also returns the size (in bytes) of the
 * f1 and f1 buffers.
 *
 * flist 1 would normally be the buffer with the shorter field list,
 * i.e., the vdata buffer, since we have to step thru the list to do
 * the buffer copy, which is done for each read.  (fmatch is called
 * only to set up buffer pointers when files are opened, not for each
 * read or write.)
 *
 * Here is an example with two disjoint flists, with the arrays p1,
 * p2 and p3 shown after a call to fmatch.
 * 
 *            flist 1                    flist 2       
 *    name     type    order     name     type    order
 *    ------   -----   -----     ------   -----   -----
 *    fish     INT32     2       fish     INT32     2  
 *    berry    INT32     1       monkey   INT32     3  
 *    turnip   FLOAT32   4       turnip   FLOAT32   4  
 *    monkey   FLOAT32   4       cheese   INT32     3
 *                               beer     FLOAT32   4
 *     p1    p2    p3    
 *    ----  ----  ----   * p1 is the start of the field in buffer 1,
 *      0     0     8      normally the vdata buffer
 *      8    BAD    0    * p2 is the start of the matching field in
 *      12   20    20      in buffer 2, normally the structure buffer
 *      28    8    20    * p3 is the number of bytes to copy
 *
 * H. Motteler
 * 18 Mar 02
 * */

#include <stdlib.h>
#include "rtp.h"

void fmatch(
	    struct FLIST (*f1)[],  /* vdata field list	  	      IN  */
	    int n1,		   /* number of vdata fields 	      IN  */
	    struct FLIST (*f2)[],  /* structure field list   	      IN  */
	    int n2,		   /* number of structure fields      IN  */
	    int **pp1,		   /* match offsets in vdata buffer   OUT */
	    int **pp2,		   /* match offsets in struct buffer  OUT */
	    int **pp3,		   /* match sizes (in bytes)          OUT */
	    int *s1,		   /* size of vdata buffer 	      OUT */
	    int *s2		   /* size of structure buffer	      OUT */
	    ) {

  int i, j, k;
  int *p1, *p2, *p3, *q2, *q3;
  extern int hsize();

  /* get space for returned arrays */
  *pp1 = p1 = (int *) malloc(n1 * sizeof(int));
  *pp2 = p2 = (int *) malloc(n1 * sizeof(int));
  *pp3 = p3 = (int *) malloc(n1 * sizeof(int));

  /* get space for working arrays */
  q2 = (int *) malloc(n2 * sizeof(int));
  q3 = (int *) malloc(n2 * sizeof(int));

  /* get offsets in FLIST 1, the vdata buffer */
  for (i=0,j=0; i < n1; i++) {
    p1[i] = j;						/* field offset  */
    p3[i] = (*f1)[i].order * hsize((*f1)[i].htype);  	/* field size    */
    j += p3[i];
  }
  *s1 = j;

  /* get offsets in FLIST 2, the structure buffer */
  for (i=0,j=0; i < n2; i++) {
    q2[i] = j;						/* field offset  */
    q3[i] = (*f2)[i].order * hsize((*f2)[i].htype);  	/* field size 	 */
    j += q3[i];

  }
  *s2 = j;

  /* match fields -- for each field in the vdata buffer f1, look for
   * a matching field in the structure buffer f2.  When a match is
   * found, set p2 to the matching field q2 in f2, and set p3 to the
   * number of bytes to copy.  If the f1 (vdata) is field is bigger
   * than the f2 (struct) field, truncate it to fit.  If no match is
   * found, set p3 to zero and p2 to BAD. */
  for (i=0; i < n1; i++) {
    for (j=0,k=0; j < n2; j++)
      if (strcmp((*f1)[i].fname, (*f2)[j].fname)==0) {
	p2[i] = q2[j];
	if (p3[i] > q3[j]) {
	  p3[i] = q3[j];
	  fprintf(stderr, "fmatch(): WARNING -- truncating field %s\n",
		  (*f1)[i].fname);
	}
	k = 1; /* flag a match */
      }
    if (k == 0) {
      p2[i] = BAD;
      p3[i] = 0;
    }
  }
  /* free up working arrays */
  free(q2);
  free(q3);
}

