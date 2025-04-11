
/* rtptest2 -- test rtp read (existing) and write
 *
 * reads "test4.hdf"
 * writes "rtptest2.hdf"
 */

#include <stdio.h>
#include <stdlib.h>
#include "hdf.h"

#define RTPDEF
#include "rtp.h"

main (int argc, char *argv[]) {

  double x, y, z;
  int i, j, k, s1, ci;
  int32 file_id;
  int32 vhead_id, vprof_id;
  int hnrec, hnfield, hnattr;
  int pnrec, pnfield, pnattr;
  struct rtp_head head1;
  struct rtp_prof prof1[48];
  struct FLIST (*hflist)[], (*pflist)[];
  struct ALIST (*halist)[], (*palist)[];
  struct ALIST halist1[8], palist1[8];
  int npro;

  fprintf(stdout, "============ read test ===========\n");

  s1 = rtpread1("test4.hdf", 
	   &head1,
	   &halist, &hnattr,
	   &palist, &pnattr, 	    
	   &ci);
  if (s1 == -1) exit(-1);

  for (i=0; i<48; i++) {
    s1 = rtpread2(ci, &prof1[i]);
    if (s1 == -1) exit(-1);
  }

  dump_chan(ci);

  rtpclose1(ci);

  dump_attrs(halist, hnattr, "rtptest() header alist dump");
  dump_attrs(palist, pnattr, "rtptest() profile alist dump");

  fprintf(stdout, "============ write test ===========\n");

  s1 = rtpwrite1("rtptest2.hdf", 
	    &head1,
	    halist, hnattr, 	    
	    palist, pnattr, 	    
	    &ci);
  if (s1 == -1) exit(-1);

  printf("calling rtpwrite1\n");
  for (i=0; i<48; i++) {
    s1 = rtpwrite2(ci, &prof1[i]);
    if (s1 == -1) exit(-1);
  }

  rtpclose1(ci);

  dump_attrs(halist, hnattr, "rtptest() header alist dump");
  dump_attrs(palist, pnattr, "rtptest() profile alist dump");


  return(0);
}
