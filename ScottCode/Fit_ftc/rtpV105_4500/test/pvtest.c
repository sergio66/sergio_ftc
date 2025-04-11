
/* pvtest -- test pvread and pvwrite family of functions
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include "hdf.h"

#define RTPDEF
#include "rtp.h"
#include "pvfnx.h"


main (int argc, char *argv[]) {

  double x, y, z;
  int i, j, k, s1;
  int32 file_id;
  int32 vhead_id, vprof_id;
  int hnrec, hnfield, hnattr;
  int pnrec, pnfield, pnattr;
  struct rtp_head head1, head2;
  struct rtp_prof prof1[4], prof2[4];
  struct FLIST (*hflist)[], (*pflist)[];
  struct ALIST (*halist)[], (*palist)[];
  struct ALIST halist1[8], palist1[8];

  int npro = 2;

  /* initialize header and profile structures
   */
  headinit(&head1);
  headinit(&head2);
  for (i = 0; i < npro; i++) {
    profinit(&prof1[i]);
    profinit(&prof2[i]);
  }

  /* fill in profile structures with some plausible data 
   */
  head1.ptype = 0;	/* level profile */
  head1.pfields = 3;	/* profile with calculated radiances */

  head1.pmin  = 10;
  head1.pmax  = 1000;
  head1.ngas  = MAXGAS;
  for (i=0; i<MAXGAS; i++) 
    head1.glist[i] = i + 1;
  head1.nchan = MAXCHAN;
  for (i=0; i<MAXCHAN; i++) {
    head1.vchan[i] = i + 600;
    head1.ichan[i] = i;
  }

  for (k=0; k < npro; k++) {
    prof1[k].plat = 32;
    prof1[k].plon = 55;
    prof1[k].ptime = 30000;
    prof1[k].stemp = 290;
    for (i=0; i<MAXRHO; i++) {
      prof1[k].rho[i] = i / 1.0;
      prof1[k].rfreq[i] = i*200 + 600;
    }
    for (i=0; i<MAXEMIS; i++) {
      prof1[k].emis[i] = 1.0 - i / 1.0;
      prof1[k].efreq[i] = i*200 + 600;
    }
    prof1[k].salti = 10;
    prof1[k].spres = 1000;

    prof1[k].nlevs = MAXLEV;

    for (j=0; j < MAXLEV; j++) {
      prof1[k].plevs[j] = j * 10;
      prof1[k].ptemp[j] = 200 + j;
    }

    for (i=0; i<MAXGAS; i++)
      for (j=0; j < MAXLEV; j++)
	prof1[k].gamnt[i][j] = (i+1) * 1000 + j + 1;

    prof1[k].scanang = 45;
    prof1[k].satzen = 42;

    for (i=0; i<MAXCHAN; i++)
      prof1[k].rcalc[i] = i / 10.0;

    strcpy((char *) prof1[k].pnote, "test comment string");
    prof1[k].udef[10] = 997;
    prof1[k].udef1 = 998;
    prof1[k].udef2 = 999;
  }

  halist1[0].fname = "header";
  halist1[0].aname = "title";
  halist1[0].atext = "attribute test file";
  halist1[1].fname = "ngas";
  halist1[1].aname = "units";
  halist1[1].atext = "(count)";
  halist1[2].fname = "\0";

  palist1[0].fname = "profiles";
  palist1[0].aname = "title";
  palist1[0].atext = "profile attribute test";
  palist1[1].fname = "gamnt";
  palist1[1].aname = "units";
  palist1[1].atext = "PPMV";
  palist1[2].fname = "\0";

  fprintf(stdout, "============ write test ===========\n");

  file_id = pvopen("pvtest.hdf", "c");

  pvwrite1("header",   file_id, 
	   &hfield, NHFIELD, 
	   &halist1, 2, 
	   &vhead_id);

  pvwrite1("profiles", file_id, 
	   &pfield, NPFIELD, 
	   &palist1, 2, 
	   &vprof_id);

  pvwrite2(vhead_id, 1, (char *) &head1);

  /*
  pvwrite2(vprof_id, 1, (char *) &prof1[0]);
  pvwrite2(vprof_id, 1, (char *) &prof1[1]);
  */
  pvwrite2(vprof_id, 2, (char *) &prof1[0]);


  pvwrite3(vhead_id);
  pvwrite3(vprof_id);
  pvclose(file_id);


  fprintf(stdout, "============ read test ============\n");

  file_id = pvopen("pvtest.hdf", "r");

  pvread1("header", file_id, 
          &hflist, &hnfield, 
	  &halist, &hnattr, 
	  &hnrec, &vhead_id);

  dump_flist(hflist, hnfield, "pvtest() header flist dump");
  dump_attrs(halist, hnattr, "pvtest() header alist dump");

  pvread1("profiles", file_id,
	  &pflist, &pnfield, 
	  &palist, &pnattr, 
	  &pnrec, &vprof_id);

  dump_flist(pflist, pnfield, "pvtest() profile flist dump");
  dump_attrs(palist, pnattr, "pvtest() profile alist dump");

  pvread2(vhead_id, 1, (char *) &head2);

  /*
  pvread2(vprof_id, 1, (char *) &prof2[0]);
  pvread2(vprof_id, 1, (char *) &prof2[1]);
  */
  pvread2(vprof_id, 2, (char *) &prof2[0]);

  dump_pstr(&head2, &prof2);

  pvread3(vprof_id);
  pvread3(vhead_id);
  pvclose(file_id);

  return(0);
}
