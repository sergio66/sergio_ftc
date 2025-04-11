
/* rtpread1 -- RTP read setup
 *
 * returns -1 on error, 0 on success
 *
 */

#include "rtp.h"
#include "pvfnx.h"
#include "rtpfnx.h"

int rtpread1(char *fname,               /* RTP file name             IN  */
	     struct rtp_head *head,	/* RTP header structure      IN  */
	     struct ALIST (**hatt)[],	/* header attributes 	     OUT */
	     int *nhatt,		/* number of header attr's   OUT */
	     struct ALIST (**patt)[],	/* profile attributes	     OUT */
	     int *npatt,		/* number of profile attr's  OUT */
	     int *pchan			/* RTP profile channel	     OUT */
	     ) {

  int i, j, k;
  int *p1, *p2, *p3, s1, s2;
  int nhvf, npvf, npsf;
  struct FLIST (*hvf)[], (*pvf)[], (*psf)[];
  int pci, hci, *ppci;
  int32 file_id, vhead_id, vprof_id;
  int nrec;
  char *c;

  hci = getchan();	/* header channel */
  pci = getchan();	/* profile channel */
  *pchan = pci;		/* return profile channel */

  /* open the HDF file */
  file_id = pvopen(fname, "r");
  if (file_id == -1) return -1;

  /* -----------------------
   * read the header record 
   * -----------------------
   */

  /* vdata header read setup */
  s1 = pvread1("header", 
	       file_id,
	       &hvf, &nhvf, 
	       hatt, nhatt,
	       &nrec, &vhead_id);

  if (s1 != 0) {
    fprintf(stderr, "rtpread1(): pvread1 header read failed\n");
    return (s1);
  }

  /*
  dump_flist(hvf, nhvf, "rtpread1(): header vdata fields");
  dump_attrs(*hatt, *nhatt, "rtpread1(): header attributes");
  */

  /* get field matchup offsets for the header */
  fmatch(hvf, nhvf, &hfield, NHFIELD, &p1, &p2, &p3, &s1, &s2);

  /* build the header channel record */
  chan[hci].mode = 1; /* header read */
  chan[hci].file_id = file_id;
  chan[hci].vdata_id = vhead_id;
  chan[hci].nvf = nhvf;
  chan[hci].p1 = p1;
  chan[hci].p2 = p2;
  chan[hci].p3 = p3;
  chan[hci].vbuf = (char *) malloc(s1);   /* header vdata buffer */
  chan[hci].flist = hvf;

  /* set fields to defaults before the read  */
  headinit(head);

  /* read the header */
  rtpread2(hci, (char *) head);
  rtpclose1(hci);

  /* rtphcheck does a sanity check of some of the header fields 
   */
  if (rtphcheck(head) != 0)
    fprintf(stderr, "rtpread1(): WARNING -- bad header record\n");

  /* -------------------
   * Profile Read Setup 
   * -------------------
   */

  /* vdata profile read setup */
  s1 = pvread1("profiles", 
	       file_id,
	       &pvf, &npvf, 
	       patt, npatt,
	       &nrec, &vprof_id);

  if (s1 != 0) {
    fprintf(stderr, "rtpread1(): pvread1 profile read failed\n");
    return (s1);
  }

  /* expand the gamnt field in the profile structure FLIST */
  psf = (struct FLIST (*)[]) 
    malloc((NPFIELD+head->ngas) * sizeof(struct FLIST));

  gsplit(head->ngas, head->glist, 
	 NPFIELD, &pfield, 
	 &npsf, psf);

  /*
  dump_flist(&pfield, NPFIELD, "rtpread1(): profile struct fields w/ gamnt");
  dump_flist(psf, npsf, "rtpread1(): profile struct fields w/ gas_n");
  dump_flist(pvf, npvf, "rtpread1(): profile vdata fields");
  dump_attrs(*patt, *npatt, "rtpread1(): profile attributes");
  */

  /* get field matchup offsets for the profiles */
  fmatch(pvf, npvf, psf, npsf, &p1, &p2, &p3, &s1, &s2);

  /* build the profile channel record */
  chan[pci].mode = 3; /* profile read */
  chan[pci].file_id = file_id;
  chan[pci].vdata_id = vprof_id;
  chan[pci].nvf = npvf;
  chan[pci].p1 = p1;
  chan[pci].p2 = p2;
  chan[pci].p3 = p3;
  chan[pci].vbuf = (char *) malloc(s1);    /* profile vdata buffer */
  chan[pci].flist = pvf;                   /* profile field list */


  /* set the header structure max fields to the profile vdata
   * field sizes */
  for (i=0; i < npvf; i++) {
    j = (*pvf)[i].order;
    c = (*pvf)[i].fname;
    if (strcmp(c, "rho")    == 0) head->mrho = j;
    if (strcmp(c, "emis")   == 0) head->memis = j;
    if (strcmp(c, "plevs")  == 0) head->mlevs = j;
    if (strcmp(c, "mwemis") == 0) head->mwmemis = j;
    if (strcmp(c, "mwstb")  == 0) head->mwmstb = j;
  }

  /* return success */
  return(0);
}

