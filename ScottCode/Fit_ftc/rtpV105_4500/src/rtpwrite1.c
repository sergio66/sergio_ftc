
/* rtpwrite1 -- RTP write setup
 * 
 * returns -1 on error, 0 on success
 *
 * rtpwrite1 starts with field lists for the static Fortran header
 * and profile structures, and uses values in the header size fields
 * (mrho, memis, mlevs, ngas, nchan, and also pfields) to build new,
 * generally much more compact, field lists for the header and profile
 * vdata buffers.  Whenever a header size field is zero, the relevant
 * header or profile field is dropped from the vdata.
 * 
 * The structure and vdata buffer field lists are matched up in
 * fmatch, which also generates buffer pointers.  These pointers,
 * the vdata buffers, and some related info are saved in the RTP
 * channel records.
 * 
 * rtpwrite1 then writes the header data, and the header and profile
 * attributes, and returns an RTP channel on which the profiles may
 * be written by rtpwrite2
 * 
 * H. Motteler
 * 12 Jan 01 
 */

#include <stdlib.h>
#include "hdf.h"

#include "rtp.h"
#include "pvfnx.h"
#include "rtpfnx.h"

int rtpwrite1(char *fname,		/* RTP file name 	     IN  */
	      struct rtp_head *head,	/* RTP header structure      IN  */
	      struct ALIST (*hatt)[],	/* header attributes 	     IN  */
	      int nhatt,		/* number of header attr's   IN  */
	      struct ALIST (*patt)[],	/* profile attributes	     IN  */
	      int npatt,		/* number of profile attr's  IN  */
	      int *pchan		/* RTP profile channel	     OUT */
	      ) {

  int i, j, k;
  int s1, s2;		      /* fmatch field counts */
  int *p1, *p2, *p3;	      /* fmatch field pointers */
  struct FLIST hsf[NHFIELD];  /* header struct field list  */
  struct FLIST hvf[NHFIELD];  /* header vdata field list   */
  struct FLIST (*pvf)[];      /* profile vdata field list  */
  struct FLIST (*psf)[];      /* profile struct fields with gas_n */
  int nhvf, npvf, npsf;	      /* number of hvf, pvf, and psf fields */
  int pci, hci;		      /* header and profile channel index */
  int32 vhead_id, vprof_id;
  int32 file_id;
  char gas_n[10];	      /* temp buffer for gas name */

  /* function to test for membership in glist */
  extern int inglist(int a, int32 L[], int n);

  /* get header and profile channels */
  hci = getchan();
  pci = getchan();
  *pchan = pci;

  /* ---------------------------------------------------------------
   * setup for header write -- build header vdata FLIST from the
   * header structure FLIST and current header contents, and build
   * the header channel record
   * ---------------------------------------------------------------
   */

  /* rtphcheck does a sanity check of some of the header fields 
   */
  if (rtphcheck(head) != 0)
    return -1;

  /* loop on header structure fields, set field size (aka field order)
     from explicit header field sizes */
  for (i=0; i < NHFIELD; i++) {

    /* copy the header field set from rtp.h */
    hsf[i].fname = hfield[i].fname;
    hsf[i].htype = hfield[i].htype;
    hsf[i].order = hfield[i].order;

    /* set gas list field size to head.ngas */
    if (strcmp(hsf[i].fname, "glist")==0 ||
	strcmp(hsf[i].fname, "gunit")==0)
      hsf[i].order = head->ngas;

    /* set channel set field sizes to head.nchan */
    if (strcmp(hsf[i].fname, "vchan")==0 ||
	strcmp(hsf[i].fname, "ichan")==0)
      hsf[i].order = head->nchan;

    /* set MW channel set field sizes to head.mwnchan */
    if (strcmp(hsf[i].fname, "mwfchan")==0)
      hsf[i].order = head->mwnchan;

    /* the header max fields sizes mrho, memis, mlevs, mwmemis, and 
     * mwmstb don't get put into the header as explicit values, so we
     * size to zero here so they will be dropped */
    if (strcmp(hsf[i].fname, "mrho")==0 ||
	strcmp(hsf[i].fname, "memis")==0 ||
	strcmp(hsf[i].fname, "mlevs")==0 ||
	strcmp(hsf[i].fname, "mwmemis")==0 ||
	strcmp(hsf[i].fname, "mwmstb")==0)
      hsf[i].order = 0;
  }

  /* loop on header structure fields, build header vdata field
     list by dropping zero-sized fields */
  for (i=0,j=0; i < NHFIELD; i++) {
    if (hsf[i].order != 0) {
      hvf[j].fname = hsf[i].fname;
      hvf[j].htype = hsf[i].htype;
      hvf[j].order = hsf[i].order;
      j++;
    }
  }
  nhvf = j; /* number of header vdata fields */

  /*
  dump_flist(&hfield, NHFIELD, "rtpwrite1(): header structure");
  dump_flist(hsf, NHFIELD, "rtpwrite1(): header with zeros");
  dump_flist(hvf, nhvf, "rtpwrite1(): header vdata fields");
  */

  /* get field matchup offsets for the header */
  fmatch(&hvf, nhvf, &hfield, NHFIELD, &p1, &p2, &p3, &s1, &s2);

  /* build the header channel record */
  chan[hci].mode = 2; /* header write */
  chan[hci].nvf = nhvf;
  chan[hci].p1 = p1;
  chan[hci].p2 = p2;
  chan[hci].p3 = p3;
  chan[hci].vbuf = (char *) malloc(s1); /* header vdata buffer */
  chan[hci].flist = &hvf;


  /* ---------------------------------------------------------------
   * setup for profile write -- build profile vdata FLIST from the
   * profile structure FLIST and current header contents, and build
   * the profile channel record
   * ---------------------------------------------------------------
   */

  /* expand the gamnt field in the profile structure FLIST to
     individual gasses specified in head.glist, to get the 
     profile structure FLIST we will actually use */

  psf = (struct FLIST (*)[]) 
    malloc((NPFIELD+head->ngas) * sizeof(struct FLIST));

  gsplit(head->ngas, head->glist, 
         NPFIELD, &pfield, 
         &npsf, psf);

  /*
  dump_flist(&pfield, NPFIELD, "rtpwrite1(): profile struct with gamnt");
  dump_flist(psf, npsf, "rtpwrite1(): profile struct with gas_n fields");
  */

  /* allocate space for profile vdata FLIST */
  pvf = (struct FLIST (*)[]) malloc(npsf * sizeof(struct FLIST));

  /* loop on profile structure fields, set orders to their real
     sizes, based on header or other info, or to zero for empty
     fields */
  for (i=0; i < npsf; i++) {

    /* start with a copy of the structure FLIST records */
    (*pvf)[i].fname = (*psf)[i].fname;
    (*pvf)[i].htype = (*psf)[i].htype;
    (*pvf)[i].order = (*psf)[i].order;

    /* set reflectance field orders to head.mrho */
    if (strcmp((*pvf)[i].fname, "rho")==0 ||
	strcmp((*pvf)[i].fname, "rfreq")==0)
      (*pvf)[i].order = head->mrho;

    /* set emissivity field orders to head.memis */
    if (strcmp((*pvf)[i].fname, "emis")==0 ||
	strcmp((*pvf)[i].fname, "efreq")==0) 
      (*pvf)[i].order = head->memis;

    /* set MW emissivity field orders to head.mwmemis */
    if (strcmp((*pvf)[i].fname, "mwemis")==0 ||
	strcmp((*pvf)[i].fname, "mwefreq")==0)
      (*pvf)[i].order = head->mwmemis;

    /* set MW surface Tb field orders to head.mwmstb */
    if (strcmp((*pvf)[i].fname, "mwstb")==0 ||
	strcmp((*pvf)[i].fname, "mwsfreq")==0)
      (*pvf)[i].order = head->mwmstb;

    /* set profile field orders to head.mlevs */
    if (strcmp((*pvf)[i].fname, "plevs")==0 ||
	strcmp((*pvf)[i].fname, "plays")==0 ||
	strcmp((*pvf)[i].fname, "palts")==0 ||
	strcmp((*pvf)[i].fname, "ptemp")==0 ||
	strncmp((*pvf)[i].fname, "gas_", 4)==0)
      (*pvf)[i].order = head->mlevs;

    /* set the "gas_xx" field order to zero */
    if (strcmp((*pvf)[i].fname, "gas_xx")==0) 
      (*pvf)[i].order = 0;

    /* set the "gxover" field order to head.ngas */
    if (strcmp((*pvf)[i].fname, "gxover")==0) 
      (*pvf)[i].order = head->ngas;

    /* if we have calculated IR data, set the rcalc field size to 
       head.nchan, otherwise set it to zero */
    if (strcmp((*pvf)[i].fname, "rcalc")==0)
      (*pvf)[i].order = (head->pfields & IRCALCBIT) ? head->nchan : 0;

    /* if we have calculated MW data, set the mwcalc field size to
       head.mwnchan, otherwise set it to zero */
    if (strcmp((*pvf)[i].fname, "mwcalc")==0)
      (*pvf)[i].order = (head->pfields & MWCALCBIT) ? head->mwnchan : 0;

    /* if we have observations, set the observation lat, lon, and time 
       field sizes to 1, otherwise set them to zero */
/*      if (strcmp((*pvf)[i].fname, "rlat")==0 || 			   */
/*  	strcmp((*pvf)[i].fname, "rlon")==0 || 				   */
/*  	strcmp((*pvf)[i].fname, "rtime")==0) 				   */
/*        (*pvf)[i].order =  						   */
/*  	(head->pfields & IROBSVBIT || head->pfields & MWOBSVBIT) ? 1 : 0;  */

    /* if we have IR observations, set the robs1 and calflag field 
       sizes to head.nchan, otherwise set them to zero */
    if (strcmp((*pvf)[i].fname, "robs1")==0 )
      (*pvf)[i].order = (head->pfields & IROBSVBIT) ? head->nchan : 0;
    if (strcmp((*pvf)[i].fname, "calflag")==0) 
      (*pvf)[i].order = (head->pfields & IROBSVBIT) ? head->nchan: 0;

    /* if we have MW observations, set the mwobs field size to 
       head.mwnchan, otherwise set it to zero */
    if (strcmp((*pvf)[i].fname, "mwobs")==0)
      (*pvf)[i].order = (head->pfields & MWOBSVBIT) ? head->mwnchan : 0;
  }
    
  /* drop the size-zero fields from the profile vdata FLIST */
  for (i=0,j=0; i < npsf; i++) {
    if ((*pvf)[i].order != 0) {
      (*pvf)[j].fname = (*pvf)[i].fname;
      (*pvf)[j].htype = (*pvf)[i].htype;
      (*pvf)[j].order = (*pvf)[i].order;
      j++;
    }
  }
  npvf = j;

  /*
  dump_flist(pvf, npvf, "rtpwrite1(): profile vdata fields");
  */

  /* get field matchup offsets for the profiles */
  fmatch(pvf, npvf, psf, npsf, &p1, &p2, &p3, &s1, &s2);

  /* build the profile channel record */
  chan[pci].mode = 4; 	  /* profile write */
  chan[pci].nvf = npvf;
  chan[pci].p1 = p1;
  chan[pci].p2 = p2;
  chan[pci].p3 = p3;
  chan[pci].vbuf = (char *) malloc(s1);   /* profile vdata buffer */
  chan[pci].flist = pvf;		  /* profile field list */
  
  /* ----------------------------------------------------------------
   * open the HDF file and call pvwrite1 to write header and profile
   * attributes and to set up header and profile writes; call pvwrite2
   * to write out the header data
   * ----------------------------------------------------------------
   */

  /* open the HDF file */
  file_id = pvopen(fname, "c");
  if (file_id == -1) {
    fprintf(stderr, "rtpwrite1(): can't open %s\n", fname);
    return (-1);
  }
  
  /* set up the header write */
  s1 = pvwrite1("header", 
		file_id,
		&hvf, nhvf,
		hatt, nhatt,
		&vhead_id);

  if (s1 == -1) return -1;

  chan[hci].file_id = file_id;
  chan[hci].vdata_id = vhead_id;

  /* write the header */
  rtpwrite2(hci, (char *) head);

  /* close the header channel */
  rtpclose1(hci);

  /* set up the profile write */
  s1 = pvwrite1("profiles", 
		file_id,
		pvf, npvf,
		patt, npatt,
		&vprof_id);

  if (s1 == -1) return -1;

  chan[pci].file_id = file_id;
  chan[pci].vdata_id = vprof_id;

  return(0);
}


/*  search for an integer in an HDF int32 list
 */
int inglist(int a, int32 L[], int n) {
  int i;
  for (i=0; i<n; i++)
    if (L[i] == a) return (1);
  return (0);
}	

