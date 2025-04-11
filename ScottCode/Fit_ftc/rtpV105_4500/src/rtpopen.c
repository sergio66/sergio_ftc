/* 

NAME

  rtpopen -- Fortran interface to open RTP files

SUMMARY

  rtpopen() is used to open an HDF RTP ("Radiative Transfer Profile")
  file for reading or writing profile data.  In addition, it reads or
  writes RTP header data and HDF header and profile attributes. 

FORTRAN PARAMETERS

  data type	        name	  short description       direction
  ---------     	-----     -----------------       ---------
  CHARACTER *(*)	fname	  RTP file name		     IN
  CHARACTER *(*)	mode 	  'c'=create, 'r'=read	     IN
  STRUCTURE /RTPHEAD/   head	  RTP header structure	     IN/OUT
  STRUCTURE /RTPATTR/	hfatt	  RTP header attributes	     IN/OUT
  STRUCTURE /RTPATTR/	pfatt	  RTP profile attributes     IN/OUT
  INTEGER		rchan	  RTP profile channel	     OUT

VALUE RETURNED

  0 if successful, -1 on errors

INCLUDE FILES

  rtpdefs.f -- Fortran header, profile, and attribute structures

DISCUSSION

  The valid open modes are 'r' to read an existing file and 'c' to
  create a new file.

  HDF attributes are read and written in an array of RTPATTR
  structures, with one structure record per attribute.  Attributes
  should be terminated with char(0), and are returned that way, for
  a read.  The end of the attribute array is flagged with a char(0)
  at the beginning of the fname field.

*/

#include <stdio.h>

#define RTPDEF
#include "rtp.h"
#include "rtpfnx.h"

int rtpopen(char *fname,               /* RTP file name            IN     */
	    char *mode,                /* "c"=create, "r"=read     IN     */
	    struct rtp_head *head,     /* RTP header structure     IN/OUT */
	    struct rtpfatt (*hfatt)[], /* header attributes        IN/OUT */
	    struct rtpfatt (*pfatt)[], /* profile attributes       IN/OUT */
	    int *rchan                 /* RTP profile channel      OUT    */
	    ) {

  int i, j, k;
  int s1;
  char cname[256], cmode[8], *p;
  int nhatt, npatt;
  struct ALIST hcatt[MAXNATTR], pcatt[MAXNATTR];
  struct ALIST (*hcattP)[], (*pcattP)[];
  extern char *fsncpy(char *a, char *b, int n);

  fsncpy(cname, fname, 256);
  fsncpy(cmode, mode, 8);

  if (strncmp(cmode, "c", 1) == 0) {

    /* ---------------------
     * create a new RTP file 
     * --------------------- */

    /* copy fortran header attributes to C structure */
    for (i=0; i < MAXNATTR && (*hfatt)[i].fname[0] != '\0'; i++) {
      /* add nulls at the tail of Fortran strings, just in case */
      (*hfatt)[i].fname[MAXVNAME-1] = '\0';
      (*hfatt)[i].aname[MAXANAME-1] = '\0';
      (*hfatt)[i].atext[MAXATEXT-1] = '\0';
      /* copy the string pointers */
      hcatt[i].fname = (*hfatt)[i].fname;
      hcatt[i].aname = (*hfatt)[i].aname;
      hcatt[i].atext = (*hfatt)[i].atext;
    }
    nhatt = i;

    /* copy fortran profile attributes to C structure */
    for (i=0; i < MAXNATTR && (*pfatt)[i].fname[0] != '\0'; i++) {
      (*pfatt)[i].fname[MAXVNAME-1] = '\0';
      (*pfatt)[i].aname[MAXANAME-1] = '\0';
      (*pfatt)[i].atext[MAXATEXT-1] = '\0';
      pcatt[i].fname = (*pfatt)[i].fname;
      pcatt[i].aname = (*pfatt)[i].aname;
      pcatt[i].atext = (*pfatt)[i].atext;
    }
    npatt = i;

    /*
    dump_attrs(&hcatt, nhatt, "header attrs");
    dump_attrs(&pcatt, npatt, "profile attrs");
    */

    /* call rtpwrite1() to do the work */
    s1 = rtpwrite1(cname, head,
		   &hcatt, nhatt,
		   &pcatt, npatt,
		   rchan);
    return s1;
  }
  else if (strncmp(cmode, "r", 1) == 0) {

    /* -------------------------
     * open an existing RTP file
     * ------------------------- */

    /* call rtpread1() to do the work */
    s1 = rtpread1(cname, head,
		   &hcattP, &nhatt,
		   &pcattP, &npatt,
		   rchan);

    if (s1 == -1) return -1;

    /* copy header attributes out to fortran attribute structure */
    for (i=0; i < MAXNATTR && i < nhatt; i++) {

      /* write blanks to the fortran fields */
      for (j=0; j < MAXVNAME; j++) (*hfatt)[i].fname[j] = ' ';
      for (j=0; j < MAXANAME; j++) (*hfatt)[i].aname[j] = ' ';
      for (j=0; j < MAXATEXT; j++) (*hfatt)[i].atext[j] = ' ';

      /* copy strings to the fortran fields */
      strncpy((*hfatt)[i].fname, (*hcattP)[i].fname, MAXVNAME);
      strncpy((*hfatt)[i].aname, (*hcattP)[i].aname, MAXANAME);
      strncpy((*hfatt)[i].atext, (*hcattP)[i].atext, MAXATEXT);
    }
    /* mark the next record as empty, if there is room */
    if (i < MAXNATTR)
      (*hfatt)[i].fname[0] = '\0';
      
    /* copy profile attributes out to fortran attribute structure */
    for (i=0; i < MAXNATTR && i < npatt; i++) {

      /* write blanks to the fortran fields */
      for (j=0; j < MAXVNAME; j++) (*pfatt)[i].fname[j] = ' ';
      for (j=0; j < MAXANAME; j++) (*pfatt)[i].aname[j] = ' ';
      for (j=0; j < MAXATEXT; j++) (*pfatt)[i].atext[j] = ' ';

      /* copy strings to the fortran fields */
      strncpy((*pfatt)[i].fname, (*pcattP)[i].fname, MAXVNAME);
      strncpy((*pfatt)[i].aname, (*pcattP)[i].aname, MAXANAME);
      strncpy((*pfatt)[i].atext, (*pcattP)[i].atext, MAXATEXT);
    }
    /* mark the next record as empty, if there is room */
    if (i < MAXNATTR)
      (*pfatt)[i].fname[0] = '\0';

    return 0;
  }
  else {

    /* -------------------------
     * error: unknown input mode 
     * ------------------------- */

    fprintf(stderr, "rtpopen(): unknown input mode %s\n", cmode);
    return -1;
  }
}


/* copy a fortran-style string to a c-style string
 */
char *fsncpy(char *a, char *b, int n) {
  int i;
  for (i=0; i < n && b[i] != '\0' && b[i] != ' '; i++)
    a[i] = b[i];
  if (i == n) i--;
  a[i] = '\0';
  return a;
}


/* wrapper for Fortran names with trailing "_" 
 */
int rtpopen_(char *fname,
	     char *mode,
	     struct rtp_head *head,
	     struct rtpfatt (*hfatt)[],
	     struct rtpfatt (*pfatt)[],
	     int *rchan
	     ) {
  return rtpopen(fname, mode, head, hfatt, pfatt, rchan);
}

/* wrapper for uppercase Fortran names
 */
int RTPOPEN(char *fname,
	     char *mode,
	     struct rtp_head *head,
	     struct rtpfatt (*hfatt)[],
	     struct rtpfatt (*pfatt)[],
	     int *rchan
	     ) {
  return rtpopen(fname, mode, head, hfatt, pfatt, rchan);
}

/* wrapper for uppercase Fortran names with trailing "_" 
 */
int RTPOPEN_(char *fname,
	     char *mode,
	     struct rtp_head *head,
	     struct rtpfatt (*hfatt)[],
	     struct rtpfatt (*pfatt)[],
	     int *rchan
	     ) {
  return rtpopen(fname, mode, head, hfatt, pfatt, rchan);
}
