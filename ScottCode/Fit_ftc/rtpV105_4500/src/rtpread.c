/* 

NAME	

  rtpread -- Fortran interface to read an RTP profile

SUMMARY

  rtpread reads a profile from an open RTP channel, and returns
  the data in the RTPPROF structure.  Successive calls to rtpread
  return successive profiles from the file, with -1 returned on
  EOF.


FORTRAN PARAMETERS

  data type	        name	  short description       direction
  ---------     	-----     -----------------       ---------
  INTEGER		rchan	  RTP profile channel	      IN
  STRUCTURE /RTPPROF/   prof	  RTP profile structure	      OUT

VALUE RETURNED

  1 (the number of profiles read) on success , -1 on errors or EOF

*/

#include "rtp.h"
#include "rtpfnx.h"

int rtpread(
	    int *rchan,              /* RTP channel             IN  */
            struct rtp_prof *prof    /* RTP profile structure   OUT */
            ) {
  int s1;

  /* set fields to defaults before the read  */
  profinit(prof);

  /* do the read  */
  s1 = rtpread2(*rchan, (char *) prof);

  return s1;
}


/* wrapper for Fortran names with trailing "_" 
 */
int rtpread_(int *rchan,
             struct rtp_prof *prof
             ) {
  return rtpread(rchan, prof);
}

/* wrapper for uppercase Fortran names
 */
int RTPREAD(int *rchan,
             struct rtp_prof *prof
             ) {
  return rtpread(rchan, prof);
}

/* wrapper for uppercase Fortran names with trailing "_"
 */
int RTPREAD_(int *rchan,
             struct rtp_prof *prof
             ) {
  return rtpread(rchan, prof);
}

