/*

NAME	

  rtpwrite -- Fortran interface to write an RTP profile

SUMMARY

  rtpwrite writes an RTP profile, represented as the contents
  of an RTPPROF structure, to an open RTP channel.  Successive
  calls write successive profiles.

FORTRAN PARAMETERS

  data type	        name	  short description       direction
  ---------     	-----     -----------------       ---------
  INTEGER		rchan	  RTP profile channel	      IN
  STRUCTURE /RTPPROF/   prof	  RTP profile structure	      IN

VALUE RETURNED

  0 on success, -1 on errors

*/

#include "rtp.h"
#include "rtpfnx.h"

int rtpwrite(int *rchan,              /* RTP channel            IN */
              struct rtp_prof *prof   /* RTP profile structure  IN */
              ) {
  int s1;
  s1 = rtpwrite2(*rchan, (char *) prof);
  return s1;
}


/* wrapper for Fortran names with trailing "_" 
 */
int rtpwrite_(int *rchan,
              struct rtp_prof *prof
              ) {
  return rtpwrite(rchan, prof);
}

/* wrapper for uppercase Fortran names
 */
int RTPWRITE(int *rchan,
              struct rtp_prof *prof
              ) {
  return rtpwrite(rchan, prof);
}

/* wrapper for uppercase Fortran names with trailing "_" 
 */
int RTPWRITE_(int *rchan,
              struct rtp_prof *prof
              ) {
  return rtpwrite(rchan, prof);
}

