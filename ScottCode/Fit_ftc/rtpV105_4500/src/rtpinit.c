/*

NAME    

  rtpinit -- initialze RTP profile structures

SUMMARY

  rtpinit initializes RTP profile structures with some sensible
  default vaules, and is used when creating a new profile set; it
  should generally not be used when modifying existing profiles.

  rtpinit sets all field sizes to zero, and all data values to
  "BAD", so that only actual values and sizes need to be written

FORTRAN PARAMETERS

  data type             name      short description       direction
  ---------             -----     -----------------       ---------
  STRUCTURE /RTPHEAD/   head      RTP header structure       OUT
  STRUCTURE /RTPPROF/   prof      RTP profile structure      OUT

VALUE RETURNED

  rtpinit always returns 0

*/

#include "rtp.h"
#include "rtpfnx.h"

int rtpinit(
            struct rtp_head *head,  /* RTP header structure  */
            struct rtp_prof *prof   /* RTP profile structure */
            ) {

  /* initialize header values */
  headinit(head);

  /* initialize profile values */
  profinit(prof);

  /* always succeed */
  return (0);
}

/* wrapper for Fortran names with trailing "_" 
 */
int rtpinit_(
             struct rtp_head *head,
             struct rtp_prof *prof
             ) {
  return rtpinit(head, prof);
}

/* wrapper for uppercase Fortran names
 */
int RTPINIT(
             struct rtp_head *head,
             struct rtp_prof *prof
            ) {
  return rtpinit(head, prof);
}

/* wrapper for uppercase Fortran names with trailing "_"
 */
int RTPINIT_(
             struct rtp_head *head,
             struct rtp_prof *prof
             ) {
  return rtpinit(head, prof);
}

