/*

NAME	

  rtpclose -- Fortran interface to close an RTP open channel

SUMMARY

  rtpclose finishes up after reading or writing an RTP file, 
  writing out any buffers and closing the HDF interface

FORTRAN PARAMETERS

  data type	name	  short description      direction
  ---------     -----     -----------------      ---------
  INTEGER	rchan	  RTP profile channel	    IN

VALUE RETURNED

  0 on success, -1 on errors

*/

#include "rtp.h"
#include "rtpfnx.h"

int rtpclose(int *ci) {

  int s1;
  s1 = rtpclose1(*ci);
  return s1;
}


/* wrapper for Fortran names with trailing "_" 
 */
int rtpclose_(int *ci) {
  return rtpclose(ci);
}

/* wrapper for uppercase Fortran names
 */
int RTPCLOSE(int *ci) {
  return rtpclose(ci);
}

/* wrapper for uppercase Fortran names with trailing "_" 
 */
int RTPCLOSE_(int *ci) {
  return rtpclose(ci);
}

