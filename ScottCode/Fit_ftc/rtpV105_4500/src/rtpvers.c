/* 

NAME

  rtpvers -- Fortran interface return the RTP code version

SUMMARY

  rtpvers returns the version string RTPVERS, defined in rtp.h

FORTRAN PARAMETERS

    data type         name        short description     direction
  -------------      ------       -----------------     ---------
  CHARACTER *(*)     version         RTP version           OUT

*/

#include <string.h>
#include "rtp.h"

void rtpvers(char *version) {
  strcpy(version, VERSION);
}

/* wrapper for Fortran names with trailing "_" 
 */
void rtpvers_(char *version) {
  rtpvers(version);
}

/* wrapper for uppercase Fortran names
 */
void RTPVERS(char *version) {
  rtpvers(version);
}

/* wrapper for uppercase Fortran names with trailing "_" 
 */
void RTPVERS_(char *version) {
  rtpvers(version);
}

