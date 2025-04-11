
/* initialize an RTP header structure
 */

#include "rtp.h"

void headinit(struct rtp_head *head) {

  int j;

  /* profile data */
  head->ptype           = LEVPRO;
  head->pfields         = PROFBIT;

  head->pmin            = BAD;
  head->pmax            = BAD;
  head->ngas            = 0;
  head->glist[0]        = BAD;
  head->gunit[0]        = BAD;

  /* radiance data */
  head->nchan           = 0;
  head->ichan[0]        = BAD;
  head->vchan[0]        = BAD;
  head->vcmin           = BAD;
  head->vcmax           = BAD;

  head->mwnchan         = 0;
  head->mwfchan[0]      = BAD;

  /* maxes for profile fields */
  head->mrho            = 0;
  head->memis           = 0;
  head->mlevs           = 0;
  head->mwmemis         = 0;
  head->mwmstb          = 0;

  /* user-defined fields */
  for (j=0; j < MAXUDEF; j++)  head->udef[j] = BAD;
  head->udef1           = BAD;
  head->udef2           = BAD;

}

