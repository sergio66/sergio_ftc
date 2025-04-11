
/* dump the RTP header structure */

#include <stdio.h>
#include "rtp.h"

void dump_hstr(struct rtp_head *head) {
  int i, j;

  fprintf(stdout, "------ Header Fields ---------\n");

  fprintf(stdout, "ptype   = %d\n",  head->ptype);
  fprintf(stdout, "pfields = %d\n",  head->pfields);
  fprintf(stdout, "pmin    = %g\n",  head->pmin);
  fprintf(stdout, "pmax    = %g\n",  head->pmax);

  fprintf(stdout, "ngas    = %d\n",  head->ngas);

  fprintf(stdout, "glist  = ");
  for (j=0; j < head->ngas; j++) 
    fprintf(stdout, " %d", head->glist[j]);
  fprintf(stdout, "\n");

  fprintf(stdout, "gunit = ");
  for (j=0; j < head->ngas; j++) 
    fprintf(stdout, " %d", head->gunit[j]);
  fprintf(stdout, "\n");

  /* radiance fields */
  fprintf(stdout, "nchan  = %d\n", head->nchan);

  fprintf(stdout, "vchan  =");
  for (j=0; j < head->nchan; j++) 
    fprintf(stdout, " %g", head->vchan[j]);
  fprintf(stdout, "\n");

  fprintf(stdout, "ichan  =");
  for (j=0; j < head->nchan; j++) 
    fprintf(stdout, " %d", head->ichan[j]);
  fprintf(stdout, "\n");

  /* MW fields */
  fprintf(stdout, "mwnchan  = %d\n", head->mwnchan);

  fprintf(stdout, "mwfchan  =");
  for (j=0; j < head->mwnchan; j++) 
    fprintf(stdout, " %g", head->mwfchan[j]);
  fprintf(stdout, "\n");

  /* user defined fields */
  fprintf(stdout, "udef =");
  for (j=0; j < MAXUDEF; j++) 
    fprintf(stdout, " %g", head->udef[j]);
  fprintf(stdout, "\n");
  fprintf(stdout, "udef1 = %g\n",  head->udef1);
  fprintf(stdout, "udef2 = %g\n",  head->udef2);

  fprintf(stdout, "-----------------------------\n");
}

