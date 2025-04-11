
/* dump_pstr -- dump an RTP profile structure
 * 
 * note: this routine is intended mainly for test purposes, only a
 * subset of the profile fields are printed out.
 *
 * size bounds from the header struct are used to restrict printing
 * to relevant values
 */

#include <stdio.h>
#include "rtp.h"

void dump_pstr(
	       struct rtp_head *head,
	       struct rtp_prof *prof
	       ) {
  int i, j, k;

  fprintf(stdout, "------ Profile Surface & Atmosphere Fields -----------\n");

  fprintf(stdout, "plat   = %g\n",  prof->plat);
  fprintf(stdout, "plon   = %g\n",  prof->plon);
  fprintf(stdout, "ptime  = %g\n",  prof->ptime);

  fprintf(stdout, "stemp  = %g\n",  prof->stemp);
  fprintf(stdout, "nrho   = %d\n",  prof->nrho);

  fprintf(stdout, "rho    =");
  for (k=0; k < prof->nrho; k++)
    fprintf(stdout, " %g", prof->rho[k]);
  fprintf(stdout, "\n");

  fprintf(stdout, "rfreq  =");
  for (k=0; k < prof->nrho; k++)
    fprintf(stdout, " %g", prof->rfreq[k]);
  fprintf(stdout, "\n");

  fprintf(stdout, "nemis  = %d\n",  prof->nemis);

  fprintf(stdout, "emis   =");
  for (k=0; k < prof->nemis; k++)
    fprintf(stdout, " %g", prof->emis[k]);
  fprintf(stdout, "\n");

  fprintf(stdout, "efreq  =");
  for (k=0; k < prof->nemis; k++)
    fprintf(stdout, " %g", prof->efreq[k]);
  fprintf(stdout, "\n");

  fprintf(stdout, "salti  = %g\n",  prof->salti);
  fprintf(stdout, "spres  = %g\n",  prof->spres);
  fprintf(stdout, "smoist = %g\n",  prof->smoist);

  fprintf(stdout, "nlevs  = %d\n", prof->nlevs);

  /* landfrac, landtype, and MW surface data should go here */

  fprintf(stdout, "plevs  =");
  for (k=0; k < prof->nlevs; k++)
    fprintf(stdout, " %g", prof->plevs[k]);
  fprintf(stdout, "\n");

  fprintf(stdout, "ptemp  =");
  for (k=0; k < prof->nlevs; k++)
    fprintf(stdout, " %g", prof->ptemp[k]);
  fprintf(stdout, "\n");

  for (j=0; j < head->ngas; j++) {
    fprintf(stdout, "gas %d  =", head->glist[j]);
    for (k=0; k < prof->nlevs; k++)
      fprintf(stdout, " %g", prof->gamnt[j][k]);
    fprintf(stdout, "\n");
  }

  fprintf(stdout, "gxover =");
  for (j=0; j < head->ngas; j++)
    fprintf(stdout, " %g", prof->gxover[j]);
  fprintf(stdout, "\n");

  fprintf(stdout, "scanang = %g\n", prof->scanang);
  fprintf(stdout, "satzen = %g\n", prof->satzen);

  fprintf(stdout, "------ Profile Radiance Fields --------------------\n");

  if (head->pfields & IRCALCBIT) {
    fprintf(stdout, "rcalc  = ");
    for (k=0; k < head->nchan; k++)
      fprintf(stdout, " %g", prof->rcalc[k]);
    fprintf(stdout, "\n");
  }

  if (head->pfields & MWCALCBIT) {
    fprintf(stdout, "mwcalc  = ");
    for (k=0; k < head->mwnchan; k++)
      fprintf(stdout, " %g", prof->mwcalc[k]);
    fprintf(stdout, "\n");
  }

  if (head->pfields & IROBSVBIT || head->pfields & MWOBSVBIT) {
    fprintf(stdout, "rlat = %g\n", prof->rlat);
    fprintf(stdout, "rlon = %g\n", prof->rlon);
    fprintf(stdout, "rtime = %g\n", prof->rtime);
  }

  if (head->pfields & IROBSVBIT) {
    fprintf(stdout, "robs1  = ");
    for (k=0; k < head->nchan; k++)
      fprintf(stdout, " %g", prof->robs1[k]);
    fprintf(stdout, "\n");
    fprintf(stdout, "calflag  = ");
    for (k=0; k < head->nchan; k++)
      fprintf(stdout, " %d", prof->calflag[k]);
    fprintf(stdout, "\n");
  }

  if (head->pfields & MWOBSVBIT) {
    fprintf(stdout, "mwobs  = ");
    for (k=0; k < head->mwnchan; k++)
      fprintf(stdout, " %g", prof->mwobs[k]);
    fprintf(stdout, "\n");
  }

  fprintf(stdout, "------ Profile User Defined Fields -------------------\n");

  fprintf(stdout, "pnote = %s\n", prof->pnote);
  fprintf(stdout, "udef =");
  for (j=0; j < MAXUDEF; j++) 
    fprintf(stdout, " %g", head->udef[j]);
  fprintf(stdout, "\n");
  fprintf(stdout, "udef1 = %g\n", prof->udef1);
  fprintf(stdout, "udef2 = %g\n", prof->udef2);

  fprintf(stdout, "------------------------------------------------------\n");
}


