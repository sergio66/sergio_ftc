
/* initialize an RTP profile structure
 */

#include "rtp.h"

void profinit(struct rtp_prof *prof) {

  int j;

   /* surface data */
  prof->plat            = BAD;
  prof->plon            = BAD;
  prof->ptime           = BAD;

  prof->stemp           = BAD;
  prof->nrho            = 0;
  prof->rho[0]          = BAD;
  prof->rfreq[0]        = BAD;
  prof->nemis           = 0;
  prof->emis[0]         = BAD;
  prof->efreq[0]        = BAD;

  prof->salti           = BAD;
  prof->spres           = BAD;
  prof->smoist          = BAD;
  prof->landfrac        = BAD;
  prof->landtype        = BAD;

  /* MW surface data */
  prof->mwnemis         = 0;
  prof->mwefreq[0]      = BAD;
  prof->mwemis[0]       = BAD;
  prof->mwnstb          = 0;
  prof->mwsfreq[0]      = BAD;
  prof->mwstb[0]        = BAD;

  /* atmospheric data */
  prof->nlevs           = 0;
  prof->plevs[0]        = BAD;
  prof->plays[0]        = BAD;
  prof->palts[0]        = BAD;
  prof->ptemp[0]        = BAD;
  prof->gamnt[0][0]     = BAD;
  prof->gxover[0]	= BAD;
  prof->txover		= BAD;
  prof->co2ppm          = BAD;

  prof->cfrac           = BAD;
  prof->ctype           = BAD;
  prof->cemis           = BAD;
  prof->cprtop          = BAD;
  prof->cprbot          = BAD;
  prof->cngwat          = BAD;
  prof->cpsize          = BAD;

  prof->wspeed          = BAD;
  prof->wsource         = BAD;

  /* common radiance data */
  prof->pobs            = BAD;
  prof->zobs            = BAD;
  prof->upwell          = BAD;

  prof->scanang         = BAD;
  prof->satzen          = BAD;
  prof->satazi          = BAD;
  prof->solzen          = BAD;
  prof->solazi          = BAD;

  prof->mwasang		= BAD;
  prof->mwaszen		= BAD;
  prof->mwbsang		= BAD;
  prof->mwbszen		= BAD;

  /* calculated radiance data */
  prof->rcalc[0]        = BAD;
  prof->mwcalc[0]       = BAD;

  /* observed radiance data */
  prof->rlat            = BAD;
  prof->rlon            = BAD;
  prof->rtime           = BAD;
  prof->robs1[0]        = BAD;
  for (j=0; j < MAXCHAN; j++)  prof->calflag[j] = (char) 0;
  prof->irinst          = BAD;

  prof->mwobs[0]        = BAD;
  prof->mwinst          = BAD;

  prof->findex          = BAD;
  prof->atrack          = BAD;
  prof->xtrack          = BAD;

  /* user-defined fields */
  for (j=0; j < MAXPNOTE; j++) prof->pnote[j] = ' ';
  for (j=0; j < MAXUDEF; j++)  prof->udef[j] = BAD;
  prof->udef1           = BAD;
  prof->udef2           = BAD;

}

