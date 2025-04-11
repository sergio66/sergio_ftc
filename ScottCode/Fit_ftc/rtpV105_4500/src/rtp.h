
/* RTP Fortran compatibility header and profile structures
 *
 * The C and Fortran structure definitions and the C field "flists"
 * giving string names to fields must all match exactly.  Also, the
 * #define constants given below should match those in the Fortran
 * parameter declarations.
 *
 *
 * H. Motteler
 * 5 Mar 02
 */

#include "hdf.h"
#include "pvdefs.h"

/* set the library version 
 */
#define VERSION "RTP version 1.05, 18 Mar 02"


/* --------------
 * RTP parameters 
 * --------------
 */

/* the JPL "BAD" value 
 */
#define BAD -9999

/* header field "ptype" defined values 
 */
#define LEVPRO   0
#define LAYPRO   1
#define AIRSLAY  2

/* heder field "pfields" bit flags for profile field groups 
 */
#define PROFBIT	    1   /* profiles                             */
#define IRCALCBIT   2   /* calculated IR radiances              */
#define IROBSVBIT   4   /* observed IR radiances                */
#define MWCALCBIT   8   /* calculated MW radiances              */
#define MWOBSVBIT   16  /* observed MW radiances                */
#define PFIELDSMAX  31  /* sum of the above values 		*/

/* RTP structure array limits
 *
 * These are max size limits for the static header and profile
 * structures defined below; the actual sizes of the corresponding
 * vdata fields are set by the relevant header structure fields 
 */
#define MAXRHO    100	/* max number of reflectance points 	*/
#define MAXEMIS   100	/* max number of emissivity points 	*/
#define MAXGAS     50	/* max number of gasses 		*/
#define MAXGASID  203	/* max valid HITRAN gas ID 		*/
#define MAXLEV    120	/* max number of levels or layers	*/
#define MAXCHAN  4500	/* max number of chanels		*/
#define MWMAXCHAN  20   /* max MW channels 			*/
#define MWMAXEMIS  10   /* max MW emiss points			*/
#define MWMAXSTB   10   /* max MW surface Tb points		*/
#define MAXPNOTE   80   /* max profile comment string		*/ 
#define MAXUDEF    20   /* max profile or header udef values	*/

/* RTP API array bounds 
 */
#define MAXOPEN     8	/* max number of open RTP channels 	*/
#define MAXNATTR   32   /* max number of attributes per file	*/

/* derived parameters
 */
#define MAXCALF    (((MAXCHAN-1)/4+1)*4)
#define MAXPN4     (((MAXPNOTE-1)/4+1)*4)


/* ----------------
 * header structure
 * ----------------
 */

struct rtp_head {

  /* profile data (7 fields)
   */
  int32   ptype;   	  	/* 0 = level prof., 1 = layer prof.	*/
  int32   pfields;	  	/* field-set code 			*/

  float32 pmin;		  	/* lowest profile press level 		*/
  float32 pmax;		  	/* hightst profile press level		*/
  int32   ngas;   	  	/* number of explicit constituents 	*/
  int32   glist[MAXGAS];  	/* constituent gas list, ngas-vector	*/
  int32   gunit[MAXGAS];  	/* constituent gas units, ngas-vector 	*/

  /* radiance data (7 fields)
   */
  int32   nchan;		/* number of IR radiance channels	*/
  int32   ichan[MAXCHAN]; 	/* IR channel numbers, nchan-vector 	*/
  float32 vchan[MAXCHAN]; 	/* IR chan center freq's, nchan-vector	*/ 
  float32 vcmin;	  	/* chan set min freq (including wings)	*/
  float32 vcmax;	  	/* chan set max freq (including wings)	*/

  int32   mwnchan;	  	/* number of MW radiance channels	*/
  float32 mwfchan[MWMAXCHAN]; 	/* MW center freq's, mwnchan-vector 	*/ 

  /* maxes for profile fields (5 fields)
   * these fields are not saved explicitly in the HDF file
   */
  int32   mrho;           	/* max number of reflectance points	*/
  int32   memis;          	/* max number of emissivity points	*/
  int32   mlevs;          	/* max number of pressure levels	*/
  int32   mwmemis;        	/* max number of MW emissivity points	*/
  int32   mwmstb ;        	/* max number of MW surface Tb points	*/

  /* user-defined fields (3 fields)
   */
  float32 udef[MAXUDEF];	/* user-defined array			*/ 
  float32 udef1;	  	/* user-defined scalar			*/
  float32 udef2;	  	/* user-defined scalar			*/ 
};

/* -----------------
 * header field list
 * ----------------- 
 */

#ifdef RTPDEF

/* define the header field list ("flist")
 *
 * sizes and HDF types must match structure fields exactly;
 */
struct FLIST hfield[] = {

  /* profile data (7 fields) */
  "ptype",   DFNT_INT32,    1,
  "pfields", DFNT_INT32,    1,

  "pmin",    DFNT_FLOAT32,  1,
  "pmax",    DFNT_FLOAT32,  1,
  "ngas",    DFNT_INT32,    1,
  "glist",   DFNT_INT32,    MAXGAS,
  "gunit",   DFNT_INT32,    MAXGAS,

  /* radiance data (7 fields) */
  "nchan",   DFNT_INT32,    1,
  "ichan",   DFNT_INT32,    MAXCHAN,
  "vchan",   DFNT_FLOAT32,  MAXCHAN,
  "vcmin",   DFNT_FLOAT32,  1,
  "vcmax",   DFNT_FLOAT32,  1,

  "mwnchan", DFNT_INT32,    1,
  "mwfchan", DFNT_FLOAT32,  MWMAXCHAN,

  /* max profile size fields (5 fields) 
   * these fields are not saved explicitly in the HDF file */
  "mrho",    DFNT_INT32,    1,
  "memis",   DFNT_INT32,    1,
  "mlevs",   DFNT_INT32,    1,
  "mwmemis", DFNT_INT32,    1,
  "mwmstb",  DFNT_INT32,    1,

  /* user-defined fields (3 fields) */
  "udef",    DFNT_FLOAT32,  MAXUDEF,
  "udef1",   DFNT_FLOAT32,  1,
  "udef2",   DFNT_FLOAT32,  1
};

#else

/* delcare the header field list as an extern 
*/
extern struct FLIST hfield[];

#endif

/* specify the number of header fields 
 */
/* #define NHFIELD (sizeof(hfield) / (sizeof(char*) + sizeof(int)*2)) */
#define NHFIELD 22


/* -----------------
 * profile structure
 * -----------------
 */

struct rtp_prof {

  /* surface data (15 fields)
   */
  float32 plat;   		/* profile latitude     	*/
  float32 plon;   		/* profile longitude    	*/
  float64 ptime;  		/* profile time         	*/

  float32 stemp;  		/* surface temperature  	*/
  int32   nrho;  		/* number of refl. pts		*/
  float32 rho[MAXRHO];  	/* surface reflectance  	*/
  float32 rfreq[MAXRHO];	/* reflectance freq's   	*/
  int32   nemis; 		/* number of emis. pts		*/
  float32 emis[MAXEMIS];	/* surface emissivities 	*/
  float32 efreq[MAXEMIS];	/* emissivity freq's    	*/

  float32 salti;  		/* surface altitude     	*/
  float32 spres;  		/* surface pressure     	*/
  float32 smoist;		/* soil moisture fraction	*/
  float32 landfrac;		/* land fraction		*/
  int32   landtype;		/* land type code		*/

  /* MW surface data (6 fields)
   */
  int32 mwnemis;       		/* number of MW emis pts	*/
  float32 mwefreq[MWMAXEMIS];	/* MW emissivity freq's		*/
  float32 mwemis[MWMAXEMIS];	/* MW emissivities		*/
  int32 mwnstb;			/* number of MW surf pts	*/
  float32 mwsfreq[MWMAXSTB];	/* MW surface Tb freq's		*/
  float32 mwstb[MWMAXSTB];	/* MW surface Tbs		*/

  /* atmospheric data (17 fields)
   */
  int32   nlevs; 		/* number of press lev's	*/
  float32 plevs[MAXLEV];	/* pressure levels      	*/
  float32 plays[MAXLEV];	/* pressure layers      	*/
  float32 palts[MAXLEV];  	/* level altitudes      	*/
  float32 ptemp[MAXLEV];  	/* temperature profile  	*/
  float32 gamnt[MAXGAS][MAXLEV]; /* constituent amounts		*/
  float32 gxover[MAXGAS];	/* constituent crossover press	*/
  float32 txover;		/* temperature crossover press	*/	
  float32 co2ppm;		/* CO2 PPMV			*/

  float32 cfrac; 		/* cloud fraction       	*/
  int32   ctype;		/* cloud type code		*/
  float32 cemis; 		/* cloud top emissivity 	*/
  float32 cprtop; 		/* cloud top pressure   	*/
  float32 cprbot; 		/* cloud bottom pressure   	*/
  float32 cngwat; 		/* cloud non-gas water  	*/
  float32 cpsize; 		/* cloud particle size   	*/

  float32 wspeed;      		/* wind speed			*/
  float32 wsource;		/* wind source			*/

  /* common radiance data (12 fields)
   */
  float32 pobs;			/* observation pressure		*/
  float32 zobs;			/* observation height		*/
  int32   upwell;		/* radiation direction		*/

  float32 scanang;		/* IR scan angle		*/
  float32 satzen;		/* IR zenith angle 		*/
  float32 satazi;		/* sat azimuth angle		*/
  float32 solzen;		/* sun zenith angle 		*/
  float32 solazi;		/* sun azimuth angle		*/

  float32 mwasang;		/* AMSU view angle		*/
  float32 mwaszen;		/* AMSU zenith angle 		*/
  float32 mwbsang;		/* HSB view angle		*/
  float32 mwbszen;		/* HSB zenith angle 		*/

  /* calculated radiance data (2 fields)
   */
  float32 rcalc[MAXCHAN]; 	/* calculated IR radiances 	*/
  float32 mwcalc[MWMAXCHAN]; 	/* calculated MW radiances 	*/

  /* observed radiance data (11 fields)
   */
  float32 rlat;           	/* obs rad lat.         	*/
  float32 rlon;           	/* obs rad lon.         	*/
  /* int32   rfill; */		/* align rtime on 8 byte bndry	*/
  float64 rtime;          	/* radiance obs time    	*/
  float32 robs1[MAXCHAN]; 	/* observed IR radiance    	*/
  uchar8  calflag[MAXCALF];   	/* IR calibration flags		*/
  int32   irinst;		/* IR instrument code		*/

  float32 mwobs[MWMAXCHAN]; 	/* observed MW BT	 	*/
  int32   mwinst;		/* MW instrument code		*/

  int32   findex;		/* file (granule) index		*/
  int32   atrack;		/* along-track index		*/
  int32   xtrack;     		/* cross-track index		*/

  /* user-defined fields (4 fields)
   */
  char8 pnote[MAXPN4];   	/* profile annotation		*/
  float32 udef[MAXUDEF];	/* user-defined array		*/ 
  float32 udef1;		/* spare, user defined  	*/
  float32 udef2;		/* spare, user defined  	*/
};

/* ------------------
 * profile field list
 * ------------------ 
 */

#ifdef RTPDEF

/* define the profile field list ("flist")
 *
 * sizes and HDF types must match structure fields exactly;
 */
struct FLIST pfield[] = {

  /* surface data (15 fields) */
  "plat",     DFNT_FLOAT32,  1,
  "plon",     DFNT_FLOAT32,  1,
  "ptime",    DFNT_FLOAT64,  1,

  "stemp",    DFNT_FLOAT32,  1,
  "nrho",     DFNT_INT32,    1,
  "rho",      DFNT_FLOAT32,  MAXRHO,
  "rfreq",    DFNT_FLOAT32,  MAXRHO,
  "nemis",    DFNT_INT32,    1,
  "emis",     DFNT_FLOAT32,  MAXEMIS,
  "efreq",    DFNT_FLOAT32,  MAXEMIS,

  "salti",    DFNT_FLOAT32,  1,
  "spres",    DFNT_FLOAT32,  1,
  "smoist",   DFNT_FLOAT32,  1,
  "landfrac", DFNT_FLOAT32,  1,
  "landtype", DFNT_INT32,    1,

  /* MW surface data (6 fields) */
  "mwnemis",  DFNT_INT32,    1,
  "mwefreq",  DFNT_FLOAT32,  MWMAXEMIS,
  "mwemis",   DFNT_FLOAT32,  MWMAXEMIS,
  "mwnstb",   DFNT_INT32,    1,
  "mwsfreq",  DFNT_FLOAT32,  MWMAXSTB,
  "mwstb",    DFNT_FLOAT32,  MWMAXSTB,

  /* atmospheric data (18 fields) */
  "nlevs",    DFNT_INT32,    1,
  "plevs",    DFNT_FLOAT32,  MAXLEV,
  "plays",    DFNT_FLOAT32,  MAXLEV,
  "palts",    DFNT_FLOAT32,  MAXLEV,
  "ptemp",    DFNT_FLOAT32,  MAXLEV,
  "gamnt",    DFNT_FLOAT32,  MAXGAS*MAXLEV,
  "gxover",   DFNT_FLOAT32,  MAXGAS,
  "txover",   DFNT_FLOAT32,  1,
  "co2ppm",   DFNT_FLOAT32,  1,

  "cfrac",    DFNT_FLOAT32,  1,
  "ctype",    DFNT_INT32,    1,
  "cemis",    DFNT_FLOAT32,  1,
  "cprtop",   DFNT_FLOAT32,  1,
  "cprbot",   DFNT_FLOAT32,  1,
  "cngwat",   DFNT_FLOAT32,  1,
  "cpsize",   DFNT_FLOAT32,  1,

  "wspeed",   DFNT_FLOAT32,  1,
  "wsource",  DFNT_FLOAT32,  1,

  /* common radiance data (12 fields) */
  "pobs",     DFNT_FLOAT32,  1,
  "zobs",     DFNT_FLOAT32,  1,
  "upwell",   DFNT_INT32,    1,

  "scanang",  DFNT_FLOAT32,  1,
  "satzen",   DFNT_FLOAT32,  1,
  "satazi",   DFNT_FLOAT32,  1,
  "solzen",   DFNT_FLOAT32,  1,
  "solazi",   DFNT_FLOAT32,  1,

  "mwasang",  DFNT_FLOAT32,  1,
  "mwaszen",  DFNT_FLOAT32,  1,
  "mwbsang",  DFNT_FLOAT32,  1,
  "mwbszen",  DFNT_FLOAT32,  1,

  /* calculated radiance data (2 fields) */
  "rcalc",    DFNT_FLOAT32,  MAXCHAN,
  "mwcalc",   DFNT_FLOAT32,  MWMAXCHAN,

  /* observed radiance data (11 fields) */
  "rlat",     DFNT_FLOAT32,  1,
  "rlon",     DFNT_FLOAT32,  1,
  /*"rfill",  DFNT_INT32,    1, */
  "rtime",    DFNT_FLOAT64,  1,
  "robs1",    DFNT_FLOAT32,  MAXCHAN,
  "calflag",  DFNT_UCHAR8,   ((MAXCHAN-1)/4+1)*4,
  "irinst",   DFNT_INT32,    1,

  "mwobs",    DFNT_FLOAT32,  MWMAXCHAN,
  "mwinst",   DFNT_INT32,    1,

  "findex",   DFNT_INT32,    1,
  "atrack",   DFNT_INT32,    1,
  "xtrack",   DFNT_INT32,    1,

  /* user-defined fields (4 fields) */
  "pnote",    DFNT_CHAR8,    ((MAXPNOTE-1)/4+1)*4,
  "udef",     DFNT_FLOAT32,  MAXUDEF,
  "udef1",    DFNT_FLOAT32,  1,
  "udef2",    DFNT_FLOAT32,  1
};

#else

/* declare the profile field list as an extern
 */
extern struct FLIST pfield[];

#endif

/* specify the number of profile fields 
 */
/* #define NPFIELD (sizeof(pfield) / (sizeof(char*) + sizeof(int)*2)) */
#define NPFIELD 68


/* ---------------------------
 * Fortran attribute structure
 * ---------------------------
 */

/* 
 * attribute structure (for fortran attributes)
 * this must match the fortran structure RTPATTR 
 */
struct rtpfatt {
  char 	fname[MAXVNAME];
  char	aname[MAXANAME];
  char	atext[MAXATEXT];
};


/* ---------------------
 * RTP channel structure
 * ---------------------
 */
struct rtp_chan {
  int mode;		    /* 1=hread, 2=hwrite, 3=pread, 4=pwrite */
  int32 file_id;	    /* HDF file ID */
  int32 vdata_id;	    /* HDF vdata header ID */
  int nvf;		    /* number of vdata fields */
  int *p1, *p2, *p3; 	    /* vdata pointer arrays */
  char *vbuf;		    /* vdata buffer */
  struct FLIST (*flist)[];  /* pointer field list (for testing) */
};

#ifdef RTPDEF

/* define the open channel structure
 */
struct rtp_chan chan[MAXOPEN];

/* oflag should be initialized with MAXOPEN zeros 
 */
int oflag[] = {0,0,0,0,0,0,0,0};

#else

/* declare the channel structures as externs
 */
extern struct rtp_chan chan[];
extern int oflag[];

#endif

