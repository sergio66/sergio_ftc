
/* RTP function prototypes */

int rtpread1(char *fname,               /* RTP file name             IN  */
             struct rtp_head *head,     /* RTP header structure      IN  */
             struct ALIST (**hatt)[],   /* header attributes         OUT */
             int *nhatt,                /* number of header attr's   OUT */
             struct ALIST (**patt)[],   /* profile attributes        OUT */
             int *npatt,                /* number of profile attr's  OUT */
             int *pchan                 /* RTP profile channel       OUT */
             );

int rtpread2(int ci,            	/* RTP channel          IN  */
             char *sbuf         	/* structure buffer     OUT */
             );

int rtpwrite1(char *fname,              /* RTP file name             IN  */
              struct rtp_head *head,    /* RTP header structure      IN  */
              struct ALIST (*hatt)[],   /* header attributes         IN  */
              int nhatt,                /* number of header attr's   IN  */
              struct ALIST (*patt)[],   /* profile attributes        IN  */
              int npatt,                /* number of profile attr's  IN  */
              int *pchan                /* RTP profile channel       OUT */
              );

int rtpwrite2(int ci,           	/* RTP channel          IN */
              char *sbuf        	/* structure buffer     IN */
              );

int rtpclose1(int ci);

void gsplit(
            int32 ngas,                 /* number of gasses     IN  */
            int32 glist[],              /* list of gasses       IN  */
            int nflist1,                /* input FLIST size     IN  */
            struct FLIST (*flist1)[],   /* input FLIST          IN  */
            int *nflist2,               /* output FLIST size    OUT */
            struct FLIST (*flist2)[]    /* output FLIST         OUT */
            );


void fmatch(
            struct FLIST (*f1)[],  /* field list f1, source       IN  */
            int n1,                /* number of f1 fields         IN  */
            struct FLIST (*f2)[],  /* field list f2, dest.        IN  */
            int n2,                /* number of f2 fields         IN  */
            int **pp1,             /* field offsets in f1 buffer  OUT */
            int **pp2,             /* field offsets in f2 buffer  OUT */
            int **pp3,             /* sizes of matching fields    OUT */
            int *s1,               /* size of f1 buffer           OUT */
            int *s2                /* size of f2 buffer           OUT */
            );

int hsize (int htype);

