C August 1999 version for Optran
C***********************************************************************
C
C      File FARRAY.F (included file) for OPTRAN
C
C      Purpose : Assigns some array dimensions for use with the fast
C                transmittance program. 
C
C***********************************************************************
C
       INTEGER IOERR   ! unit number for error messages
       INTEGER MAXPRO
       INTEGER MAXLAY
       INTEGER MAXANG
       PARAMETER( IOERR = 0)
       PARAMETER( MAXPRO = 48 )
       PARAMETER( MAXLAY = 100 )
       PARAMETER( MAXANG = 6 )
C
       REAL NODATA
       PARAMETER( NODATA = -9.99E+2 )
C
       INTEGER NOPLEV
       PARAMETER( NOPLEV = 300 )
C
       INTEGER NPREDW
       INTEGER MAXPRD
       PARAMETER( NPREDW = 9 )
       PARAMETER( MAXPRD = NPREDW )
C
       INTEGER   MAXM  ! max num of rows = num profs x num angles
       INTEGER   MAXN  ! max num of cols = max number of predictors
       PARAMETER(   MAXM = MAXPRO*MAXANG )
       PARAMETER(   MAXN = 9 )
C
       INTEGER MAXBOG
       INTEGER NUMGAS
       INTEGER  IDFIX
       PARAMETER(MAXBOG=6)
       PARAMETER(NUMGAS=1)
       PARAMETER(IDFIX=2)
C      ---------------------DATA FILES----------------------------------
C      The following variables specify the total number of sets of conv
C      trans data in the data files, as well as the indices of the trans
C      date to be used in the fits.
C      -----------------------------------------------------------------
C      Note: these values are specific to FOWp conv trans data files
       INTEGER NUMSET  ! number of gases/trans in conv trans data files
       PARAMETER(NUMSET=4)  ! data files are FOWp = F,FO,FOW,FOWp
C
       INTEGER  IBOKX  ! index for whatever
       INTEGER  IBOKW  ! index for whatever & water
       PARAMETER( IBOKX=2 )  ! FO=index 1
       PARAMETER( IBOKW=3 )  ! FOW=index 2
C
C***********************************************************************
