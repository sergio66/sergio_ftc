C***********************************************************************
C This version for set4 using FCOWp data files
C FCOW fit with (11 fixed, 11 CO, 3 ozone, 13 water)
C
C      File FARRAY.F (included file) 
C
C      Purpose : Assigns some array dimensions for use with the fast
C                transmittance program. 
C
C      Arguments
C         MAXPRO    I o  : the number of fitting profiles
C         MAXLAY    I o  : the number of levels
C         NODATA    R o  : a value used to indicate no (good) data
C
C***********************************************************************
C
C      -------------------------GENERIC---------------------------------
C      Generic parameters independent of the details of how the gases
C      are to be broken out or the fine details of the data files & fits
C      -----------------------------------------------------------------
       INTEGER IOERR   ! unit number for error messages
       INTEGER IDFIX   ! HITRAN gas ID number to use for fixed gases
       INTEGER MAXPRO  ! number of regression profiles
       INTEGER MAXLAY  ! number of layers
       INTEGER MAXANG  ! number of angles
       REAL NODATA  ! flag value for no data
       REAL NEGDAT  ! flag value for transmittance < 0
       DOUBLE PRECISION  DKMIN  ! minimum optical depth to consider
       DOUBLE PRECISION  DKMAX  ! maximum optical depth to consider
C
       DOUBLE PRECISION  DKZMIN  ! minimum l-to-s op depth to consider
       DOUBLE PRECISION  DKZMAX  ! maximum l-to-s op depth to consider
       REAL TAUMIN               ! minimum transmittance to consider
       REAL TAUDEL               ! minimum delta trans to consider
C
       PARAMETER(IOERR=0)
       PARAMETER(IDFIX=2)
       PARAMETER(MAXPRO=48)
       PARAMETER(MAXLAY=100)
       PARAMETER(MAXANG=12)
       PARAMETER(NODATA=-99.0)
       PARAMETER(NEGDAT=-100.0)
C
       PARAMETER(DKMIN=1.5D-5)
       PARAMETER(DKMAX=6.1D+0)
C
       PARAMETER(DKZMIN=2.0D-5)
       PARAMETER(DKZMAX=6.7D+0)
       PARAMETER(TAUMIN=1.5E-5)
       PARAMETER(TAUDEL=2.5E-5)
C
       INTEGER   MAXM  ! max rows (conv trans) for M x N fit array
       PARAMETER(MAXM=MAXPRO*MAXANG)
C
       INTEGER MAXBOG  ! max possible number of gases to breakout & fit
C      Note: MAXBOG affects the number of variables needed in BREAKOUT
C      GASES and DATA FILES sections below, as well as the "sort.f"
C      routine.  Currently set up for MAXBOG=6
       PARAMETER(MAXBOG=6)
C
C      ----------------------BREAKOUT GASES-----------------------------
C      The following variables specify the number of gases to be broken
C      out and fit, as well as the order of breakout and the gas IDs.
C      Use the HITRAN gas ID numbers.  The fixed gases should probably
C      always be the first gas broken out.  This info is used to load
C      up LSTGAS in the executable portion of the code.  There must be
C      an IGAS<i> variable declared and assigned for i=1 to MAXBOG.
C      Assign values of 0 to IGAS<i> for i > NUMGAS.
C      -----------------------------------------------------------------
C      Note: these values are for all FCOW (ie F,FC,FCO,FCOW) fits.
       INTEGER NUMGAS  ! number of gases to fit
       PARAMETER(NUMGAS=4)
C
C      Note: currently set for MAXBOG=6
       INTEGER  IGAS1  ! first breakout gas ID
       INTEGER  IGAS2  ! second breakout gas ID
       INTEGER  IGAS3  ! third breakout gas ID
       INTEGER  IGAS4  ! fourth breakout gas ID
       INTEGER  IGAS5  ! fifth breakout gas ID
       INTEGER  IGAS6  ! sixth breakout gas ID
       PARAMETER( IGAS1=IDFIX )  ! fixed
       PARAMETER( IGAS2=5 )      ! CO
       PARAMETER( IGAS3=3 )      ! ozone
       PARAMETER( IGAS4=1 )      ! water
       PARAMETER( IGAS5=0 )
       PARAMETER( IGAS6=0 )
C
C
C      ---------------------DATA FILES----------------------------------
C      The following variables specify the total number of sets of conv
C      trans data in the data files, as well as the indices of the trans
C      data to be used in the fits.  There must be an IBOK<i> variable
C      declared and assigned for i=1 to MAXBOG.  Assign values of 0 to
C      IBOK<i> for i > NUMGAS.
C      -----------------------------------------------------------------
C      Note: these values are specific to FCOWp conv trans data files
       INTEGER NUMSET  ! number of gases/trans in conv trans data files
c       PARAMETER(NUMSET=5)  ! data files are FCOWp = F,FC,FCO,FCOW,FCOWp
       PARAMETER(NUMSET=4)  ! data files are FCOWp = F,FC,FCO,FCOW
C
C      Note: currently set for MAXBOG=6
       INTEGER  IBOK1  ! first breakout gas index
       INTEGER  IBOK2  ! second breakout gas index
       INTEGER  IBOK3  ! third breakout gas index
       INTEGER  IBOK4  ! fourth breakout gas index
       INTEGER  IBOK5  ! fifth breakout gas index
       INTEGER  IBOK6  ! sixth breakout gas index
       PARAMETER( IBOK1=1 )  ! F=index 1
       PARAMETER( IBOK2=2 )  ! FC=index 2
       PARAMETER( IBOK3=3 )  ! FCO=index 3
       PARAMETER( IBOK4=4 )  ! FCOW=index 4
       PARAMETER( IBOK5=0 )
       PARAMETER( IBOK6=0 )
C
C      ---------------------FIT COEFFICIENTS----------------------------
C      Specify the number of coefficients for each fit gas, as well as
C      the maximum number of coefs for any one fit and the total number
C      of coefs.
C      -----------------------------------------------------------------
C      Note: the NPREDx variables may not exist in all other versions
C      of the fitting program.
       INTEGER NPREDF  ! number of Fixed gases coefs
       INTEGER NPREDC  ! number of CO coefs
       INTEGER NPREDO  ! number of Ozone coefs
       INTEGER NPREDW  ! number of Water coefs
C
       PARAMETER(NPREDF=11)
       PARAMETER(NPREDC=11)
       PARAMETER(NPREDO=3)
       PARAMETER(NPREDW=13)
C
C      Note: the MAXx variables exist in all other versions of the
C      program, but the values are specific to sunmfbw.
       INTEGER   MAXN  ! max of NPREDx; max cols in M x N fit array
       INTEGER MAXPRD  ! sum of NPREDx

       PARAMETER(MAXN=13)
       PARAMETER(MAXPRD=NPREDF + NPREDC + NPREDO + NPREDW)
C
C***********************************************************************
