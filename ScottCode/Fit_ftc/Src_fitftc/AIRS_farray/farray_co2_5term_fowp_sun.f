C***********************************************************************
c This version for CO2 perturbation (4 term)
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
ccc
cC      IASI values
c       PARAMETER(DKMIN=1.5D-5)
c       PARAMETER(DKMAX=6.1D+0)
c       PARAMETER(DKZMIN=2.0D-5)
c       PARAMETER(DKZMAX=6.7D+0)
c       PARAMETER(TAUMIN=1.5E-5)
c       PARAMETER(TAUDEL=2.5E-5)
ccc
C
C      AIRS values
       PARAMETER(DKMIN=1.0D-6)
       PARAMETER(DKMAX=2.5D+1)
       PARAMETER(DKZMIN=5.0D-7)
       PARAMETER(DKZMAX=3.5E+1)
       PARAMETER(TAUMIN=1.0E-15)
       PARAMETER(TAUDEL=1.0E-11)
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
C      Note: these values are for all CO2 fits.
       INTEGER NUMGAS  ! number of gases to breakout
       PARAMETER(NUMGAS=2)
C
C      Note: currently set for MAXBOG=6
       INTEGER  IGAS1  ! first breakout gas ID
       INTEGER  IGAS2  ! second breakout gas ID
       INTEGER  IGAS3  ! third breakout gas ID
       INTEGER  IGAS4  ! fourth breakout gas ID
       INTEGER  IGAS5  ! fifth breakout gas ID
       INTEGER  IGAS6  ! sixth breakout gas ID
       PARAMETER( IGAS1=IDFIX )  ! all gases
       PARAMETER( IGAS2=-IDFIX ) ! all gases with CO2 pert
       PARAMETER( IGAS3=0 )
       PARAMETER( IGAS4=0 )
       PARAMETER( IGAS5=0 )
       PARAMETER( IGAS6=0 )
C
C
C      ---------------------DATA FILES----------------------------------
C      The following variables specify the total number of sets of conv
C      trans data in the data files, as well as the indices of the trans
C      date to be used in the fits.  There must be an IBOK<i> variable
C      declared and assigned for i=1 to MAXBOG.  Assign values of 0 to
C      IBOK<i> for i > NUMGAS.
C      -----------------------------------------------------------------
C      Note: these values are specific to FOWP conv trans data files
       INTEGER NUMSET  ! number of gases/trans in conv trans data files
       PARAMETER(NUMSET=4) ! data files are FOWp = F,FO,FOW,FOWp
C
C      Note: currently set for MAXBOG=6
       INTEGER  IBOK1  ! first breakout gas index
       INTEGER  IBOK2  ! second breakout gas index
       INTEGER  IBOK3  ! third breakout gas index
       INTEGER  IBOK4  ! fourth breakout gas index
       INTEGER  IBOK5  ! fifth breakout gas index
       INTEGER  IBOK6  ! sixth breakout gas index
       PARAMETER( IBOK1=3 )  ! all gases
       PARAMETER( IBOK2=4 )  ! all gases with CO2 pert
       PARAMETER( IBOK3=0 )
       PARAMETER( IBOK4=0 )
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
       INTEGER NPREDP  ! number of CO2 Pert coefs
C
c       PARAMETER(NPREDP=4)
       PARAMETER(NPREDP=5)
C
C      Note: the MAXx variables exist in all other versions of the
C      program, but the values are specific to sunmfbw.
       INTEGER   MAXN  ! max of NPREDx; max cols in M x N fit array
       INTEGER MAXPRD  ! sum of NPREDx

c       PARAMETER(MAXN=4)
       PARAMETER(MAXN=5)
       PARAMETER(MAXPRD=NPREDP)
C
C      Specify the CO2 coefficent multipler.
C      Note: currently the AIRS-RTA fast model assumes the CO2
C      perturbation coefficients are normalized for a +3% offset.
C      If the regression data was calced using a different CO2
C      offset, then CMULT must be set to re-normalize the output
C      coefficients.  For example, if the regreesion data is for
C      a +5% offset, then CMULT should be 0.6 (ie 3/5).
       REAL CMULT      ! multipler to apply to output coefs
C
       PARAMETER (CMULT=0.6)
C
C***********************************************************************
