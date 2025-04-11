c One term "so" (small ozone) ozone fit
C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             FITO_so1
C
!F77====================================================================


!ROUTINE NAME:
C    FITO


!ABSTRACT:
C    Solve for the ozone fast transmittance coefs.


!CALL PROTOCOL:
C    FITO(LSTGAS, ICHAN, NDTPTS, NNEG, IOFFST, PRED, KLTOS, KLAYER,
C       KZMIN, KZMAX, KMIN, KMAX, SECANG, FTCOEF)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INT ARR    LSTGAS         Gas IDs for fit    none
C    INTEGER    ICHAN          Channel no.        none
C    INT ARR    NDTPTS         No. of data points none
C    INT ARR    NNEG           No. of negative pt none
C    INT ARR    IOFFST         PRED index offsets none
C    REAL ARR   PRED           Predictors         various
C    REAL ARR   KLTOS          Eff l-to-s op dpth none
C    REAL ARR   KLAYER         Eff layer op depth none
C    REAL ARR   KZMIN          Min l-to-s op dpth none
C    REAL ARR   KZMAX          Max l-to-s op dpth none
C    REAL ARR   KMIN           Min layer op depth none
C    REAL ARR   KMAX           Max layer op depth none
C    REAL ARR   SECANG         Angle secants      none


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    REAL ARR   FTCOEF         Fast trans coefs   various


!INPUT/OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    FITFTC


!ROUTINES CALLED:
C    WGHT2A: weight the regression data
C    FITGAS: Solves for the fast trans coefs


!FILES ACCESSED:
C    none


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine FITO solves for the ozone fast transmittance
C    (actually optical depth) coefficients. The routine determines
C    how many predictors to use and does the regression. It then does
C    another regression with one fewer predictors, and compares the
C    fit error with the those from the previous regression. If the fit
C    with the fewer predictors is not significantly worse, then it
C    repeats this process of subtracting a predictor and comparing
C    errors. It ends when the fits get much worse, or it reaches the
C    minimun number of predictors.
C    This versions is for FWO.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    25 Jun 97 Scott Hannon      Created from 5 terms FWO version
C    26 Aug 99 Scott Hannon      Add LSTGAS stuff
C    21 May 02 Scott Hannon      Error messages now use IOERR (was 6)


!END ===================================================================


C      =================================================================
       SUBROUTINE FITO(LSTGAS, ICHAN, NDTPTS, NNEG, IOFFST, PRED,
     $    KLTOS, KLAYER, KZMIN, KZMAX, KMIN, KMAX, SECANG, FTCOEF)
C      =================================================================


C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
       INCLUDE 'farray.f'


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C    none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input parameters
       INTEGER LSTGAS(MAXBOG)
       INTEGER  ICHAN
       INTEGER NDTPTS(MAXLAY,NUMGAS)
       INTEGER   NNEG(MAXLAY,NUMGAS)
       INTEGER IOFFST(NUMGAS)
       REAL   PRED(MAXPRD,MAXPRO,MAXANG,MAXLAY)
       REAL  KLTOS(MAXPRO,MAXANG,MAXLAY,NUMGAS)
       REAL KLAYER(MAXPRO,MAXANG,MAXLAY,NUMGAS)
       REAL  KZMIN(MAXLAY,NUMGAS)
       REAL  KZMAX(MAXLAY,NUMGAS) 
       REAL   KMIN(MAXLAY,NUMGAS)
       REAL   KMAX(MAXLAY,NUMGAS)
       REAL SECANG(MAXANG)
C
C      Output parameters
       REAL FTCOEF(MAXPRD,MAXLAY)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      I
       INTEGER  IGASF
       INTEGER  IGASO
       INTEGER  IGASW
       INTEGER   ILAY
       INTEGER      J
       INTEGER MINPTS(  MAXN)
       INTEGER   MPTS
       INTEGER MXPRED
       INTEGER NEGPTS
       INTEGER  NPRED
C
       REAL ALLRMS(  MAXN)
       REAL RATNEG
       REAL    XKO
C
C      For WGHT
       INTEGER  NGASZ
       INTEGER  IGASZ(NUMGAS)
       LOGICAL   LUSE(  MAXN)
       DOUBLE PRECISION   AMAT(  MAXM,  MAXN)
       DOUBLE PRECISION   BVEC(  MAXM)
C
C      For FITGAS
       INTEGER   IBAD
       DOUBLE PRECISION   XVEC(  MAXN)


C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C    none


C***********************************************************************
C***********************************************************************
C                            EXECUTABLE CODE
C***********************************************************************
C***********************************************************************
C
C      Check the gases are as expected
       IGASF=-1
       IGASW=-1
       IGASO=-1
       DO I=1,NUMGAS
          IF (LSTGAS(I) .EQ. IDFIX) IGASF=I
          IF (LSTGAS(I) .EQ. 1)     IGASW=I
          IF (LSTGAS(I) .EQ. 3)     IGASO=I
       ENDDO
       IF (NUMGAS .NE. 3) THEN
          WRITE(IOERR,1010) NUMGAS
 1010     FORMAT('Error: fito_fwo_so1 expected NUMGAS=3, not ',I2)
          STOP
       ENDIF
       IF ((IGASF .NE. 1) .OR.
     $    (IGASW .NE. 2 .AND. IGASW .NE. 3) .OR.
     $    (IGASO .NE. 2 .AND. IGASO .NE. 3)) THEN
          WRITE(IOERR,1020) IDFIX, (LSTGAS(I),I=1,3)
 1020     FORMAT('Error: fito_fwo_so1 expected LSTGAS=(',I2,
     $    ',[1 or 3], [3 or 1]), not (',I2,',',I2,',',I2,')')
          STOP
       ENDIF
       IF (NPREDO .NE. 1) THEN
          WRITE(IOERR,1030) NPREDO
 1030     FORMAT('Error: fito_fwo_so1 expecting NPREDO=1, not ',I2)
          STOP
       ENDIF
C
C
C      Define the mininum number of points needed to do a regression
C      with a certain number of predictors.
       J=MAXANG + 3
       DO I=1,MAXN
          MINPTS(I)=J*I
          LUSE(I)=.FALSE.
       ENDDO
C
       NGASZ=3
       IGASZ(1)=1
       IGASZ(2)=2
       IGASZ(3)=3
C
C      ------------------------------------------
C      Loop over the layers and do the regression
C      ------------------------------------------
       DO ILAY=1,MAXLAY
C
          MPTS=NDTPTS(ILAY,IGASO)
          NEGPTS=NNEG(ILAY,IGASO)
          RATNEG=FLOAT(NEGPTS)/FLOAT(MPTS + NEGPTS)
          XKO=KMAX(ILAY,IGASO)
C
C         ------------------------------------
C         Determine how many predictors to use
C         ------------------------------------
C
          IF ((XKO .GT. 5.0E-7) .AND. (MPTS .GT. MINPTS(1)) .AND.
     $    (RATNEG .LT. 0.50)) THEN
             MXPRED=1
             LUSE(1)=.TRUE.
          ELSE
             MXPRED=0
             FTCOEF(IOFFST(IGASO)+1,ILAY)=0.0E+0
          ENDIF 
C
C         -----------------
C         Do the regression (if necessary)
C         -----------------
          IF (MXPRED .EQ. 1) THEN
C
C            -------------------------
C            Scale the regression data
C            and load AMAT and BVEC.
C            -------------------------
C
             CALL WGHT2A(ILAY, IGASO, NGASZ, IGASZ, IGASF, MPTS,
     $          MXPRED, IOFFST, PRED, LUSE, KLTOS, KLAYER, SECANG,
     $          AMAT, BVEC)
C
C            -----------------
C            Do the regression
C            -----------------
C
             NPRED=1
C
             CALL FITGAS(MPTS, NPRED, AMAT, BVEC, XVEC, ALLRMS(NPRED),
     $          IBAD)
C
C            Check the "bad fit" flag and move fit results to FTCOEF
             IF (IBAD .EQ. 0) THEN
                FTCOEF(IOFFST(IGASO)+1,ILAY)=SNGL( XVEC(1) )
             ELSE
                FTCOEF(IOFFST(IGASO)+1,ILAY)=0.0E+0
             ENDIF
C
          ENDIF
C
       ENDDO
C      End loop over layers
C
       RETURN
       END
