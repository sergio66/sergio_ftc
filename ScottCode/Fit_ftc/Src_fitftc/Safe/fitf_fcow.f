C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             FITF_fcow
C
!F77====================================================================


!ROUTINE NAME:
C    FITF


!ABSTRACT:
C    Solve for the fixed gases fast transmittance coefs.


!CALL PROTOCOL:
C    FITF(LSTGAS, ICHAN, NDTPTS, NNEG, IOFFST, PRED, KLTOS, KLAYER,
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
C    REAL ARR   SECANG         Angle secant       none


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
C    Routine FITF solves for the fixed gases fast transmittance
C    (actually optical depth) coefficients. The routine determines
C    how many predictors to use and does the regression. It then does
C    another regression with one fewer predictors, and compares the
C    fit error with the those from the previous regression. If the fit
C    with the fewer predictors is not significantly worse, then it
C    repeats this process of subtracting a predictor and comparing
C    errors. It ends when the fits get much worse, or it reaches the
C    minimun number of predictors.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C     7 Jan 97 Scott Hannon      Created
C    19 May 97 Scott Hannon      Modified for sun
C    30 Aug 99 Scott Hannon      Add LSTGAS stuff; changed from wght1
C                                   to WGHT2A
C    21 May 02 Scott Hannon      Error messages now use IOERR (was 6)
C    19 Dec 07 Scott Hannon      Added NUANG unique angle count & checks

!END ===================================================================


C      =================================================================
       SUBROUTINE FITF(LSTGAS, ICHAN, NDTPTS, NNEG, IOFFST, PRED,
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
       INTEGER  IBEST
       INTEGER  IGASC
       INTEGER  IGASF
       INTEGER  IGASO
       INTEGER  IGASW
       INTEGER   ILAY
       INTEGER  IPRED
       INTEGER      J
       INTEGER MINCOF
       INTEGER MINPTS(  MAXN)
       INTEGER   MPTS
       INTEGER MXPRED
       INTEGER  MXUSE
       INTEGER  NPRED
C
       REAL ALLCOF(  MAXN,  MAXN)
       REAL ALLRMS(  MAXN)
       REAL    DIF
c       REAL  DIFWN
       REAL   RATK
       REAL SIGDIF
       REAL    XKF
       REAL XKFCOW
       REAL    XKZ
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

C      For count of unique angles
       INTEGER  IUANG
       INTEGER  NUANG

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
       IGASC=-1
       IGASO=-1
       IGASW=-1
       DO I=1,NUMGAS
          IF (LSTGAS(I) .EQ. IDFIX) IGASF=I
          IF (LSTGAS(I) .EQ. 5)     IGASC=I
          IF (LSTGAS(I) .EQ. 3)     IGASO=I
          IF (LSTGAS(I) .EQ. 1)     IGASW=I
       ENDDO
       IF (NUMGAS .NE. 4) THEN
          WRITE(IOERR,1010) NUMGAS
 1010     FORMAT('Error: fitf_fcow expected NUMGAS=4, not ',I2)
          STOP
       ENDIF
       IF ((IGASF .NE. 1) .OR.
     $    (IGASC .NE. 2 .AND. IGASC .NE. 3) .OR.
     $    (IGASO .NE. 2 .AND. IGASO .NE. 3) .OR.
     $    (IGASW .NE. 4)) THEN
          WRITE(IOERR,1020) IDFIX, (LSTGAS(I),I=1,4)
 1020     FORMAT('Error: fitf_fcow expected LSTGAS=(',I2,
     $    ',[5 or 3], [3 or 5], 1), not (',I2,',',I2,',',I2,',',I2,')')
          STOP
       ENDIF
       IF (NPREDF .NE. 11) THEN
          WRITE(IOERR,1030) NPREDF
 1030     FORMAT('Error: fitf_fcow expecting NPREDF=11, not ',I2)
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
       MINCOF=3
C
       NGASZ=1
       IGASZ(1)=IGASF
C
C      Assign the value to use in testing test for
C      a significant difference in fit results.
       SIGDIF=1.0E-2
C
C      ------------------------------------------
C      Loop over the layers and do the regression
C      ------------------------------------------
       DO ILAY=1,MAXLAY
C
          MPTS=NDTPTS(ILAY,IGASF)
          XKF=KMAX(ILAY,IGASF)
          XKFCOW=KMIN(ILAY,IGASF) + KMIN(ILAY,IGASC) +
     $       KMIN(ILAY,IGASO) + KMIN(ILAY,IGASW)
          XKZ=KZMIN(ILAY,IGASF) + KZMIN(ILAY,IGASC) +
     $       KZMIN(ILAY,IGASO) + KZMIN(ILAY,IGASW)
          RATK=XKF/XKFCOW
C         Note: Since F is the first gas broken out, there should
C         never be any negative points.
C

C         Count the number of unique angles
C         Note: MPTS = number of KLAYER > 0
          NUANG = 0
          J=0
          DO I=1,MAXANG
             IUANG=0
             DO J=1,MAXPRO
                IF (KLAYER(J,I,ILAY,IGASF) .GT. 0.0E+0) THEN
                   IUANG=1
                ENDIF
             ENDDO
             NUANG=NUANG + IUANG
          ENDDO

C         ------------------------------------
C         Determine how many predictors to use
C         ------------------------------------
C
          IF ((MPTS .GT. MINPTS(3)) .AND. (XKF .GT. 8.0E-7)
     $    .AND. (NUANG .GE. 2)) THEN
             MXPRED=3
             LUSE(1)=.TRUE.
             LUSE(2)=.TRUE.
             LUSE(3)=.TRUE.
             MXUSE=3
C
             IF ((MPTS .GT. MINPTS(4)) .AND. (XKF .GT. 1.0E-6)
     $       .AND. (XKZ .LT. 1.0E+1)) THEN
                MXPRED=4
                LUSE(4)=.TRUE.
                MXUSE=MXUSE+1
C
                IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $          (XKF .GT.1.0E-5) .AND. (RATK .GT. 1.0E-2)) THEN
                   MXPRED=5
                   LUSE(5)=.TRUE.
                   MXUSE=MXUSE+1
C
                   IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $             (XKF .GT. 1.0E-4) .AND. (RATK .GT. 1.0E-2)) THEN
                      MXPRED=6
                      LUSE(6)=.TRUE.
                      MXUSE=MXUSE+1
C
                      IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $                (XKF .GT. 1.0E-3) .AND. (XKZ .LT. 7.0) .AND.
     $                (RATK .GT. 0.02)) THEN
                         IF (ILAY .GT. 2) THEN
                            MXPRED=7
                            LUSE(7)=.TRUE.
                            MXUSE=MXUSE+1
                         ENDIF
C
                         IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $                   (RATK .GT. 0.03) .AND. (NUANG .GE. 3)) THEN
                            IF (ILAY .GT. 2) THEN
                               MXPRED=8
                               LUSE(8)=.TRUE.
                               MXUSE=MXUSE+1
                            ENDIF
C
                            IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $                      (RATK .GT. 0.04)) THEN
                               MXPRED=9
                               LUSE(9)=.TRUE.
                               MXUSE=MXUSE+1
C
                               IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $                         (RATK .GT. 0.05) .AND. (NUANG .GE. 4))
     $                         THEN
                                  MXPRED=10
                                  LUSE(10)=.TRUE.
                                  MXUSE=MXUSE+1
C
                                  IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $                            (RATK .GT. 0.10) .AND. (ILAY .GT. 2)
     $                            .AND. (NUANG .GE. 5)) THEN
                                     MXPRED=11
                                     LUSE(11)=.TRUE.
                                     MXUSE=MXUSE+1
C
                                  ENDIF
                               ENDIF
                            ENDIF
                         ENDIF
                      ENDIF
                   ENDIF 
                ENDIF
             ENDIF
C
          ELSE
             MXPRED=0
             DO IPRED=1,NPREDF
                FTCOEF(IPRED+IOFFST(IGASF),ILAY)=0.0E+0
             ENDDO
          ENDIF 
C
c      write(6,*) 'f layer=',ilay,', mpts=',mpts,', mxpred=',mxpred
C         -----------------
C         Do the regression (if necessary)
C         -----------------
          IF (MXPRED .GT. 0) THEN
C
C            -------------------------
C            Scale the regression data
C            and load AMAT and BVEC.
C            -------------------------
C
             CALL WGHT2A(ILAY, IGASF, NGASZ, IGASZ, IGASF, MPTS,
     $          MXPRED, IOFFST, PRED, LUSE, KLTOS, KLAYER, SECANG,
     $          AMAT, BVEC)
C
C            -----------------
C            Do the regression
C            -----------------
C
             NPRED=MXUSE + 1
C
 10          NPRED=NPRED - 1
C
             CALL FITGAS(MPTS, NPRED, AMAT, BVEC, XVEC, ALLRMS(NPRED),
     $          IBAD)
c      write(6,*) 'f npred=',npred,', rms=',ALLRMS(NPRED)
C
C            Check the "bad fit" flag
             IF (IBAD .EQ. 0) THEN
                DO IPRED=1,NPRED
                   ALLCOF(IPRED,NPRED)=SNGL( XVEC(IPRED) )
                ENDDO
             ELSE
c      write(6,*) 'f npred=',npred,', ibad=',ibad
                DO IPRED=1,NPRED
                   ALLCOF(IPRED,NPRED)=0.0E+0
                ENDDO
             ENDIF
C
C            --------------------------------------
C            Determine whether or not to do another
C            regression with fewer predictors.
C            --------------------------------------
C
             IF (MXUSE .EQ. MINCOF) THEN
C               The last fit is the only fit allowed
                IBEST=MINCOF
             ELSE
                IF (NPRED .EQ. MXUSE) THEN
C                  The last fit was the first fit; do another
                   IBEST=MXUSE
                   GOTO 10
                ELSE
C                  Compare the last fit to the best so far
                   DIF=(ALLRMS(NPRED) - ALLRMS(IBEST))/ALLRMS(IBEST)
                   IF (DIF .LT. SIGDIF) THEN
C
C                     Re-set IBEST if the last fit is better than
C                     previous best (this shouldn't happen, but...)
                      IF (DIF .LT. 0.0E+0) IBEST=NPRED
C
C                     Set IBEST if this the last fit allowed
                      IF (NPRED .EQ. MINCOF) IBEST=MINCOF
C
C                     The last fit was good; do another
                      IF (NPRED .GT. MINCOF) GOTO 10
                   ELSE
C                     The last fit was not good enough
                      IBEST=NPRED + 1
                   ENDIF
                ENDIF
             ENDIF
C
C            Load out the fit results into FTCOEF
             I=0
             DO IPRED=1,NPREDF
                IF ((I .LT. IBEST) .AND. (LUSE(IPRED))) THEN
                   I=I + 1
                   FTCOEF(IPRED+IOFFST(IGASF),ILAY)=ALLCOF(I,IBEST)
                ELSE
                   FTCOEF(IPRED+IOFFST(IGASF),ILAY)=0.0E+0
                ENDIF
             ENDDO
C
c      write(6,*) 'f ibest=',ibest
          ENDIF
C
       ENDDO
C      End loop over layers
C
       RETURN
       END
