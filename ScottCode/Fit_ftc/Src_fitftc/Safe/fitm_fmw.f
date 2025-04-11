C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             FITM_fmw
C
!F77====================================================================


!ROUTINE NAME:
C    FITM


!ABSTRACT:
C    Solve for the methane fast transmittance coefs.
C    This version is for FMW.


!CALL PROTOCOL:
C    FITM(LSTGAS, ICHAN, NDTPTS, NNEG, IOFFST, PRED, KLTOS, KLAYER,
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
C    Routine FITM solves for the methane fast transmittance
C    (actually optical depth) coefficients. The routine determines
C    how many predictors to use and does the regression. It then does
C    another regression with one fewer predictors, and compares the
C    fit error with the those from the previous regression. If the fit
C    with the fewer predictors is not significantly worse, then it
C    repeats this process of subtracting a predictor and comparing
C    errors. It ends when the fits get much worse, or it reaches the
C    minimun number of predictors.
C    This versions is for FMW.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    15 Jan 97 Scott Hannon      Created
C    30 Aug 99 Scott Hannon      Add LSTGAS & SECANG
C    21 May 02 Scott Hannon      Error messages now use IOERR (was 6)


!END ===================================================================


C      =================================================================
       SUBROUTINE FITM(LSTGAS, ICHAN, NDTPTS, NNEG, IOFFST, PRED,
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
       INTEGER  IGASF
       INTEGER  IGASM
       INTEGER  IGASW
       INTEGER   ILAY
       INTEGER  IPRED
       INTEGER      J
       INTEGER MINCOF
       INTEGER MINPTS(  MAXN)
       INTEGER   MPTS
       INTEGER MXPRED
       INTEGER  MXUSE
       INTEGER NEGPTS
       INTEGER  NPRED
C
       REAL ALLCOF(  MAXN,  MAXN)
       REAL ALLRMS(  MAXN)
       REAL    DIF
c       REAL  DIFWN
       REAL   RATK
       REAL SIGDIF
       REAL     XF
       REAL    XKM
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
       IGASM=-1
       IGASW=-1
       DO I=1,NUMGAS
          IF (LSTGAS(I) .EQ. IDFIX) IGASF=I
          IF (LSTGAS(I) .EQ. 6)     IGASM=I
          IF (LSTGAS(I) .EQ. 1)     IGASW=I
       ENDDO
       IF (NUMGAS .NE. 3) THEN
          WRITE(IOERR,1010) NUMGAS
 1010     FORMAT('Error: fitm_fmw expected NUMGAS=3, not ',I2)
          STOP
       ENDIF
       IF ((IGASF .NE. 1) .OR. (IGASM .NE. 2) .OR.
     $    (IGASW .NE. 3)) THEN
          WRITE(IOERR,1020) IDFIX, (LSTGAS(I),I=1,3)
 1020     FORMAT('Error: fitm_fmw expected LSTGAS=(',I2,
     $    ', 6, 1), not (',I2,',',I2,',',I2,')')
          STOP
       ENDIF
       IF (NPREDM .NE. 9) THEN
          WRITE(IOERR,1030) NPREDM
 1030     FORMAT('Error: fitm_fmw expecting NPREDM=9, not ',I2)
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
       MINCOF=1
C
       NGASZ=2
       IGASZ(1)=IGASF
       IGASZ(2)=IGASM
C
C      Assign the value to use in testing for
C      a significant difference in fit results.
       SIGDIF=1.0E-2
C
C      ------------------------------------------
C      Loop over the layers and do the regression
C      ------------------------------------------
       DO ILAY=1,MAXLAY
C
          MPTS=NDTPTS(ILAY,IGASM)
          NEGPTS=NNEG(ILAY,IGASM)
          XKM=KMAX(ILAY,IGASM)
          XF=KMIN(ILAY,IGASF)
          XKZ=KZMIN(ILAY,IGASF) + KZMIN(ILAY,IGASM)
          RATK=XKM/XF
C
C         ---------------------------------
C         Determine which predictors to use
C         ---------------------------------
C
          IF ((MPTS .GT. MINPTS(1)) .AND. (MPTS .GT. NEGPTS) .AND.
     $    (NEGPTS .LT. 20)) THEN
             MXPRED=1
             LUSE(1)=.TRUE.
             MXUSE=1
C
             IF ((MPTS .GT. MINPTS(2)) .AND. (XKM .GT. 1.0E-5) .AND.
     $       (XKZ .LT. 1.0E+1) .AND. (NEGPTS .LT. 10) .AND.
     $       (RATK .GT. 1.0E-2)) THEN
                MXPRED=2
                LUSE(2)=.TRUE.
                MXUSE=2
C
                IF ((MPTS .GT. MINPTS(3)) .AND. (XKZ .LT. 8.0E+0)
     $          .AND. (XKM .GT. 2.0E-4)) THEN
                   MXPRED=3
                   LUSE(3)=.TRUE.
                   MXUSE=3
C
                   IF (MPTS .GT. MINPTS(MXUSE+1) .AND.
     $             (XKM .GT. 4.0E-4)) THEN
                      MXPRED=4
                      LUSE(4)=.TRUE.
                      MXUSE=4
C
                      IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $                (XKM .GT. 7.0E-4) .AND. (XKZ .LT. 7.0E+0)) THEN
                         MXPRED=5
                         LUSE(5)=.TRUE.
                         MXUSE=5
C
                         IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $                   (XKM .GT. 1.0E-3) .AND. (RATK .GT. 5.0E-2))
     $                   THEN
                            IF (ILAY .GT. 3) THEN
                               MXPRED=6
                               LUSE(6)=.TRUE.
                               MXUSE=MXUSE + 1
                            ENDIF
C
                            IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $                      (XKZ .LT. 6.0E+0)) THEN
                               MXPRED=7
                               LUSE(7)=.TRUE.
                               MXUSE=MXUSE + 1
C
                               IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $                         (XKM .GT. 5.0E-3) .AND. (ILAY .GT. 3)
     $                         .AND. (XKZ .LT. 5.0E+0) .AND.
     $                         (RATK .GT. 1.0E-1)) THEN
                                  MXPRED=8
                                  LUSE(8)=.TRUE.
                                  MXUSE=MXUSE + 1
C
                                  IF (MPTS .GT. MINPTS(MXUSE+1)) THEN
                                     MXPRED=9
                                     LUSE(9)=.TRUE.
                                     MXUSE=MXUSE + 1
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
             DO IPRED=1,NPREDM
                FTCOEF(IPRED+IOFFST(IGASM),ILAY)=0.0E+0
             ENDDO
          ENDIF 
C
c      write(6,*) 'm layer=',ilay,', mpts=',mpts,', mxpred=',mxpred,
c     $   ', mxuse=',mxuse
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
             CALL WGHT2A(ILAY, IGASM, NGASZ, IGASZ, IGASF, MPTS,
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
c      write(6,*) 'm npred=',npred,', rms=',ALLRMS(NPRED)
C
C            Check the "bad fit" flag
             IF (IBAD .EQ. 0) THEN
                DO IPRED=1,NPRED
                   ALLCOF(IPRED,NPRED)=SNGL( XVEC(IPRED) )
                ENDDO
             ELSE
c      write(6,*) 'm npred=',npred,', ibad=',ibad
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
             DO IPRED=1,NPREDM
                IF ((I .LT. IBEST) .AND. (LUSE(IPRED))) THEN
                   I=I + 1
                   FTCOEF(IPRED+IOFFST(IGASM),ILAY)=ALLCOF(I,IBEST)
                ELSE
                   FTCOEF(IPRED+IOFFST(IGASM),ILAY)=0.0E+0
                ENDIF
             ENDDO
C
c      write(6,*) 'm ibest=',ibest
          ENDIF
C
       ENDDO
C      End loop over layers
C
       RETURN
       END
