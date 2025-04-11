C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             FITGAS
C
!F77====================================================================


!ROUTINE NAME:
C    FITGAS


!ABSTRACT:
C    Do the regression for a gas to determine the fast trans coefs.


!CALL PROTOCOL:
C    FITGAS(MPTS, NPRED, AMAT, BVEC, XVEC, RMS, IBAD)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INTEGER    MPTS           No. of data points none
C    INTEGER    NPRED          No. of predictors  none
C    DOUBLE ARR AMAT           Matrix A of preds  none
C    DOUBLE ARR BVEC           Vector b of op dp  none


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    DOUBLE ARR XVEC           Vector x of coefs  none
C    REAL       RMS            RMS of fit         none
C    INTEGER    IBAD           Bad fit flag       none


!INPUT/OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    FITF
C    FITW
C    FITO
C    FITC
C    FITM


!ROUTINES CALLED:
C    XFIT#: Solves the equation A * x = b


!FILES ACCESSED:
C    OUTPUT:
C       unit IOERR : error messages (if any)


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine FITGAS does a regression on the effective optical depths
C    and the predictors to determine the fast transmittance (really
C    fast optical depth) coefficiants. It then computes the RMS of
C    Calc - Fit, with and without systematic noise added to the coefs.
C    If the difference between results with and without the noise
C    is significant, it sets IBAD to -1 to indicate the fit was
C    bad (unstable). Otherwise IBAD=0.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    17 Dec 96 Scott Hannon      Created
C    21 May 02 Scott Hannon      Error messages now use IOERR (was 6)


!END ===================================================================


C      =================================================================
       SUBROUTINE FITGAS(MPTS, NPRED, AMAT, BVEC, XVEC, RMS, IBAD)
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
C      none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input parameters
       INTEGER   MPTS
       INTEGER  NPRED
       DOUBLE PRECISION   AMAT(  MAXM,  MAXN)
       DOUBLE PRECISION   BVEC(  MAXM)
C
C      Output parameters
       DOUBLE PRECISION   XVEC(  MAXN)
       REAL    RMS
       INTEGER   IBAD


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER  IPRED
       INTEGER    IPT
C
       REAL   RDIF
c       REAL  RJUNK
       REAL  RMSP1
       REAL  RMSP2
       REAL  RMSWN
       REAL SIGDIF
C
       DOUBLE PRECISION   DDIF
       DOUBLE PRECISION DDIFP1
       DOUBLE PRECISION DDIFP2
       DOUBLE PRECISION  DJUNK
       DOUBLE PRECISION DJUNK1
       DOUBLE PRECISION DJUNK2
       DOUBLE PRECISION     DM
       DOUBLE PRECISION   DSUM
       DOUBLE PRECISION DSUMP1
       DOUBLE PRECISION DSUMP2
       DOUBLE PRECISION  PERT1
       DOUBLE PRECISION  PERT2
       DOUBLE PRECISION TOOBIG
       DOUBLE PRECISION XVECP1(  MAXN)
       DOUBLE PRECISION XVECP2(  MAXN)


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
C
C      Maximum allowable fast trans coef
       TOOBIG=1.0D+6
C
C      Min ratio change in RMS fit considered to be significant
       SIGDIF=1.0E-2
c       SIGDIF=4.0E-2
c       SIGDIF=1.0E-1
C
C      Perturbation scale factors to use to test the sensitivity of
C      the fast trans coefs to systematic noise.
       PERT1=1.0001D+0
       PERT2=0.9999D+0
C
       IBAD=0
C
       IF (NPRED .GE. 1 .AND. NPRED .LE. MAXN) THEN
C
          DM=DBLE( MPTS )
C
          IF(NPRED .EQ. 13) THEN
             CALL XFIT13(MPTS, AMAT, BVEC, XVEC)
          ELSEIF(NPRED .EQ. 12) THEN
             CALL XFIT12(MPTS, AMAT, BVEC, XVEC)
          ELSEIF(NPRED .EQ. 11) THEN
             CALL XFIT11(MPTS, AMAT, BVEC, XVEC)
          ELSEIF(NPRED .EQ. 10) THEN
             CALL XFIT10(MPTS, AMAT, BVEC, XVEC)
          ELSEIF(NPRED .EQ. 9) THEN
             CALL XFIT9(MPTS, AMAT, BVEC, XVEC)
          ELSEIF(NPRED .EQ. 8) THEN
             CALL XFIT8(MPTS, AMAT, BVEC, XVEC)
          ELSEIF(NPRED .EQ. 7) THEN
             CALL XFIT7(MPTS, AMAT, BVEC, XVEC)
          ELSEIF(NPRED .EQ. 6) THEN
             CALL XFIT6(MPTS, AMAT, BVEC, XVEC)
          ELSEIF(NPRED .EQ. 5) THEN
             CALL XFIT5(MPTS, AMAT, BVEC, XVEC)
          ELSEIF(NPRED .EQ. 4) THEN
             CALL XFIT4(MPTS, AMAT, BVEC, XVEC)
          ELSEIF(NPRED .EQ. 3) THEN
             CALL XFIT3(MPTS, AMAT, BVEC, XVEC)
          ELSEIF(NPRED .EQ. 2) THEN
             CALL XFIT2(MPTS, AMAT, BVEC, XVEC)
          ELSEIF(NPRED .EQ. 1) THEN
             DSUM=0.0D+0
             DO IPT=1,MPTS
                DSUM=DSUM + BVEC(IPT)/AMAT(IPT,1)
             ENDDO
             XVEC(1)=DSUM/DM
          ENDIF
C
C         ------------------
C         Calc the RMS error
C         ------------------
C
C         Add a little systematic noise to the coefs
          DO IPRED=1,NPRED

             XVECP1(IPRED)=PERT1*XVEC(IPRED)
             XVECP2(IPRED)=PERT2*XVEC(IPRED)
C
C            Check to see all the coefs are reasonable
             IF (ABS(XVEC(IPRED)) .GT. TOOBIG) THEN
                IBAD=-2
                RMS=1.0E+16
             ENDIF
          ENDDO
C
          IF (IBAD .EQ. 0) THEN
C
C            Calculate the RMS of Kcalc-Ktrue/Ktrue
             DSUM=0.0D+0
             DSUMP1=0.0D+0
             DSUMP2=0.0D+0
             DO IPT=1,MPTS
                DDIF=-BVEC(IPT)
                DDIFP1=-BVEC(IPT)
                DDIFP2=-BVEC(IPT)
                DO IPRED=1,NPRED
                   DDIF=DDIF + AMAT(IPT,IPRED)*XVEC(IPRED)
                   DDIFP1=DDIFP1 + AMAT(IPT,IPRED)*XVECP1(IPRED)
                   DDIFP2=DDIFP2 + AMAT(IPT,IPRED)*XVECP2(IPRED)
                ENDDO
                DJUNK=DDIF/BVEC(IPT)
                DJUNK1=DDIFP1/BVEC(IPT)
                DJUNK2=DDIFP2/BVEC(IPT)
                DSUM=DSUM + DJUNK*DJUNK
                DSUMP1=DSUMP1 + DJUNK1*DJUNK1
                DSUMP2=DSUMP2 + DJUNK2*DJUNK2
             ENDDO
             RMS=SQRT( SNGL( DSUM/DM ) )
             RMSP1=SQRT( SNGL( DSUMP1/DM ) )
             RMSP2=SQRT( SNGL( DSUMP2/DM ) )
             RMSWN=5.0E-1*(RMSP1 + RMSP2)
c      write(6,*) 'fitgas: npred=',npred,', rms=',rms,', rmswn=',rmswn
C
C            Compare results with and without noise
C Note: RMS is ratio of error to data (mult by 100 for percent).  Thus
C a fit with RMS < 0.01 is better than 1%, and RMS < 1.0E-3 is better
C than 0.1%, which is better than we ever need.
c             RDIF=(RMSWN - RMS)/RMS
             RDIF=(RMSWN - RMS)/(RMS + 1.0E-3)
             IF (RMS .LT. 1.0E-3) RDIF=RDIF*RMS*1.0E+3
C
             IF ((RDIF .GT. SIGDIF) .AND. (NPRED .GT. 1)) THEN
C               The change in fitting error with small systematic noise
C               added to the fast transmitance coefficients is rather
C               big. This suggests the fit is probably unstable.
                IBAD=-1
                WRITE(6,1010) NPRED, 1.0E+2*RDIF, RMS, RMSWN
 1010           FORMAT('IBAD warning: ',I2,' predictors. ',
     $             F7.1,'%, rms=',1PE8.2,', rmswn=',1PE8.2)
                RMS=1.0E+16
             ELSEIF (RDIF .LT. -3.0E-2) THEN
C               Write a warning if the fit with noise is more than a
C               few percent better than the fit without noise.
                WRITE(6,1020) NPRED, -1.0E+2*RDIF
 1020           FORMAT('WARNING! Fitgas with ',I2,' predictors.',/,
     $          'Fit with noise was',F7.1,
     $          '% better than fit with no noise!')
             ENDIF
C
          ENDIF
C
       ELSE
          WRITE(IOERR,1030) NPRED
 1030     FORMAT('Error in FITGAS',/,
     $    'Illegal number of predictors for fit:',I3)
          STOP
       ENDIF
C
       RETURN
       END
