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
C    RAND: some pseudo-random number generator function


!FILES ACCESSED:
C    OUTPUT:
C       unit IOERR : error messages (if any)


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine FITGAS does a regression on the effective optical depths
C    and the predictors to determine the fast transmittance (really
C    fast optical depth) coefficiants. It then computes the RMS of
C    Calc - Fit, with and without random noise added to the coefs.
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
       REAL RAND


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input parameters
       INTEGER MPTS, NPRED
       DOUBLE PRECISION AMAT(MAXM,MAXN), BVEC(MAXM)
C
C      Output parameters
       DOUBLE PRECISION XVEC(MAXN)
       REAL RMS
       INTEGER IBAD


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER IPT, IPRED
       REAL R, RFAC, RNUM, SIGDIF, RDIF, RMSWN, RJUNK
       DOUBLE PRECISION DM, DDIF, DSUM, DDIFWN, DSUMWN, XVECWN(MAXN),
     $    TOOBIG, DJUNK1, DJUNK2


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
       TOOBIG=1.0D+6
c       SIGDIF=4.0E-2
       SIGDIF=1.0E-1
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
C         R and RFAC are for the random numbers:
C            R=0 instructs RAND to return the next random number
C            RFAC is the randomization fraction
          R=0.0E+0
          RFAC=4.0E-4
C
C         Add a little random noise to the coefs
          DO IPRED=1,NPRED
             RJUNK=RAND(R) - 5.0E-1
             IF (ABS(RJUNK) .LT. 0.1) RJUNK=SIGN(0.1,RJUNK)
             RNUM=1.0E+0 + RJUNK*RFAC
             XVECWN(IPRED)=DBLE( RNUM )*XVEC(IPRED)
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
             DSUMWN=0.0D+0
             DO IPT=1,MPTS
                DDIF=-BVEC(IPT)
                DDIFWN=-BVEC(IPT)
                DO IPRED=1,NPRED
                   DDIF=DDIF + AMAT(IPT,IPRED)*XVEC(IPRED)
                   DDIFWN=DDIFWN + AMAT(IPT,IPRED)*XVECWN(IPRED)
                ENDDO
                DJUNK1=DDIF/BVEC(IPT)
                DJUNK2=DDIFWN/BVEC(IPT)
                DSUM=DSUM + DJUNK1*DJUNK1
                DSUMWN=DSUMWN + DJUNK2*DJUNK2
             ENDDO
             RMS=SQRT( SNGL( DSUM/DM ) )
             RMSWN=SQRT( SNGL( DSUMWN/DM ) )
c      write(6,*) 'fitgas: npred=',npred,', rms=',rms,', rmswn=',rmswn
C
C            Compare results with and without noise
             RDIF=(RMSWN - RMS)/RMS
             IF ((RDIF .GT. SIGDIF) .AND. (NPRED .GT. 1)) THEN
C               The change in fitting error with small random noise
C               added to the fast transmitance coefficients is rather
C               big. This suggests the fit is probably unstable.
                IBAD=-1
                RMS=1.0E+16
                WRITE(6,1010) NPRED,1.0E+2*rdif
 1010           FORMAT('IBAD warning: Fitgas with ',I2,' predictors. ',
     $             F7.1,'%')
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
