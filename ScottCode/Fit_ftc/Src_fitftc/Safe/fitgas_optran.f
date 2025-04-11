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
       INTEGER     IPT
       INTEGER   IPRED
       REAL      R
       REAL   RFAC
       REAL   RNUM
       REAL SIGDIF
       REAL   RDIF
       REAL  RMSWN
       DOUBLE PRECISION     DM
       DOUBLE PRECISION   DDIF
       DOUBLE PRECISION   DSUM
       DOUBLE PRECISION DDIFWN
       DOUBLE PRECISION DSUMWN
       DOUBLE PRECISION XVECWN(  MAXN)
       DOUBLE PRECISION TOOBIG
       DOUBLE PRECISION DJUNK1
       DOUBLE PRECISION DJUNK2


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
       SIGDIF=4.0E-2
C      Note: A sigdif of 4E-2 corresponds to an average error of 20%
C
       IBAD=0
C
       IF (NPRED .GE. 1 .AND. NPRED .LE. MAXN) THEN
C
          DM=DBLE( MPTS )
C
ccc
c          IF(NPRED .EQ. 11) THEN
c             CALL XFIT11(MPTS, AMAT, BVEC, XVEC)
c          ELSEIF(NPRED .EQ. 10) THEN
c             CALL XFIT10(MPTS, AMAT, BVEC, XVEC)
c          ELSEIF(NPRED .EQ. 9) THEN
ccc
          IF(NPRED .EQ. 9) THEN
ccc
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
C         Note: the "RMS" and "RMSWN" variables are not
C         root-mean-square errors; they are now the average
C         of the fractional error squared.
C
C         R and RFAC are for the random numbers:
C            R=0 instructs RAND to return the next random number
C            RFAC is the randomization fraction
          R=0.0E+0
          RFAC=4.0E-4
C
C         Add a little random noise to the coefs
          DO IPRED=1,NPRED
             RNUM=1.0E+0 + ( RAND(R) - 5.0E-1 )*RFAC
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
             RMS=SNGL( DSUM/DM )
             RMSWN=SNGL( DSUMWN/DM )
c      write(6,*) 'fitgas: npred=',npred,', rms=',rms,', rmswn=',rmswn
C
C            Compare results with and without noise
             RDIF=RMSWN - RMS
             IF (RDIF .GT. SIGDIF) THEN
C               The change in fitting error with small random noise
C               added to the fast transmitance coefficients is rather
C               big. This suggests the fit is probably unstable.
                IBAD=-1
                RMS=1.0E+16
cccccc
c       write(6,1234) (SNGL(xvec(ipred)),ipred=1,npred)
c 1234  format('fitgas: ibad=-1, xvec=',9(X,1PE11.3))
cccccc
             ELSEIF (RDIF .LT. -1.0E-2) THEN
                WRITE(6,1010) NPRED, -1.0E+2*RDIF
 1010           FORMAT('WARNING! Fitgas with ',I2,' predictors.',/,
     $          'Fit with noise was',F8.2,
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
