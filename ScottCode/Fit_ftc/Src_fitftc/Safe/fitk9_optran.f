C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             FITK9_OPTRAN
C
!F77====================================================================


!ROUTINE NAME:
C    FITF9


!ABSTRACT:
C    Do the regression to determine the fast transmittance coefs.
C    This version for 9 terms.


!CALL PROTOCOL:
C    FITK( ODZOP, KOP, POP, TOP, PZOP, TZOP, ANGLE, AZOP,
C     $    AVGP, AVGT, AVGPZ, AVGTZ, MINCOF, MINANG, COEF )


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    REAL ARR   ODZOP          Optical Depth      none
C    REAL ARR   KOP            Absorption Coeff   cm^2/kmoles
C    REAL ARR   POP            Pressure           atm
C    REAL ARR   TOP            Temperature        K
C    REAL ARR   PZOP           Pressue above      atm
C    REAL ARR   TZOP           Temperature above  K
C    REAL ARR   AVGP           Average pressure   atm
C    REAL ARR   AVGT           Average temp       K
C    REAl ARR   AVGPZ          Average pres above atm
C    REAl ARR   AVGTZ          Average temp above K
C    INTEGER    MINCOF         Min no. coef/preds none
C    INT ARR    MINANG         Min no. of angles  none


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    REAL ARR   COEF           Fast trans coefs   various


!INPUT/OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    FASTTRAN


!ROUTINES CALLED:
C    FITGAS: Solves for the fast trans coefs


!FILES ACCESSED:
C    none


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine FITK solves for the fast transmittance (actually
C    absorption coefficient) coefficients. The routine determines
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
C    16 Jan 98 Scott Hannon      Created based on AIRS version


!END ===================================================================


C      =================================================================
       SUBROUTINE FITK9( ODZOP, KOP, POP, TOP, PZOP, TZOP, ANGLE, AZOP,
     $    AVGP, AVGT, AVGPZ, AVGTZ, MINCOF, MINANG, COEF )
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
C      Input
       REAL  ODZOP(NOPLEV,  MAXM)
       REAL    KOP(NOPLEV,  MAXM)
       REAL    POP(NOPLEV,  MAXM)
       REAL    TOP(NOPLEV,  MAXM)
       REAL   PZOP(NOPLEV,  MAXM)
       REAL   TZOP(NOPLEV,  MAXM)
       REAL  ANGLE(  MAXM)
       REAL   AZOP(NOPLEV)
       REAL   AVGP(NOPLEV)
       REAL   AVGT(NOPLEV)
       REAL  AVGPZ(NOPLEV)
       REAL  AVGTZ(NOPLEV)
       INTEGER MINCOF
       INTEGER MINANG(  MAXN)
C
C      Output parameters
       REAL   COEF(     9,NOPLEV)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       LOGICAL   LUSE(  MAXN)
       LOGICAL NEWANG
       INTEGER      I
       INTEGER  IBEST
       INTEGER     IJ
       INTEGER  IPRED
       INTEGER      J
       INTEGER    LOP
       INTEGER   MCHK
       INTEGER MINPTS(  MAXN)
       INTEGER   MPTS
       INTEGER MXPRED
       INTEGER  MXUSE
       INTEGER  NPRED
       INTEGER   IANG
       INTEGER ICOUNT(MAXANG)
       INTEGER   NANG
       INTEGER NANGDT
       INTEGER LOWANG
       REAL ALLCOF(  MAXN,  MAXN)
       REAL ALLRMS(  MAXN)
       REAL    AOP
       REAL    DIF
       REAL  ODMIN
       REAL SIGDIF
       REAL    XOD
       REAL XODMAX
       REAL  XZMIN
       REAL ANGSEC(MAXANG)
C
C      For FITGAS
       INTEGER IBAD
       DOUBLE PRECISION   AMAT(  MAXM,  MAXN)
       DOUBLE PRECISION   BVEC(  MAXM)
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
C      Assign the minimum count of data points needed to say there
C      is sufficient data at a particular angle to be significant.
       LOWANG=MAXPRO/4
C
C      Define the mininum number of points needed to do a regression
C      with a certain number of predictors.
c       J=MAXANG + 3
       J=MAXANG + 7
       DO I=1,MAXN
          MINPTS(I)=J*I
          LUSE(I)=.FALSE.
       ENDDO
C
       ODMIN=8.0E-7
C
C      Assign the value to use in testing for
C      a significant difference in fit results.
       SIGDIF=5.0E-3
C
C      ------------------------------------------
C      Loop over the layers and do the regression
C      ------------------------------------------
       DO LOP=1,NOPLEV
C
C         Determine OPTRAN layer amount
          IF (LOP .EQ. 1) THEN
             AOP=AZOP(1)
          ELSE
             AOP=AZOP(LOP) - AZOP(LOP-1)
          ENDIF
C
C         Initialize the min and max optical depths
          XODMAX=0.0E+0
          XZMIN=1.0E+30
C         Initialize the predictor on/off switches
          DO I=1,MAXN
             LUSE(I)=.FALSE.
          ENDDO
C         Initialize the data point counter
          MPTS=0
C         Initialize the angle counter
          NANG=0
C
C         Loop over the prof*angle
          DO IJ=1,MAXM
C            Convert abs coef to optical depth
             XOD=AOP*KOP(LOP,IJ)
             IF (XOD .GT. ODMIN) THEN
C               It's a good data point
                MPTS=MPTS + 1
                IF (XOD .GT. XODMAX) XODMAX=XOD
                IF (ODZOP(LOP,IJ) .LT. XZMIN) XZMIN=ODZOP(LOP,IJ)
C
C               Check the angle
                NEWANG=.TRUE.
                DO IANG=1,NANG
                   IF (ANGLE(IJ) .GT. ANGSEC(IANG)-0.01 .AND.
     $             ANGLE(IJ) .LT. ANGSEC(IANG)+0.01) THEN
                      NEWANG=.FALSE.
                      ICOUNT(IANG)=ICOUNT(IANG) + 1
                   ENDIF
                ENDDO
                IF (NEWANG) THEN
                   NANG=NANG + 1
                   ANGSEC(NANG)=ANGLE(IJ)
                   ICOUNT(NANG)=1
                ENDIF
C
             ENDIF
          ENDDO
C
C         Finish checking the angles
          NANGDT=0
          DO IANG=1,NANG
             IF (ICOUNT(IANG) .GT. LOWANG) NANGDT=NANGDT + 1
          ENDDO
C
C         ---------------------------------
C         Determine which predictors to use
C         ---------------------------------
          MXPRED=0
          MXUSE=0
C
          IF (MPTS .GT. MINPTS(2)) THEN
             MXPRED=1
             LUSE(1)=.TRUE.
             MXUSE=1
C
             IF (NANGDT .GE. MINANG(2)) THEN
                MXPRED=2
                LUSE(2)=.TRUE.
                MXUSE=2
             ENDIF
C
             IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND. (XZMIN .LT. 1.0E+1)
     $       .AND. (XODMAX .GT. 1.0E-6)) THEN
                MXPRED=3
                LUSE(3)=.TRUE.
                MXUSE=MXUSE + 1
C
                IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $          (XODMAX .GT. 5.0E-6)) THEN
C
                   IF (NANGDT .GE. MINANG(4)) THEN
                      MXPRED=4
                      LUSE(4)=.TRUE.
                      MXUSE=MXUSE + 1
                   ENDIF
C
                   IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $             (XODMAX .GT. 1.0E-5)) THEN
                      MXPRED=5
                      LUSE(5)=.TRUE.
                      MXUSE=MXUSE + 1
C
                      IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $                (XODMAX .GT. 5.0E-5)) THEN
C
                         IF (NANGDT .GE. MINANG(6)) THEN
                            MXPRED=6
                            LUSE(6)=.TRUE.
                            MXUSE=MXUSE + 1
                         ENDIF
C
                         IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $                   (XODMAX .GT. 1.0E-4)) THEN
C
                            IF (NANGDT .GE. MINANG(7)) THEN
                               MXPRED=7
                               LUSE(7)=.TRUE.
                               MXUSE=MXUSE + 1
                            ENDIF
C
                            IF ((MPTS .GT. MINPTS(MXUSE+1)) .AND.
     $                      (LOP .GT. 2)) THEN
C
                               IF (NANGDT .GE. MINANG(8)) THEN
                                  MXPRED=8
                                  LUSE(8)=.TRUE.
                                  MXUSE=MXUSE + 1
                               ENDIF
C
                               IF (MPTS .GT. MINPTS(MXUSE+1)) THEN
                                  MXPRED=9
                                  LUSE(9)=.TRUE.
                                  MXUSE=MXUSE + 1
                               ENDIF
C
                            ENDIF
                         ENDIF
                      ENDIF
                   ENDIF
                ENDIF
             ENDIF
          ENDIF 
C
cccccc
c      if (lop .eq. 1) then
c       write(6,1234) lop, mpts, nangdt, mxpred, mxuse
c 1234  format(' lop=',I4,', mpts=',I4,', nangdt=',I2,', mxpred=',
c     $   I2,', mxuse=',I2)
c      endif
cccccc
c      write(6,*) 'luse=',(luse(ipred),ipred=1,mxpred)
C         -----------------
C         Do the regression (if necessary)
C         -----------------
          IF (MXPRED .GE. MINCOF) THEN
C
C            ------------------
C            Load BVEC and AMAT
C            ------------------
             MCHK=0
             DO IJ=1,MAXM
                XOD=AOP*KOP(LOP,IJ)
                IF (XOD .GT. ODMIN) THEN
C                  It's a good data point
                   MCHK=MCHK + 1
C
C                  Due to the sizes of the numbers, it is probably
C                  safer to fit optical depth than absorption coeff.
C                   BVEC(MCHK)=DBLE( KOP(LOP,IJ) )
                   BVEC(MCHK)=DBLE( XOD )
C
                   IPRED=0
                   IF (LUSE(1)) THEN
                      IPRED=IPRED + 1
                      AMAT(MCHK,IPRED)=
     $                   1.0D+0
                   ENDIF
C
                   IF (LUSE(2)) THEN
                      IPRED=IPRED + 1
                      AMAT(MCHK,IPRED)=
     $                   DBLE( POP(LOP,IJ)/AVGP(LOP) )
                   ENDIF
C
                   IF (LUSE(3)) THEN
                      IPRED=IPRED + 1
                      AMAT(MCHK,IPRED)=
     $                   DBLE( TOP(LOP,IJ)/AVGT(LOP) )
                   ENDIF
C
                   IF (LUSE(4)) THEN
                      IPRED=IPRED + 1
                      AMAT(MCHK,IPRED)=
     $                   DBLE( SQRT( POP(LOP,IJ)/AVGP(LOP) ) )
                   ENDIF
C
                   IF (LUSE(5)) THEN
                      IPRED=IPRED + 1
                      AMAT(MCHK,IPRED)=
     $                   DBLE( (TOP(LOP,IJ)/AVGT(LOP))**2 )
                   ENDIF
C
                   IF (LUSE(6)) THEN
                      IPRED=IPRED + 1
                      AMAT(MCHK,IPRED)=
     $                   DBLE( TOP(LOP,IJ)*POP(LOP,IJ)/
     $                   ( AVGT(LOP)*AVGP(LOP) ) )
                   ENDIF
C
                   IF (LUSE(7)) THEN
                      IPRED=IPRED + 1
                      AMAT(MCHK,IPRED)=
     $                   DBLE( ANGLE(IJ) )
                   ENDIF
C
                   IF (LUSE(8)) THEN
                      IPRED=IPRED + 1
                      AMAT(MCHK,IPRED)=
     $                   DBLE( PZOP(LOP,IJ)/AVGPZ(LOP) )
                   ENDIF
C
                   IF (LUSE(9)) THEN
                      IPRED=IPRED + 1
                      AMAT(MCHK,IPRED)=
     $                   DBLE( TZOP(LOP,IJ)/AVGTZ(LOP) )
                   ENDIF
C
                ENDIF
             ENDDO
C
ccccccc
c      if (lop .eq. 1) then
c      do mchk=1,mpts
c       write(6,9876) mchk, SNGL(bvec(mchk)),
c     $   (SNGL(amat(mchk,ipred)),ipred=1,mxuse)
c 9876  format('mchk=',I3,', OD=',1PE10.3,', pred=',9(E10.3,X))
c      enddo
c      endif
ccccccc
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
c      write(6,*) 'npred=',npred,', rms=',ALLRMS(NPRED)
C
C            Check the "bad fit" flag
             IF (IBAD .EQ. 0) THEN
                DO IPRED=1,NPRED
                   ALLCOF(IPRED,NPRED)=SNGL( XVEC(IPRED) )
                ENDDO
             ELSE
c      write(6,*) 'npred=',npred,', ibad=',ibad
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
C               The first fit is the only fit allowed
                IBEST=MINCOF
             ELSE
                IF (NPRED .EQ. MXUSE) THEN
C                  Initialize best as first fit, and do another fit
                   IBEST=MXUSE
                   GOTO 10
                ELSE
C                  Compare the current fit to the best so far
                   DIF=(ALLRMS(NPRED) - ALLRMS(IBEST))/ALLRMS(IBEST)
                   IF (DIF .LT. SIGDIF) THEN
C
C                     Re-set IBEST if the last fit is better than
C                     previous best (this shouldn't happen, but...)
                      IF (DIF .LT. 0.0E+0) IBEST=NPRED
C
C                     Set IBEST if this is the last fit allowed
                      IF (NPRED .EQ. MINCOF) IBEST=MINCOF
C
C                     Do another fit if allowed
                      IF (NPRED .GT. MINCOF) GOTO 10
                   ELSE
C                     Current fit is not good; best was previous fit
                      IBEST=NPRED + 1
                   ENDIF
                ENDIF
             ENDIF
C
C            Load the fit results into COEF
             I=0
             DO IPRED=1,9
                IF ((I .LT. IBEST) .AND. (LUSE(IPRED))) THEN
                   I=I + 1
                   COEF(IPRED,LOP)=ALLCOF(I,IBEST)
                ELSE
                   COEF(IPRED,LOP)=0.0E+0
                ENDIF
             ENDDO
C
c      write(6,*) 'ibest=',ibest
cccccc
c      write(6,4321) lop, ibest, (coef(ipred,lop),ipred=1,ibest)
c 4321 format(X,'lop=',I4,', ibest=',I2,', coef=',9(1PE11.3,X))
cccccc
C
          ELSE
C            Not enough points to do the regression
             DO IPRED=1,9
                COEF(IPRED,LOP)=0.0E+0
             ENDDO
          ENDIF
C
       ENDDO
C      End loop over layers
C
       RETURN
       END
