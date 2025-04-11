C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             WGHT2x
C
!F77====================================================================


!ROUTINE NAME:
C    WGHTA


!ABSTRACT:
C    Weight (type "2x") the data to be used in the regression.


!CALL PROTOCOL:
C    WGHT2A(ILAY, IGAS, NGASZ, IGASZ, IFIXZ, MPTS, NPRED, IOFFST,
C       PRED, LUSE, KLTOS, KLAYER, SECANG, AMAT, BVEC)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INTEGER    ILAY           Layer number       none
C    INTEGER    IGAS           Gas number         none
C    INTEGER    NGASZ          No. gases for Kz   none
C    INT ARR    IGASZ          Gases no.s for Kz  none
C    INTEGER    IFIXZ          Fixed gas number   none
C    INTEGER    MPTS           M, no. of points   none
C    INTEGER    NPRED          N, no. of predicts none
C    INT ARR    IOFFST         PRED index offsets none
C    REAL ARR   PRED           Predictors         various
C    LOG ARR    LUSE           Use data?          none
C    REAL ARR   KLTOS          Eff l-to-s op dpth none
C    REAL ARR   KLAYER         Eff layer op depth none
C    REAL ARR   SECANG         Angle secant       none


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    DOUBLE ARR AMAT           A matrix (MxN)     various
C    DOUBLE ARR BVEC           B vector (Mx1)     none


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
C    none


!FILES ACCESSED:
C    OUTPUT:
C       IOERR: error messages (if any)


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine WGHTA weights the matrix A of profile predictors and
C    vector B of optical depths. The KLAYER optical depths and PRED
C    predictors are weighted according to the estimated importance
C    of the optical depth to radiative transfer.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C Date        Programmer        Comments
C ----------- --------------- ------------------------------------------
C  9 Jun 1997 Scott Hannon    Created from "wght1.f"
C 21 May 2002 Scott Hannon    Error messages now use IOERR (was 6)
C 29 Apr 2008 Scott Hannon    "2x" varaint createrd from "2a" with
C                             revised angle weighting

!END ===================================================================


C      =================================================================
       SUBROUTINE WGHT2A(ILAY, IGAS, NGASZ, IGASZ, IFIXZ, MPTS,
     $    NPRED, IOFFST, PRED, LUSE, KLTOS, KLAYER, SECANG, AMAT, BVEC)
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
       INTEGER   ILAY         ! layer index
       INTEGER   IGAS         ! gas index
       INTEGER  NGASZ
       INTEGER  IGASZ(NUMGAS)
       INTEGER  IFIXZ
       INTEGER   MPTS         ! expected number of data points
       INTEGER  NPRED         ! number of predictors
       INTEGER IOFFST(NUMGAS)
       REAL   PRED(MAXPRD,MAXPRO,MAXANG,MAXLAY)
       LOGICAL   LUSE(  MAXN)
       REAL  KLTOS(MAXPRO,MAXANG,MAXLAY,NUMGAS)
       REAL KLAYER(MAXPRO,MAXANG,MAXLAY,NUMGAS)
       REAL SECANG(MAXANG)
C
C      Output parameters
       DOUBLE PRECISION   AMAT(  MAXM,  MAXN)
       DOUBLE PRECISION   BVEC(  MAXM)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      I
       INTEGER   IANG
       INTEGER  IPRED
       INTEGER   IPRO
       INTEGER   MCHK  ! count of data points found
       REAL      A(8)
       REAL      B(8)
       REAL      X(9)
       REAL      Y(9)
       REAL     XK
       REAL    XKZ
       REAL   XKFZ
       REAL     WK
       REAL    WKZ
       REAL   WKFZ
       REAL KTOFIT
       REAL WEIGHT
       REAL     WA
       REAL     AA
       REAL     BA
       REAL     XA(2)
       REAL     YA(2)
       REAL  XJUNK


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
C      ------------------------------------
C      Assign weighting function for angles
C      ------------------------------------
ccc "2a"
c       XA(1)=2.0
c       XA(2)=9.0
c       YA(1)=1.0
c       YA(2)=0.5
ccc "2x" try1
c       XA(1)=1.8
c       XA(2)=2.5
c       YA(1)=1.0
c       YA(2)=0.3
ccc "2x" try2
       XA(1)=2.0
       XA(2)=2.5
       YA(1)=0.5
       YA(2)=0.25
ccc
       AA=( YA(2)-YA(1) )/( XA(2)-XA(1) )
       BA=YA(1) - AA*XA(1)
C
C      ------------------------------------------
C      Assign fit weighting function for k and kz
C      ------------------------------------------
       X(1)=5.53E-2
       X(2)=1.90E-1
       X(3)=3.85E-1
       X(4)=5.40E-1
       X(5)=1.00E+0
       X(6)=1.65E+0
       X(7)=2.90E+0
       X(8)=3.49E+0
       X(9)=4.41E+0
C
       Y(1)=1.0
       Y(2)=3.0
       Y(3)=5.0
       Y(4)=6.0
       Y(5)=7.0
       Y(6)=6.0
       Y(7)=3.0
       Y(8)=2.0
       Y(9)=1.0
C
C      Calc the linear functions for the six regions defined above
       DO I=1,8
          A(I)=( Y(I+1)-Y(I) )/( X(I+1)-X(I) )
          B(I)=Y(I)-A(I)*X(I)
       ENDDO
C
C      Initialize the data point counter
       MCHK=0
C
C      --------------------
C      Loop over the angles
C      --------------------
       DO IANG=1,MAXANG
C
C         ---------------------------
C         Calc weight for each angles
C         ---------------------------
          WA=1.0
ccc "2x" try2
          WA=1.0/SECANG(IANG)
ccc
          IF (SECANG(IANG) .GT. XA(1)) THEN
             IF (SECANG(IANG) .GT. XA(2)) THEN
                WA=YA(2)
             ELSE
                WA=AA*SECANG(IANG) + BA
             ENDIF
          ENDIF
C
C         ----------------------
C         Loop over the profiles
C         ----------------------
          DO IPRO=1,MAXPRO
C
             IF (KLAYER(IPRO,IANG,ILAY,IGAS) .GT. 0.0E+0) THEN
                MCHK=MCHK + 1
                XK=KLAYER(IPRO,IANG,ILAY,IGAS)
                XKFZ=KLTOS(IPRO,IANG,ILAY,IFIXZ)
                XKZ=0.0E+0
                DO I=1,NGASZ
                   XJUNK=KLTOS(IPRO,IANG,ILAY,IGASZ(I))
                   IF (XJUNK .GT. 0.0E+0) XKZ=XKZ + XJUNK
                ENDDO
C
C               -----------------------
C               Weight for layer K (XK)
C               -----------------------
                IF (XK .LT. X(1)) THEN 
                   WK=Y(1)*SQRT( XK/X(1) )
                ELSEIF (XK .GE. X(1) .AND. XK .LT. X(2)) THEN
                   WK=XK*A(1) + B(1)
                ELSEIF (XK .GE. X(2) .AND. XK .LT. X(3)) THEN
                   WK=XK*A(2) + B(2)
                ELSEIF (XK .GE. X(3) .AND. XK .LT. X(4)) THEN
                   WK=XK*A(3) + B(3)
                ELSEIF (XK .GE. X(4) .AND. XK .LT. X(5)) THEN
                   WK=XK*A(4) + B(4)
                ELSEIF (XK .GE. X(5) .AND. XK .LT. X(6)) THEN
                   WK=XK*A(5) + B(5)
                ELSEIF (XK .GE. X(6) .AND. XK .LE. X(7)) THEN
                   WK=XK*A(6) + B(6)
                ELSEIF (XK .GE. X(7) .AND. XK .LE. X(8)) THEN
                   WK=XK*A(7) + B(7)
                ELSEIF (XK .GE. X(8) .AND. XK .LE. X(9)) THEN
                   WK=XK*A(8) + B(8)
                ELSE
C                  XK > X(9)
                   WK=Y(9)
                ENDIF
C
C               ---------------------------------
C               Weight for layer-to-space K (XKZ)
C               This may be a sum of gases
C               ---------------------------------
                IF (XKZ .LT. X(1)) THEN 
                   WKZ=Y(1)
                ELSEIF (XKZ .GE. X(1) .AND. XKZ .LT. X(2)) THEN
                   WKZ=XKZ*A(1) + B(1)
                ELSEIF (XKZ .GE. X(2) .AND. XKZ .LT. X(3)) THEN
                   WKZ=XKZ*A(2) + B(2)
                ELSEIF (XKZ .GE. X(3) .AND. XKZ .LT. X(4)) THEN
                   WKZ=XKZ*A(3) + B(3)
                ELSEIF (XKZ .GE. X(4) .AND. XKZ .LT. X(5)) THEN
                   WKZ=XKZ*A(4) + B(4)
                ELSEIF (XKZ .GE. X(5) .AND. XKZ .LT. X(6)) THEN
                   WKZ=XKZ*A(5) + B(5)
                ELSEIF (XKZ .GE. X(6) .AND. XKZ .LE. X(7)) THEN
                   WKZ=XKZ*A(6) + B(6)
                ELSE
                   WKZ=Y(9)*X(9)/XKZ
                ENDIF
                WKZ=5.0E-1*( WKZ + 1.0E+0 )
C
C
C               --------------------------------------------
C               Weight for the fixed layer-to-space K (XKFZ)
C               --------------------------------------------
                IF (XKFZ .LT. X(1)) THEN 
                   WKFZ=Y(1)
                ELSEIF (XKFZ .GE. X(1) .AND. XKFZ .LT. X(2)) THEN
                   WKFZ=XKFZ*A(1) + B(1)
                ELSEIF (XKFZ .GE. X(2) .AND. XKFZ .LT. X(3)) THEN
                   WKFZ=XKFZ*A(2) + B(2)
                ELSEIF (XKFZ .GE. X(3) .AND. XKFZ .LT. X(4)) THEN
                   WKFZ=XKFZ*A(3) + B(3)
                ELSEIF (XKFZ .GE. X(4) .AND. XKFZ .LT. X(5)) THEN
                   WKFZ=XKFZ*A(4) + B(4)
                ELSEIF (XKFZ .GE. X(5) .AND. XKFZ .LT. X(6)) THEN
                   WKFZ=XKFZ*A(5) + B(5)
                ELSEIF (XKFZ .GE. X(6) .AND. XKFZ .LE. X(7)) THEN
                   WKFZ=XKFZ*A(6) + B(6)
                ENDIF
                IF (XKFZ .GT. X(9)) THEN
                   WKFZ=Y(9)*X(9)/XKFZ
                ELSE
                   WKFZ=2.0E-1*( WKFZ + 4.0E+0 )
                ENDIF
C
C               ------------------
C               Apply weights to k
C               ------------------
                KTOFIT=WK*WKZ*WKFZ*WA
                WEIGHT=KTOFIT/XK
C
C               ------------------
C               Load BVEC and AMAT
C               ------------------
                BVEC(MCHK)=DBLE( KTOFIT )
                IPRED=0
                DO I=1,NPRED
                   IF (LUSE(I)) THEN
                      IPRED=IPRED + 1
                      AMAT(MCHK,IPRED)=DBLE( WEIGHT*
     $                   PRED(I+IOFFST(IGAS),IPRO,IANG,ILAY) )
                   ENDIF
                ENDDO
C
             ENDIF
C
          ENDDO
       ENDDO
C
C      Make sure MCHK = MPTS
       IF (MCHK .NE. MPTS) THEN
          WRITE(IOERR,1010) ILAY, IGAS, MCHK, MPTS
 1010     FORMAT('Error, layer ',I3, ', gas ',I1,
     $       ', unexpected number of data points.',/,
     $       'Found ',I4,' data points, expecting ',I4)
ccc
c uncomment for data dump
c      do IPRO=1,MAXPRO
c      write(6,1234) (KLAYER(IPRO,IANG,ILAY,IGAS),IANG=1,MAXANG)
c 1234 format(12(1X,1PE12.5))
c      enddo
ccc
          STOP
       ENDIF
C
       RETURN
       END
