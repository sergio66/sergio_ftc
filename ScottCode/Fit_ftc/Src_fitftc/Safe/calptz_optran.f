c This version has Az at center of pressure layer, not at lower boundary
C***********************************************************************
C
C      Purpose : Compute various sums of amounts, pressures, and
C         temperatures for use with our OPTRAN FTC.
C
C      Arguments
C         PROANG aR i : profile angles (actually 1/cos(angle))
C         PRES   aR i : pressure levels
C         TPROF  aR i : profile temperature
C         WAP    aR i : water gas amount
C         ANGLE  ar o : angles (actually 1/cos(angle))
C         WTZ    aR o : water gas temperature above
C         WAZ    aR o : water gas amount above
C         WP     aR o : water gas pressure
C         WT     aR o : water gas temperature
C         WPZ    aR o : water gas pressure above
C
C***********************************************************************
C
C      =================================================================
       SUBROUTINE CALPTZ( PROANG, PRES, TPROF, WAP, ANGLE,
     $    WAZ, WP, WT, WPZ, WTZ )
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
C      Input arguments
       REAL PROANG(MAXPRO,MAXANG)
       REAL   PRES(MAXLAY)
       REAL  TPROF(MAXPRO,MAXLAY)
       REAL    WAP(MAXPRO,MAXLAY)
C
C      Output arguments
       REAL  ANGLE(  MAXM)
       REAL    WAZ(MAXLAY,  MAXM)
       REAL     WP(MAXLAY,  MAXM)
       REAL     WT(MAXLAY,  MAXM)
       REAL    WPZ(MAXLAY,  MAXM)
       REAL    WTZ(MAXLAY,  MAXM)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      I
       INTEGER     IJ
       INTEGER      J
       INTEGER      K
       INTEGER      L
c       INTEGER    LM1
       REAL  WAANG
       REAL  WAZSUM
       REAL  WPZSUM
       REAL  WTZSUM


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
C      ------------------------------
C      Loop over the fitting profiles
C      ------------------------------
       DO K=1,MAXPRO
C
C         Calc the MAXM starting index
          I=(K-1)*MAXANG 
C
C         Loop over the angles
          DO J=1,MAXANG
C
C            Profile-by-angle index
             IJ=I + J
             ANGLE(IJ)=PROANG(K,J)
C
C            Initialize the layer-to-space sums
             WAZSUM=0.0E+0
             WPZSUM=0.0E+0
             WTZSUM=0.0E+0
C
C            Loop over the layers
             DO L=1,MAXLAY
C
C               Layer mean temperature and pressure
                WP(L,IJ)=PRES(L)
                WT(L,IJ)=TPROF(K,L)
C
C               Layer amount*angle
                WAANG=WAP(K,L)*PROANG(K,J)
C
C               Center-of-layer-to-space amount*angle
C               Note: do this before updating AZSUM
                WAZ(L,IJ)=5.0E-1*WAANG + WAZSUM
C
C               Bottom-of-layer-to-space amount*angle
                WAZSUM=WAANG + WAZSUM
C
C               Temp and pres (times amount*angle) above sums
                WPZSUM=WAANG*PRES(L)    + WPZSUM
                WTZSUM=WAANG*TPROF(K,L) + WTZSUM
C
C               Temperature and pressure above
                WPZ(L,IJ)=WPZSUM/WAZSUM
                WTZ(L,IJ)=WTZSUM/WAZSUM
C
             ENDDO
C            End of loop over layers
C
          ENDDO
C         End of loop over angles
C
       ENDDO
C      End of loop over profiles
C
       RETURN
       END
