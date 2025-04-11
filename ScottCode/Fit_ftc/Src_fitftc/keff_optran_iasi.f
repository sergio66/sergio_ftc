C***********************************************************************
C
C      Subroutine KEFF_optran_cris
C
C      Purpose : compute the individual gas layer-to-space abs coef
C
C      Arguments
C         TAUX   aR i : layer-to-space whatever transmittance
C         TAUXW  aR i : layer-to-space whatever&water trans
C         PROANG aR i : profile angles
C         WAP    aR i : water amount
C         WODZ   aR o : water layer-to-space optical depth
C         WK     aR o : water layer abs coef
C
C Created 6 May 2003 Scott Hannon to enable doing a CrIS fast model
C very nearly the same as the standard AIRS model.  Only changes
C for CrIS are the min/max trans and optical depth constants.
C Update: 03 Jan 2008, S.Hannon - minor mods for IASI
C***********************************************************************
C
C      =================================================================
       SUBROUTINE KEFF(TAUX, TAUXW, PROANG, WAP, WODZ, WK)
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
       REAL   TAUX(MAXPRO,MAXLAY,MAXANG)
       REAL  TAUXW(MAXPRO,MAXLAY,MAXANG)
       REAL PROANG(MAXPRO,MAXANG)
       REAL  WAP(MAXPRO,MAXLAY)
C
C      Output arguments
       REAL   WODZ(MAXLAY,  MAXM)
       REAL     WK(MAXLAY,  MAXM)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      I
       INTEGER     IJ
       INTEGER      J
       INTEGER      K
       INTEGER      L
       DOUBLE PRECISION DELTAK
       DOUBLE PRECISION DTXZ
       DOUBLE PRECISION DTXWZ
       DOUBLE PRECISION DKXZ
       DOUBLE PRECISION DKWZ
       DOUBLE PRECISION DKWZM1
       DOUBLE PRECISION DNODAT

       DOUBLE PRECISION DELTAT
C      Constants for testing transmittance and optical depth
       DOUBLE PRECISION TAUMIN
       DOUBLE PRECISION TAUMAX
       DOUBLE PRECISION TAUDEL
       DOUBLE PRECISION DKZMIN
       DOUBLE PRECISION DKZMAX
       DOUBLE PRECISION  DKMIN
       DOUBLE PRECISION  DKMAX

C-----------------------------------------------------------------------
C      SAVE STATEMENTS
C-----------------------------------------------------------------------
C    none


C***********************************************************************
C***********************************************************************
C                            EXECUTABLE CODE
C***********************************************************************
C***********************************************************************

C      Assign values to test constants
C      Note: values are for CrIS using Hamming apodization
ccc
c       TAUMIN=1.5D-6
c       TAUMAX=1.0D+0 - TAUMIN
c       TAUDEL=2.5D-6
c       DKZMIN=2.0D-6
c       DKZMAX=7.7D+0
c       DKMIN=1.5D-6
c       DKMAX=7.1D+0
ccc
c       TAUMIN=1.5D-5
c       TAUMAX=1.0D+0 - TAUMIN
c       TAUDEL=2.5D-5
c       DKZMIN=2.0D-5
c       DKZMAX=6.7D+0
c       DKMIN=1.5D-5
c       DKMAX=6.1D+0
ccc
       TAUMIN=1.0D-5
       TAUMAX=1.0D+0 - TAUMIN
       TAUDEL=2.0D-5
       DKZMIN=1.5D-5
       DKZMAX=6.8D+0
       DKMIN=1.0D-5
       DKMAX=6.2D+0
C
C
       DNODAT=DBLE(NODATA)
C
C      Loop over the fitting profiles
       DO K=1,MAXPRO
C
C         Calc the MAXM starting index
          I=(K-1)*MAXANG 
C
C         Loop over the angles
          DO J=1,MAXANG
C
C            Initialize the previous layer layer-to-space optical depth
             DKWZM1=0.0D+0
C
C            The profile by angle index
             IJ=I + J
C
C            Loop over the layers
             DO L=1,MAXLAY
C
                DTXZ=DBLE( TAUX(K,L,J) )
                IF (DTXZ .GT. TAUMIN .AND. DTXZ .LE. TAUMAX) THEN
                   DKXZ=-DLOG( DTXZ )
C
                   DTXWZ=DBLE( TAUXW(K,L,J) )
                   DELTAT=DTXZ - DTXWZ
                   IF ((DTXWZ .GT. TAUMIN .AND. DTXWZ .LE. TAUMAX)
     $             .AND. (DELTAT .GT. TAUDEL)) THEN
                      DKWZ=-DLOG( DTXWZ/DTXZ )
                      WODZ(L,IJ)=SNGL( DKWZ )
C
                      DELTAK=DKWZ - DKWZM1
                      IF ((DKWZM1 .GE. 0.0D+0) .AND.
     $                (DELTAK .GT. DKMIN .AND. DELTAK .LT. DKMAX)) THEN
                         WK(L,IJ)=SNGL( DELTAK )/
     $                      ( PROANG(K,J)*WAP(K,L) )
                      ELSE
                         WK(L,IJ)=NODATA
                      ENDIF
C
                   ELSE
                      DKWZ=DNODAT
                      WODZ(L,IJ)=NODATA
                      WK(L,IJ)=NODATA
                   ENDIF
C
                ELSE
                   DKWZ=DNODAT
                   WODZ(L,IJ)=NODATA
                   WK(L,IJ)=NODATA
                ENDIF
C
C               Update the previous layer layer-to-space optical depth
                DKWZM1=DKWZ
C
ccc
c This block for testing only
c       write(6,1234) k,l,j,ij, wodz(l,ij),wk(l,ij)
c 1234  format(X,I3,X,I3,X,I2,X,I4,2(X,1PE11.3))
ccc
             ENDDO
C            End loop over layers
C
          ENDDO
C         End loop over angles
C
       ENDDO
C      End loop over profiles
C
       RETURN
       END
