C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             NAST
C
C             KEFF
C
!F77====================================================================


!ROUTINE NAME:
C    KEFF


!ABSTRACT:
C    Calculate effective layer and layer-to-space optical depths for
C    each gas from the layer-to-space convolved transmittances.


!CALL PROTOCOL:
C    KEFF(IPRO, ISORT, TAULTS, KLTOS, KLAYER, NDTPTS, NNEG,
C       KZMIN, KZMAX, KMIN, KMAX)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INTEGER    IPRO           Profile no.        none
C    INT ARR    ISORT          Sorted gas indices none
C    REAL ARR   TAULTS         Lay-to-space trans none (transmittance)


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    REAL ARR   KLTOS          l-to-s eff op dpth none (optical depth)
C    REAL ARR   KLAYER         layer eff op depth none (optical depth)


!INPUT/OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INT ARR    NDTPTS         No. of data points none
C    INT ARR    NNEG           No. of negative pt none
C    REAL ARR   KZMIN          min. l-to-s op dp  none
C    REAL ARR   KZMAX          max. l-to-s op dp  none
C    REAL ARR   KMIN           min. layer op dpth none
C    REAL ARR   KMAX           max. layer op dpth none


!RETURN VALUES:
C    none


!PARENT(S):
C    RDCHAN


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    none


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine KEFF takes the layer-to-space convolved transmittace data
C    for one profile in TAULTS and converts it into effective optical
C    depths for each of the gases. ISORT indicates the order to break
C    out the gases. The min and max and number of good and negative
C    data points are updated.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    10 Dec 97 Scott Hannon      Created based on Jan97 AIRS version
C    14 Jul 98 Scott Hannon      Added checks of DDKMIN


!END ===================================================================


C      =================================================================
       SUBROUTINE KEFF(IPRO, ISORT, TAULTS, KLTOS, KLAYER, NDTPTS, NNEG,
     $    KZMIN, KZMAX, KMIN, KMAX)
C      =================================================================


C-----------------------------------------------------------------------
C      IMPLICIT NONE
C-----------------------------------------------------------------------
       IMPLICIT NONE


C-----------------------------------------------------------------------
C      INCLUDE FILES
C-----------------------------------------------------------------------
#include "farray.f"


C-----------------------------------------------------------------------
C      EXTERNAL FUNCTIONS
C-----------------------------------------------------------------------
C    none


C-----------------------------------------------------------------------
C      ARGUMENTS
C-----------------------------------------------------------------------
C      Input parameters
       INTEGER   IPRO
       INTEGER  ISORT(MAXGAS)
       REAL TAULTS(MAXLAY,MAXGAS,MAXANG)
C
C      Output parameters
       REAL  KLTOS(MAXPRO,MAXANG,MAXLAY,NUMGAS)
       REAL KLAYER(MAXPRO,MAXANG,MAXLAY,NUMGAS)
C
C      Input/Output parameters
       INTEGER NDTPTS(MAXLAY,NUMGAS)
       INTEGER   NNEG(MAXLAY,NUMGAS)
       REAL  KZMIN(MAXLAY,NUMGAS)
       REAL  KZMAX(MAXLAY,NUMGAS)
       REAL   KMIN(MAXLAY,NUMGAS)
       REAL   KMAX(MAXLAY,NUMGAS)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER   IANG  ! Angle looping variable
       INTEGER   ILAY  ! Layer looping variable
       INTEGER   IGAS  ! Gas looping variable
       REAL     XK  ! Work layer k
       REAL    XKZ  ! Work layer-to-space kz 
       REAL    XDT  ! Work l-to-s or layer change in transmittance
       DOUBLE PRECISION    DKZ(NUMGAS)  ! Work layer-to-space kz
       DOUBLE PRECISION DKZRAW(NUMGAS)  ! Work mixed gas l-to-s kz
       DOUBLE PRECISION  DKZLA(NUMGAS)  ! Work layer-above l-to-s kz
       DOUBLE PRECISION     DK          ! Work layer k


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
C      --------------------
C      Loop over the angles
C      --------------------
       DO IANG=1,MAXANG
C
C         Initialize the Layer Above optical depths to
C         a negligibly small (but not zero) value.
          DO IGAS=1,NUMGAS
             DKZLA(IGAS)=1.0D-30
          ENDDO
C
C         --------------------
C         Loop over the layers
C         --------------------
          DO ILAY=1,MAXLAY
C
C            --------------------------------------
C            Convert transmittance to optical depth
C            --------------------------------------
C            Order the gases by ISORT. Do the math with doubles
C            to minimize precision and round-off errors.
C
             DO IGAS=1,NUMGAS
                IF (TAULTS(ILAY,ISORT(IGAS),IANG) .GT. TAUMIN) THEN
                   DKZRAW(IGAS)=-DLOG(
     $                DBLE( TAULTS(ILAY,ISORT(IGAS),IANG) ) )
                ELSE
                   DKZRAW(IGAS)=DBLE( NODATA )
                ENDIF
             ENDDO
C
C            -------------------------------------------------
C            Break out the single gas effective layer-to-space
C            optical depths from the combined gases l-to-s kz
C            -------------------------------------------------
C
             DO IGAS=NUMGAS,2,-1
                IF ( (DKZRAW(IGAS)   .GT. DKZMIN  .AND.
     $                DKZRAW(IGAS)   .LT. DKZMAX) .AND.
     $                DKZRAW(IGAS-1) .LT. DKZMAX ) THEN
                   IF ( DKZRAW(IGAS-1) .GT. DKZMIN ) THEN
                      DKZ(IGAS)=DKZRAW(IGAS) - DKZRAW(IGAS-1)
                   ELSE
                      DKZ(IGAS)=DKZRAW(IGAS)
                   ENDIF
C
C                  Check the change in transmittance between the
C                  two combined gas transmittances.
                   XDT=TAULTS(ILAY,ISORT(IGAS-1),IANG) -
     $                TAULTS(ILAY,ISORT(IGAS),IANG)
                   IF (XDT .LT. TAUDEL) DKZ(IGAS)=DBLE( NODATA )
C
                ELSE
                   DKZ(IGAS)=DBLE( NODATA )
                ENDIF
             ENDDO
C            Do the gas that was already broken out
             IF ( DKZRAW(1) .GT. DKZMIN .AND.
     $            DKZRAW(1) .LT. DKZMAX ) THEN
                DKZ(1)=DKZRAW(1)
             ELSE
                DKZ(1)=DBLE( NODATA )
             ENDIF
C
C            -----------------------------------------------------   
C            Calculate effective layer optical depths for each gas
C            -----------------------------------------------------
C
             DO IGAS=1,NUMGAS
C               The effective layer optical depth is the effective
C               l-to-s optical depth in the current layer minus the
C               value in the layer above.
                IF (  DKZ(IGAS) .GT. 0.0D+0 .AND.
     $              DKZLA(IGAS) .GT. 0.0D+0) THEN
                   DK=DKZ(IGAS) - DKZLA(IGAS)
C                  Note that it is possible DK < 0
                ELSE
C                  Set 0 <= DK < DKMIN
                   DK=1.0D-30
                ENDIF
C
C               ----------------------------
C               Check that the optical depth
C               is not too big or too small.
C               ----------------------------
C
                IF ((DK .GT. DKMIN) .AND. (DK .LT. DKMAX)) THEN
C                  Optical depth is in the OK range
                   XK=SNGL(DK)
                   XKZ=SNGL(DKZ(IGAS))
                   KLAYER(IPRO,IANG,ILAY,IGAS)=XK
                   KLTOS(IPRO,IANG,ILAY,IGAS)=XKZ
C
C                  Update the number of good data points
                   NDTPTS(ILAY,IGAS)=NDTPTS(ILAY,IGAS) + 1
C
C                  Check the min/max
                   IF (XKZ .LT. KZMIN(ILAY,IGAS)) KZMIN(ILAY,IGAS)=XKZ
                   IF (XKZ .GT. KZMAX(ILAY,IGAS)) KZMAX(ILAY,IGAS)=XKZ
                   IF (XK .LT. KMIN(ILAY,IGAS)) KMIN(ILAY,IGAS)=XK
                   IF (XK .GT. KMAX(ILAY,IGAS)) KMAX(ILAY,IGAS)=XK
C
                ELSE
C                  Optical depth is too big or too small; ignore
                   KLAYER(IPRO,IANG,ILAY,IGAS)=NODATA
C
                   IF (DK .LT. 0.0D+0) THEN
C                     Optical depth is negative; unfitable
                      KLAYER(IPRO,IANG,ILAY,IGAS)=NEGDAT
C
C                     Update the number of negative data points
                      NNEG(ILAY,IGAS)=NNEG(ILAY,IGAS) + 1
                   ENDIF
C
                ENDIF
C
C               Update the layer above layer-to-space optical depth
                DKZLA(IGAS)=DKZ(IGAS)
C
             ENDDO
C
          ENDDO
C         End loop over layers
C
       ENDDO
C      End loop over angles
C
       RETURN
       END
