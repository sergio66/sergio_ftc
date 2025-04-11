C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
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
C    KEFF(IPRO, ISORTK, TAULTS, KLTOS, KLAYER, NDTPTS, NNEG,
C       KZMIN, KZMAX, KMIN, KMAX)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INTEGER    IPRO           Profile no.        none
C    INT ARR    ISORTK         trans data indices none
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
C    depths for each of the gases. ISORTK indicates the order to break
C    out the gases. The min and max and number of good and negative
C    data points are updated.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    16 Jan 97 Scott Hannon      Created
C    12 Aug 98 Scott Hannon      Fix error breaking out dkz(1)
C    26 Aug 99 Scott Hannon      Re-order data in TAULTS; ISORT(NUMSET)
C                                   changed to ISORTK(MAXBOG)


!END ===================================================================


C      =================================================================
       SUBROUTINE KEFF(IPRO, ISORTK, TAULTS, KLTOS, KLAYER, NDTPTS,
     $    NNEG, KZMIN, KZMAX, KMIN, KMAX)
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
       INTEGER   IPRO
       INTEGER ISORTK(MAXBOG)
C old order      REAL TAULTS(MAXLAY,NUMSET,MAXANG)
       REAL TAULTS(MAXANG,MAXLAY,NUMSET)
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
       INTEGER   IANG
       INTEGER   IGAS
       INTEGER   ILAY
       REAL     XK
       REAL    XKZ
       DOUBLE PRECISION     DK
       DOUBLE PRECISION  DKZLA(NUMGAS)
       DOUBLE PRECISION    DKZ(NUMGAS)
       DOUBLE PRECISION DKZRAW(NUMGAS)


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
C         zero for the current angle
          DO IGAS=1,NUMGAS
             DKZLA(IGAS)=0.0D+0
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
C            Order the gases by ISORTK. Do the math with doubles
C            to minimize precision and round-off errors.
C
             DO IGAS=1,NUMGAS
                DKZRAW(IGAS)=-DLOG(
     $             DBLE( TAULTS(IANG,ILAY,ISORTK(IGAS)) ) )
C old order     $             DBLE( TAULTS(ILAY,ISORTK(IGAS),IANG) ) )
             ENDDO
C
C            ----------------------------------
C            Calculate effective l-to-s optical
C            depths by breaking out the gases.
C            ----------------------------------
C            Do the gas that was already broken out
             DKZ(1)=DKZRAW(1)
C
C            Do the other gases
             DO IGAS=2,NUMGAS
                DKZ(IGAS)=DKZRAW(IGAS) - DKZRAW(IGAS-1)
             ENDDO
C
C            -----------------------------------------------------   
C            Calculate effective layer optical depths for each gas
C            -----------------------------------------------------
C
             DO IGAS=1,NUMGAS
C               The effective layer optical depth is the effective
C               l-to-s optical depth in the current layer minus the
C               value in the layer above.
                DK=DKZ(IGAS) - DKZLA(IGAS)
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
C                  Optical depth is too big, too small, or NaN; ignore
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
