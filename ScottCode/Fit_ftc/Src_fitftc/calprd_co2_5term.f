C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             CALPRD_co2
C
!F77====================================================================


!ROUTINE NAME:
C    CALPRD


!ABSTRACT:
C    Calculate the predictors for the regression data.
C    This version is for CO2


!CALL PROTOCOL:
C    CALPRD(LSTGAS, SECANG, PRES, TREF, AMTREF, TPRO, AMTPRO, IOFFST,
C       PRED)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INT ARR    LSTGAS         Gas IDs for fit    none
C    REAL ARR   SECANG         Angle secant       none
C    REAL ARR   PRES           Pressure           mbar
C    REAL ARR   TREF           Ref prof temp      K
C    REAL ARR   AMTREF         Ref prof amounts   k.mol/cm^2
C    REAL ARR   TPRO           Profile temp       K
C    REAL ARR   AMTPRO         Profile amounts    k.mol/cm^2


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INT ARR    IOFFST         Pred index offsets none
C    REAL ARR   PRED           Predictors         various


!INPUT/OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    PREP


!ROUTINES CALLED:
C    none


!FILES ACCESSED:
C    none


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine CALPRD calculates the predictors to be used in the
C    regression of the effective layer transmittances to determine
C    the corresponding fast transmittance (actually optical depth)
C    coefficients. The routine first calculates a few terms that
C    depend only on the reference profile values and/or the layer
C    pressure (the same for all profiles). It then loops over the
C    regression profiles. It loops over the layers, and for each
C    layer it calculates the profile dependent terms. Finally, it
C    loops over the angles and calculates the predictors using the
C    profile independent and dependent layer terms, and the angle
C    secant.
C    This version for CO2.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C Date      Programmer        Comments
C ----------- --------------- ---------------------------------------
C 21 Jan 1999 Scott Hannon    Created based on calprd_fwo
C 21 May 2002 Scott Hannon    Error messages now use IOERR (was 6)
C 30 Apr 2008 Scott Hannon    create 5 term variant

!END ===================================================================


C      =================================================================
       SUBROUTINE CALPRD(LSTGAS, SECANG, PRES, TREF, AMTREF, TPRO,
     $    AMTPRO, IOFFST, PRED)
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
       REAL SECANG(MAXANG)
       REAL   PRES(MAXLAY)
       REAL   TREF(MAXLAY)
       REAL AMTREF(MAXLAY,NUMGAS)
       REAL   TPRO(MAXLAY,MAXPRO)
       REAL AMTPRO(MAXLAY,MAXPRO,NUMGAS)
C
C      Output parameters
       INTEGER IOFFST(NUMGAS)
       REAL   PRED(MAXPRD,MAXPRO,MAXANG,MAXLAY)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER   IANG
       INTEGER   IGASP
       INTEGER   ILAY
       INTEGER   IPRO
C
       REAL     TR


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
C      All gases with CO2 pert is the 2nd break-out gas
       IGASP=2
C
C      ------------------------------
C      Check the number of predictors
C      ------------------------------
       IF (NPREDP .NE. 5) THEN
          WRITE(IOERR,1010) NPREDP
 1010     FORMAT('Error, unexpected number of CO2 predictors.',/,
     $    'Expecting 5, but "farray.f" has NPREDP = ',I2'.')
          STOP
       ENDIF
C
C      Assign the predictor array offsets for CO2
       IOFFST(IGASP)=0
C
C      ---------------------------------
C      Loop over the regression Profiles
C      ---------------------------------
       DO IPRO=1,MAXPRO
C
C         --------------------
C         Loop over the layers
C         --------------------
          DO ILAY=1,MAXLAY
C
C            -----------------------------------
C            Calculate all the Profile dependent
C            terms used in the predictors.
C            -----------------------------------
             TR=TPRO(ILAY,IPRO)/TREF(ILAY)
C
C            -------------------------
C            Loop over the angles and
C            calculate the predictors.
C            -------------------------
             DO IANG=1,MAXANG
C
C               CO2 Pert (NPREDP = 5)
                PRED(IOFFST(IGASP)+1,IPRO,IANG,ILAY)=SECANG(IANG)
                PRED(IOFFST(IGASP)+2,IPRO,IANG,ILAY)=TR
                PRED(IOFFST(IGASP)+3,IPRO,IANG,ILAY)=SECANG(IANG)*TR
                PRED(IOFFST(IGASP)+4,IPRO,IANG,ILAY)=SECANG(IANG)*TR*TR
                PRED(IOFFST(IGASP)+5,IPRO,IANG,ILAY)=
     $             SECANG(IANG)*SECANG(IANG)
C
             ENDDO
C            End loop over angles
C 
          ENDDO
C         End loop over layers
C
       ENDDO
C      End loop over profiles
C
       RETURN
       END
