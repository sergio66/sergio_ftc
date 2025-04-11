c This version for fwo "bfsw" (big fixed & small water)
C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             CALPRD_bfsw
C
!F77====================================================================


!ROUTINE NAME:
C    CALPRD


!ABSTRACT:
C    Calculate the predictors for the regression data.


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
C    This version for FWO and FOW.  Although it works with FOW, it
C    is not recommended for use in regions with significant ozone
C    absorption.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    15 Jan 97 Scott Hannon      Created
C    27 Aug 99 Scott Hannon      Add LSTGAS & IGAS[FWO] & IDFIX to
C                                   replace hardcoded numbers
C    21 May 02 Scott Hannon      Error messages now use IOERR (was 6)


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
       INTEGER  IGASF
       INTEGER  IGASO
       INTEGER  IGASW
       INTEGER   ILAY
       INTEGER   IPRO
C
       REAL    A_O
       REAL    A_W
       REAL  AJUNK
       REAL   AZ_W
       REAL     DT
c       REAL  MZREF(MAXLAY)
       REAL    PDP(MAXLAY)
       REAL  PNORM(MAXLAY)
       REAL  RJUNK
       REAL     TR
       REAL    TRZ
       REAL     TZ
       REAL     WZ
       REAL  WZREF(MAXLAY)
       REAL  ZJUNK


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
C      Check LSTGAS to see it makes sense
C      Note: mfbw works with either F,FO,FOW or F,FW,FWO data
       IGASF=-1
       IGASW=-1
       IGASO=-1
       DO IPRO=1,NUMGAS
          IF (LSTGAS(IPRO) .EQ. IDFIX) IGASF=IPRO
          IF (LSTGAS(IPRO) .EQ. 1)     IGASW=IPRO
          IF (LSTGAS(IPRO) .EQ. 3)     IGASO=IPRO
       ENDDO
C

C      -----------------------------------
C      Check that the number of predictors
C      for F,M,W are what is expected.
C      -----------------------------------
C
       IF (NPREDF .NE. 11) THEN
          WRITE(IOERR,1010) NPREDF
 1010     FORMAT('Error, unexpected number of F predictors.',/,
     $    'Expecting 11, but "farray.f" has NPREDF = ',I2'.')
          STOP
       ENDIF
       IF (IGASF .NE. 1) THEN
          WRITE(IOERR,1020) IGASF
 1020     FORMAT('Error, unexpected breakout order for Fixed gases.',/,
     $    'Expecting fixed as gas #1, but "farray.f" LSTGAS has'
     $    ' it as #',I2)
           STOP
       ENDIF
C
       IF (NPREDW .NE. 3) THEN
          WRITE(IOERR,1030) NPREDW
 1030     FORMAT('Error, unexpected number of W predictors.',/,
     $    'Expecting 3, but "farray.f" has NPREDW = ',I2'.')
          STOP
       ENDIF
       IF (IGASW .NE. 2 .AND. IGASW .NE. 3) THEN
          WRITE(IOERR,1040) IGASW
 1040     FORMAT('Error, unexpected breakout order for Water.',/,
     $    'Expecting water as gas #2 or #3, but "farray.f" LSTGAS'
     $    ' has it as #',I2) 
           STOP
       ENDIF
C
       IF (NPREDO .NE. 1) THEN
          WRITE(IOERR,1070) NPREDW
 1070     FORMAT('Error, unexpected number of O predictors.',/,
     $    'Expecting 1, but "farray.f" has NPREDO = ',I2'.')
          STOP
       ENDIF
       IF (IGASO .NE. 2 .AND. IGASO .NE. 3) THEN
          WRITE(IOERR,1080) IGASO
 1080     FORMAT('Error, unexpected breakout order for Ozone.',/,
     $    'Expecting ozone as gas #2 or #3, but "farray.f" LSTGAS'
     $    ' has it as #',I2) 
           STOP
       ENDIF
C
C      Assign the predictor array offsets for F,W,O
C      Note: the offsets indicate what part of
C      the PRED array are used by F,W,O.
       IOFFST(IGASF)=0
       IOFFST(IGASW)=IOFFST(IGASF) + NPREDF
       IOFFST(IGASO)=IOFFST(IGASW) + NPREDW
C
C      -------------------------------------------
C      Calculate the terms used in some predictors
C      that do not vary with regression profile.
C      -------------------------------------------
C
C      Assign 1st layer values of terms used in layers-above predictors
       PDP(1)=PRES(1)*( PRES(2) - PRES(1) )
       PNORM(1)=0.0E+0
       WZREF(1)=PDP(1)*AMTREF(1,IGASW)
C
C      Calculate the values for all the other layers
       DO ILAY=2,MAXLAY
          PDP(ILAY)=PRES(ILAY)*( PRES(ILAY) - PRES(ILAY-1) )
          PNORM(ILAY)=PNORM(ILAY-1) + PDP(ILAY)
          WZREF(ILAY)=WZREF(ILAY-1) + PDP(ILAY)*AMTREF(ILAY,IGASW)
       ENDDO
C
C      ---------------------------------
C      Loop over the regression Profiles
C      ---------------------------------
       DO IPRO=1,MAXPRO
C
C         Initialize temperature and amount above terms
          TZ=0.0E+0
          TRZ=0.0E+0
          WZ=0.0E+0
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
C
C            Note: TZ uses layer-above TR
C            PNORM(1)=0, so don't divide by 0
             IF (ILAY .GT. 1) THEN
                TZ=TZ + PDP(ILAY)*TR
                TRZ=TZ/PNORM(ILAY)
             ENDIF
C
             TR=TPRO(ILAY,IPRO)/TREF(ILAY)
             DT=TPRO(ILAY,IPRO) - TREF(ILAY)
C
             A_W=AMTPRO(ILAY,IPRO,IGASW)/AMTREF(ILAY,IGASW)
             WZ=WZ + PDP(ILAY)*AMTPRO(ILAY,IPRO,IGASW)
             AZ_W=WZ/WZREF(ILAY)
C
             A_O=AMTPRO(ILAY,IPRO,IGASO)/AMTREF(ILAY,IGASO)
C
C            -------------------------
C            Loop over the angles and
C            calculate the predictors.
C            -------------------------
             DO IANG=1,MAXANG
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C               Fixed (NPREDF = 11)
                PRED(IOFFST(IGASF)+ 1,IPRO,IANG,ILAY)=SECANG(IANG)
                PRED(IOFFST(IGASF)+ 2,IPRO,IANG,ILAY)=SECANG(IANG)*
     $             SECANG(IANG)
                PRED(IOFFST(IGASF)+ 3,IPRO,IANG,ILAY)=SECANG(IANG)*TR
                PRED(IOFFST(IGASF)+ 4,IPRO,IANG,ILAY)=SECANG(IANG)*TR*TR
                PRED(IOFFST(IGASF)+ 5,IPRO,IANG,ILAY)=TR
                PRED(IOFFST(IGASF)+ 6,IPRO,IANG,ILAY)=TR*TR
                PRED(IOFFST(IGASF)+ 7,IPRO,IANG,ILAY)=SECANG(IANG)*TRZ
                PRED(IOFFST(IGASF)+ 8,IPRO,IANG,ILAY)=SECANG(IANG)*
     $             TRZ/TR
                PRED(IOFFST(IGASF)+ 9,IPRO,IANG,ILAY)=SECANG(IANG)*
     $             SECANG(IANG)*TR
                PRED(IOFFST(IGASF)+10,IPRO,IANG,ILAY)=SQRT(SECANG(IANG))
                PRED(IOFFST(IGASF)+11,IPRO,IANG,ILAY)=TRZ
C
C               Water (NPREDW = 3)
                AJUNK=SECANG(IANG)*A_W
                RJUNK=SQRT(AJUNK)
                ZJUNK=A_W/AZ_W
                PRED(IOFFST(IGASW)+1,IPRO,IANG,ILAY)=AJUNK
                PRED(IOFFST(IGASW)+2,IPRO,IANG,ILAY)=AJUNK*RJUNK
                PRED(IOFFST(IGASW)+3,IPRO,IANG,ILAY)=AJUNK*DT
C
C               Ozone (NPREDO = 1)
                AJUNK=SECANG(IANG)*A_O
                PRED(IOFFST(IGASO)+1,IPRO,IANG,ILAY)=AJUNK
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
