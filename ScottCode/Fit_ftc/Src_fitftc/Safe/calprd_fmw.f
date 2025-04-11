C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             CALPRD_fmw
C
!F77====================================================================


!ROUTINE NAME:
C    CALPRD


!ABSTRACT:
C    Calculate the predictors for the regression data.
C    This version is for FMW.


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


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C     3 Jan 97 Scott Hannon      Created
C    30 Aug 99 Scott Hannon      Add LSTGAS & IGAS[FMW] & IDFIX to
C                                   replace hardcoded numbers; change
C                                   NPREDW to 11 (was 13)
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
       INTEGER   IGASF
       INTEGER   IGASM
       INTEGER   IGASW
       INTEGER   ILAY
       INTEGER   IPRO
C
       REAL    A_M
       REAL    A_W
       REAL  AJUNK
       REAL   AZ_M
       REAL   AZ_W
       REAL     DT
       REAL     MZ
       REAL  MZREF(MAXLAY)
       REAL    PDP(MAXLAY)
       REAL  PNORM(MAXLAY)
       REAL  RJUNK
       REAL  TAZ_M
       REAL    TMZ
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
       IGASF=-1
       IGASM=-1
       IGASW=-1
       DO IPRO=1,NUMGAS
          IF (LSTGAS(IPRO) .EQ. IDFIX) IGASF=IPRO
          IF (LSTGAS(IPRO) .EQ. 6)     IGASM=IPRO
          IF (LSTGAS(IPRO) .EQ. 1)     IGASW=IPRO
       ENDDO
C
C
C      -----------------------------------
C      Check that the number of predictors
C      for F,M,W are what is expected.
C      -----------------------------------
C
       IF (NPREDF .NE. 8) THEN
          WRITE(IOERR,1010) NPREDF
 1010     FORMAT('Error, unexpected number of F predictors.',/,
     $    'Expecting 8, but "farray.f" has NPREDF = ',I2'.')
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
       IF (NPREDM .NE. 9) THEN
          WRITE(IOERR,1030) NPREDM
 1030     FORMAT('Error, unexpected number of M predictors.',/,
     $    'Expecting 9, but "farray.f" has NPREDM = ',I2'.')
          STOP
       ENDIF
       IF (IGASM .NE. 2) THEN
          WRITE(IOERR,1040) IGASM
 1040     FORMAT('Error, unexpected breakout order for Methane.',/,
     $    'Expecting methane as gas #2, but "farray.f" LSTGAS'
     $    ' has it as #',I2) 
           STOP
       ENDIF
C
       IF (NPREDW .NE. 11) THEN
          WRITE(IOERR,1070) NPREDW
 1070     FORMAT('Error, unexpected number of W predictors.',/,
     $    'Expecting 11, but "farray.f" has NPREDW = ',I2'.')
          STOP
       ENDIF
       IF (IGASW .NE. 3) THEN
          WRITE(IOERR,1080) IGASW
 1080     FORMAT('Error, unexpected breakout order for Water.',/,
     $    'Expecting water as gas #3, but "farray.f" LSTGAS'
     $    ' has it as #',I2) 
           STOP
       ENDIF
C
C      Assign the predictor array offsets for F,M,W
C      Note: the offsets indicate what part of
C      the PRED array are used by F,M,W.
       IOFFST(IGASF)=0
       IOFFST(IGASM)=IOFFST(IGASF) + NPREDF
       IOFFST(IGASW)=IOFFST(IGASM) + NPREDM
C
C      -------------------------------------------
C      Calculate the terms used in some predictors
C      that do not vary with regression profile.
C      -------------------------------------------
C
C      Assign 1st layer values of terms used in layers-above predictors
       PDP(1)=PRES(1)*( PRES(2) - PRES(1) )
       PNORM(1)=0.0E+0
       MZREF(1)=PDP(1)*AMTREF(1,IGASM)
       WZREF(1)=PDP(1)*AMTREF(1,IGASW)
C
C      Calculate the values for all the other layers
       DO ILAY=2,MAXLAY
          PDP(ILAY)=PRES(ILAY)*( PRES(ILAY) - PRES(ILAY-1) )
          PNORM(ILAY)=PNORM(ILAY-1) + PDP(ILAY)
          MZREF(ILAY)=MZREF(ILAY-1) + PDP(ILAY)*AMTREF(ILAY,IGASM)
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
          MZ=0.0E+0
          WZ=0.0E+0
          TMZ=0.0E+0
          TAZ_M=0.0E+0
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
             IF (ILAY .GT. 1) THEN
                TZ=TZ + PDP(ILAY)*TR
                TRZ=TZ/PNORM(ILAY)
                TMZ=TMZ + PDP(ILAY)*TR*A_M
                TAZ_M=TMZ/PNORM(ILAY)
             ENDIF
C
             TR=TPRO(ILAY,IPRO)/TREF(ILAY)
             DT=TPRO(ILAY,IPRO) - TREF(ILAY)
C
             A_M=AMTPRO(ILAY,IPRO,IGASM)/AMTREF(ILAY,IGASM)
             MZ=MZ + PDP(ILAY)*AMTPRO(ILAY,IPRO,IGASM)
             AZ_M=MZ/MZREF(ILAY)
C
             A_W=AMTPRO(ILAY,IPRO,IGASW)/AMTREF(ILAY,IGASW)
             WZ=WZ + PDP(ILAY)*AMTPRO(ILAY,IPRO,IGASW)
             AZ_W=WZ/WZREF(ILAY)
C
C            -------------------------
C            Loop over the angles and
C            calculate the predictors.
C            -------------------------
             DO IANG=1,MAXANG
C
C               Fixed (NPREDF = 8)
                PRED(IOFFST(IGASF)+1,IPRO,IANG,ILAY)=SECANG(IANG)
                PRED(IOFFST(IGASF)+2,IPRO,IANG,ILAY)=SECANG(IANG)*
     $             SECANG(IANG)
                PRED(IOFFST(IGASF)+3,IPRO,IANG,ILAY)=SECANG(IANG)*TR
                PRED(IOFFST(IGASF)+4,IPRO,IANG,ILAY)=SECANG(IANG)*TR*TR
                PRED(IOFFST(IGASF)+5,IPRO,IANG,ILAY)=TR
                PRED(IOFFST(IGASF)+6,IPRO,IANG,ILAY)=TR*TR
                PRED(IOFFST(IGASF)+7,IPRO,IANG,ILAY)=SECANG(IANG)*TRZ
                PRED(IOFFST(IGASF)+8,IPRO,IANG,ILAY)=SECANG(IANG)*TRZ/TR
C
C               Methane (NPREDM = 9)
                AJUNK=SECANG(IANG)*A_M
                RJUNK=SQRT(AJUNK)
                ZJUNK=SECANG(IANG)*AZ_M
                PRED(IOFFST(IGASM)+1,IPRO,IANG,ILAY)=AJUNK
                PRED(IOFFST(IGASM)+2,IPRO,IANG,ILAY)=RJUNK
                PRED(IOFFST(IGASM)+3,IPRO,IANG,ILAY)=AJUNK*DT
                PRED(IOFFST(IGASM)+4,IPRO,IANG,ILAY)=AJUNK*AJUNK
                PRED(IOFFST(IGASM)+5,IPRO,IANG,ILAY)=AJUNK*SECANG(IANG)
                PRED(IOFFST(IGASM)+6,IPRO,IANG,ILAY)=ZJUNK
                PRED(IOFFST(IGASM)+7,IPRO,IANG,ILAY)=A_M*DT
                PRED(IOFFST(IGASM)+8,IPRO,IANG,ILAY)=SECANG(IANG)*TAZ_M
                PRED(IOFFST(IGASM)+9,IPRO,IANG,ILAY)=SQRT( ZJUNK )
C
C               Water (NPREDW = 11)
                AJUNK=SECANG(IANG)*A_W
                RJUNK=SQRT(AJUNK)
                ZJUNK=A_W/AZ_W
                PRED(IOFFST(IGASW)+ 1,IPRO,IANG,ILAY)=AJUNK
                PRED(IOFFST(IGASW)+ 2,IPRO,IANG,ILAY)=RJUNK
                PRED(IOFFST(IGASW)+ 3,IPRO,IANG,ILAY)=AJUNK*ZJUNK
                PRED(IOFFST(IGASW)+ 4,IPRO,IANG,ILAY)=AJUNK*DT
                PRED(IOFFST(IGASW)+ 5,IPRO,IANG,ILAY)=AJUNK*AJUNK
                PRED(IOFFST(IGASW)+ 6,IPRO,IANG,ILAY)=RJUNK*DT
                PRED(IOFFST(IGASW)+ 7,IPRO,IANG,ILAY)=SQRT(RJUNK)
                PRED(IOFFST(IGASW)+ 8,IPRO,IANG,ILAY)=AJUNK*AJUNK*AJUNK
                PRED(IOFFST(IGASW)+ 9,IPRO,IANG,ILAY)=A_W
                PRED(IOFFST(IGASW)+10,IPRO,IANG,ILAY)=RJUNK*ZJUNK
                PRED(IOFFST(IGASW)+11,IPRO,IANG,ILAY)=RJUNK*AZ_M*
     $             SECANG(IANG)
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
