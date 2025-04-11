C***********************************************************************
C
C      Purpose : Interpolate the various amounts, pressures, and
C         temperatures to the 300 layers for use with our OPTRAN FTC.
C
C      Arguments
C         AZ     aR i : gas amount above
C         AZOP   aR i : gas amount above grid for OPTRAN
C         P      aR i : pressure
C         T      aR i : temperature
C         PZ     aR i : pressure above
C         TZ     aR i : temperature above
C         ODZ    aR i : optical depth above
C         K      aR i : absorption coefficient
C         POP    aR o : pressure for OPTRAN
C         TOP    aR o : temperature for OPTRAN
C         PZOP   aR o : pressure above for OPTRAN
C         TZOP   aR o : temperature above for OPTRAN
C         ODZOP  aR o : optical depth above for OPTRAN
C         KOP    aR o : abs coef for OPTRAN
C         AVGP   aR o : average POP
C         AVGT   aR o : average TOP
C         AVGPZ  aR o : average PZOP
C         AVGTZ  aR o : average TZOP
C
C***********************************************************************
C
C      =================================================================
       SUBROUTINE INTERP(AZ, AZOP,  P,   T,   PZ,   TZ,   ODZ,   K,
     $                              POP, TOP, PZOP, TZOP, ODZOP, KOP,
     $    AVGP, AVGT, AVGPZ, AVGTZ)
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
       REAL     AZ(MAXLAY,  MAXM)
       REAL   AZOP(NOPLEV)
       REAL      P(MAXLAY,  MAXM)
       REAL      T(MAXLAY,  MAXM)
       REAL     PZ(MAXLAY,  MAXM)
       REAL     TZ(MAXLAY,  MAXM)
       REAL    ODZ(MAXLAY,  MAXM)
       REAL      K(MAXLAY,  MAXM)
C
C      Output arguments
       REAL    POP(NOPLEV,  MAXM)
       REAL    TOP(NOPLEV,  MAXM)
       REAL   PZOP(NOPLEV,  MAXM)
       REAL   TZOP(NOPLEV,  MAXM)
       REAL  ODZOP(NOPLEV,  MAXM)
       REAL    KOP(NOPLEV,  MAXM)
       REAL   AVGP(NOPLEV)
       REAL   AVGT(NOPLEV)
       REAL  AVGPZ(NOPLEV)
       REAL  AVGTZ(NOPLEV)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       LOGICAL LAST
       INTEGER      I
       INTEGER      L
       INTEGER    LOP
       INTEGER LOPMIN
       INTEGER    LPL
       INTEGER    LPU
       INTEGER NCOUNT(NOPLEV)
       REAL     DA
       REAL  RJUNK
       REAL   SUMP(NOPLEV)
       REAL   SUMT(NOPLEV)
       REAL  SUMPZ(NOPLEV)
       REAL  SUMTZ(NOPLEV)


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
C      Initialize the predictor sums
       DO LOP=1,NOPLEV
          SUMP(LOP)=0.0E+0
          SUMT(LOP)=0.0E+0
          SUMPZ(LOP)=0.0E+0
          SUMTZ(LOP)=0.0E+0
          NCOUNT(LOP)=0
       ENDDO
C
C      Loop over the profiles*angles
       DO I=1,MAXM
C
C         Find the max OPTRAN level that is less than AZ(1)
          LOPMIN=1
 30       IF (AZOP(LOPMIN+1) .LT. AZ(1,I)) THEN
             LOPMIN=LOPMIN + 1
             GOTO 30
          ENDIF
          DO LOP=1,LOPMIN-1
             KOP(LOP,I)=NODATA
          ENDDO
C
C         Initialize the upper and lower (pressure) layer index
          LPL=1
          LPU=2
          LAST=.FALSE.
C
C         Loop over the OPTRAN layers (while loop)
          LOP=LOPMIN
 10       IF (LOP .LE. NOPLEV) THEN
C
C            Find the two pressure layer grid points that bracket the
C            current OPTRAN layer grid point
 20          IF (AZ(LPU,I) .LT. AZOP(LOP)) THEN
                IF (LPU .LT. MAXLAY) THEN
                   LPL=LPU
                   LPU=LPU + 1
                   GOTO 20
                ELSE
                   LAST=.TRUE.
                ENDIF
             ENDIF
C
C            Compute the interpolation fractor
             DA=(AZOP(LOP) - AZ(LPL,I))/(AZ(LPU,I) - AZ(LPL,I))
C
C            Do the interpolation
              POP(LOP,I)=DA*(  P(LPU,I) -  P(LPL,I) ) +  P(LPL,I)
              TOP(LOP,I)=DA*(  T(LPU,I) -  T(LPL,I) ) +  T(LPL,I)
             PZOP(LOP,I)=DA*( PZ(LPU,I) - PZ(LPL,I) ) + PZ(LPL,I)
             TZOP(LOP,I)=DA*( TZ(LPU,I) - TZ(LPL,I) ) + TZ(LPL,I)
C
             IF (ODZ(LPU,I) .GT. 0.0 .AND. ODZ(LPL,I) .GT. 0.0) THEN
                ODZOP(LOP,I)=DA*( ODZ(LPU,I) - ODZ(LPL,I) ) + ODZ(LPL,I)
                IF (K(LPU,I) .GT. 0.0 .AND. K(LPL,I) .GT. 0.0) THEN
                   KOP(LOP,I)=DA*( K(LPU,I) - K(LPL,I) ) + K(LPL,I)
                ELSE
                   KOP(LOP,I)=NODATA
                ENDIF
             ELSE
                ODZOP(LOP,I)=NODATA
                KOP(LOP,I)=NODATA
             ENDIF
C
C            Increment the predictor sums
             SUMP(LOP)=SUMP(LOP) + POP(LOP,I)
             SUMT(LOP)=SUMT(LOP) + TOP(LOP,I)
             SUMPZ(LOP)=SUMPZ(LOP) + PZOP(LOP,I)
             SUMTZ(LOP)=SUMTZ(LOP) + TZOP(LOP,I)
             NCOUNT(LOP)=NCOUNT(LOP) + 1
C
C            Update LOP and loop
             IF (LAST .EQ. .TRUE.) THEN
C               Flag the array elements without data as "nodata"
                DO L=LOP+1,NOPLEV
                   ODZOP(L,I)=NODATA
                   KOP(L,I)=NODATA
                ENDDO
                LOP=NOPLEV + 1
             ELSE
                LOP=LOP + 1
             ENDIF
             GOTO 10
C
          ENDIF
C         End while loop over LOP
C
       ENDDO
C      End loop over profiles*angles
C
C      Compute the average predictor values
       DO LOP=1,NOPLEV
          IF (NCOUNT(LOP) .GT. 0) THEN
             RJUNK=FLOAT( NCOUNT(LOP) )
             AVGP(LOP)=SUMP(LOP)/RJUNK
             AVGT(LOP)=SUMT(LOP)/RJUNK
             AVGPZ(LOP)=SUMPZ(LOP)/RJUNK
             AVGTZ(LOP)=SUMTZ(LOP)/RJUNK
          ELSE
             WRITE(6,1020) LOP
 1020        FORMAT('WARNING! No data points for OPTRAN layer',I4)
          ENDIF
       ENDDO
C
       RETURN
       END
