C=======================================================================
C=======================================================================
C  University of Maryland, Baltimore County [UMBC]
C  L. Larrabee Strow, AIRS Team Member
C  Routine created by: Scott Hannon
C
C  URAD: for use with useconv
C  
C=======================================================================
C=======================================================================
C
C ROUNTINE NAME: URad
C     Useconv RADiance calc
C
C ABSTRACT:
C     Use a temperature profile, layer transmittances, and boundary
C     conditions to compute the radiance from the top layer.
C     Nadir view, and no clouds.
C
C CALLING PROTOCAL:
C    Type        Routine_name(arg1, arg2, ..., argN)
C    ----------  -------------------------------------------------------
C    SUBROUTINE  URAD(FREQ,TAUZ,TEMP,TTOP,TBOT,EBOT,RAD)
C
C ARGUMENTS:
C    TYPE          NAME      I/O   PURPOSE
C    ------------  --------  ----  -------------------------------------
C    REAL          EBOT      I     emissivity of surface
C    REAL          FREQ      I     frequency [in cm-1]
C    REAL          RAD       O     computed radiance
C    REAL          TBOT      I     temperature of surface [in K]
C    REAL array    TEMP        I     temperature profile [in K]
C    REAL array    TAUZ      I     transmittance of profile
C    REAL          TTOP      I     temperature of space [in K]
C=======================================================================
C
C ROUNTINES CALLED: none.
C
C FILES ACCESSED: none.
C
C GLOBAL VARIABLES: none.
C
C RETURN VALUES: na
C
C=======================================================================
C
C FUNCTIONAL DESCRIPTION:
C    The routine starts out fresh each time it is run, and makes no
C    assumptions about file positioning, previous processing, etc.
C       The routine first propagates the 3K blackbody temperature of
C    space down to the surface [its basically irrelevant], and then
C    propagates the reflected and emitted radiance at the surface back
C    up thru the layers to space. Each atmos layer also emits as if a
C    black body at the profiles temperature.
C
C ALGORITHM REFERENCES: [a standard nadir view radiative transfer eq]
C
C KNOWN BUGS AND LIMITATIONS: none.
C
C ROUTINE HISTORY:
C    Version    Date        Programmer               Comments
C    ---------  ----------  -----------------------  -------------------
C    2.0        Dec 07, 92  Scott Hannon             Simple but it works
C
C***********************************************************************
C***********************************************************************
C                    EXECUTABLE CODE follows...
C***********************************************************************
C***********************************************************************
C      Subroutine uRAD
C
C      Purpose : Compute the radiance for the top layers
C
C      Arguements
C         FREQ   R i  : the frequency
C         TAUZ  aR i  : the transmissions thru the layers
C         TEMP  aR i  : the temperatures of the layers
C         TTOP   R i  : the temp above the top layer (T of space)
C         TBOT   R i  : the temp below the bottom layer (T of ground)
C         EBOT   R i  : the emissivity of the ground
C         RAD    R o  : the top layer radiance
C
C***********************************************************************
C
       SUBROUTINE URAD(FREQ, TAUZ, TEMP, NANG, LBOT, TBOT, EBOT, RAD)
C
       INCLUDE 'farray.f'
C
       INTEGER NANG, LBOT
       REAL FREQ, TAUZ(MAXLAY,MAXANG), TEMP(MAXLAY), TBOT, EBOT,
     $    RAD(MAXANG)
C
C      Local variables
       INTEGER ILAY
       REAL C1, C2, C1FC, C2FREQ, EMS(MAXLAY), EMSB
C
C
C****************************100****************************************
C
       C1=1.1911E-8
       C2=1.4387863E+0
C
       C1FC=C1*FREQ*FREQ*FREQ
       C2FREQ=C2*FREQ
C
C      -----------------------------------
C      Calculate the Planck function black
C      body emission for each layer.
C      -----------------------------------
       DO ILAY=1,LBOT
          EMS(ILAY)=C1FC/(EXP( C2FREQ/TEMP(ILAY) ) - 1.0E+0)
       ENDDO
       EMSB=EBOT*C1FC/(EXP( C2FREQ/TBOT ) - 1.0E+0)
C
C      --------------------
C      Loop over the angles
C      --------------------
       DO IANG=1,NANG
C
C         -----------------
C         Calc the radiance
C         -----------------
C         Note: No reflected radiance.
C
C         Add on the radiance coming from the ground
          RAD(IANG)=EMSB*TAUZ(LBOT,IANG)
C
C         Calc up going radiance
          DO ILAY=LBOT,2,-1
             RAD(IANG)=RAD(IANG) +
     $          (TAUZ(ILAY-1,IANG) - TAUZ(ILAY,IANG))*EMS(ILAY)
          ENDDO
          RAD(IANG)=RAD(IANG) +
     $          (1.0E+0 - TAUZ(1,IANG))*EMS(ILAY)
C
       ENDDO
C      End of loop over angles
C
       RETURN
       END
