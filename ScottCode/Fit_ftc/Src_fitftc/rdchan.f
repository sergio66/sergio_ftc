C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             RDCHAN
C
!F77====================================================================


!ROUTINE NAME:
C    RDCHAN


!ABSTRACT:
C    Read all data for a channel, and calculate the effective
C    layer-to-space and layer optical depths.


!CALL PROTOCOL:
C    RDCHAN(ICHAN, IOIN, ISORTK, NDTPTS, NNEG, IDCHAN, FREQ,
C       KLTOS, KLAYER, KZMIN, KZMAX, KMIN, KMAX)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INTEGER    ICHAN          Channel number     none
C    INT ARR    IOIN           Data file unit no. none
C    INT ARR    ISORTK         trans data indices none


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INT ARR    NDTPTS         No. of data points none
C    INT ARR    NNEG           No. of negative pt none
C    INTEGER    IDCHAN         Channel ID number  none
C    REAL       FREQ           Chan center freq   cm^-1
C    REAL ARR   KLTOS          l-to-s eff op dpth none
C    REAL ARR   KLAYER         layer eff op depth none
C    REAL ARR   KZMIN          min l-to-s op dpth none
C    REAL ARR   KZMAX          max l-to-s op dpth none
C    REAL ARR   KMIN           min layer op depth none
C    REAL ARR   KMAX           max layer op depth none


!INPUT/OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    none


!RETURN VALUES:
C    none


!PARENT(S):
C    FITFTC


!ROUTINES CALLED:
C    CHECK2: checks channel data for consistency across profiles
C    KEFF: compute effective optical depths


!FILES ACCESSED:
C    INPUT:
C       IOIN: conv data files.


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine RDCHAN reads all the data for one channel and computes
C    the effective optical depths, which is passed back to the
C    calling program.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    13 Jan 97 Scott Hannon      Created
C    19 Aug 99 Scott Hannon      Deleted SRF info from data files reads
C    25 Aug 99 Scott Hannon      Re-arranged order of data in files and
C                                   in TAULTS
C     7 Sep 99 Scott Hannon      Add IDCHAN to output
C    19 Dec 07 Scott Hannon      Add LANGLE and NUANG


!END ===================================================================


C      =================================================================
       SUBROUTINE RDCHAN(ICHAN, IOIN, ISORTK, NUANG, NDTPTS, NNEG,
     $    IDCHAN, FREQ, KLTOS, KLAYER, KZMIN, KZMAX, KMIN, KMAX)
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
       INTEGER  ICHAN
       INTEGER   IOIN(MAXPRO)
       INTEGER ISORTK(MAXBOG)
C
C      Output parameters
       INTEGER  NUANG(MAXLAY,NUMGAS)
       INTEGER NDTPTS(MAXLAY,NUMGAS)
       INTEGER   NNEG(MAXLAY,NUMGAS)
       INTEGER IDCHAN
       REAL   FREQ
       REAL  KLTOS(MAXPRO,MAXANG,MAXLAY,NUMGAS)
       REAL KLAYER(MAXPRO,MAXANG,MAXLAY,NUMGAS)
       REAL  KZMIN(MAXLAY,NUMGAS)
       REAL  KZMAX(MAXLAY,NUMGAS) 
       REAL   KMIN(MAXLAY,NUMGAS)
       REAL   KMAX(MAXLAY,NUMGAS)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER   IPRO
       INTEGER   ILAY
       INTEGER   IGAS
       INTEGER   IANG
       INTEGER IDCCHK
C
       REAL FRECHK
       REAL TAULTS(MAXANG,MAXLAY,NUMSET)
       REAL    RES
       REAL RESCHK
       REAL RNFCHK
       REAL RNFWHM

       LOGICAL LANGLE(MAXANG,MAXLAY,NUMGAS)

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
C      Initialize the no. of data pts and min/max optical depths
       DO IGAS=1,NUMGAS
          DO ILAY=1,MAXLAY
             NUANG(ILAY,IGAS)=0
             NDTPTS(ILAY,IGAS)=0
             NNEG(ILAY,IGAS)=0
C
             KZMIN(ILAY,IGAS)=1.0E+16
             KZMAX(ILAY,IGAS)=0.0E+0
             KMIN(ILAY,IGAS)=1.0E+16
             KMAX(ILAY,IGAS)=0.0E+0

             DO IANG=1,MAXANG
                LANGLE(IANG,ILAY,IGAS)=.FALSE.
             ENDDO

          ENDDO
       ENDDO
C
C      --------------------------------------
C      Read in data for the first profile and
C      calc the effective optical depths.
C      --------------------------------------
       IPRO=1
C
       READ(IOIN(IPRO)) FRECHK, IDCCHK, RESCHK, RNFCHK,
     $    (((TAULTS(IANG,ILAY,IGAS),IANG=1,MAXANG),ILAY=1,MAXLAY),
     $    IGAS=1,NUMSET)
C
       CALL KEFF(IPRO, ISORTK, TAULTS, KLTOS, KLAYER, NDTPTS, NNEG,
     $    KZMIN, KZMAX, KMIN, KMAX, LANGLE)
C
C      ----------------------------
C      Loop over the other profiles
C      ----------------------------
       DO IPRO=2,MAXPRO
C
C         Read the data for the curent profile
          READ(IOIN(IPRO)) FREQ, IDCHAN, RES, RNFWHM,
     $       (((TAULTS(IANG,ILAY,IGAS),IANG=1,MAXANG),ILAY=1,MAXLAY),
     $       IGAS=1,NUMSET)
C
C         Check the data for consistency WRT the channel
          CALL CHECK2(ICHAN, IPRO, IDCCHK, FRECHK, RESCHK, RNFCHK,
     $       IDCHAN, FREQ, RES, RNFWHM)
C
C         Calculate  the effective optical depths
          CALL KEFF(IPRO, ISORTK, TAULTS, KLTOS, KLAYER, NDTPTS, NNEG,
     $       KZMIN, KZMAX, KMIN, KMAX, LANGLE)
C
       ENDDO
C      End loop over profiles
C
C      Plug in default values for KMIN, KMAX, KZMIN, KZMAX if there
C      are no data points in the current layer.
       DO IGAS=1,NUMGAS
          IF (NDTPTS(1,IGAS) .EQ. 0) THEN
             KZMIN(1,IGAS)=1.0E-7
             KZMAX(1,IGAS)=1.0E-7
             KMIN(1,IGAS)=1.0E-7
             KMAX(1,IGAS)=1.0E-7
          ELSE
             DO IANG=1,MAXANG
                IF (LANGLE(IANG,1,IGAS)) THEN
                   NUANG(1,IGAS)=NUANG(1,IGAS) + 1
                ENDIF
             ENDDO
          ENDIF
          DO ILAY=2,MAXLAY
             IF (NDTPTS(ILAY,IGAS) .EQ. 0) THEN
                KZMIN(ILAY,IGAS)=KZMIN(ILAY-1,IGAS)
                KZMAX(ILAY,IGAS)=KZMAX(ILAY-1,IGAS)
                KMIN(ILAY,IGAS)=1.0E-7
                KMAX(ILAY,IGAS)=1.0E-7
             ELSE
                DO IANG=1,MAXANG
                   IF (LANGLE(IANG,ILAY,IGAS)) THEN
                      NUANG(ILAY,IGAS)=NUANG(ILAY,IGAS) + 1
                   ENDIF
                ENDDO
             ENDIF
          ENDDO
       ENDDO
C

ccc
c       if (ICHAN .EQ. 1) then
c       write(ioerr,2010) ICHAN
c 2010  format('ichan=',I5,': nuang(ilay,1)')
c       do ILAY=1,MAXLAY
c       write(ioerr,2011) ILAY, NUANG(ILAY,1)
c 2011  format(I3,1X,I2)
c       enddo
c       endif
ccc

       RETURN
       END
