C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             FITFTC_fwo
C
!F77====================================================================


!ROUTINE NAME:
C    FITFTC

!ABSTRACT:
C    Do a fit (regression) to determine fast transmittance
C    coefficients.
C    This version for fitting gases: Fixed, Water, and Ozone


!CALL PROTOCOL:
C    main program


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    main program


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    main program


!INPUT/OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    main program


!RETURN VALUES:
C    none


!PARENT(S):
C    main program


!ROUTINES CALLED:
C    PREP: does the housekeeping preperation
C    RDCHAN: read a channel of convolved transmittance data


!FILES ACCESSED:
C    none (except thorough routines)


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Skeleton driver for the routines. Program FITFTC does a
C    regression on effective layer optical depths to determine the
C    the fast transmittance coefficients.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    25 Jun 97 Scott Hannon      Created from fitftc_fwo
C    26 Aug 99 Scott Hannon      Add LSTGAS; made generic for all fits
C                                of F,W,O in any order.
C     7 Sep 99 Scott Hannon      Add IDCHAN to RDCHAN & output file


!END ===================================================================


C      =================================================================
       PROGRAM FITFTC
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
C    main program


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
c       INTEGER   IANG
       INTEGER  ICHAN
c       INTEGER   IGAS
       INTEGER   ILAY
       INTEGER  IPRED
       INTEGER   IPRO
C
C      For PREP
       INTEGER   IOIN(MAXPRO)
       INTEGER  IOOUT
       INTEGER  NCHAN
       INTEGER LSTGAS(MAXBOG)
       INTEGER ISORTK(MAXBOG)
       INTEGER IOFFST(NUMGAS)
       REAL   PRED(MAXPRD,MAXPRO,MAXANG,MAXLAY)
       REAL SECANG(MAXANG)
C
C      For RDCHAN
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
C
C      For FITF
       REAL FTCOEF(MAXPRD,MAXLAY)


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
C      ---------------------------------------
C      Open files and calculate the predictors
C      ---------------------------------------
C
       CALL PREP(IOIN, IOOUT, NCHAN, LSTGAS, ISORTK, IOFFST, PRED,
     $    SECANG)
      write(6,*) 'done with prep'
      write(6,*) 'lstgas=',lstgas
      write(6,*) 'isortk=',isortk
C
C      ----------------------
C      Loop over the channels
C      ----------------------
       DO ICHAN=1,NCHAN
C
C         Prepare the regression data for this channel
C
      write(6,*) 'ichan=',ichan
C
          CALL RDCHAN(ICHAN, IOIN, ISORTK, NDTPTS, NNEG, IDCHAN,
     $       FREQ, KLTOS, KLAYER, KZMIN, KZMAX, KMIN, KMAX)
C
      write(6,*) 'fixed'
          CALL FITF(LSTGAS, ICHAN, NDTPTS, NNEG, IOFFST, PRED, KLTOS,
     $       KLAYER, KZMIN, KZMAX, KMIN, KMAX, SECANG, FTCOEF)
C
      write(6,*) 'water'
          CALL FITW(LSTGAS, ICHAN, NDTPTS, NNEG, IOFFST, PRED, KLTOS,
     $       KLAYER, KZMIN, KZMAX, KMIN, KMAX, SECANG, FTCOEF)
C
      write(6,*) 'ozone'
          CALL FITO(LSTGAS, ICHAN, NDTPTS, NNEG, IOFFST, PRED, KLTOS,
     $       KLAYER, KZMIN, KZMAX, KMIN, KMAX, SECANG, FTCOEF)
C
          WRITE(IOOUT) IDCHAN, FREQ,
     $       ((FTCOEF(IPRED,ILAY),IPRED=1,MAXPRD),ILAY=1,MAXLAY)
C
       ENDDO
C      End loop over channels
C
C      Close all the files
       CLOSE(IOOUT)
       DO IPRO=1,MAXPRO
          CLOSE(IOIN(IPRO))
       ENDDO
C
       STOP
       END
