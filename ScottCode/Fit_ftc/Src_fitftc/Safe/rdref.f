C=======================================================================
C=======================================================================
C
C             UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C             AIRS
C
C             RDREF
C
!F77====================================================================


!ROUTINE NAME:
C    RDREF


!ABSTRACT:
C    Read the reference profile.


!CALL PROTOCOL:
C    RDREF(IOUN, LSTGAS, PREF, TREF, AMTREF)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INTEGER    IOUN           I/O unit no.       none
C    INT ARR    LSTGAS         gas IDs for fit    none


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    REAL ARR   PREF           Pressure           atm
C    REAL ARR   TREF           Temperature        K
C    REAL ARR   AMTREF         Layer gas amounts  k.mol/cm^2


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
C    INPUT:
C        I/O unit 5 (keyboard): enter filename
C        I/O unit IOUN: reference file (text)
C    OUTPUT:
C        I/O unit 6 (screen): prompt for filename; error messages


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine RDREF reads in the refernce profile.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer        Comments
C    --------- ----------------- ---------------------------------------
C    13 Dec 96 Scott Hannon      Created
C    26 Aug 99 Scott Hannon      Added LSTGAS & IDFIX stuff
C    21 May 02 Scott Hannon      Error messages now use IOERR (was 6)


!END ===================================================================


C      =================================================================
       SUBROUTINE RDREF(IOUN, LSTGAS, PREF, TREF, AMTREF)
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
       INTEGER   IOUN
       INTEGER LSTGAS(MAXBOG)
C
C      Output parameters
       REAL   PREF(MAXLAY)
       REAL   TREF(MAXLAY)
       REAL AMTREF(MAXLAY,NUMGAS)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER   IERR
       INTEGER   IGAS
       INTEGER   ILAY
       INTEGER   LNUM
C
       REAL    ALT
       REAL  AMNTC
       REAL  AMNTF
       REAL  AMNTM
       REAL  AMNTO
       REAL  AMNTW
       REAL  THICK
C
       CHARACTER*79   FNAM
       CHARACTER*130  CLINE


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
C      Ask for the reference profile filename
       WRITE(6,1010)
 1010  FORMAT('Enter the name of the reference profile')
       READ(5,9000) FNAM
 9000  FORMAT(A79)
C
C      Open the file
       OPEN(UNIT=IOUN,FILE=FNAM,STATUS='OLD',FORM='FORMATTED',
     $    IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(IOERR,1020) IERR, FNAM
 1020     FORMAT('Error ',I5,' openning file:',/,A79)
          STOP
       ENDIF
C
C      Initialize loop counter
C      Note: the ref prof entries are ordered from the ground up,
C      and so must be read in in reverse order.
       ILAY=MAXLAY+1
C
C      --------------------------------------------
C      Read the reference profile file (while loop)
C      --------------------------------------------
 20    READ(IOUN,9010,END=90) CLINE
 9010  FORMAT(A130)
C
C      Skip comment lines
       IF (CLINE(1:1) .EQ. '!') THEN
          GOTO 20
       ELSE
C
C         Note: the first non-comment line is the title (ignore)
C
          IF (ILAY .LT. MAXLAY+1) THEN
C            Read a line of data from the file
C
C            The FCOW reference profile is arranged: layer#, altitude,
C            thickness, pressure, temperature, F,W,O,C. The order of
C            the gases in AMTREF should be F,C,O,W.
             READ(CLINE,*) LNUM, ALT, THICK, PREF(ILAY), TREF(ILAY),
     $          AMNTF, AMNTW, AMNTO, AMNTC, AMNTM

             DO IGAS=1,NUMGAS
                IF (LSTGAS(IGAS) .EQ. IDFIX) AMTREF(ILAY,IGAS)=AMNTF
                IF (LSTGAS(IGAS) .EQ. 1)     AMTREF(ILAY,IGAS)=AMNTW
                IF (LSTGAS(IGAS) .EQ. 3)     AMTREF(ILAY,IGAS)=AMNTO
                IF (LSTGAS(IGAS) .EQ. 5)     AMTREF(ILAY,IGAS)=AMNTC
                IF (LSTGAS(IGAS) .EQ. 6)     AMTREF(ILAY,IGAS)=AMNTM
             ENDDO
C
C            Check LNUM to see it is what was expected
             IF (MAXLAY + 1 - LNUM .NE. ILAY) THEN
                WRITE(IOERR,1030) LNUM, MAXLAY+1-ILAY, FNAM
 1030           FORMAT('Error, unexpected layer number in',
     $             'reference profile.',/,'Read ref layer ',I3,
     $             ', expecting ',I3,'. Ref profile file:',/,A79)
                STOP
             ENDIF
          ENDIF
C
C         Increment the layer/loop counter
          ILAY=ILAY - 1
C
       ENDIF
       IF (ILAY .GT. 0) GOTO 20
C
 90    CLOSE(IOUN)
C
       IF (ILAY .NE. 0) THEN
          WRITE(IOERR,1050) ILAY, MAXLAY, FNAM
 1050     FORMAT('Error, unexpected number of layers in reference',
     $       ' profile.',/,'Read ',I3,' layers, expecting ',I3,
     $       '. Ref profile file:',/,A79)
          STOP
       ENDIF
C
       RETURN
       END
