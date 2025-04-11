C=======================================================================
C=======================================================================
C
C
C        UNIVERSITY OF MARYLAND BALTIMORE COUNTY (UMBC)
C
C        AIRS
C
C        OPCONV
C
C
!F77====================================================================


!ROUTINE NAME:
C    OPCONV


!ABSTRACT: 
C    Opens a convolved layer-to-space transmittance data file for one
C    of the regression data set profiles and reads the header info.


!CALL PROTOCOL:
C    OPCONV(IPROF, IOUN, NCHAN, IDGAS, SECANG, TEMP, AMOUNT)


!INPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INTEGER    IPROF          Profile no.        none
C    INTEGER    IOUN           File I/O unit no.  none


!OUTPUT PARAMETERS:
C    type       name           purpose            units
C    ---------- -------------- ------------------ ----------------------
C    INTEGER    NCHAN          No. of channels    none
C    INT ARR    IDGAS          ID no.s of gases   none (HITRAN mol ID)
C    REAL ARR   SECANG         Angle secants      none
C    REAL ARR   TEMP           Prof temperature   Kelvin
C    REAL ARR   AMOUNT         Prof nadir amount  k.mol/cm^2


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
C       I/O Unit 5 (keyboard) : enter filename
C    OUTPUT:
C       I/O Unit 6 (screen) : prompt for filename
C       I/O unit IOERR : error messages (if any)
C       I/O Unit IOUN : convolved layer-to-space transmittance file


!COMMON BLOCKS:
C    none


!DESCRIPTION:
C    Routine OPCONV asks for a filename of a convolved layer-to-space
C    transmittance file. It then opens the file (unit number IOUN) and
C    reads the header info. It checks some of the header info to see
C    that it agrees with the values assigned in the INCLUDE file
C    FARRAY. If they disagree, the program writes out an error message
C    and stops. Otherwise some of the header info is passed back to
C    the calling program. The file openned remains open.


!ALGORITHM REFERENCES:
C    none


!KNOWN BUGS AND LIMITATIONS:
C    none


!ROUTINE HISTORY:
C    Date      Programmer      Comments
C    --------- --------------- -----------------------------------------
C    11 Dec 96 Scott Hannon    Created
C    25 Aug 99 Scott Hannon    Change order of data in 1st header record
C                                 and no longer reverse order of layers
C    21 May 02 Scott Hannon    Error messages now use IOERR (was 6)


!END ===================================================================


C      =================================================================
       SUBROUTINE  OPCONV(IPROF, IOUN, NCHAN, IDGAS, SECANG, TEMP,
     $    AMOUNT)
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
       INTEGER  IPROF
       INTEGER   IOUN
C
C      Output parameters
       INTEGER  NCHAN
       INTEGER  IDGAS(NUMSET)
       REAL SECANG(MAXANG)
       REAL   TEMP(MAXLAY)
       REAL AMOUNT(MAXLAY,NUMSET)


C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C-----------------------------------------------------------------------
       INTEGER      I
       INTEGER   IERR
       INTEGER      J
       INTEGER   NANG
       INTEGER   NGAS
       INTEGER   NLAY
C
       CHARACTER*79   FNAM
       CHARACTER*40  TITLE


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
C      Ask for filename
       WRITE(6,1010) IPROF, MAXPRO
 1010  FORMAT('Enter name of convolved trans file ',I2,' of ',I2)
       READ(5,9000) FNAM
 9000  FORMAT(A79)
C
C      Open the file and make sure it openned OK
       OPEN(UNIT=IOUN,FILE=FNAM,STATUS='OLD',FORM='UNFORMATTED',
     $ IOSTAT=IERR)
       IF (IERR .NE. 0) THEN
          WRITE(IOERR,1020) IERR,FNAM
 1020     FORMAT('Error ',I4,' opening file:',/,A79)
          STOP
       ENDIF
C
C      Read the first header record in the file
C old order       READ(IOUN) NLAY, NGAS, NANG, NCHAN
       READ(IOUN) NANG, NLAY, NGAS, NCHAN
C
C      Check that NLAY, NGAS, NANG are what was expected
       IF (NLAY .NE. MAXLAY) THEN
          WRITE(IOERR,1030) NLAY, MAXLAY, FNAM
 1030     FORMAT('Error : file has ',I4,' layers, expecting ',I4,
     $       ', file:',/,A79)
          STOP
       ENDIF
       IF (NGAS .NE. NUMSET) THEN
          WRITE(IOERR,1040) NGAS, NUMSET, FNAM
 1040     FORMAT('Error : file has ',I4,' gases, expecting ',I4,
     $       ', file:',/,A79)
          STOP
       ENDIF
       IF (NANG .NE. MAXANG) THEN
          WRITE(IOERR,1050) NANG, MAXANG, FNAM
 1050     FORMAT('Error : file has ',I4,' angles, expecting ',I4,
     $       ', file:',/,A79)
          STOP
       ENDIF
C
C      Read the ID's of the gases
       READ(IOUN) (IDGAS(I),I=1,NUMSET)
C
C      Read the angle secants
       READ(IOUN) (SECANG(I),I=1,MAXANG)
C
C      Read the title and show it on the screen
       READ(IOUN) TITLE
       WRITE(6,1060) TITLE
 1060  FORMAT('Title of profile <',A40,'>.')
C
C      Read the temperature
C old order       READ(IOUN) (TEMP(I),I=MAXLAY,1,-1)
       READ(IOUN) (TEMP(I),I=1,MAXLAY)
C
C      Read profile amount for each gas
       DO I=1,NUMSET
C old order          READ(IOUN) (AMOUNT(J,I),J=MAXLAY,1,-1)
          READ(IOUN) (AMOUNT(J,I),J=1,MAXLAY)
       ENDDO

C
       RETURN
       END
